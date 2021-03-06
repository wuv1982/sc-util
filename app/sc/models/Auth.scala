package sc.models

import scala.Left
import scala.Right
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.Logger
import play.api.libs.json.Format
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Controller
import play.api.mvc.Result
import play.api.mvc.Results.Ok
import play.api.mvc.Results.Redirect
import play.api.cache.Cache
import play.api.Application
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import sc.forms.SignInForm
import sc.forms.SignUpForm
import sc.util.db.DBHelper
import sc.util.fmt.MessageShorter.M
import sc.util.http.ActionHelper
import sc.util.http.SessionAction
import sc.util.http.UserCookie
import sc.util.http.UserSession
import sc.ma.Json._
import sc.util.mail.EmailProvider
import sc.util.mail.EmailTemplate
import sc.util.security.SecurityUtil
import sc.util.http.AuthActionHelper
import sc.util.http.OnActionResult
import com.typesafe.config.ConfigFactory

case class Auth(
	_id: Oid, // sequenced index id
	uuid: String, // unreadable index id
	uid: String, // readable index id
	email: Option[String],
	password: String,
	token: Token,
	avaliable: Boolean,
	role: Int) extends ModelEntity[Auth] {

	override def collection = Auth.collection
}

case class Token(
	tokenId: String = SecurityUtil.makeRandom(32),
	requestCode: String = SecurityUtil.makeRandom(8),
	expireDate: Long = 0)

case class HisAuth(
	_id: Oid = Oid.newOid,
	timestamp: Long = System.currentTimeMillis(),
	authId: Oid,
	uid: String,
	email: Option[String]) extends ModelEntity[HisAuth] {

	override def collection = Auth.hisAuth
}

object Auth extends ModelQuery[Auth] {
	override val collection = DBHelper.getCollection("userAuth")

	lazy val hisAuth = DBHelper.getCollection("hisAuth")
	implicit val DURATION_EMAIL_VERIFY_EXPIRE = 1000 * 60 * 60 * 24 * 3

	implicit val tokenFmt: Format[Token] = Json.format[Token]
	implicit val authFmt: Format[Auth] = Json.format[Auth]
	implicit val hisAuthFmt: Format[HisAuth] = Json.format[HisAuth]

	val AUTH_ROLE_NORMAL: Int = 500
	val AUTH_ROLE_UNVERIFY: Int = 200
	val AUTH_ROLE_ANONYMOUS: Int = 100
	val AUTH_ROLE_ADMIN: Int = 900

	def createAnonymousUser(implicit exec: ExecutionContext): Future[Auth] = {
		val anonymous = Auth(
			Oid.newOid,
			SecurityUtil.makeUUID,
			SecurityUtil.makeRandom(8),
			None,
			SecurityUtil.sha("anonymouspwd"),
			Token(),
			true,
			AUTH_ROLE_ANONYMOUS)
		anonymous.save.map { _ =>
			anonymous
		}
	}

	def sighUp(actionHelper: ActionHelper)(
		appendProfile: Auth => Future[_] = { _ => Future.successful(Unit) })(
			implicit exec: ExecutionContext): Action[JsValue] = {
		actionHelper.asyncJson[SignUpForm, Auth](signUpForm => request => {
			find(Json.toJson(signUpForm.copy(redirect = None))).flatMap {
				case userList if userList.isEmpty => {
					for {
						user <- Future.successful {
							Auth(
								Oid.newOid,
								SecurityUtil.makeUUID,
								signUpForm.uid,
								Some(signUpForm.uid),
								SecurityUtil.sha(signUpForm.password),
								Token(
									expireDate = System.currentTimeMillis() + Auth.DURATION_EMAIL_VERIFY_EXPIRE),
								false,
								AUTH_ROLE_NORMAL)
						}

						_ <- user.save

						_ <- appendProfile(user)

						_ <- user.email.map { email =>
							val conf = ConfigFactory.load()
							val host = conf.getString("auth.host")

							EmailProvider.send(
								EmailTemplate(("support@wishbank.jp", M("m.email.from")),
									Seq(email),
									M("m.email.subject"),
									M("m.email.content", user.token.tokenId, user.uuid, host)))
						}.getOrElse(Future.successful(Unit))

						_ <- HisAuth(authId = user._id, uid = user.uid, email = user.email).save
					} yield {
						Right(user)
					}
				}
				case _ =>
					Future.successful(Left(M("e.user.duplicated")))
			}
		},
			signUpForm => OnActionResult { request =>
				auth =>
					signUpForm.redirect.map { redirectUrl =>
						Logger.info(s"redirect : $redirectUrl")
						Ok($("redirectUrl" -> redirectUrl))
					}.getOrElse {
						Ok
					}

			})
	}

	def signIn(actionHelper: ActionHelper)(
		implicit exec: ExecutionContext, app: Application): Action[JsValue] = {
		actionHelper.asyncJson[SignInForm, Auth](
			signInForm => request => {
				val selector = $(
					"uid" -> signInForm.uid,
					"password" -> SecurityUtil.sha(signInForm.password),
					"avaliable" -> true)

				find(selector).map {
					case Seq(user) => Right(user)
					case _ => Left(M("e.user.avaliable"))
				}
			},

			signInForm => OnActionResult { req =>
				auth => {
					signInForm.redirect.map { redirectUrl =>
						val callbackUrl = s"$redirectUrl?id=${auth._id.$oid}&requestCode=${auth.token.requestCode}&allowCookie=${signInForm.allowCookie.getOrElse(false)}"
						Logger.debug(s"callbackUrl = $callbackUrl")
						Ok($("redirectUrl" -> callbackUrl))

					}.getOrElse {

						val maySaveCookie = if (signInForm.allowCookie.getOrElse(false)) {
							UserCookie(auth.uuid).save
						} else {
							identity[Result] _
						}

						val jsAuth = Json.toJson(auth)

						val pruneToken = (__ \ 'token).json.prune
						val prunePassword = (__ \ 'password).json.prune
						val jsRs = jsAuth.transform(pruneToken andThen prunePassword).asOpt.getOrElse(jsAuth)

						UserSession(auth.uuid).save andThen maySaveCookie apply Ok(jsRs)
					}
				}
			})
	}

	def signOut(actionHelper: ActionHelper)(
		implicit exec: ExecutionContext, app: Application): Action[AnyContent] = {
		actionHelper.asyncSessionAny[JsValue](
			userSession => request => {
				Future.successful {
					Right($("msg" -> M("i.user.signout")))
				}
			},
			{
				OnActionResult { _ => js => SessionAction.removeUserCookie(Ok(js)).withNewSession }
			})
	}

	def verify(actionHelper: ActionHelper)(
		implicit exec: ExecutionContext, app: Application): Action[AnyContent] = {
		actionHelper.asyncAny[JsValue](request => {
			(for {
				uuid <- request.getQueryString("uuid")
				tokenId <- request.getQueryString("tokenId")
			} yield {
				find($("uuid" -> uuid,
					"token.tokenId" -> tokenId,
					"token.expireDate" -> $("$gt" -> System.currentTimeMillis())))
					.flatMap {
						case Seq(user) =>
							user.copy(
								token = Token(),
								avaliable = true)
								.save
								.map { _ => Right($("msg" -> M("i.user.verify"))) }

						case _ => Future.successful(Left(M("e.user.avaliable")))
					}
			}).getOrElse {
				Future.successful(Left(M("e.user.verify")))
			}
		},
			OnActionResult { request =>
				msg =>
					Ok("/verify/success")

			},
			OnActionResult { request =>
				error =>
					Ok("/verify/failed")
			})
	}

}
