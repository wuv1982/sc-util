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

case class Auth(
	_id: Oid,
	uid: String,
	email: Option[String],
	password: String,
	token: Token,
	avaliable: Boolean,
	role: Int) extends ModelEntity[Auth] {

	override def collection = Auth.collection
}

case class Token(
	tokenId: String = SecurityUtil.makeToken(32),
	verified: Boolean = false,
	expireDate: Long = System.currentTimeMillis() + Auth.DATE_EXPIRE_DURATION)

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
	implicit val DATE_EXPIRE_DURATION = 1000 * 60 * 60 * 3

	implicit val tokenFmt: Format[Token] = Json.format[Token]
	implicit val authFmt: Format[Auth] = Json.format[Auth]
	implicit val hisAuthFmt: Format[HisAuth] = Json.format[HisAuth]

	val AUTH_ROLE_NORMAL: Int = 2
	val AUTH_ROLE_ANONYMOUS: Int = 1
	val AUTH_ROLE_ADMIN: Int = 9

	def createAnonymousUser(implicit exec: ExecutionContext): Future[Auth] = {
		val anonymous = Auth(
			Oid.newOid,
			SecurityUtil.makeToken(16),
			None,
			SecurityUtil.sha("anonymouspwd"),
			Token(
				verified = true,
				expireDate = 0),
			true,
			AUTH_ROLE_ANONYMOUS)
		anonymous.save.map { _ =>
			anonymous
		}
	}

	def sighUp(actionHelper: ActionHelper)(
		appendProfile: Auth => Future[_] = { _ => Future.successful(Unit) })(
			implicit exec: ExecutionContext): Action[JsValue] = {
		actionHelper.asyncJson[SignUpForm](signUpForm => request => {
			find(Json.toJson(signUpForm)).flatMap {
				case userList if userList.isEmpty => {
					for {
						user <- Future.successful {
							Auth(
								Oid.newOid,
								signUpForm.uid,
								Some(signUpForm.uid),
								SecurityUtil.sha(signUpForm.password),
								Token(),
								false,
								AUTH_ROLE_NORMAL)
						}

						_ <- user.save

						_ <- appendProfile(user)

						_ <- user.email.map { email =>
							EmailProvider.send(
								EmailTemplate(("support@wishbank.jp", M("m.email.from")),
									Seq(email),
									M("m.email.subject"),
									M("m.email.content", user.token.tokenId)))
						}.getOrElse(Future.successful(Unit))

						_ <- HisAuth(authId = user._id, uid = user.uid, email = user.email)
							.save
					} yield {
						Right(Json.toJson(user))
					}
				}
				case _ =>
					Future.successful(Left(M("e.user.duplicated")))
			}
		})
	}

	def signIn(actionHelper: ActionHelper)(
		implicit exec: ExecutionContext, app: Application): Action[JsValue] = {
		actionHelper.asyncJson[SignInForm](
			signInForm => request => {
				val selector = $(
					"uid" -> signInForm.uid,
					"password" -> SecurityUtil.sha(signInForm.password),
					"avaliable" -> true)
				val projection = $(
					"password" -> 0)
				DBHelper.query(collection)(selector, projection).map {
					case Some(user) => Right(Json.toJson(user.head))
					case None => Left(M("e.user.avaliable"))
				}
			},
			signInForm => OnActionResult { req =>
				jsAuth => {
					val maySaveCookie = if (signInForm.allowCookie.getOrElse(false)) {
						(jsAuth \ "token" \ "tokenId").asOpt[String]
							.map { tokenId =>
								UserCookie(tokenId).save
							}
							.getOrElse(identity[Result] _)
					} else {
						identity[Result] _
					}

					val jsPrune = (__ \ 'token).json.prune
					val jsRs = jsAuth.transform(jsPrune).asOpt.getOrElse(jsAuth)

					val oid = (jsAuth \ "_id" \ "$oid").as[String]

					UserSession(oid).save andThen maySaveCookie apply Ok(jsRs)
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
		actionHelper.asyncAny[JsValue] { request =>
			request.getQueryString("tokenId").map { tokenId =>
				find($("token.tokenId" -> tokenId)).flatMap {
					_.headOption.map { user =>
						user.copy(
							token = Token(verified = true, expireDate = 0),
							avaliable = true)
							.save
							.map { _ => Right($("msg" -> M("i.user.verified"))) }

					}.getOrElse(Future.successful(Left(M("e.user.avaliable"))))
				}
			}.getOrElse(Future.successful(Left(M("e.user.verified"))))
		}
	}

}
