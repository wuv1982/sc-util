package sc.models

import scala.Left
import scala.Right
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import play.api.libs.json.Format
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Controller
import play.api.cache.Cache
import play.api.Application
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import sc.forms.SignInForm
import sc.forms.SignUpForm
import sc.util.db.DBHelper
import sc.util.fmt.Short.M
import sc.util.http.AuthActionHelper
import sc.util.http.SessionAction
import sc.util.http.UserCookie
import sc.util.http.UserSession
import sc.ma.Json._
import sc.util.mail.EmailProvider
import sc.util.mail.EmailTemplate
import sc.util.security.SecurityUtil

case class Auth(
	_id: Oid,
	uid: String,
	email: Option[String],
	password: String,
	token: Token,
	avaliable: Boolean,
	role: String) extends Model[Auth] {

	override def collection = AuthDB.auth

	override def save(implicit exec: ExecutionContext, format: Format[Auth]): Future[Boolean] = {
		super.save.map { rs =>
			if (rs) {
				import play.api.Play.current
				Cache.set(uid, this)
			}
			rs
		}
	}
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
	email: Option[String]) extends Model[HisAuth] {

	override def collection = AuthDB.hisAuth
}

object Auth {
	implicit val DATE_EXPIRE_DURATION = 1000 * 60 * 60 * 3

	implicit val tokenFmt: Format[Token] = Json.format[Token]
	implicit val authFmt: Format[Auth] = Json.format[Auth]
	implicit val hisAuthFmt: Format[HisAuth] = Json.format[HisAuth]

	val AUTH_ROLE_NORMAL: String = "normal"
	val AUTH_ROLE_ANONYMOUS: String = "anonymous"
	val AUTH_ROLE_ADMIN: String = "admin"

	def createAnonymousUser(implicit exec: ExecutionContext): Auth = {
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
		anonymous.save
		anonymous
	}

	def getCachedAuth(uid: String)(implicit exec: ExecutionContext, app: Application): Future[Option[Auth]] = {
		Cache.getAs[Auth](uid)
			.map(auth => Future.successful(Some(auth)))
			.getOrElse(find($("uid" -> uid)))
	}

	def find(selector: JsValue)(implicit exec: ExecutionContext): Future[Option[Auth]] = {
		import play.api.Play.current

		AuthDB.query(AuthDB.auth)(selector, $()).map {
			_.flatMap { rs =>
				Auth.authFmt.reads(rs.head).asOpt.map { auth =>
					Cache.set(auth.uid, auth)
					auth
				}
			}
		}
	}

	def sighUp[K, T <: AuthActionHelper[K]](actionHelper: T)(implicit exec: ExecutionContext): Action[JsValue] = {
		actionHelper.asyncJson[SignUpForm](signUpForm => request => {
			find(Json.toJson(signUpForm)).flatMap {
				case None => {
					val user = Auth(
						Oid.newOid,
						signUpForm.uid,
						Some(signUpForm.uid),
						SecurityUtil.sha(signUpForm.password),
						Token(),
						false,
						AUTH_ROLE_NORMAL)
					user.save.map { _ =>

						user.email.map { email =>
							EmailProvider.send(
								EmailTemplate(("accounts@sheo.com", M("m.email.from")),
									Seq(email),
									M("m.email.subject"),
									M("m.email.content")))
						}

						HisAuth(authId = user._id, uid = user.uid, email = user.email).save

						Right(Json.toJson(user))
					}
				}
				case _ =>
					Future.successful(Left(M("e.user.duplicated")))
			}
		})
	}

	def signIn(actionHelper: AuthActionHelper[_])(implicit exec: ExecutionContext): Action[JsValue] = {
		actionHelper.asyncJson[SignInForm](signInForm => request => {
			val selector = $(
				"uid" -> signInForm.uid,
				"password" -> SecurityUtil.sha(signInForm.password),
				"available" -> true)
			find(selector).map {
				case Some(user) => Right(Json.toJson(user))
				case None => Left(M("e.user.deny"))
			}
		}, signInForm => {
			case (jsAuth, response) => {
				val tokenId = (jsAuth \ "token" \ "tokenId").as[String]
				UserSession(signInForm.uid).save andThen UserCookie(tokenId).save apply response
			}
		})
	}

	def signOut(actionHelper: AuthActionHelper[_])(implicit exec: ExecutionContext): Action[AnyContent] = {
		actionHelper.asyncSessionAny(userSession => request => {
			Future.successful {
				Right($("msg" -> M("i.user.signout")))
			}
		}, {
			case (_, response) => {
				SessionAction.removeUserCookie(response).withNewSession
			}
		})
	}

	def verify(actionHelper: AuthActionHelper[_])(implicit exec: ExecutionContext): Action[AnyContent] = {
		actionHelper.asyncAny { request =>
			request.getQueryString("tokenId").map { tokenId =>
				find($("token.tokenId" -> tokenId)).flatMap {
					case Some(user) =>
						user.copy(
							token = Token(verified = true, expireDate = 0),
							avaliable = true)
							.save
							.map { _ => Right($("msg" -> M("i.user.verified"))) }

					case None => Future.successful(Left(M("e.user.verified")))
				}
			}.getOrElse(Future.successful(Left(M("e.user.verified"))))
		}
	}

}

object AuthDB extends DBHelper with Controller with MongoController {
	lazy val auth = db.collection[JSONCollection]("auth")
	lazy val hisAuth = db.collection[JSONCollection]("hisAuth")
}
