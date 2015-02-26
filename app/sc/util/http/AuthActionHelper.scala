package sc.util.http

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.Application
import play.api.libs.json.Json
import play.api.mvc.Request
import sc.models.Auth
import sc.models.Auth.tokenFmt
import sc.models.Token
import play.api.Logger

class AuthActionHelper[T](
	val appendProfile: Auth => Future[T] = { auth: Auth => Future.successful(Unit) })(
		implicit exec: ExecutionContext) extends ActionHelper {

	override def sessionAction = new SessionAction with Anonymousable with Cookieable {

		override def anonymousUid: Future[Option[(UserSession, AnonymousUserCookie)]] = {
			Auth.createAnonymousUser.flatMap { anonymousUser =>
				appendProfile(anonymousUser).map { _ =>
					Some(UserSession(anonymousUser._id.$oid), AnonymousUserCookie(anonymousUser.token.tokenId))
				}
			}.recover {
				case ex =>
					Logger.error("failed to create anonymous user")
					None
			}
		}

		override def findByCookie[T]: Request[T] => Future[Option[UserSession]] = request => {
			request.cookies.get(SessionAction.KEY_COOKIE_TID)
				.orElse(request.cookies.get(SessionAction.KEY_COOKIE_ANONYMOUS_TID))
				.map { tid =>
					Auth.find(Json.obj("token" -> Token(tid.value, true, 0))).map {
						_.headOption.map(user => UserSession(user._id.$oid))
					}
				}.getOrElse {
					Future.successful(None)
				}
		}
	}

}
