package sc.util.http

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import play.api.libs.json.Json
import play.api.mvc.Request
import sc.models.Auth
import sc.models.Auth.tokenFmt
import sc.models.Token

class AuthActionHelper[T](val appendProfile: Auth => T = {auth:Auth => auth})(implicit exec: ExecutionContext) extends ActionHelper {
	override def sessionAction = new SessionAction with Anonymousable with Cookieable {

		override def anonymousUid: Option[(UserSession, AnonymousUserCookie)] = {
			val anonymousUser = Auth.createAnonymousUser
			appendProfile(anonymousUser)
			Some(UserSession(anonymousUser.uid), AnonymousUserCookie(anonymousUser.token.tokenId))
		}

		override def findByCookie[T]: Request[T] => Future[Option[UserSession]] = request => {
			request.cookies.get(SessionAction.KEY_COOKIE_TID)
				.orElse(request.cookies.get(SessionAction.KEY_COOKIE_ANONYMOUS_TID))
				.map { tid =>
					Auth.find(Json.toJson(Token(tid.value, true, 0))).map {
						_.map(user => UserSession(user.uid))
					}
				}.getOrElse {
					Future.successful(None)
				}
		}
	}

}
