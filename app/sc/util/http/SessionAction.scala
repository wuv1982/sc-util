package sc.util.http

import play.api.mvc._
import scala.concurrent.Future
import sc.ma.Json._
import sc.util.fmt.MessageShorter._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.Logger
import sc.models.Oid

trait AnonymousSpec {
	def anonymousUid: Future[Option[(UserSession, AnonymousUserCookie)]] = Future.successful(None)
}

trait CookieSpec {
	def findByCookie[A]: Request[A] => Future[Option[UserSession]] = request => Future.successful(None)
}

case class UserSession(uuid: String) {
	def save: Result => Result = response => {
		Logger.info(s"save user session [uuid]")
		response.withSession(SessionAction.KEY_SESSION_UUID -> uuid)
	}
}

case class UserCookie(uuid: String) {
	val COOKIE_EXPIRE_10_YEAR: Option[Int] = Some(60 * 60 * 24 * 365 * 10)

	def save: Result => Result = response => {
		Logger.info(s"save user cookie [uuid]")
		response.withCookies(
			Cookie(SessionAction.KEY_COOKIE_UUID, uuid, COOKIE_EXPIRE_10_YEAR))
	}
}

case class AnonymousUserCookie(uuid: String) {
	val COOKIE_EXPIRE_10_YEAR: Option[Int] = Some(60 * 60 * 24 * 365 * 10)

	def save: Result => Result = response => {
		Logger.info(s"save anonymous user cookie [uuid]")
		response.withCookies(
			Cookie(SessionAction.KEY_COOKIE_ANONYMOUS_UUID, uuid, COOKIE_EXPIRE_10_YEAR))
	}
}

case class SessionRequest[A](userSession: UserSession, request: Request[A]) extends WrappedRequest[A](request)

object SessionAction {
	val KEY_SESSION_UUID: String = "sc_sid"
	val KEY_COOKIE_UUID: String = "sc_cid"
	val KEY_COOKIE_ANONYMOUS_UUID: String = "sc_anonymous_cid"

	def removeUserCookie: Result => Result = response => {
		Logger.info(s"remove user cookie")
		response.discardingCookies(
			DiscardingCookie(KEY_COOKIE_UUID))
	}
}

trait SessionAction extends ActionBuilder[SessionRequest] {
	self: AnonymousSpec with CookieSpec =>

	override def invokeBlock[A](request: Request[A], block: (SessionRequest[A]) => Future[Result]) = {
		authorize(
			onSuccess = userSession => block(SessionRequest(userSession, request)))(request)
	}

	def onUnauthorized[A]: Request[A] => Future[Result] = { _ =>
		Future.successful(Results.Unauthorized($("msg" -> M("e.user.unauthorized"))))
	}

	private def authorize[A](
		onSuccess: UserSession => Future[Result])(implicit request: Request[A]): Future[Result] = {

		request.session.get(SessionAction.KEY_SESSION_UUID).map { authId =>
			Logger.debug(s"in session [$authId]")
			onSuccess(UserSession(authId))
		}.getOrElse {
			findByCookie(request).flatMap {
				case Some(userSession) =>
					Logger.debug(s"found by cookied")
					onSuccess(userSession)
						.map(userSession.save)
				case None =>
					Logger.debug(s"not found by cookied.")
					anonymousUid.flatMap {
						case Some((userSession, userCookie)) =>
							Logger.info("create anonymous user")
							onSuccess(userSession)
								.map(userSession.save andThen userCookie.save)
						case _ => {
							Logger.warn("unauthorized user")
							onUnauthorized(request)
						}
					}
			}
		}
	}
}
