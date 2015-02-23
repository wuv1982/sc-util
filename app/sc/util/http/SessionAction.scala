package sc.util.http

import play.api.mvc._
import scala.concurrent.Future
import sc.ma.Json._
import sc.util.fmt.MessageShorter._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.Logger

trait Anonymousable {
	def anonymousUid: Future[Option[(UserSession, AnonymousUserCookie)]] = Future.successful(None)
}

trait Cookieable {
	def findByCookie[A]: Request[A] => Future[Option[UserSession]] = request => Future.successful(None)
}

case class UserSession(uid: String) {
	def save: Result => Result = response => {
		response.withSession(SessionAction.KEY_SESSION_OID -> uid)
	}
}

case class UserCookie(tokenId: String) {
	val COOKIE_EXPIRE_10_YEAR: Option[Int] = Some(60 * 60 * 24 * 365 * 10)

	def save: Result => Result = response => {
		response.withCookies(
			Cookie(SessionAction.KEY_COOKIE_TID, tokenId, COOKIE_EXPIRE_10_YEAR))
	}
}

case class AnonymousUserCookie(tokenId: String) {
	val COOKIE_EXPIRE_10_YEAR: Option[Int] = Some(60 * 60 * 24 * 365 * 10)

	def save: Result => Result = response => {
		response.withCookies(
			Cookie(SessionAction.KEY_COOKIE_ANONYMOUS_TID, tokenId, COOKIE_EXPIRE_10_YEAR))
	}
}

case class SessionRequest[A](userSession: UserSession, request: Request[A]) extends WrappedRequest[A](request)

object SessionAction {
	val KEY_SESSION_OID: String = "oid"
	val KEY_COOKIE_TID: String = "tid"
	val KEY_COOKIE_ANONYMOUS_TID: String = "anonymous_tid"

	def removeUserCookie: Result => Result = response => {
		response.discardingCookies(
			DiscardingCookie(KEY_COOKIE_TID))
	}
}

trait SessionAction extends ActionBuilder[SessionRequest] {
	self: Anonymousable with Cookieable =>

	override def invokeBlock[A](request: Request[A], block: (SessionRequest[A]) => Future[Result]) = {
		authorize(
			onSuccess = userSession => block(SessionRequest(userSession, request)))(request)
	}

	def onUnauthorized[A]: Request[A] => Future[Result] = { _ =>
		Future.successful(Results.Unauthorized($("msg" -> M("e.user.unauthorized"))))
	}

	private def authorize[A](
		onSuccess: UserSession => Future[Result])(implicit request: Request[A]): Future[Result] = {

		request.session.get(SessionAction.KEY_SESSION_OID).map { oid =>
			Logger.debug(s"in session [$oid]")
			onSuccess(UserSession(oid))
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
