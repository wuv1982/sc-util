package sc.util.http

import play.api.mvc._
import play.api.mvc.BodyParsers.parse
import scala.concurrent.Future
import play.api.libs.json.JsValue
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Logger
import play.api.mvc.Results._
import play.api.libs.json.Json
import sc.util.fmt.MessageShorter._
import play.api.libs.json.Reads
import play.api.libs.json.Format
import play.api.libs.json.JsError

trait ActionHelper {

	def sessionAction: SessionAction

	def asyncJson[T](
		f: T => Request[JsValue] => Future[Either[String, JsValue]],
		r: T => JsValue => Result = { form: T => rs: JsValue => Ok(rs) })(implicit validator: Format[T]): Action[JsValue] = {
		Action.async(parse.json) { request =>
			invokeRequest(f, r)(validator)(request)
		}
	}

	def async[A](parser: BodyParser[A])(
		f: Request[A] => Future[Either[String, JsValue]],
		r: JsValue => Result = { rs => Ok(rs) }): Action[A] = {
		Action.async(parser) { request =>
			invokeRequest(f, r)(request)
		}
	}

	def asyncAny(
		f: Request[AnyContent] => Future[Either[String, JsValue]],
		r: JsValue => Result = { rs => Ok(rs) }): Action[AnyContent] = {
		async(parse.anyContent)(f, r)
	}

	def asyncSessionJson[T](
		f: UserSession => T => Request[JsValue] => Future[Either[String, JsValue]],
		r: T => JsValue => Result = { form: T => rs: JsValue => Ok(rs) })(implicit validator: Format[T]): Action[JsValue] = {

		sessionAction.async(parse.json) { sessionRequest =>
			invokeRequest(f(sessionRequest.userSession), r)(validator)(sessionRequest.request)
		}
	}

	def asyncSession[A](parser: BodyParser[A])(
		f: UserSession => Request[A] => Future[Either[String, JsValue]],
		r: JsValue => Result = { rs => Ok(rs) }): Action[A] = {
		sessionAction.async(parser) { sessionRequest =>
			invokeRequest(f(sessionRequest.userSession), r)(sessionRequest.request)
		}
	}

	def asyncSessionAny(
		f: UserSession => Request[AnyContent] => Future[Either[String, JsValue]],
		r: JsValue => Result = { rs => Ok(rs) }): Action[AnyContent] = {
		asyncSession(parse.anyContent)(f, r)
	}

	private def invokeRequest[T](
		f: Request[T] => Future[Either[String, JsValue]],
		r: JsValue => Result = { rs => Ok(rs) }): Request[T] => Future[Result] = request => {
		f(request).map {
			case Left(failure) => {
				Logger.warn(s"Forbidden [403] : $failure")
				Forbidden(Json.obj("msg" -> failure))
			}
			case Right(success) => {
				Logger.info(s"Ok [200]: ${Json.prettyPrint(success)}")
				r(success)
			}
		}.recover {
			case ex: Throwable =>
				Logger.error("System Error [500]", ex)
				InternalServerError(Json.obj("msg" -> M("e.system")))
		}
	}

	private def invokeRequest[T](
		f: T => Request[JsValue] => Future[Either[String, JsValue]],
		r: T => JsValue => Result)(implicit validator: Format[T]): Request[JsValue] => Future[Result] = request => {

		validator.reads(request.body).fold(
			invalid => {
				val msg = JsError.toFlatJson(invalid)
				Logger.warn(s"BadRequest [400]: $msg")
				Future.successful(BadRequest(Json.obj("msg" -> msg)))
			}, valid => {
				invokeRequest(f(valid), r(valid))(request)
			})
	}

}