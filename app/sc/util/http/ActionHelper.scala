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
import play.mvc.Http.Response
import play.api.http.MediaRange

trait OnActionResult[A, T] extends Function2[Request[A], T, Result]

object OnActionResult {
	def apply[A, T](f: Request[A] => T => Result): OnActionResult[A, T] = {
		new OnActionResult[A, T] {
			override def apply(req: Request[A], result: T) = f(req)(result)
		}
	}
}

trait ActionHelper {

	val sessionAction: SessionAction

	private def errorResult[A]: OnActionResult[A, (Int, String)] = OnActionResult[A, (Int, String)] { request =>
		{
			case (403, error) => Forbidden(Json.obj("msg" -> error))
			case (401, error) => BadRequest(Json.obj("msg" -> error))
			case (500, error) => InternalServerError(Json.obj("msg" -> error))
			case _ => InternalServerError(Json.obj("msg" -> "unknown error"))
		}
	}

	private def defaultResult[A, T] = OnActionResult[A, T] { request =>
		result =>
			result match {
				case jsResult: JsValue => Ok(jsResult)
				case rep: Result => rep
				case _ => Ok
			}
	}

	def asyncJson[F, T](
		f: F => Request[JsValue] => Future[Either[String, T]],
		r: F => OnActionResult[JsValue, T] = { form: F => defaultResult[JsValue, T] },
		e: OnActionResult[JsValue, (Int, String)] = errorResult)(implicit validator: Format[F]): Action[JsValue] = {
		Action.async(parse.json) { request =>
			invokeValidateRequest(f, r, e)(validator)(request)
		}
	}

	def async[A, T](parser: BodyParser[A])(
		f: Request[A] => Future[Either[String, T]],
		r: OnActionResult[A, T] = defaultResult,
		e: OnActionResult[A, (Int, String)] = errorResult): Action[A] = {
		Action.async(parser) { request =>
			invokeRequest(f, r, e)(request)
		}
	}

	def asyncAny[T](
		f: Request[AnyContent] => Future[Either[String, T]],
		r: OnActionResult[AnyContent, T] = defaultResult[AnyContent, T],
		e: OnActionResult[AnyContent, (Int, String)] = errorResult): Action[AnyContent] = {
		async(parse.anyContent)(f, r, e)
	}

	def asyncSessionJson[F, T](
		f: UserSession => F => Request[JsValue] => Future[Either[String, T]],
		r: F => OnActionResult[JsValue, T] = { form: F => defaultResult[JsValue, T] },
		e: OnActionResult[JsValue, (Int, String)] = errorResult)(implicit validator: Format[F]): Action[JsValue] = {

		sessionAction.async(parse.json) { sessionRequest =>
			invokeValidateRequest(f(sessionRequest.userSession), r, e)(validator)(sessionRequest.request)
		}
	}

	def asyncSession[A, T](parser: BodyParser[A])(
		f: UserSession => Request[A] => Future[Either[String, T]],
		r: OnActionResult[A, T] = defaultResult,
		e: OnActionResult[A, (Int, String)] = errorResult): Action[A] = {
		sessionAction.async(parser) { sessionRequest =>
			invokeRequest(f(sessionRequest.userSession), r, e)(sessionRequest.request)
		}
	}

	def asyncSessionAny[T](
		f: UserSession => Request[AnyContent] => Future[Either[String, T]],
		r: OnActionResult[AnyContent, T] = defaultResult[AnyContent, T],
		e: OnActionResult[AnyContent, (Int, String)] = errorResult): Action[AnyContent] = {
		asyncSession(parse.anyContent)(f, r, e)
	}

	private def invokeRequest[A, T](
		f: Request[A] => Future[Either[String, T]],
		r: OnActionResult[A, T],
		e: OnActionResult[A, (Int, String)]): Request[A] => Future[Result] = request => {
		f(request).map {
			case Left(failure) => {
				Logger.warn(s"Forbidden [403] : $failure")
				e(request, (403, failure))
			}
			case Right(success) => {
				if (success.isInstanceOf[JsValue]) {
					Logger.info(s"Ok [200]: ${Json.prettyPrint(success.asInstanceOf[JsValue])}")
				} else {
					Logger.info("Ok [200]")
				}
				r(request, success)
			}
		}.recover {
			case ex: Throwable =>
				Logger.error("System Error [500]", ex)
				e(request, (500, M("e.system")))
		}
	}

	private def invokeValidateRequest[F, T](
		f: F => Request[JsValue] => Future[Either[String, T]],
		r: F => OnActionResult[JsValue, T],
		e: OnActionResult[JsValue, (Int, String)])(implicit validator: Format[F]): Request[JsValue] => Future[Result] = request => {

		validator.reads(request.body).fold(
			invalid => {
				val error = JsError.toFlatJson(invalid)
				Logger.warn(s"BadRequest [400]: $error")
				Future.successful {
					e(request, (400, error.toString))
				}
			}, valid => {
				invokeRequest(f(valid), r(valid), e)(request)
			})
	}

}
