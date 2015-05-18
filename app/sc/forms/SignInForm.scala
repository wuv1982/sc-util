package sc.forms

import play.api.libs.json.Format
import play.api.libs.json.Json

case class SignInForm(
	uid: String,
	password: String,
	allowCookie:Option[Boolean],
	redirect: Option[String])

object SignInForm {
	implicit val signUpFormFmt: Format[SignInForm] = Json.format[SignInForm]
}
