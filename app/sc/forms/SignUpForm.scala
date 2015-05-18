package sc.forms

import play.api.libs.json.Format
import play.api.libs.json.Json

case class SignUpForm(
	uid: String,
	password: String,
	redirect: Option[String])

object SignUpForm {
	implicit val signUpFormFmt: Format[SignUpForm] = Json.format[SignUpForm]
}
