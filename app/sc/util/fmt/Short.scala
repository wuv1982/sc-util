package sc.util.fmt

import play.api.i18n.Lang
import play.api.i18n.Messages

object MessageShorter {
	def M(msgCode: String, args: Any*)(implicit lang: Lang) = {
		Messages(msgCode, args: _*)(lang)
	}
}