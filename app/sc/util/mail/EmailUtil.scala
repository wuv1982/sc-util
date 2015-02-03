package sc.util.mail

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import play.api.Logger
import com.typesafe.config.ConfigFactory
import org.apache.commons.mail.HtmlEmail

case class EmailTemplate(
	from: (String, String), //(address, nickname)
	to: Seq[String],
	subject: String,
	content: String) {

	def render: String = {
		// TODO email html template
		views.html.util.email.render(from._2, subject, content).toString
	}
}

object EmailProvider {

	private[this] object Emailconf {
		val conf = ConfigFactory.load()
		val host = conf.getString("mail.smtp.host")
		val port = conf.getInt("mail.smtp.port")
		val uid = conf.getString("mail.smtp.uid")
		val passwd = conf.getString("mail.smtp.passwd")
		val CHARSET: String = "utf-8"
	}

	def send(email: EmailTemplate)(implicit executionCtx: ExecutionContext): Future[Unit] = {

		Future {
			val e = new HtmlEmail()
			e.setHostName(Emailconf.host)
			e.setSmtpPort(Emailconf.port)
			e.setCharset(Emailconf.CHARSET)

			e.setSSLOnConnect(true)
			e.setAuthentication(Emailconf.uid, Emailconf.passwd)
			e.setFrom(email.from._1, email.from._2)

			e.addTo(email.to: _*)
			e.setSubject(email.subject)
			e.setHtmlMsg(email.content)

			e.send()
			Logger.info(s"send email[${email.subject}] to ${email.to}")

		}.recoverWith {
			case ex: Throwable =>
				Logger.error(s"email[${email.subject}] sent to ${email.to} failed!", ex)
				Future.failed(ex)
		}
	}
}
