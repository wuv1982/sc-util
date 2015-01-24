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

	private[this] val conf = ConfigFactory.load()
	private[this] val host = conf.getString("mail.smtp.host")
	private[this] val port = conf.getInt("mail.smtp.port")
	private[this] val uid = conf.getString("mail.smtp.uid")
	private[this] val passwd = conf.getString("mail.smtp.passwd")
	private[this] val CHARSET: String = "utf-8"

	def send(email: EmailTemplate)(implicit executionCtx: ExecutionContext): Future[Unit] = {

		Future {
			val e = new HtmlEmail()
			e.setHostName(host)
			e.setSmtpPort(port)
			e.setCharset(CHARSET)

			e.setSSLOnConnect(true)
			e.setAuthentication(uid, passwd)
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
