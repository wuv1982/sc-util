package sc.util.security

import scala.util.Random
import com.typesafe.config.ConfigFactory
import java.security.MessageDigest
import java.util.UUID

object SecurityUtil {

	def makeRandom(len: Int) = {
		(new Random).alphanumeric.take(len).mkString
	}

	def makeUUID = {
		UUID.randomUUID().toString()
	}

	private lazy val mdSHA: MessageDigest = java.security.MessageDigest.getInstance("SHA-256")

	private lazy val SALT_SHA: String = {
		val config = ConfigFactory.load()
		val keyPath = "application.secret"
		if (config.hasPath(keyPath)) {
			config.getString(keyPath)
		} else {
			"sc"
		}
	}

	def sha(s: String): String = {
		// add a salt. can replace salt with generated salt value
		val v = SALT_SHA + s

		// return encoded value
		new sun.misc.BASE64Encoder().encode(mdSHA.digest(v.getBytes("UTF-8")))
	}
}
