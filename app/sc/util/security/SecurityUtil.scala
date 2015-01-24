package sc.util.security

import scala.util.Random

object SecurityUtil {

	def makeToken(len: Int) = {
		(new Random).alphanumeric.take(len).mkString
	}

	private val mdSHA = java.security.MessageDigest.getInstance("SHA-256")
	private val SALT_SHA = "sc"

	def sha(s: String): String = {
		// add a salt. can replace salt with generated salt value
		val v = SALT_SHA + s

		// return encoded value
		new sun.misc.BASE64Encoder().encode(mdSHA.digest(v.getBytes("UTF-8")))
	}
}
