package sc.util.fmt.string

import play.api.Logger
import scala.Left
import scala.Right

object Implicits {

	implicit class StringParserable(str: String) {

		def asOpt[T](implicit parser: Parser[T]): Option[T] = {
			parser.parse(str).fold(
				invalid => {
					Logger.warn(s"convert failed on $str to Int", invalid)
					None
				},
				valid => Some(valid))
		}
	}

	trait Parser[A] {
		def parse(input: String): Either[Exception, A]
	}

	implicit object IntParser extends Parser[Int] {
		override def parse(input: String): Either[Exception, Int] = {
			try {
				Right(Integer.parseInt(input))
			} catch {
				case ex: NumberFormatException => Left(ex)
			}
		}
	}
}
