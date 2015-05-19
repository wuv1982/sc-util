package sc.util.fmt.string

import play.api.Logger
import scala.Left
import scala.Right

case class StringParseException(msg:String) extends Exception

object Implicits {

	implicit class StringParserable(str: String) {

		def asOpt[T](implicit parser: Parser[T]): Option[T] = {
			parser.parse(str).fold(
				invalid => {
					Logger.warn(s"failed on $str parse", invalid)
					None
				},
				valid => Some(valid))
		}
	}

	trait Parser[A] {
		def parse(input: String): Either[StringParseException, A]
	}

	implicit object IntParser extends Parser[Int] {
		override def parse(input: String): Either[StringParseException, Int] = {
			try {
				Right(Integer.parseInt(input))
			} catch {
				case ex: NumberFormatException => Left(StringParseException(s"${this.getClass} failed on $input"))
			}
		}
	}

	implicit object BooleanParser extends Parser[Boolean] {
		override def parse(input: String): Either[StringParseException, Boolean] = {
			input match {
				case "true" => Right(true)
				case "false" => Right(false)
				case _ => Left(StringParseException(s"${this.getClass} failed on $input"))
			}
		}
	}
}
