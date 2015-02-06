package sc.util.fmt

import java.util.Date

object DateFmt {

	def toYMD: String = {
		toYMD(new Date)
	}

	def toYMD(date: Date): String = {
		"%tY/%<tm/%<td" format date
	}
}
