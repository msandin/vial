package msandin.vial

abstract sealed class Limit[C]

case class RefLimit[C](ref :C) extends Limit[C]
case class ApplyLimit[C](ref :C, args :Seq[(C, C)]) extends Limit[C]

