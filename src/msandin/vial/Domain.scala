package msandin.vial

case class Domain[C](imports :Seq[Import], definitions :Seq[Def[C]])

case class Import(name :Name)

case class Def[C](name :Name, limit :Limit[C])

abstract sealed class DefFlag
case object ValueFlag extends DefFlag