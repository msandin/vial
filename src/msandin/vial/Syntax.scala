package msandin.vial

case class Name(text :String)

object Syntax {

  
  // path
  abstract sealed class Path
  object Path {
    case class Role(name :Name) extends Path
    case class ExternalRole(domainName :Name, name :Name) extends Path
    case class Access(path :Path, name :Name) extends Path
  }

  // constraints
  sealed abstract class Cstr[I]
  object Cstr {
    case class Role[I](id :I) extends Cstr[I]
    case class Apply[I](id :I, args :Seq[(Name, Cstr[I])]) extends Cstr[I]
  }
  
  // expressions
  sealed abstract class Expr[I]
  
  // domains
  case class Domain(name :Name, imports :Seq[Import], defines :Seq[Define])
  case class Define(name :Name, cstr :Cstr[Path], expr :Expr[Path])
  
  // imports
  case class Import(name :Name)
  
}

