package msandin.vial

  abstract sealed class Path
  object Path {
    case class LocalName(name :Name) extends Path
    case class ExternalName(domain :Name, name :Name) extends Path
    case class Access(path :Path, name :Name) extends Path
  }