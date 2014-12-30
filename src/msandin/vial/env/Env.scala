package msandin.vial.env

class Envs {
  
  type Name
  
  abstract sealed class Path
  case class LocalName(name :Name) extends Path
  case class ExternalName(domain :Name, name :Name) extends Path
  case class Access(path :Path, name :Name) extends Path

  
  abstract sealed class Ref[C] {
    def env :Env[C]
    def name :Name
    def deref :C
  } 
  case class LocalRef[C](val env :Env[C], val name :Name) extends Ref[C] {
    def deref :Option[C] = env.lookupLocal(name)
  }
  case class ExternalRef[C](val env :Env[C], val name :Name, val content :C) extends Ref[C] {
    def deref :Option[C] = Some(content)
  }
  
  class Env[C](
    outer :Option[Env[C]],
    makeContent :Env[C] => Seq[(Name, C)],
    lookupExternal :(Name, Name) => Option[Ref[C]]) { 
   
    private val content = 
      makeContent(this)
    
    private val index =
      content.foldLeft(Map.empty[Name, C])((index, tuple) => index + tuple)
  
    def lookupLocal(name :Name) :Option[C] = index.get(name)
    
    def lookup(path :Path) :Option[Ref[C]] = path match {
      case LocalName(name) => Some(LocalRef(this, name))
      case ExternalName(domain, name) =>
        lookupExternal(domain, name)
        .map(ref => new ExternalRef[C](ref.env, ref.name, ref.deref))
      case Access(prefix, name) =>
        lookup(prefix).flatMap(ref => ref.env.lookup(LocalName(name)))
    }
  
  }
  
  
}
