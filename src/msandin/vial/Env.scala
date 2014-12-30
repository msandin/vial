package msandin.vial

class Ref[C](val env :Env[C], val content :C)


trait Env[C] {
  
  object ContentScope extends Scopes[Name, C, Ref[C]]

  def scope :ContentScope.S
  
  def lookup(path :Path) :Option[Ref[C]]
  
  
  def resolveDomain(domain :Domain[Path]) :Option[Domain[Ref[C]]] = {
    // this is a bit more... complicated... because at this point we need to construct a
    // env within which we should resolve the defs... which is kinds circular
    return Some(Domain[Ref[C]](domain.imports, null))
  }
  
  def resolveDef(definition :Def[Path]) :Option[Def[Ref[C]]] =
    this.resolveLimit(definition.limit).map(limit => Def(definition.name, limit))
          
  def resolveLimit(limit :Limit[Path]) :Option[Limit[Ref[C]]] = 
    limit match {
      case RefLimit(path) => lookup(path).map(r => RefLimit(r))
      case ApplyLimit(path, args) =>
        for (pathRef <- lookup(path)) yield {
	        def combine(result :Option[List[(Ref[C], Ref[C])]], arg :(Path, Path)) = 
	          for(prefix <- result;
	              argNameRef <- pathRef.env.lookup(arg._1);
	              argPathRef <- lookup(arg._2))
	          yield prefix :+ (argNameRef, argPathRef)       
	        return args.foldLeft(Some(List.empty) :Option[List[(Ref[C], Ref[C])]])(combine)
	                   .map(x => ApplyLimit(pathRef, x))
	    }
    }
  
}


class ExternalEnv[C](domain :Env[C]) extends Env[C] {
  
  def scope :ContentScope.S = domain.scope
  
  def lookup(path :Path) :Option[Ref[C]] = 
    domain.lookup(path).map(ref => new Ref(this, ref.content))  
    
}
	
class LocalEnv[C](outer :Option[Env[C]], lookupExternal :(Name, Name) => Option[Ref[C]], content :Seq[(Name, C)])
	extends Env[C] {
    
  val scope :ContentScope.S =
    ContentScope.ofSeq(
        (s, n, c) => new Ref(LocalEnv.this, c),
        outer.map(_.scope), content)
    
  def lookup(path :Path) :Option[Ref[C]] = path match {
    case Path.LocalName(name) => lookup(name)
    case Path.ExternalName(domain, name) =>
      lookupExternal(domain, name)
      .map(ref => new Ref[C](new ExternalEnv(ref.env), ref.content))    
    case Path.Access(prefix, name) =>
      lookup(prefix).flatMap(_.env.lookup(Path.LocalName(name)))
  }
  
  private def lookup(name :Name) :Option[Ref[C]] = scope.lookup(name)
  
}