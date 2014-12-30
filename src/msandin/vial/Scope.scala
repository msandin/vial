package msandin.vial

 
class Scopes[N, A, B] {
    type S = Scope[N, B]
    type F = (S, N, A) => B
	def empty(f :F) = new LazyScope[N, A, B](None, Map.empty, f)
	def ofMap(f :F)(outer :Option[Scope[N, B]], content :Map[N, A]) :S =
	  new LazyScope[N, A, B](outer, content, f)
	def ofSeq(f :F, outer :Option[Scope[N, B]], content :Seq[(N, A)]) :S =
	  new LazyScope[N, A, B](outer, content.foldLeft(Map.empty[N, A])((index, tuple) => index + tuple), f)
}



trait Scope[N, T] {  
  def lookup(name :N) :Option[T]
}

class LazyScope[N, A, B](outer :Option[Scope[N, B]], index :Map[N, A], f :(Scope[N, B], N, A) => B) extends Scope[N, B] {
  
  def lookup(name :N) :Option[B] =
    index.get(name).map(x => f(this, name, x)).orElse(outer.flatMap(_.lookup(name)))
    
}
