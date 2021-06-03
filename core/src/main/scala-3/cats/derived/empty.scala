package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.compiletime.*

object empty extends EmptyDerivation
object DerivedEmpty:
  type Of[A] = Derived.Or[Empty[A]]

  given product[A](using inst: K0.ProductInstances[Of, A]): Derived[Empty[A]] =
    Derived(Empty(inst.construct([A] => (_: Of[A]).dealias.empty)))

  inline given coproduct[A](using gen: K0.CoproductGeneric[A]): Derived[Empty[A]] =
    Derived(K0.summonFirst[Of, gen.MirroredElemTypes, A].dealias)

trait EmptyDerivation:
  extension (E: Empty.type)
    inline def derived[A]: Empty[A] = 
      import DerivedEmpty.given
      summonInline[Derived[Empty[A]]].dealias




import empty._
    
final case class Foo(a: Int) derives Empty

@main def run = println(Empty[Foo].empty)