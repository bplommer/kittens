package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.annotation.*
import scala.compiletime.*

object empty extends EmptyDerivation

opaque type Derived[A] = A

object Derived:
  def apply[A](e: A): Derived[A] = e
  
  extension[A](derived: Derived[A])
    def dealias: A = derived

  opaque type Or[A] = A
  object Or extends OrInstances:
    def from[A](a: A): Or[A] = a
    def fromDerived[A](a: Derived[A]): Or[A] = a
    extension[A](or: Or[A])
      def dealias: A = or

trait OrInstances:
  inline given [A]: Derived.Or[A] = summonFrom {
    case a: A => Derived.Or.from(a)
    case da: Derived[A] => Derived.Or.fromDerived(da)
  }

type DerivedEmpty[A] = Derived[Empty[A]]

object DerivedEmpty extends DerivedEmptyInstances:
  type Of[A] = Derived.Or[Empty[A]]

trait DerivedEmptyInstances:
  import DerivedEmpty.Of

  given product[A](using inst: K0.ProductInstances[Of, A]): DerivedEmpty[A] =
    Derived(Empty(inst.construct([A] => (_: Of[A]).dealias.empty)))

  inline given coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    Derived(K0.summonFirst[Of, gen.MirroredElemTypes, A].dealias)

trait EmptyDerivation:
  extension (E: Empty.type)
    inline def derived[A]: Empty[A] = 
      import DerivedEmpty.given
      summonInline[DerivedEmpty[A]].dealias

import empty._
    
final case class Foo(a: Int) derives Empty

@main def run = println(Empty[Foo].empty)