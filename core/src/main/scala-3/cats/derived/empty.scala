package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.annotation.*

object empty extends EmptyDerivation

opaque type DerivedEmpty[A] = Empty[A]

object DerivedEmpty extends DerivedEmptyInstances:
  type Of[A] = Alt[Empty[A], DerivedEmpty[A]]
  def apply[A](e: Empty[A]): DerivedEmpty[A] = e

  extension[A](de: DerivedEmpty[A])
    def empty: Empty[A] = de
trait DerivedEmptyInstances:
  import DerivedEmpty.Of

  given product[A](using inst: K0.ProductInstances[Of, A]): DerivedEmpty[A] =
    DerivedEmpty(Empty(inst.unifyUnsafe.construct([A] => (A: Empty[A]) => A.empty)))

  inline given coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    DerivedEmpty(K0.summonFirst[Of, gen.MirroredElemTypes, A].unifyUnsafe)

trait EmptyDerivation:
  extension (E: Empty.type)
    def derived[A](using instance: DerivedEmpty[A]): Empty[A] = instance.empty

import empty._
    
final case class Foo(a: Int) derives Empty

@main def run = println(Empty[Foo].empty)