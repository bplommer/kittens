package cats.derived

import scala.compiletime.*

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