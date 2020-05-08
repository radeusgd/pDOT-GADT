package examples
import Nats._

sealed trait Vector[A, Len <: Nat]
final case class VNil[A]() extends Vector[A, _0]
final case class VCons[A, N <: Nat](head: A, tail: Vector[A, N]) extends Vector[A, S[N]]

object Vector {
  def [A,N <: Nat] (h: A) :: (t: Vector[A,N]): Vector[A, S[N]] = VCons(h, t)
  def nil[A]: Vector[A, _0] = VNil()

  def head[A, N <: Nat](v: Vector[A, S[N]]): A = v match {
    case VCons(h, _) => h
  }

  def tail[A, N <: Nat](v: Vector[A, S[N]]): Vector[A,N] = v match {
    case VCons(_, t) => t
  }

  def zipWith[A,B,C, N <: Nat](f: (A, B) => C, a: Vector[A, N], b: Vector[B, N]): Vector[C, N] = (a, b) match {
    case (VNil(), VNil()) => VNil()
    case (VCons(ha, ta), VCons(hb, tb)) => VCons(f(ha, hb), zipWith(f, ta, tb))
  }

  def zip[A,B, N <: Nat](a: Vector[A, N], b: Vector[B, N]): Vector[(A,B), N] = zipWith((x: A, y: B) => (x,y), a, b)

  def map[A,B, N <: Nat](f: A => B, v: Vector[A, N]): Vector[B, N] = v match {
    case VNil() => VNil()
    case VCons(h, t) => VCons(f(h), map(f, t))
  }

  def toList[A,N <: Nat](v: Vector[A, N]): List[A] = v match {
    case VNil() => Nil
    case VCons(h, t) => h :: toList(t)
  }

  // def fromList[A, N <: Nat](lst: List[A], len: N): Option[Vector[A, N]] = len match {
  //   case s: S[n] =>
  //     val ev1: S[n] =:= N = refl
  //     lst match {
  //       case h :: t =>
  //         fromList[A, n](t, s.pred).map((vt: Vector[A, n]) => {
  //           VCons(h, vt)
  //         })
  //       case _ => None
  //     }
  //   case z: Z =>
  //     val ev1: Z =:= N = refl
  //     lst match {
  //       case Nil => Some(VNil())
  //       case _ => None
  //     }
  // }

  // def concat[A, N1 <: Nat, N2 <: Nat](v1: Vector[A, N1], v2: Vector[A, N2]): Vector[A, Plus[N1, N2]] = v1 match {
  //   case VNil() => v2
  //   case cons : VCons[a, n] =>
  //     val h = cons.head
  //     val t = cons.tail
  //     val rest: Vector[A, Plus[n, N2]] = concat(t, v2)
  //     val ev1: S[Plus[n, N2]] =:= Plus[S[n], N2] = refl
  //     val vec = (VCons(h, concat(t, v2)) : Vector[A, S[Plus[n, N2]]]) : Vector[A, Plus[S[n], N2]]
  //     val ev2: S[n] =:= N1 = refl
  //     val ev3: Plus[S[n], N2] =:= Plus[N1, N2] = refl // this one fails
  //     vec // this fails as Plus is expanded too eagerly
  // }

  object Tests {
    val vv = zip(map((x: Int) => 10*x, 1 :: 2 :: 3 :: nil), 4 :: 5 :: 6 :: nil)
    // val err = zip(1 :: 2 :: 3 :: nil, 4 :: 5 :: nil)
  }
}
