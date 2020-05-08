package encodings

object Evidences {
  trait Eq[A,B] {
    type T >: B <: A
    type F >: A <: B
  }

  case class Refl[A]() extends Eq[A,A] {
    type T = A
    type F = A
  }

  sealed trait Expr[A]
  case class LitC[A](x: Int, ev: Eq[A, Int]) extends Expr[A]
  case class PlusC[A](lhs: Expr[Int], rhs: Expr[Int], ev: Eq[A, Int]) extends Expr[A]
  case class PairC[A,B,C](lhs: Expr[B], rhs: Expr[C], ev: Eq[A, (B,C)]) extends Expr[A]

  def Lit(x: Int): Expr[Int] = LitC(x, Refl())
  def Plus(lhs: Expr[Int], rhs: Expr[Int]): Expr[Int] = PlusC(lhs, rhs, Refl())
  def Pair[B,C](lhs: Expr[B], rhs: Expr[C]): Expr[(B,C)] = PairC(lhs, rhs, Refl())

  def eval[A](e: Expr[A]): A = e match {
    case LitC(x, ev) => x: ev.T
    case PlusC(lhs, rhs, ev) => eval(lhs) + eval(rhs): ev.T
    case PairC(lhs, rhs, ev) => (eval(lhs), eval(rhs)): ev.T
  }

  def usage(): Unit = println(eval(Pair(Plus(Lit(40), Lit(2)), Lit(23))))

  trait Contradiction {
    type C >: String <: Int
  }

  // object X extends Contradiction // error: object X cannot be instantiated since it has a member C with possibly conflicting bounds String <: ... <: Int

  def lista[A,B](lst: List[A], ev: Eq[A, B]): List[B] = lst: List[ev.F]
}
