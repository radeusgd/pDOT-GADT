package encodings

object Functions {
  trait Eq[A,B] {
    def to(x: A): B
    def from(y: B): A
  }

  case class Refl[A]() extends Eq[A,A] {
    def to(x: A): A = x
    def from(y: A): A = y
  }

  sealed trait Expr[A]
  case class LitC[A](x: Int, ev: Eq[A, Int]) extends Expr[A]
  case class PlusC[A](lhs: Expr[Int], rhs: Expr[Int], ev: Eq[A, Int]) extends Expr[A]
  case class PairC[A,B,C](lhs: Expr[B], rhs: Expr[C], ev: Eq[A, (B,C)]) extends Expr[A]

  def Lit(x: Int): Expr[Int] = LitC(x, Refl())
  def Plus(lhs: Expr[Int], rhs: Expr[Int]): Expr[Int] = PlusC(lhs, rhs, Refl())
  def Pair[B,C](lhs: Expr[B], rhs: Expr[C]): Expr[(B,C)] = PairC(lhs, rhs, Refl())

  def eval[A](e: Expr[A]): A = e match {
    case LitC(x, ev) => ev.from(x)
    case PlusC(lhs, rhs, ev) => ev.from(eval(lhs) + eval(rhs))
    case PairC(lhs, rhs, ev) => ev.from((eval(lhs), eval(rhs)))
  }

  def usage(): Unit = println(eval(Pair(Plus(Lit(40), Lit(2)), Lit(23))))

  def lista[A,B](lst: List[A], ev: Eq[A, B]): List[B] = lst.map(ev.to)
}
