package encodings

object Native {
  enum Expr[A] {
    case Lit(x: Int) extends Expr[Int]
        case Plus(lhs: Expr[Int], rhs: Expr[Int]) extends Expr[Int]
        case Pair[B,C](lhs: Expr[B], rhs: Expr[C]) extends Expr[(B, C)]
  }

  import Expr._
  def eval[A](e: Expr[A]): A = e match {
    case Lit(x) => x
    case Plus(lhs, rhs) => eval(lhs) + eval(rhs)
    case Pair(lhs, rhs) => (eval(lhs), eval(rhs))
  }

  def usage(): Unit = println(eval(Pair(Plus(Lit(40), Lit(2)), Lit(23))))
}
