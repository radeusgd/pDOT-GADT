package encodings

object Intersections {

  sealed trait Expr { self =>
    type A
    def visit[R](lit: LitC & self.type => R, plus: PlusC & self.type => R, pair: PairC & self.type => R): R
  }

  trait LitC extends Expr { self =>
    type A = Int
    val x: Int
    def visit[R](lit: LitC & self.type => R, plus: PlusC & self.type => R, pair: PairC & self.type => R): R = lit(this)
  }
  trait PlusC extends Expr { self =>
    type A = Int
    val lhs: Expr { type A = Int }
    val rhs: Expr { type A = Int }
    def visit[R](lit: LitC & self.type => R, plus: PlusC & self.type => R, pair: PairC & self.type => R): R = plus(this)
  }
  trait PairC extends Expr { self =>
    type B
    type C
    type A = (B,C)
    val lhs: Expr { type A = B }
    val rhs: Expr { type A = C }
    def visit[R](lit: LitC & self.type => R, plus: PlusC & self.type => R, pair: PairC & self.type => R): R = pair(this)
  }

  def Lit(_x: Int): Expr { type A = Int } = new LitC {
    val x = _x
  }
  def Plus(_lhs: Expr { type A = Int }, _rhs: Expr { type A = Int }): Expr { type A = Int } = new PlusC {
    val lhs = _lhs
    val rhs = _rhs
  }
  def Pair[B_,C_](_lhs: Expr { type A = B_ }, _rhs: Expr { type A = C_ }): Expr { type A = (B_,C_) } = new PairC {
    type B = B_
    type C = C_
    val lhs = _lhs
    val rhs = _rhs
  }

  def eval[R](e: Expr { type A = R }): R = e.visit[R](
    lit  = c => c.x : c.A,
    plus = c => eval(c.lhs) + eval(c.rhs): c.A,
    pair = c => {
      (eval(c.lhs), eval(c.rhs)) : c.A
    }
  )

  def evalH(e: Expr): e.A = eval(e)

  def usage(): Unit = {
    val e: Expr { type A = (Int, Int) } = Pair(Plus(Lit(40), Lit(2)), Lit(23))
    val r: (Int, Int) = evalH(e)
    println(r)
  }

  //println(evalHelper()
}
