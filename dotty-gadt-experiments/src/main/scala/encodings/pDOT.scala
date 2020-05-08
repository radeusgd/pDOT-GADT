package encodings

object pDOT {

  trait TL { type T }

  sealed trait Expr { self =>
    type A
    def visit(R: TL)(lit: LitC & self.type => R.T,
      plus: PlusC & self.type => R.T,
      pair: PairC & self.type => R.T): R.T
  }

  trait LitC extends Expr { self =>
    type A = Int
    def x: Int
    def visit(R: TL)(lit: LitC & self.type => R.T,
      plus: PlusC & self.type => R.T,
      pair: PairC & self.type => R.T): R.T = lit(this)
  }
  trait PlusC extends Expr { self =>
    type A = Int
    def lhs: Expr & { type A = Int }
    def rhs: Expr & { type A = Int }
    def visit(R: TL)(lit: LitC & self.type => R.T,
      plus: PlusC & self.type => R.T,
      pair: PairC & self.type => R.T): R.T = plus(this)
  }
  trait PairC extends Expr { self =>
    type B
    type C
    type A = (B,C)
    def lhs: Expr & { type A = B }
    def rhs: Expr & { type A = C }
    def visit(R: TL)(lit: LitC & self.type => R.T,
      plus: PlusC & self.type => R.T,
      pair: PairC & self.type => R.T): R.T = pair(this)
  }

  def Lit(_x: Int): Expr & { type A = Int } = new LitC {
    def x = _x
  }
  def Plus(_lhs: Expr & { type A = Int }, _rhs: Expr & { type A = Int }): Expr & { type A = Int } = new PlusC {
    def lhs = _lhs
    def rhs = _rhs
  }
  def Pair(BT: TL, CT: TL)(_lhs: Expr & { type A = BT.T }, _rhs: Expr & { type A = CT.T }): Expr & { type A = (BT.T,CT.T) } = new PairC {
    type B = BT.T
    type C = CT.T
    def lhs = _lhs
    def rhs = _rhs
  }

  def eval(R: TL)(e: Expr & { type A = R.T }): R.T = e.visit(R)(
    lit => lit.x : lit.A,
    plus =>
      val T = new TL { type T = Int }
      eval(T)(plus.lhs) + eval(T)(plus.rhs): plus.A,
    pair => {
      val TB = new TL { type T = pair.B }
      val TC = new TL { type T = pair.C }
      (eval(TB)(pair.lhs), eval(TC)(pair.rhs)) : pair.A
    }
  )

  def evalH(e: Expr): e.A = eval(new TL { type T = e.A })(e)

  def usage(): Unit = {
    val ITL = new TL { type T = Int }
    val e: Expr { type A = (Int, Int) } = Pair(ITL, ITL)(Plus(Lit(40), Lit(2)), Lit(23))
    val r: (Int, Int) = evalH(e)
    println(r)
  }

}
