package examples

object Nats {

  sealed trait Nat
  final case class Z() extends Nat
  final case class S[N <: Nat](pred: N) extends Nat

  type _0 = Z
  type _1 = S[Z]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
  type _6 = S[_5]
  type _7 = S[_6]
  type _8 = S[_7]
  type _9 = S[_8]

  implicit def zero: Z = Z()
  implicit def succ[N <: Nat](implicit p: N): S[N] = S[N](p)

  type Plus[A <: Nat, B <: Nat] <: Nat = A match {
    case Z => B
    case S[n] => S[Plus[n, B]]
  }

  type ++[A <: Nat, B <: Nat] = Plus[A,B]

  type Mul[A <: Nat, B <: Nat] <: Nat = A match {
    case Z => Z
    case S[Z] => B
    case S[S[n]] => Plus[Mul[S[n], B], B]
  }
  type **[A <: Nat, B <: Nat] = Mul[A,B]

  object Tests {
    val p1: _0 ++ _1 =:= _1 = refl
    val p2: _1 ++ _1 =:= _2 = refl
    val p3: _3 ++ _4 =:= _7 = refl
    // val p4: _1 ++ _1] =:= _7 = refl // this of course fails to compile

    val m0: _3 ** _0 =:= _0 = refl
    val m1: _2 ** _2 =:= _4 = refl
    val m2: _3 ** _3 =:= _9 = refl
    val m3: _3 ** _2 =:= _6 = refl
    // val m4: _3 ** _2 =:= _7 = refl

    val n3 = implicitly[_3]

    def plus[A <: Nat, B <: Nat]: S[A ++ B] =:= S[A] ++ B = refl
  }
}
