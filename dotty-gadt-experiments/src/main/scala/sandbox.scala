object Sandbox {

  trait T {
    type A
    val x: A
  }
  def f(v: T): v.A = v.x

  trait A {
    def g(v: Int): String
  }
  trait B {
    val x: Int
  }
  def f(obj: A & B): String = obj.g(obj.x)

  class AB extends A with B {
    def g(v: Int): String = v.toString
    val x: Int = 42
  }

  def ab_example(): Unit = {
    f(new AB)
  }

  def self_type(): Unit = {
    val x = 5
    def f(v: x.type) = ???
    f(x)
    // f(5) // Type Mismatch

    trait T { self =>
      val x: self.type
    }
    val t = new T { val x = this }
  }

  def polymorhpism(): Unit = {
    {
      def id[A](x: A): A = x
      id[Int](42)
    }
    {
      trait TypeLabel { type Typ }
      def id(T: TypeLabel)(x: T.Typ): T.Typ = x
      id(new TypeLabel { type Typ = Int })(42)
    }
  }
}
