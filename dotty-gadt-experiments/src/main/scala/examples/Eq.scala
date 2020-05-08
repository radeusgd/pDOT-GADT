package examples

sealed trait =:=[A,B]
def refl[A]: A =:= A = new =:=[A,A] {}
