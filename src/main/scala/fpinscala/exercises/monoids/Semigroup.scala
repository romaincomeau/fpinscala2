package fpinscala.exercises.monoids

trait Semigroup[A]:
  def combine(x: A, y: A): A
