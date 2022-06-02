package com.evolution.bootcamp.typeclass

/**
 * Try to accomplish as many tasks as you can
 */
object TypeClassesHomework {

  object OrderingTask {

    final case class Money(amount: BigDecimal)

    implicit val moneyOrdering: Ordering[Money] = Ordering.by[Money, BigDecimal](_.amount)
  }

  object ShowTask {

    trait Show[T] { // Fancy toString
      def show(entity: T): String
    }

    final case class User(id: String, name: String)

    implicit val userShow: Show[User] = (entity: User) => entity.toString

    def apply[A](implicit sh: Show[A]) = sh

    object ops {
      implicit class ShowOps[A: Show](a: A) {
        def show = ShowTask[A].show(a)
      }
    }
  }

  object ParseTask {

    type Error = String

    trait Parse[T] { // Feel free to use any format. It could be CSV or anything else.
      def parse(entity: String): Either[Error, T]
    }

    final case class User(id: String, name: String)

    def parseCsvLine(line: String, separator: String = ";"): Option[User] = {
      line.split(separator).map(_.trim) match {
        case Array(id, name) => Some(User(id, name))
        case _ => None
      }
    }

    def apply[T](implicit parse: Parse[T]): Parse[T] = parse

    def parse[T: Parse](entity: String): Either[Error, T] = ParseTask[T].parse(entity)

    object ops {

      implicit class ParseOps[T: Parse](string: String) {
        def parse: Either[Error, T] = ParseTask[T].parse(string)
      }
    }

    implicit val userParse: Parse[User] = (string: String) => {
      parseCsvLine(string).toRight(s"Cannot parse input line: ${string}")
    }
  }

  object EqualsTask {
    trait Equals[T] {
      def tripleEq(a: T, b: T): Boolean
    }

    implicit val intEquals: Equals[Int] = (a: Int, b: Int) => a == b
    implicit val stringEquals: Equals[String] = (a: String, b: String) => a == b

    def apply[T: Equals](implicit eq: Equals[T]) = eq

    object ops {
      implicit class EqualsOps[T: Equals](entity: T) {
        def ===(other: T): Boolean = EqualsTask[T].tripleEq(entity, other)
      }
    }
    // Define the typeclass (think of a method signature)
    // Keep in mind that `a method b` is `a.method(b)`
  }

  object Foldable {

    trait Semigroup[A] {
      def combine(x: A, y: A): A
    }

    object Semigroup {
      def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }

    object Monoid {
      def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
    }

    trait Foldable[F[_]] {
      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

      def foldMap[A, B](as: F[A])(f: A => B)(implicit monoid: Monoid[B]): B
    }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }

    implicit val stringMonoid: Monoid[String] = new Monoid[String] {
      override def empty: String = ""

      override def combine(x: String, y: String): String = x + y
    }

    //    implicit def optionMonoid[A](implicit monoid: Monoid[A]) = new Monoid[Option[A]] {
    implicit def optionMonoid[A: Monoid] = new Monoid[Option[A]] {
      override def empty: Option[A] = Option(Monoid[A].empty)

      override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
        case (Some(a), Some(b)) => Option(Monoid[A].combine(a, b))
        //        case (a, None) => a
        //        case (None, b) => b
        //        case (None, None) => None
        case _ => x.orElse(y)
      }
    }

    implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
        case Some(value) => f(z, value)
        case None => z
      }

      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
        case Some(value) => f(value, z)
        case None => z
      }

      override def foldMap[A, B](as: Option[A])(f: A => B)(implicit monoid: Monoid[B]): B = as match {
        case Some(value) => f(value)
        case None => monoid.empty
      }
    }

    implicit val listFoldable: Foldable[List] = new Foldable[List] {
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldMap[A, B](as: List[A])(f: A => B)(implicit monoid: Monoid[B]): B = as.map(f).fold(monoid.empty)((a, b) => monoid.combine(a, b))
    }

    sealed trait Tree[A]

    object Tree {
      final case class Leaf[A](value: A) extends Tree[A]

      final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    }

    //    implicit val treeFoldable: Foldable[Tree] = ??? // TODO Implement Foldable instance for Tree
  }

  object ApplicativeTask {

    trait Semigroupal[F[_]] {
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    trait Apply[F[_]] extends Functor[F] with Semigroupal[F] {

      def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] // "ap" here stands for "apply" but it's better to avoid using it

      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(map(fa)(a => (b: B) => (a, b)))(fb)

      def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = map(product(fa, fb)) {
        ab => f(ab._1, ab._2)
      }
    }

    trait Applicative[F[_]] extends Apply[F] {
      def pure[A](a: A): F[A]
    }

    // TODO Implement Applicative instantce for Option
    implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
      override def pure[A](a: A): Option[A] = Option(a)

      override def ap[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = fab.flatMap(f => fa.map(f))

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }

    // TODO Implement traverse using `map2`
    def traverse[F[_] : Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = ???

    // TODO Implement sequence (ideally using already defined things)
    def sequence[F[_] : Applicative, A](fas: List[F[A]]): F[List[A]] = ???
  }
}