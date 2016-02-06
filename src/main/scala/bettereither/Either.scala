package bettereither

sealed abstract class \/[+A, +B] extends Product with Serializable {

  /** Return `true` if this disjunction is left. */
  def isLeft: Boolean =
    this match {
      case -\/(_) => true
      case \/-(_) => false
    }

  /** Return `true` if this disjunction is right. */
  def isRight: Boolean =
    this match {
      case -\/(_) => false
      case \/-(_) => true
    }

  /** Map on the right of this disjunction. */
  def map[D](g: B => D): (A \/ D) =
    this match {
      case \/-(a)     => \/-(g(a))
      case b @ -\/(_) => b
    }

  /** Filter on the right of this disjunction. */
  def filter[AA >: A](p: B => Boolean)(implicit Z: Zero[AA]): (AA \/ B) =
    this match {
      case -\/(_) => this
      case \/-(b) => if(p(b)) this else -\/(Z.zero)
    }

  /** Run the given function on the left value. */
  def leftMap[C](f: A => C): (C \/ B) =
    this match {
      case -\/(a) => -\/(f(a))
      case b @ \/-(_) => b
    }

  /** Bind through the right of this disjunction. */
  def flatMap[AA >: A, D](g: B => (AA \/ D)): (AA \/ D) =
    this match {
      case a @ -\/(_) => a
      case \/-(b) => g(b)
    }

}

/** A left disjunction
  *
  * Often used to represent the failure case of a result
  */
final case class -\/[+A](a: A) extends (A \/ Nothing)

/** A right disjunction
  *
  * Often used to represent the success case of a result
  */
final case class \/-[+B](b: B) extends (Nothing \/ B)

trait Zero[F] {
  /** The identity element for `append`. */
  def zero: F
}

object ZeroString extends Zero[String] {
  override def zero: String = ""
}

object Either {
  implicit val zeroString = ZeroString
}