package proofmi

/**
 * Represents a term in the untyped lambda calculus.
 */
sealed trait LambdaTerm {
  /**
   * All names in this term.
   */
  lazy val names: Set[Name] = this match {
    case LambdaVariable(name) => Set(name)
    case LambdaApplication(function, argument) => function.names ++ argument.names
    case LambdaAbstraction(parameter, body) => body.names + parameter
  }

  /**
   * All names of free variables in this term.
   */
  lazy val freeNames: Set[Name] = this match {
    case LambdaVariable(name) => Set(name)
    case LambdaApplication(function, argument) => function.freeNames ++ argument.freeNames
    case LambdaAbstraction(parameter, body) => body.freeNames - parameter
  }

  /**
   * All names of bound variables in this term.
   */
  lazy val boundNames: Set[Name] = this match {
    case LambdaVariable(name) => Set()
    case LambdaApplication(function, argument) => function.boundNames ++ argument.boundNames
    case LambdaAbstraction(parameter, body) => body.boundNames + parameter
  }

  /**
   * Checks if this term is a combinator (e.a. a closed term).
   */
  def isCombinator = freeNames.isEmpty
  
  /**
   * Apply this term to an argument.
   */
  def app(argument: LambdaTerm) = LambdaApplication(this, argument)
  
  /**
   * Unsafe substitution: it is partial, but doesn't change
   * the variables in the terms.
   */
  def unsafeSubstitute(x: Name, t: LambdaTerm): LambdaTerm = this match {
    case LambdaVariable(y) =>
      if (x == y) t else this
    case LambdaApplication(f, a) =>
      LambdaApplication(f.unsafeSubstitute(x, t), a.unsafeSubstitute(x, t))
    case LambdaAbstraction(y, b) =>
      if (x == y) this
      else if (b.freeNames.contains(x) && t.freeNames.contains(y))
        throw new IllegalArgumentException(
            s"Cannot substitute $x by $t in $this because $x occurs freely in $b and $y occurs freely in $t.")        
      else LambdaAbstraction(y, b.unsafeSubstitute(x, t))
  }
  
  /**
   * Safe substitution: renames bounded variables when there is
   * a possibility of capture.
   */
  def captureAvoidingSubstitute(x: Name, t: LambdaTerm): LambdaTerm = this match {
    case LambdaVariable(y) =>
      if (x == y) t else this
    case LambdaApplication(f, a) =>
      LambdaApplication(f.captureAvoidingSubstitute(x, t), a.captureAvoidingSubstitute(x, t))
    case LambdaAbstraction(y, b) =>
      if (x == y) this
      else if (b.freeNames.contains(x) && t.freeNames.contains(y)) {
        val y1 = y.fresh()
        LambdaAbstraction(y1, b.unsafeSubstitute(y, LambdaVariable(y1)).captureAvoidingSubstitute(x, t))
      } else LambdaAbstraction(y, b.captureAvoidingSubstitute(x, t))
  }
  
  /**
   * By default, use the capture-avoiding substitution as a general mechanism.
   */
  def substitute(x: Name, t: LambdaTerm) = captureAvoidingSubstitute(x, t)
  
  /**
   * Returns a compact string without too many unneeded brackets.
   */
  lazy val compactString: String = this match {
    case LambdaVariable(name) => name.toString()
    case LambdaAbstraction(parameter, body) =>
      s"\\$parameter.${body.compactString}"
    case LambdaApplication(function, argument) => { 
      val (f, a) = (function.compactString, argument.compactString)
      (function, argument) match {
        case (LambdaAbstraction(_, _), _) => s"($f) $a"
        case (_, LambdaApplication(_, _)) => s"$f ($a)"
        case _ => s"$f $a"
      }
    }
  }
  
  override def toString = s"LambdaTerm[$compactString]"
}

case class LambdaVariable(name: Name) extends LambdaTerm

case class LambdaAbstraction(parameter: Name, body: LambdaTerm) extends LambdaTerm

case class LambdaApplication(function: LambdaTerm, argument: LambdaTerm) extends LambdaTerm

object LambdaTerm {
  /**
   * Convenient lambda abstraction creation.
   */
  object lambda {
    def apply(name: Name, names: Name*): LambdaTerm => LambdaAbstraction = {
      if (names.isEmpty) (body: LambdaTerm) => LambdaAbstraction(name, body)
      else apply(names.head, names.tail: _*) andThen { LambdaAbstraction(name, _) }
    }
  }
  
  /**
   * Convenient lambda variable creation.
   */
  implicit def lvar(code: String) = LambdaVariable(Name.decode(code))
}