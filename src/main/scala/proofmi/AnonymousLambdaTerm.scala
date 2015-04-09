package proofmi

sealed trait AnonymousLambdaTerm {
  /**
   * Returns a string representation of this lambda term without too many
   * unneeded parenthesis.
   */
  lazy val compactString: String = this match {
    case AnonymousLambdaVariable(index) => s"$index"
    case AnonymousLambdaAbstraction(body) =>
      s"\\${body.compactString}"
    case AnonymousLambdaApplication(function, argument) => {
      val (f, a) = (function.compactString, argument.compactString)
      (function, argument) match {
        case (AnonymousLambdaAbstraction(_), _) => s"($f) $a"
        case (_, AnonymousLambdaApplication(_, _)) => s"$f ($a)"
        case (_, _) => s"$f $a"
      }
    }
  }
  
  override def toString = s"AnonymousLambdaTerm[$compactString]"
}

case class AnonymousLambdaVariable(index: Int) extends AnonymousLambdaTerm

case class AnonymousLambdaApplication(function: AnonymousLambdaTerm, argument: AnonymousLambdaTerm)
  extends AnonymousLambdaTerm

case class AnonymousLambdaAbstraction(body: AnonymousLambdaTerm)
  extends AnonymousLambdaTerm

object AnonymousLambdaTerm {
  def fromLambdaTerm(term: LambdaTerm, context: List[Name] = Nil): AnonymousLambdaTerm = term match {
    case LambdaVariable(name) => AnonymousLambdaVariable(context.indexOf(name))
    case LambdaApplication(function, argument) =>
      AnonymousLambdaApplication(fromLambdaTerm(function, context), fromLambdaTerm(argument, context))
    case LambdaAbstraction(parameter, body) =>
      AnonymousLambdaAbstraction(fromLambdaTerm(body, parameter :: context))
  }
  
  def boundNames(term: LambdaTerm): List[Name] = term match {
    case LambdaVariable(_) => Nil
    case LambdaAbstraction(parameter, body) => parameter :: boundNames(body)
    case LambdaApplication(function, argument) => boundNames(function) ++ boundNames(argument)
  }

  def toLambdaTerm(anon: AnonymousLambdaTerm, context: List[Name] = Nil)(
      implicit namer: Iterator[Name]): LambdaTerm = anon match {
    case AnonymousLambdaVariable(index) => LambdaVariable(context(index))
    case AnonymousLambdaApplication(function, argument) =>
      LambdaApplication(toLambdaTerm(function, context), toLambdaTerm(argument, context))
    case AnonymousLambdaAbstraction(body) =>
      val name = namer.next()
      LambdaAbstraction(name, toLambdaTerm(body, name :: context))
  }
}