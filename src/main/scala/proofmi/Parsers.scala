package proofmi

import scala.util.parsing.combinator.RegexParsers

object Parsers extends RegexParsers {
  // Parser for names
  val name: Parser[Name] = Name.nameRegex ^^ Name.decode
  
  // Parser for lambda variables
  val variable: Parser[LambdaVariable] = name map LambdaVariable
  
  // Parser for lambda factors
  val factor = variable | abstraction | "(" ~> term <~ ")"

  // Parser for abstractions
  val abstraction: Parser[LambdaAbstraction] = "\\" ~> name ~ "." ~ term ^^ {
    case parameter ~ _ ~ body => LambdaAbstraction(parameter, body)
  }
  
  // Parser for general terms
  val term: Parser[LambdaTerm] = (factor+) ^^ (_.reduceLeft(LambdaApplication))	  
}