package proofmi

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Name._
import LambdaTerm._
import Parsers._

class ParsersSpec extends FlatSpec with Matchers {
  // TODO: Add more tests
  "A name parser" should "parse names" in {
    parse(name, "x12").get shouldEqual Name("x", 12)
    parse(name, "alabala").get shouldEqual Name("alabala")
  }
  
  "A variable parser" should "parse variables" in {
    parse(variable, "x12").get shouldEqual LambdaTerm.lvar("x12")
  }
  
  "A lambda parser" should "work" in {
    println(parse(term, """\x.x"""))
    println(parse(term, """\n.\f.\x.n f (f x)"""))
  }
}