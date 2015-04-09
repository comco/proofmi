package proofmi

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import proofmi.LambdaTerm._

class AnonymousLambdaTermSpec extends FlatSpec with Matchers {
  val x = AnonymousLambdaVariable(0)
  val y = AnonymousLambdaAbstraction(x)
  val z = AnonymousLambdaApplication(y, x)
  
  "An AnonymousLambdaTerm" should "have compact represenation" in {
    x.compactString shouldEqual "0"
    y.compactString shouldEqual """\0"""
    z.compactString shouldEqual """(\0) 0"""
  }
  
  it should "translate from named lambda terms" in {
    val id = lambda ("x") { "x" }
    val anonId = AnonymousLambdaTerm.fromLambdaTerm(id)
    anonId.compactString shouldEqual """\0"""
    
    val id2 = id app id
    val anonId2 = AnonymousLambdaTerm.fromLambdaTerm(id2)
    anonId2.compactString shouldEqual """(\0) \0"""
  }
  
  it should "translate to namded lambda terms" in {
	  val c_succ = lambda("n", "f", "x") { "n" app "f" app ("f" app "x") }
    val anonc_succ = AnonymousLambdaTerm.fromLambdaTerm(c_succ)
    val vars = AnonymousLambdaTerm.boundNames(c_succ)
    vars shouldEqual List(Name("n"), Name("f"), Name("x"))
    val cc_succ = AnonymousLambdaTerm.toLambdaTerm(anonc_succ, Nil)(vars.iterator)
    cc_succ shouldEqual c_succ
  }
}