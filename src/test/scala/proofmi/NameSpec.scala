package proofmi

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Name._

class NameSpec extends FlatSpec with Matchers {
  "A Name" should "be constructable from string" in {
    val name = Name("x")
    name.toString() shouldEqual "x"
  }
  
  it should "generate fresh names" in {
    val x = Name("x")
    x.index shouldEqual 0
    val x1 = x.fresh()
    x1.index shouldEqual 1
    val x2 = x.fresh()
    x2.index shouldEqual 2
  }
  
  it should "print names cleanly" in {
    val t3 = Name("t").fresh().fresh().fresh()
    t3.toString() shouldEqual "t3"
  }
  
  it should "decode correcly" in {
    "x".string shouldEqual "x"
    "x".index shouldEqual 0
    
    "ala23".index shouldEqual 23
    "bala3".string shouldEqual "bala"
  }
}