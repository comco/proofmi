package proofmi

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Name._
import LambdaTerm._

class LambdaTermSpec extends FlatSpec with Matchers {
  val c_id = lambda("x") { "x" }
  val c_first = lambda("x", "y") { "x" }
  val c_second = lambda("x", "y") { "y" }
  val c_zero = lambda("f", "x") { "x" }
  val c_succ = lambda("n", "f", "x") { "n" app "f" app ("f" app "x") }
  val c_omega = lambda("x") { "x" app "x" }
  val c_Omega = c_omega app c_omega
  val c_y = lambda("x") { "f" app ("x" app "x") }
  val c_Y = lambda("f") { c_y app c_y }

  "A LambdaTerm" should "support compact string representation" in {
    c_id.compactString shouldEqual """\x.x"""

    c_first.compactString shouldEqual """\x.\y.x"""

    c_second.compactString shouldEqual """\x.\y.y"""

    c_zero.compactString shouldEqual """\f.\x.x"""

    c_succ.compactString shouldEqual """\n.\f.\x.n f (f x)"""

    c_omega.compactString shouldEqual """\x.x x"""

    c_Omega.compactString shouldEqual """(\x.x x) \x.x x"""

    c_y.compactString shouldEqual """\x.f (x x)"""

    c_Y.compactString shouldEqual """\f.(\x.f (x x)) \x.f (x x)"""
  }

  it should "compute the right names, bound names and free names" in {
    def set(names: String*) = (names map Name.decode).toSet

    c_id.names shouldEqual set("x")
    c_id.boundNames shouldEqual set("x")
    c_id.freeNames shouldEqual set()

    c_y.names shouldEqual set("x", "f")
    c_y.boundNames shouldEqual set("x")
    c_y.freeNames shouldEqual set("f")
  }

  it should "substitute variable by another and not substitute when shouldn't" in {
    "x" unsafeSubstitute ("x", "y") shouldEqual lvar("y")
  }

  it should "not change the term when the substitution is vacuous" in {
    "x" unsafeSubstitute ("y", "z") shouldEqual lvar("x")

    c_id unsafeSubstitute ("x", "y") shouldEqual c_id

    c_y.unsafeSubstitute("f", "g") shouldEqual lambda("x") { "g" app ("x" app "x") }
  }

  it should """Not substitute f by LambdaTerm[x x] in LambdaTerm[\x.f (x x)] because f occurs freely in LambdaTerm[f (x x)] and x occurs freely in LambdaTerm[x x]""" in {
    an[IllegalArgumentException] should be thrownBy {
      c_y.unsafeSubstitute("f", "x" app "x")
    }
  }

  it should """substitute [f -> x x] in \x.f x x by capture-avoiding to produce: \x1.x x (x1 x1)""" in {
    c_y.captureAvoidingSubstitute("f", "x" app "x") shouldEqual
      lambda("x1") { "x" app "x" app ("x1" app "x1") }
  }
}