import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Frizzy.*
import Frizzy.ExpOperation.*
import scala.collection.mutable


class FrizzyTest extends AnyFlatSpec with Matchers {

  it should "assign and retrieve variables correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))
    val setA = Map("x1" -> 0.5, "x2" -> 0.7)
    eval(Assign("A", FuzzySetInstance(setA)))
    val result = eval(Variable("A"))
    result shouldEqual setA
  }

  it should "perform Union, Intersect, and XOR operation correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))

    val setA = Map("x1" -> 0.2, "x2" -> 0.8)
    val setB = Map("x1" -> 0.6, "x2" -> 0.4)

    val unionResult = eval(Union(FuzzySetInstance(setA), FuzzySetInstance(setB)))
    val intersectResult = eval(Intersection(FuzzySetInstance(setA), FuzzySetInstance(setB)))
    val XORResult = eval(XOR(FuzzySetInstance(setA), FuzzySetInstance(setB)))
    val expectedUnion = Map(
      "x1" -> 0.6,
      "x2" -> 0.8
    )
    val expectedIntersect = Map(
      "x1" -> 0.2,
      "x2" -> 0.4
    )
    val expectedXOR = Map(
      "x1" -> 0.39999999999999997,
      "x2" -> 0.4
    )
    unionResult shouldEqual expectedUnion
    intersectResult shouldEqual expectedIntersect
    XORResult shouldEqual expectedXOR
  }

  it should "variable bindings should be consistent with Begin and End scopes" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))

    val setOuter = Map("x1" -> 0.5)
    val setInner = Map("x1" -> 0.8)

    eval(Assign("A", FuzzySetInstance(setOuter)))

    eval(BeginScope("InnerScope"))
    eval(Assign("A", FuzzySetInstance(setInner)))
    val innerResult = eval(Variable("A"))
    innerResult shouldEqual setInner
    eval(EndScope("InnerScope"))

    val outerResult = eval(Variable("A"))
    outerResult shouldEqual setOuter
  }

  it should "tests creation and invoking of named scopes, and sequence of operations" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))

    val setA = Map("x1" -> 0.2, "x2" -> 0.8)
    val setB = Map("x1" -> 0.5, "x2" -> 0.6)

    eval(CreateScope("MyScope",
      Perform(List(
        Assign("x", FuzzySetInstance(setA)),
        Assign("y", FuzzySetInstance(setB)),
        Assign("unionResult", Union(Variable("x"), Variable("y")))
      ))
    ))
    val result = eval(SummonScope("MyScope"))
    val expected = Map(
      "x1" -> 0.5,
      "x2" -> 0.8
    )
    result shouldEqual expected
  }

  it should "should obscure variables defined in invoked scopes from outside" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))

    val setA = Map("x1" -> 0.2)

    eval(CreateScope("MyScope",
      Perform(List(
        Assign("A", FuzzySetInstance(setA))
      ))
    ))

    eval(SummonScope("MyScope"))

    an[Exception] should be thrownBy eval(Variable("A"))
  }

  it should "perform Complement operation correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))

    val setA = Map("x1" -> 0.2, "x2" -> 0.8, "x3" -> 0.5)

    val complementResult = eval(Complement(FuzzySetInstance(setA)))

    val expected = Map(
      "x1" -> 0.8,
      "x2" -> 0.2,
      "x3" -> 0.5
    )

    complementResult shouldEqual expected
  }

  it should "perform AlphaCut operation correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))

    val setA = Map("x1" -> 0.2, "x2" -> 0.8, "x3" -> 0.5, "x4" -> 0.9)

    val alpha = 0.6

    val alphaCutResult = eval(AlphaCut(alpha, FuzzySetInstance(setA)))

    val expected = Map(
      "x2" -> 1.0,
      "x4" -> 1.0
    )
    alphaCutResult shouldEqual expected
  }


}

