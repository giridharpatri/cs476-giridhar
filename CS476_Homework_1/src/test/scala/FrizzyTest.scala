import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Frizzy.*
import Frizzy.ExpOperation.*
import scala.collection.mutable
import HelperFunctions.*


class FrizzyTest extends AnyFlatSpec with Matchers {

  it should "assign and retrieve variables correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
    val setA = Map("x1" -> 0.5, "x2" -> 0.7)
    eval(Assign("A", FuzzySetInstance(setA)))
    val result = eval(Variable("A"))
    result shouldEqual setA
  }

  it should "perform Union, Intersect, and XOR operation correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

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
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

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

//  it should "test creation and invoking of named scopes, and sequence of operations" in {
//    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
//
//    val setA = Map("x1" -> 0.2, "x2" -> 0.8)
//    val setB = Map("x1" -> 0.5, "x2" -> 0.6)
//
//    // Create a scope "MyScope" with a sequence of operations
//    eval(CreateScope("MyScope",
//      Perform(List(
//        Assign("x", FuzzySetInstance(setA)),
//        Assign("y", FuzzySetInstance(setB)),
//        Assign("unionResult", Union(Variable("x"), Variable("y")))
//      ))
//    ))
//
//    // Summon the scope "MyScope" and execute its body
//    val result = eval(SummonScope("MyScope"))
//
//    // The result should be the value assigned to "unionResult" in the scope
//    val expected = Map(
//      "x1" -> 0.5,
//      "x2" -> 0.8
//    )
//    result shouldEqual expected
//  }

  it should "should obscure variables defined in invoked scopes from outside" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

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
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

    val setA = Map("x1" -> 0.2, "x2" -> 0.8, "x3" -> 0.5)

    val complementResult = eval(Complement(FuzzySetInstance(setA)))

    val expected = Map(
      "x1" -> 0.8,
      "x2" -> 0.19999999999999996,
      "x3" -> 0.5
    )

    complementResult shouldEqual expected
  }

  it should "perform AlphaCut operation correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

    val setA = Map("x1" -> 0.2, "x2" -> 0.8, "x3" -> 0.5, "x4" -> 0.9)

    val alpha = 0.6

    val alphaCutResult = eval(AlphaCut(alpha, FuzzySetInstance(setA)))

    val expected = Map(
      "x2" -> 1.0,
      "x4" -> 1.0
    )
    alphaCutResult shouldEqual expected
  }

  it should "assign and retrieve class variables correctly" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

    // Define a simple class with a variable and getter/setter methods
    eval(ClassDef("SimpleClass",
      body = Perform(List(
        ClassVar("x", "int"),
        MethodDef("getX", List(), Variable("x")),
        MethodDef("setX", List(Parameter("value", "int")), Assign("x", Variable("value")))
      ))
    ))

    // Create an instance of SimpleClass
    eval(CreateInstance("SimpleClass", "instance"))

    // Set x to 42
    eval(InvokeMethod("instance", "setX", List(Value(42))))

    // Retrieve x and check if it's 42
    val result = eval(InvokeMethod("instance", "getX", List()))
    result shouldEqual 42
  }

  it should "inherit methods and allow method overriding" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

    // Define a base class with a method
    eval(ClassDef("Animal",
      body = Perform(List(
        MethodDef("speak", List(), Value("Animal sound"))
      ))
    ))

    // Define a subclass that overrides the method
    eval(ClassDef("Dog",
      body = Perform(List(
        MethodDef("speak", List(), Value("Woof"))
      )),
      parent = Some("Animal")
    ))

    // Create instances of both classes
    eval(CreateInstance("Animal", "animal"))
    eval(CreateInstance("Dog", "dog"))

    // Test the speak method on both instances
    val animalSpeak = eval(InvokeMethod("animal", "speak", List()))
    val dogSpeak = eval(InvokeMethod("dog", "speak", List()))

    animalSpeak shouldEqual "Animal sound"
    dogSpeak shouldEqual "Woof"
  }

  it should "allow variable shadowing in subclasses" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

    // Define a base class with a variable x
    eval(ClassDef("ParentClass",
      body = Perform(List(
        ClassVar("x", "int"),
        MethodDef("getX", List(), Variable("x"))
      ))
    ))

    // Define a subclass that declares a variable x, shadowing the parent variable
    eval(ClassDef("ChildClass",
      body = Perform(List(
        ClassVar("x", "int"),
        MethodDef("getX", List(), Variable("x"))
      )),
      parent = Some("ParentClass")
    ))

    // Create instances
    eval(CreateInstance("ParentClass", "parentInstance"))
    eval(CreateInstance("ChildClass", "childInstance"))

    // Assign values to x in both instances
    eval(Assign("parentInstance.x", Value(10)))
    eval(Assign("childInstance.x", Value(20)))

    // Retrieve x from both instances
    val parentX = eval(InvokeMethod("parentInstance", "getX", List()))
    val childX = eval(InvokeMethod("childInstance", "getX", List()))

    parentX shouldEqual 10
    childX shouldEqual 20
  }

//  it should "invoke methods with parameters and perform fuzzy set operations" in {
//    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
//
//    // Define a class with a method that performs Union of two fuzzy sets
//    eval(ClassDef("FuzzySetOperations",
//      body = Perform(List(
//        MethodDef("unionSets", List(Parameter("set1", "FuzzySet"), Parameter("set2", "FuzzySet")),
//          Union(Variable("set1"), Variable("set2"))
//        )
//      ))
//    ))
//
//    // Create an instance of the class
//    eval(CreateInstance("FuzzySetOperations", "fuzzyOps"))
//
//    // Define two fuzzy sets
//    val setA = Map("x1" -> 0.3, "x2" -> 0.7)
//    val setB = Map("x1" -> 0.6, "x2" -> 0.4)
//
//    // Invoke the unionSets method
//    val result = eval(InvokeMethod("fuzzyOps", "unionSets", List(FuzzySetInstance(setA), FuzzySetInstance(setB))))
//
//    // Expected result after union
//    val expected = Map(
//      "x1" -> 0.6,
//      "x2" -> 0.7
//    )
//
//    result shouldEqual expected
//  }


//  it should "invoke methods that create and use scopes" in {
//    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
//
//    // Define a class with a method that creates a scope and performs operations
//    eval(ClassDef("ScopeTester",
//      body = Perform(List(
//        MethodDef("testScope", List(),
//          Perform(List(
//            BeginScope("methodScope"),
//            Assign("A", FuzzySetInstance(Map("x1" -> 0.5))),
//            EndScope("methodScope"),
//            Variable("A")
//          ))
//        )
//      ))
//    ))
//
//    // Create an instance of the class
//    eval(CreateInstance("ScopeTester", "scopeTester"))
//
//    // Invoke the testScope method
//    val result = eval(InvokeMethod("scopeTester", "testScope", List()))
//
//    // Expected result
//    val expected = Map(
//      "x1" -> 0.5
//    )
//
//    result shouldEqual expected
//  }


}

  

