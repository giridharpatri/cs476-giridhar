import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Frizzy.*
import Frizzy.ExpOperation.*
import scala.collection.mutable

class FrizzyTest extends AnyFlatSpec with Matchers {

  it should "partially evaluateeee conditional expressions with unknown variables" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
    envStack.clear()
    envStack.push(("", mutable.Map[String, Any]()))

    // Define variables with known and unknown values
    envStack.top._2.update("knownVar", 5)
    // "unknownVar" is not defined

    // Construct a conditional expression
    val conditionalExpr = IFTRUE(
      GREATEREQUAL(
        Variable("unknownVar"), // Unknown variable
        Variable("knownVar")    // Known variable
      ),
      Assign("result", Add(Variable("unknownVar"), Value(10))),
      Assign("result", Value(0))
    )

    // Evaluate the conditional expression
    val result = eval(conditionalExpr).asInstanceOf[ExpOperation]

    // The expected result is a partially evaluated IFTRUE expression
    val expected = IFTRUE(
      GREATEREQUAL(Variable("unknownVar"), Value(5)),
      Assign("result", Add(Variable("unknownVar"), Value(10))),
      Assign("result", Value(0))
    )

    result shouldEqual expected

    // Since the condition cannot be evaluated, 'result' should not be assigned
    // Expect an exception when trying to evaluate 'result'
    //an[Exception] should be thrownBy eval(Variable("result"))
  }

  it should "partially evaluate method invocations with unknown arguments" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
    envStack.clear()
    envStack.push(("", mutable.Map[String, Any]()))

    // Define a class with a method that uses an argument
    eval(ClassDef("Adder",
      body = Perform(List(
        MethodDef("addTen", List(Parameter("x", "int")),
          Assign("result", Add(Variable("x"), Value(10)))
        )
      ))
    ))

    // Create an instance of Adder
    eval(CreateInstance("Adder", "adderInstance"))

    // Invoke addTen with a known argument
    eval(InvokeMethod("adderInstance", "addTen", List(Value(5))))
    val knownResult = eval(Variable("adderInstance.result"))
    knownResult shouldEqual 15

    // Clear "result" from instance variables
    instanceRegistry("adderInstance").variables.remove("result")

    // Invoke addTen with an unknown argument
    eval(InvokeMethod("adderInstance", "addTen", List(Variable("unknownX"))))

    // Since "unknownX" is not defined, "result" should be a partially evaluated expression
    val partialResult = eval(Variable("adderInstance.result")).asInstanceOf[ExpOperation]
    partialResult shouldEqual Add(Variable("unknownX"), Value(10))
  }

  it should "correctly handle method overriding with partial evaluation in inheritance hierarchies" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
    envStack.clear()
    envStack.push(("", mutable.Map[String, Any]()))

    // Define a base class with a method
    eval(ClassDef("BaseClass",
      body = Perform(List(
        MethodDef("compute", List(Parameter("x", "int")),
          Assign("result", Multiply(Variable("x"), Value(2)))
        )
      ))
    ))

    // Define a subclass that overrides the method
    eval(ClassDef("SubClass",
      body = Perform(List(
        MethodDef("compute", List(Parameter("x", "int")),
          Assign("result", Multiply(Variable("x"), Value(3)))
        )
      )),
      parent = Some("BaseClass")
    ))

    // Create an instance of SubClass
    eval(CreateInstance("SubClass", "subInstance"))

    // Invoke compute with a known argument
    eval(InvokeMethod("subInstance", "compute", List(Value(5))))
    val knownResult = eval(Variable("subInstance.result"))
    knownResult shouldEqual 15

    // Clear "result" from instance variables
    instanceRegistry("subInstance").variables.remove("result")

    // Invoke compute with an unknown argument
    eval(InvokeMethod("subInstance", "compute", List(Variable("unknownX"))))
    val partialResult = eval(Variable("subInstance.result")).asInstanceOf[ExpOperation]
    partialResult shouldEqual Multiply(Variable("unknownX"), Value(3))
  }

  it should "apply reduction rules and handle associativity in partial evaluations" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
    envStack.clear()
    envStack.push(("", mutable.Map[String, Any]()))

    // Define an expression with nested operations
    val complexExpr = Add(
      Multiply(Value(2), Add(Variable("x"), Value(3))),
      Value(5)
    )

    // With x = 7
    envStack.top._2.update("x", 7)
    val resultKnown = eval(complexExpr)
    resultKnown shouldEqual 2 * (7 + 3) + 5 // Should be 2 * 10 + 5 = 25

    // With x unknown
    envStack.top._2.remove("x")
    val resultUnknown = eval(complexExpr).asInstanceOf[ExpOperation]
    // Should be partially evaluated to Add(Multiply(Value(2), Add(Variable("x"), Value(3))), Value(5))
    val expectedPartial = Add(Multiply(Value(2), Add(Variable("x"), Value(3))), Value(5))
    resultUnknown shouldEqual expectedPartial
  }

  it should "partially evaluate method calls in inheritance hierarchies with dynamic dispatch" in {
    given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))
    envStack.clear()
    envStack.push(("", mutable.Map[String, Any]()))

    // Define a base class with a method
    eval(ClassDef("Animal",
      body = Perform(List(
        MethodDef("makeSound", List(),
          Assign("sound", Value("Some generic sound"))
        )
      ))
    ))

    // Define a subclass that overrides the method
    eval(ClassDef("Dog",
      body = Perform(List(
        MethodDef("makeSound", List(),
          Assign("sound", Value("Bark"))
        )
      )),
      parent = Some("Animal")
    ))

    // Define another subclass without overriding the method
    eval(ClassDef("Cat",
      body = Perform(List(
        // No overriding of makeSound
      )),
      parent = Some("Animal")
    ))

    // Create instances
    eval(CreateInstance("Dog", "dogInstance"))
    eval(CreateInstance("Cat", "catInstance"))

    // Invoke makeSound on dogInstance
    eval(InvokeMethod("dogInstance", "makeSound", List()))
    val dogSound = eval(Variable("dogInstance.sound"))
    dogSound shouldEqual "Bark"

    // Clear "sound" from instance variables
    instanceRegistry("catInstance").variables.remove("sound")

    // Invoke makeSound on catInstance
    eval(InvokeMethod("catInstance", "makeSound", List()))
    val catSound = eval(Variable("catInstance.sound"))
    catSound shouldEqual "Some generic sound"
  }

}
