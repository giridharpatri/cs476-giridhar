import scala.collection.mutable

object Frizzy:
  // Define types for a fuzzy value
  type FuzzyValue = Double
  type Element = String
  // Define a fuzzy set
  type FuzzySet = Map[Element, FuzzyValue]

  type Environment = mutable.Map[String, Any]
  type EnvironmentStack = mutable.Stack[(String, Environment)]
  type EvalContext = EnvironmentStack ?=> Any

  // Initialize the environment stack with a global environment
  given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

  // Registries for scopes, classes, and instances
  val scopeRegistry: mutable.Map[String, ExpOperation] = mutable.Map()
  val classRegistry: mutable.Map[String, ClassInfo] = mutable.Map()
  val instanceRegistry: mutable.Map[String, InstanceInfo] = mutable.Map()

  // Class information
  case class ClassInfo(
                        name: String,
                        parent: Option[String],
                        variables: mutable.Map[String, String], // variable name and type
                        methods: mutable.Map[String, MethodInfo],
                        nestedClasses: mutable.Map[String, ClassInfo]
                      )

  // Method information
  case class MethodInfo(name: String, params: List[Parameter], body: ExpOperation)

  // Instance information
  case class InstanceInfo(className: String, variables: mutable.Map[String, Any])
  case class Parameter(name: String, paramType: String)

  enum ExpOperation:
    // Variable and assignment operations
    case FuzzySetInstance(set: FuzzySet)
    case Variable(name: String)
    case Assign(name: String, expr: ExpOperation)
    case Value(value: Any)

    // Fuzzy logic operations
    case Union(p1: ExpOperation, p2: ExpOperation)
    case Intersection(p1: ExpOperation, p2: ExpOperation)
    case Complement(p: ExpOperation)
    case AlphaCut(alpha: FuzzyValue, set: ExpOperation)
    case XOR(p1: ExpOperation, p2: ExpOperation)

    // Scope operations
    case BeginScope(name: String)
    case EndScope(name: String)
    case Perform(ops: List[ExpOperation])
    case CreateScope(name: String, body: ExpOperation)
    case SummonScope(name: String)

    // Object-oriented programming constructs
    case ClassDef(name: String, body: ExpOperation, parent: Option[String] = None)
    case ClassVar(name: String, varType: String)
    case MethodDef(methodName: String, params: List[Parameter], body: ExpOperation)
    case CreateInstance(className: String, instanceName: String)
    case InvokeMethod(instanceName: String, methodName: String, args: List[ExpOperation])
    case Extends(subclass: String, superclass: String)
    case NestedClassDef(outerClassName: String, nestedClass: ExpOperation)
    case SuperInvoke(methodName: String, args: List[ExpOperation])

    // Arithmetic and comparison operations
    case Add(lhs: ExpOperation, rhs: ExpOperation)
    case Multiply(lhs: ExpOperation, rhs: ExpOperation)
    case GREATEREQUAL(lhs: ExpOperation, rhs: ExpOperation)

    // Conditional constructs
    case IFTRUE(condition: ExpOperation, thenBranch: ExpOperation, elseBranch: ExpOperation)

  import ExpOperation.*

  // Helper function to convert any value to ExpOperation
  def asExpOperation(v: Any): ExpOperation = v match
    case e: ExpOperation => e
    case i: Int => Value(i)
    case b: Boolean => Value(b)
    case other => Value(other)

  // Evaluation function
  def eval(exp: ExpOperation): EvalContext = exp match
    case FuzzySetInstance(set) => set
    case Value(v) => v

    case Add(lhs, rhs) =>
      val leftEval = eval(lhs)
      val rightEval = eval(rhs)
      (leftEval, rightEval) match
        case (l: Int, r: Int) => l + r
        case _ => Add(asExpOperation(leftEval), asExpOperation(rightEval))

    case Multiply(lhs, rhs) =>
      val leftEval = eval(lhs)
      val rightEval = eval(rhs)
      (leftEval, rightEval) match
        case (l: Int, r: Int) => l * r
        case _ => Multiply(asExpOperation(leftEval), asExpOperation(rightEval))

    case GREATEREQUAL(lhs, rhs) =>
      val leftEval = eval(lhs)
      val rightEval = eval(rhs)
      (leftEval, rightEval) match
        case (l: Int, r: Int) => l >= r
        case _ => GREATEREQUAL(asExpOperation(leftEval), asExpOperation(rightEval))

    case IFTRUE(condition, thenBranch, elseBranch) =>
      val condEval = eval(condition)
      condEval match
        case true => eval(thenBranch)
        case false => eval(elseBranch)
        case _ => IFTRUE(asExpOperation(condEval), thenBranch, elseBranch)

    case ClassDef(name, body, parent) =>
      val classInfo = ClassInfo(
        name = name,
        parent = parent,
        variables = mutable.Map(),
        methods = mutable.Map(),
        nestedClasses = mutable.Map()
      )
      // Evaluate the class body to populate classInfo
      evalClassBody(body, classInfo)
      // Register the class in the global classRegistry
      classRegistry.update(name, classInfo)
      ()

    case CreateInstance(className, instanceName) =>
      val classInfo = classRegistry.getOrElse(className, throw new Exception(s"Class '$className' not found"))
      // Create a new instance environment
      val instanceEnv = mutable.Map[String, Any]()
      // Initialize variables from class definition
      initializeInstanceVariables(classInfo, instanceEnv)
      // Add the instance to the registry
      instanceRegistry.update(instanceName, InstanceInfo(className, instanceEnv))
      ()

    case InvokeMethod(instanceName, methodName, args) =>
      val instanceInfo = instanceRegistry.getOrElse(instanceName, throw new Exception(s"Instance '$instanceName' not found"))
      // Lookup the method, considering inheritance
      val methodInfo = lookupMethod(instanceInfo.className, methodName)
      // Bind arguments to parameters
      val methodEnv = mutable.Map[String, Any]()
      methodInfo.params.zip(args).foreach { case (param, argExp) =>
        methodEnv.update(param.name, eval(argExp))
      }
      // Set up the environment stack for method execution
      val envStack = summon[EnvironmentStack]
      // Push method environment
      envStack.push((s"method_${methodName}", methodEnv))
      // Push instance environment (for 'this' reference)
      envStack.push((s"instance_${instanceName}", instanceInfo.variables))
      // Evaluate the method body
      val result = eval(methodInfo.body)
      // Pop environments
      envStack.pop() // instance environment
      envStack.pop() // method environment
      result

    case Variable(name) =>
      val envStack = summon[EnvironmentStack]

      if name.contains(".") then
        // Handle instance variable access like instanceName.variableName
        val parts = name.split("\\.")
        if parts.length != 2 then
          throw new Exception(s"Invalid variable name '$name'")
        val instanceName = parts(0)
        val varName = parts(1)
        val instanceInfo = instanceRegistry.getOrElse(instanceName, throw new Exception(s"Instance '$instanceName' not found"))
        instanceInfo.variables.get(varName) match
          case Some(value) => value match
            case exp: ExpOperation => eval(exp)
            case other => other
          case None => throw new Exception(s"Variable '$varName' not found in instance '$instanceName'")
      else
        // Regular variable lookup
        envStack.find(_._2.contains(name)) match
          case Some((_, env)) => env(name) match
            case value: ExpOperation => eval(value)
            case other => other
          case None =>
            // If not found, check instance variables
            instanceRegistry.values.find(_.variables.contains(name)) match
              case Some(instanceInfo) => instanceInfo.variables(name) match
                case value: ExpOperation => eval(value)
                case other => other
              case None => Variable(name)

    case Assign(name, expr) =>
      val value = eval(expr)
      val envStack = summon[EnvironmentStack]

      if name.contains(".") then
        // Handle instance variable assignment
        val parts = name.split("\\.")
        if parts.length != 2 then
          throw new Exception(s"Invalid assignment to '$name'")
        val instanceName = parts(0)
        val varName = parts(1)
        val instanceInfo = instanceRegistry.getOrElse(instanceName, throw new Exception(s"Instance '$instanceName' not found"))
        instanceInfo.variables.update(varName, value)
      else
        // Regular variable assignment in the current environment
        envStack.top._2.update(name, value)
      value

    // Fuzzy logic operations with partial evaluation
    case Union(p1, p2) =>
      try
        val set1 = evalFuzzySet(p1)
        val set2 = evalFuzzySet(p2)
        val unionSet = (set1.keySet ++ set2.keySet).map { element =>
          val mu1 = set1.getOrElse(element, 0.0)
          val mu2 = set2.getOrElse(element, 0.0)
          element -> Math.max(mu1, mu2)
        }.toMap
        unionSet
      catch
        case _: Exception => Union(p1, p2)

    case Intersection(p1, p2) =>
      try
        val set1 = evalFuzzySet(p1)
        val set2 = evalFuzzySet(p2)
        val intersectionSet = (set1.keySet ++ set2.keySet).map { element =>
          val mu1 = set1.getOrElse(element, 0.0)
          val mu2 = set2.getOrElse(element, 0.0)
          element -> Math.min(mu1, mu2)
        }.toMap
        intersectionSet
      catch
        case _: Exception => Intersection(p1, p2)

    case Complement(p) =>
      try
        val set = evalFuzzySet(p)
        val complementSet = set.map { case (element, mu) =>
          element -> (1.0 - mu)
        }
        complementSet
      catch
        case _: Exception => Complement(p)

    case AlphaCut(alpha, setExpr) =>
      try
        val set = evalFuzzySet(setExpr)
        val alphaCutSet = set.collect {
          case (element, mu) if mu >= alpha => element -> 1.0
        }
        alphaCutSet
      catch
        case _: Exception => AlphaCut(alpha, setExpr)

    case XOR(p1, p2) =>
      try
        val set1 = evalFuzzySet(p1)
        val set2 = evalFuzzySet(p2)
        val xorSet = (set1.keySet ++ set2.keySet).map { element =>
          val mu1 = set1.getOrElse(element, 0.0)
          val mu2 = set2.getOrElse(element, 0.0)
          element -> Math.abs(mu1 - mu2)
        }.toMap
        xorSet
      catch
        case _: Exception => XOR(p1, p2)

    // Scope operations
    case BeginScope(name) =>
      val envStack = summon[EnvironmentStack]
      envStack.push((name, mutable.Map[String, Any]()))
      ()

    case EndScope(name) =>
      val envStack = summon[EnvironmentStack]
      if envStack.nonEmpty && envStack.top._1 == name then
        envStack.pop()
      else
        throw new Exception(s"'$name' is not a scope...")
      ()

    case Perform(ops) =>
      var result: Any = ()
      for op <- ops do result = eval(op)
      result

    case CreateScope(name, body) =>
      scopeRegistry.update(name, body)
      ()

    case SummonScope(name) =>
      scopeRegistry.get(name) match
        case Some(body) =>
          val envStack = summon[EnvironmentStack]
          envStack.push((name, mutable.Map[String, Any]()))
          val result = eval(body)
          // Merge scope variables into parent environment
          val scopeEnv = envStack.pop()._2
          val parentEnv = envStack.top._2
          parentEnv ++= scopeEnv
          result
        case None =>
          throw new Exception(s"Scope '$name' is not defined.")

  // Helper functions
  def evalFuzzySet(exp: ExpOperation): FuzzySet = exp match
    case FuzzySetInstance(set) => set
    case Variable(name) =>
      val envStack = summon[EnvironmentStack]
      if name.contains(".") then
        // Handle instance variable access
        eval(Variable(name)) match
          case fs: FuzzySet => fs
          case _ => throw new Exception(s"Variable '$name' is not a FuzzySet")
      else
        envStack.find(_._2.contains(name)) match
          case Some((_, env)) => env(name) match
            case fs: FuzzySet => fs
            case exp: ExpOperation => eval(exp) match
              case fs: FuzzySet => fs
              case _ => throw new Exception(s"Variable '$name' is not a FuzzySet")
            case _ => throw new Exception(s"Variable '$name' is not a FuzzySet")
          case None => throw new Exception(s"Variable '$name' not found")
    case _ => throw new Exception(s"Cannot evaluate expression to FuzzySet: $exp")

  def evalClassBody(body: ExpOperation, classInfo: ClassInfo): Unit = body match
    case Perform(ops) =>
      ops.foreach(op => evalClassBody(op, classInfo))
    case ClassVar(name, varType) =>
      classInfo.variables.update(name, varType)
    case MethodDef(methodName, params, methodBody) =>
      classInfo.methods.update(methodName, MethodInfo(methodName, params, methodBody))
    case ClassDef(nestedClassName, nestedBody, parent) =>
      val nestedClassInfo = ClassInfo(
        name = nestedClassName,
        parent = parent,
        variables = mutable.Map(),
        methods = mutable.Map(),
        nestedClasses = mutable.Map()
      )
      evalClassBody(nestedBody, nestedClassInfo)
      classInfo.nestedClasses.update(nestedClassName, nestedClassInfo)
    case _ => // Handle other cases if needed

  def initializeInstanceVariables(classInfo: ClassInfo, instanceEnv: mutable.Map[String, Any]): Unit =
    // Initialize variables from parent class first
    classInfo.parent.foreach { parentClassName =>
      val parentClassInfo = classRegistry.getOrElse(parentClassName, throw new Exception(s"Class '$parentClassName' not found"))
      initializeInstanceVariables(parentClassInfo, instanceEnv)
    }
    // Initialize this class's variables
    classInfo.variables.foreach { case (varName, varType) =>
      instanceEnv.update(varName, null) // Initialize with null or a default value
    }

  def lookupMethod(className: String, methodName: String): MethodInfo =
    val classInfo = classRegistry.getOrElse(className, throw new Exception(s"Class '$className' not found"))
    classInfo.methods.get(methodName) match
      case Some(methodInfo) => methodInfo
      case None =>
        classInfo.parent match
          case Some(parentClassName) => lookupMethod(parentClassName, methodName)
          case None => throw new Exception(s"Method '$methodName' not found in class hierarchy of '$className'")

@main def main(): Unit =
  import Frizzy.*
  import Frizzy.ExpOperation.*
  import Frizzy.given

  val envStack = summon[EnvironmentStack]

  // **Test 1: Partial Evaluation with Known Variables**

  // Define variables with known values
  envStack.top._2.update("var", 2)
  envStack.top._2.update("var1", 10)

  // Construct a conditional expression
  val conditionalExprKnown = IFTRUE(
    GREATEREQUAL(
      Multiply(Value(15), Variable("var")),
      Add(Value(2), Variable("var1"))
    ),
    Assign("somevar", Add(Variable("var"), Value(3))),
    Assign("somevar", Value(0))
  )

  // Evaluate the conditional expression
  eval(conditionalExprKnown)

  // Retrieve and print the value of "somevar"
  val somevarValueKnown = eval(Variable("somevar"))
  println(s"Test 1 - somevar (known variables) = $somevarValueKnown") // Expected output: 5

  // **Test 2: Partial Evaluation with Unknown Variables**

  // Clear the environment to simulate unknown variables
  envStack.top._2.clear()

  // Construct the same conditional expression
  val conditionalExprUnknown = IFTRUE(
    GREATEREQUAL(
      Multiply(Value(15), Variable("var")),
      Add(Value(2), Variable("var1"))
    ),
    Assign("somevar", Add(Variable("var"), Value(3))),
    Assign("somevar", Value(0))
  )

  // Evaluate the conditional expression
  val resultUnknown = eval(conditionalExprUnknown)

  // Print the partially evaluated result
  println(s"Test 2 - Result (unknown variables): $resultUnknown")

  // Try to retrieve "somevar" (may not be assigned)
  try
    val somevarValueUnknown = eval(Variable("somevar"))
    println(s"Test 2 - somevar (unknown variables) = $somevarValueUnknown")
  catch
    case e: Exception => println(s"Test 2 - somevar not assigned due to unknown variables.")

  // **Test 3: Partial Evaluation in Method Invocation**

  // Define a class with a method that uses variables
  eval(ClassDef("Calculator",
    body = Perform(List(
      MethodDef("compute", List(Parameter("x", "int")),
        Perform(List(
          IFTRUE(
            GREATEREQUAL(Variable("x"), Value(10)),
            Assign("result", Multiply(Variable("x"), Value(2))),
            Assign("result", Multiply(Variable("x"), Value(3)))
          )
        ))
      )
    ))
  ))

  // Create an instance of Calculator
  eval(CreateInstance("Calculator", "calc"))

  // **Test 3a: Invoke method with known argument**

  // Invoke compute with x = 15
  eval(InvokeMethod("calc", "compute", List(Value(15))))

  // Retrieve and print "result"
  val resultComputeKnown = eval(Variable("calc.result"))
  println(s"Test 3a - result (x=15) = $resultComputeKnown") // Expected output: 30

  // **Test 3b: Invoke method with unknown argument**

  // Clear "result" from instance variables
  instanceRegistry("calc").variables.remove("result")

  // Invoke compute with x = Variable("unknownX")
  eval(InvokeMethod("calc", "compute", List(Variable("unknownX"))))

  // Since "unknownX" is not defined, the condition cannot be fully evaluated

  // Retrieve and print "result"
  try
    val resultComputeUnknown = eval(Variable("calc.result"))
    println(s"Test 3b - result (x=unknown) = $resultComputeUnknown")
  catch
    case e: Exception => println(s"Test 3b - result not assigned due to unknown variable 'unknownX'.")

  // **Test 4: Partial Evaluation with Fuzzy Set Operations**

  // Define fuzzy sets
  val setA = Map("x1" -> 0.5, "x2" -> 0.7)
  val setB = Map("x1" -> 0.6, "x2" -> 0.4)
  val setC = Variable("unknownSet") // Unknown fuzzy set

  // **Test 4a: Union of known fuzzy sets**

  val unionKnown = Union(FuzzySetInstance(setA), FuzzySetInstance(setB))
  val resultUnionKnown = eval(unionKnown)
  println(s"Test 4a - Union of known sets: $resultUnionKnown")
  // Expected output: Map(x1 -> 0.6, x2 -> 0.7)

  // **Test 4b: Union with an unknown fuzzy set**

  val unionUnknown = Union(FuzzySetInstance(setA), setC)
  val resultUnionUnknown = eval(unionUnknown)
  println(s"Test 4b - Union with unknown set: $resultUnionUnknown")
  // Expected output: Partially evaluated expression

  // **Test 5: Partial Evaluation with Scopes**

  // Create a scope with known variables
  eval(CreateScope("MyScope",
    Perform(List(
      Assign("x", Value(5)),
      Assign("y", Value(10)),
      Assign("sum", Add(Variable("x"), Variable("y")))
    ))
  ))

  // Summon the scope and evaluate
  eval(SummonScope("MyScope"))

  // Retrieve and print "sum"
  val sumValue = eval(Variable("sum"))
  println(s"Test 5 - sum in scope: $sumValue") // Expected output: 15

  // **Test 6: Partial Evaluation with Undefined Variables in Scope**

  // Create a scope with an undefined variable
  eval(CreateScope("UnknownVarScope",
    Perform(List(
      Assign("x", Variable("unknownVar")),
      Assign("y", Value(10)),
      Assign("sum", Add(Variable("x"), Variable("y")))
    ))
  ))

  // Summon the scope and evaluate
  eval(SummonScope("UnknownVarScope"))

  // Retrieve and print "sum"
  val sumValueUnknownVar = eval(Variable("sum"))
  println(s"Test 6 - sum with unknown variable: $sumValueUnknownVar")
// Expected output: Partially evaluated expression
