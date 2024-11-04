import scala.collection.mutable
import HelperFunctions.*


  // TESTING TO SEE IF SOURCE CONTROL IS WORKING!!
object Frizzy:
  // define types for a fuzzy value
  type FuzzyValue = Double
  type Element = String 
  // define a fuzzy set
  type FuzzySet = Map[Element, FuzzyValue]

  type Environment = mutable.Map[String, Any]
  type EnvironmentStack = mutable.Stack[(String, Environment)]
  type EvalContext = EnvironmentStack ?=> Any

  // Initialize the environment stack with a global environment, keeps track of what scope is in
  // session currently. 
  given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, Any]()))

  // Scope registry for storing scope definitions
  val scopeRegistry: mutable.Map[String, ExpOperation] = mutable.Map()
  val classRegistry: mutable.Map[String, ClassInfo] = mutable.Map()
  val instanceRegistry: mutable.Map[String, InstanceInfo] = mutable.Map()
  // Class information
  case class ClassInfo(name: String, parent: Option[String], variables: mutable.Map[String, String], // variable name and type
                        methods: mutable.Map[String, MethodInfo],
                        nestedClasses: mutable.Map[String, ClassInfo]
                      )

  // Method information
  case class MethodInfo(name: String, params: List[Parameter], body: ExpOperation)

  // Instance information
  case class InstanceInfo(className: String, variables: mutable.Map[String, Any] )
  case class Parameter(name: String, paramType: String)



  enum ExpOperation:
    // define variables and assignment operation. FuzzySetInstance is just a FuzzySet 
    // initialized with a Map
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
    case Scope(body: ExpOperation)
    
    // Use this to have multiple operatons executed one after another, used when 
    // creating a scope
    case Perform(ops: List[ExpOperation])
    
    // create and summon a scope (for modularity)
    case CreateScope(name: String, body: ExpOperation)
    case SummonScope(name: String)
    // oop related stuff now
    case ClassDef(name: String, body: ExpOperation, parent: Option[String] = None)
    case ClassVar(name: String, varType: String)
    case MethodDef(methodName: String, params: List[Parameter], body: ExpOperation)
    //case Parameter(name: String, paramType: String)
    case CreateInstance(className: String, instanceName: String)
    case InvokeMethod(instanceName: String, methodName: String, args: List[ExpOperation])
    case Extends(subclass: String, superclass: String)
    case NestedClassDef(outerClassName: String, nestedClass: ExpOperation)
    case SuperInvoke(methodName: String, args: List[ExpOperation])


  import ExpOperation.*
  // evaluation function

  def eval(exp: ExpOperation): EvalContext = exp match
    case FuzzySetInstance(set) => set
    case Value(v) => v

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
      // Return an empty value or any appropriate result
      ()



    //    case ClassVar(name, varType) =>
//      // Add the variable to the current class environment
//      val currentEnv = summon[EnvironmentStack].top._2
//      currentEnv.update(name, null) // Initialize with null or a default value
//      // Record the variable in the classInfo
//      // (Assuming we have access to the current ClassInfo object)
//      // currentClassInfo.variables.update(name, varType)
//      Map.empty[Element, FuzzyValue]
//
//    case MethodDef(methodName, params, body) =>
//      // Record the method in the current class's method map
//      // currentClassInfo.methods.update(methodName, MethodInfo(methodName, params, body))
//      Map.empty[Element, FuzzyValue]

    case CreateInstance(className, instanceName) =>
      val classInfo = classRegistry.getOrElse(className, throw new Exception(s"Class '$className' not found"))
      // Create a new instance environment
      val instanceEnv = mutable.Map[String, Any]()
      // Initialize variables from class definition
      initializeInstanceVariables(classInfo, instanceEnv)
      // Add the instance to the registry
      instanceRegistry.update(instanceName, InstanceInfo(className, instanceEnv))
      Map.empty[Element, FuzzyValue]


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

    case Extends(subclass, superclass) =>
      val subclassInfo = classRegistry.getOrElse(subclass, throw new Exception(s"Class '$subclass' not found"))
      // Create a new ClassInfo with the updated parent
      val updatedSubclassInfo = subclassInfo.copy(parent = Some(superclass))
      // Update the classRegistry with the new ClassInfo
      classRegistry.update(subclass, updatedSubclassInfo)
      Map.empty[Element, FuzzyValue]


    case NestedClassDef(outerClassName, nestedClassExp) =>
      val outerClassInfo = classRegistry.getOrElse(outerClassName, throw new Exception(s"Class '$outerClassName' not found"))
      eval(nestedClassExp)
      // After evaluation, the nested class should be registered in classRegistry
      // We can retrieve it and add it to the outer class's nestedClasses
      nestedClassExp match
        case ClassDef(nestedClassName, _, _) =>
          val nestedClassInfo = classRegistry(nestedClassName)
          outerClassInfo.nestedClasses.update(nestedClassName, nestedClassInfo)
          // Remove from global registry if necessary
          classRegistry.remove(nestedClassName)
        case _ => throw new Exception("Invalid nested class definition")
      Map.empty[Element, FuzzyValue]

    case Variable(name) =>
      val envStack = summon[EnvironmentStack]
      // Search all environments from top to bottom
      envStack.find(_._2.contains(name)) match
        case Some((_, env)) => env(name)
        case None => throw new Exception(s"Variable '$name' not found")



    case Assign(name, expr) =>
      val value = eval(expr)
      val currentEnv = summon[EnvironmentStack].top._2
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
        // Regular variable assignment
        currentEnv.update(name, value)
      value



    case Union(p1, p2) =>
      val set1 = evalFuzzySet(p1)
      val set2 = evalFuzzySet(p2)
      val unionSet = (set1.keySet ++ set2.keySet).map { element =>
        val mu1 = set1.getOrElse(element, 0.0)
        val mu2 = set2.getOrElse(element, 0.0)
        element -> Math.max(mu1, mu2)
      }.toMap
      unionSet


    case Intersection(p1, p2) =>
      val set1 = evalFuzzySet(p1)
      val set2 = evalFuzzySet(p2)
      val intersectionSet = (set1.keySet ++ set2.keySet).map { element =>
        val mu1 = set1.getOrElse(element, 0.0)
        val mu2 = set2.getOrElse(element, 0.0)
        element -> Math.min(mu1, mu2)
      }.toMap
      intersectionSet

    case Complement(p) =>
      val set = evalFuzzySet(p)
      val complementSet = set.map { case (element, mu) =>
        element -> (1.0 - mu)
      }
      complementSet

    case AlphaCut(alpha, setExpr) =>
      val set = evalFuzzySet(setExpr)
      val alphaCutSet = set.collect {
        case (element, mu) if mu >= alpha => element -> 1.0
      }
      alphaCutSet

    case XOR(p1, p2) =>
      val set1 = evalFuzzySet(p1)
      val set2 = evalFuzzySet(p2)
      val xorSet = (set1.keySet ++ set2.keySet).map { element =>
        val mu1 = set1.getOrElse(element, 0.0)
        val mu2 = set2.getOrElse(element, 0.0)
        element -> Math.abs(mu1 - mu2)
      }.toMap
      xorSet

    // use to create an "anonymous" scope that ends, cannot be reused
    case BeginScope(name) =>
      val envStack = summon[EnvironmentStack]
      envStack.push((name, mutable.Map[String, Any]()))
      Map.empty[Element, FuzzyValue]
   // end the scope that was started with the latest BeginScope call
    case EndScope(name) =>
      val envStack = summon[EnvironmentStack]
      if envStack.nonEmpty && envStack.top._1 == name then
        envStack.pop()
      else
        throw new Exception(s"'$name' is not a scope...")
      Map.empty[Element, FuzzyValue]

//    case Scope(body) =>
//      val envStack = summon[EnvironmentStack]
//      envStack.push(("", mutable.Map[String, FuzzySet]()))
//      val result = eval(body)
//      envStack.pop()
//      result

    // this executes a sequence of operations. Use this when creating a new scope
    case Perform(ops) =>
      var result: Any =()
      for (op <- ops){ result = eval(op)}
      result

    // this is used when you want to create a new Scope.
    // it takes a Perform construct, which in turn takes a list of ExpOperation objects
    case CreateScope(name, body) =>
      scopeRegistry.update(name, body)
      Map.empty[Element, FuzzyValue]

    // this calls the sequence of operations for called scope
    case SummonScope(name) =>
      scopeRegistry.get(name) match
        case Some(body) =>
          val envStack = summon[EnvironmentStack]
          envStack.push((name, mutable.Map[String, Any]()))
          val result = eval(body)
          envStack.pop()
          result
        case None =>
          throw new Exception(s"this scope is not defined...")





@main def main(): Unit =
  // Import necessary items if not already in scope
  import Frizzy.*
  import Frizzy.ExpOperation.*
  import HelperFunctions.*
  import Frizzy.given

  // **Test 1: Class Definitions and Inheritance**

  // Define the Base class
  eval(ClassDef("Base",
    body = Perform(List(
      ClassVar("x", "int"),
      MethodDef("getX", List(), Variable("x")),
      MethodDef("setX", List(Parameter("value", "int")), Assign("x", Variable("value")))
    ))
  ))

  // Define the Derived class that extends Base
  eval(ClassDef("Derived",
    body = Perform(List(
      ClassVar("x", "int"), // Shadows Base.x
      MethodDef("getX", List(), Variable("x")), // Overrides Base.getX
      MethodDef("setX", List(Parameter("value", "int")), Assign("x", Variable("value")))
    )),
    parent = Some("Base")
  ))

  // **Test 2: Instance Creation and Method Invocation**

  // Create an instance of Derived
  eval(CreateInstance("Derived", "d"))

  // Set x in Derived instance 'd'
  eval(InvokeMethod("d", "setX", List(Value(10))))

  // Get x from Derived instance 'd'
  val derivedX = eval(InvokeMethod("d", "getX", List()))
  println(s"Value of x in Derived instance 'd': $derivedX") // Expected output: 10

  // Create an instance of Base
  eval(CreateInstance("Base", "b"))

  // Set x in Base instance 'b'
  eval(InvokeMethod("b", "setX", List(Value(5))))

  // Get x from Base instance 'b'
  val baseX = eval(InvokeMethod("b", "getX", List()))
  println(s"Value of x in Base instance 'b': $baseX") // Expected output: 5

  // **Test 3: Inheritance without Overriding**

  // Define the Derived2 class that extends Base but does not override getX or setX
  eval(ClassDef("Derived2",
    body = Perform(List(
      ClassVar("y", "int"),
      MethodDef("setY", List(Parameter("value", "int")), Assign("y", Variable("value")))
    )),
    parent = Some("Base")
  ))

  // Create an instance of Derived2
  eval(CreateInstance("Derived2", "d2"))

  // Set x in Derived2 instance 'd2' (inherited from Base)
  eval(InvokeMethod("d2", "setX", List(Value(20))))

  // Get x from Derived2 instance 'd2' (should use Base's getX)
  val derived2X = eval(InvokeMethod("d2", "getX", List()))
  println(s"Value of x in Derived2 instance 'd2': $derived2X") // Expected output: 20

  // **Test 4: Variable Shadowing in Nested Classes**

    // Define the Outer class with a nested Inner class
    eval(ClassDef("Outer",
      body = Perform(List(
        ClassVar("x", "int"),
        MethodDef("getX", List(), Variable("x")),
        // Define the nested Inner class
        ClassDef("Inner",
          body = Perform(List(
            ClassVar("x", "int"), // Shadows Outer.x
            MethodDef("getX", List(), Variable("x"))
          ))
        )
      ))
    ))

  // Create an instance of Outer
  eval(CreateInstance("Outer", "outerInstance"))

  // Set x in Outer instance 'outerInstance'
  eval(Assign("outerInstance.x", Value(100)))

  // Create an instance of Inner
  // Assuming Inner is registered when evaluating the Outer class definition
  eval(CreateInstance("Inner", "innerInstance"))

  // Set x in Inner instance 'innerInstance'
  eval(Assign("innerInstance.x", Value(200)))

  // Invoke getX on innerInstance
  val innerX = eval(InvokeMethod("innerInstance", "getX", List()))
  println(s"Value of x in Inner instance 'innerInstance': $innerX") // Expected output: 200

  // Invoke getX on outerInstance
  val outerX = eval(InvokeMethod("outerInstance", "getX", List()))
  println(s"Value of x in Outer instance 'outerInstance': $outerX") // Expected output: 100

  // **Test 5: Method Overriding and Dynamic Dispatch**

  // Define the Parent class
  eval(ClassDef("Parent",
    body = Perform(List(
      MethodDef("greet", List(), Value("Hello from Parent"))
    ))
  ))

  // Define the Child class that extends Parent
  eval(ClassDef("Child",
    body = Perform(List(
      MethodDef("greet", List(), Value("Hello from Child"))
    )),
    parent = Some("Parent")
  ))

  // Create instances of Parent and Child
  eval(CreateInstance("Parent", "p"))
  eval(CreateInstance("Child", "c"))

  // Invoke greet on Parent instance 'p'
  val parentGreet = eval(InvokeMethod("p", "greet", List()))
  println(s"Parent instance 'p' says: $parentGreet") // Expected output: "Hello from Parent"

  // Invoke greet on Child instance 'c'
  val childGreet = eval(InvokeMethod("c", "greet", List()))
  println(s"Child instance 'c' says: $childGreet") // Expected output: "Hello from Child"

  // **Test 6: Inherited Method without Overriding**

  // Define Sibling class that extends Parent but does not override greet
  eval(ClassDef("Sibling",
    body = Perform(List(
      // No greet method defined here
    )),
    parent = Some("Parent")
  ))

  // Create an instance of Sibling
  eval(CreateInstance("Sibling", "s"))

  // Invoke greet on Sibling instance 's'
  val siblingGreet = eval(InvokeMethod("s", "greet", List()))
  println(s"Sibling instance 's' says: $siblingGreet") // Expected output: "Hello from Parent"















