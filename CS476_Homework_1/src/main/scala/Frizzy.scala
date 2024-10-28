import scala.collection.mutable

  // TESTING TO SEE IF SOURCE CONTROL IS WORKING!!
object Frizzy:
  // define types for a fuzzy value
  type FuzzyValue = Double
  type Element = String 
  // define a fuzzy set
  type FuzzySet = Map[Element, FuzzyValue]

  type Environment = mutable.Map[String, FuzzySet]
  type EnvironmentStack = mutable.Stack[(String, Environment)]
  type EvalContext = EnvironmentStack ?=> FuzzySet

  // Initialize the environment stack with a global environment, keeps track of what scope is in
  // session currently. 
  given envStack: EnvironmentStack = mutable.Stack(("", mutable.Map[String, FuzzySet]()))

  // Scope registry for storing scope definitions
  val scopeRegistry: mutable.Map[String, ExpOperation] = mutable.Map()

  enum ExpOperation:
    // define variables and assignment operation. FuzzySetInstance is just a FuzzySet 
    // initialized with a Map
    case FuzzySetInstance(set: FuzzySet)
    case Variable(name: String)
    case Assign(name: String, expr: ExpOperation)

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

  import ExpOperation.*

  // evaluation function
  def eval(exp: ExpOperation): EvalContext = exp match
    case FuzzySetInstance(set) => set

    case Variable(name) =>
      summon[EnvironmentStack].find(_._2.contains(name)) match
        case Some((_, env)) => env(name)
        case None => throw new Exception(s"Variable '$name' not found")

    case Assign(name, expr) =>
      val value = eval(expr)
      val currentEnv = summon[EnvironmentStack].top._2
      currentEnv.update(name, value)
      value

    case Union(p1, p2) =>
      val set1 = eval(p1)
      val set2 = eval(p2)
      val unionSet = (set1.keySet ++ set2.keySet).map { element =>
        val mu1 = set1.getOrElse(element, 0.0)
        val mu2 = set2.getOrElse(element, 0.0)
        element -> Math.max(mu1, mu2)
      }.toMap
      unionSet

    case Intersection(p1, p2) =>
      val set1 = eval(p1)
      val set2 = eval(p2)
      val intersectionSet = (set1.keySet ++ set2.keySet).map { element =>
        val mu1 = set1.getOrElse(element, 0.0)
        val mu2 = set2.getOrElse(element, 0.0)
        element -> Math.min(mu1, mu2)
      }.toMap
      intersectionSet

    case Complement(p) =>
      val set = eval(p)
      val complementSet = set.map { case (element, mu) =>
        element -> (1.0 - mu)
      }
      complementSet

    case AlphaCut(alpha, setExpr) =>
      val set = eval(setExpr)
      val alphaCutSet = set.collect {
        case (element, mu) if mu >= alpha => element -> 1.0
      }
      alphaCutSet

    case XOR(p1, p2) =>
      val set1 = eval(p1)
      val set2 = eval(p2)
      val xorSet = (set1.keySet ++ set2.keySet).map { element =>
        val mu1 = set1.getOrElse(element, 0.0)
        val mu2 = set2.getOrElse(element, 0.0)
        element -> Math.abs(mu1 - mu2)
      }.toMap
      xorSet

    // use to create an "anonymous" scope that ends, cannot be reused
    case BeginScope(name) =>
      val envStack = summon[EnvironmentStack]
      envStack.push((name, mutable.Map[String, FuzzySet]()))
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
      var result: FuzzySet = Map.empty[Element, FuzzyValue]
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
          envStack.push((name, mutable.Map[String, FuzzySet]()))
          val result = eval(body)
          envStack.pop()
          result
        case None =>
          throw new Exception(s"this scope is not defined...")

  // main file, tests included in test folder (sbt test)
  @main def main(): Unit =
    // The environment stack and scope registry are already initialized ??

    // Define elements (da universe)
    val universe: Set[Element] = Set("x1", "x2", "x3", "x4")

    // Create fuzzy sets A, B, and C
    val setA = Map("x1" -> 0.2,  "x2" -> 0.8,  "x3" -> 0.5, "x4" -> 1.0)
    val setB = Map("x1" -> 0.5,  "x2" -> 0.6,  "x3" -> 0.3, "x4" -> 0.7)
    val setC = Map("x1" -> 0.75, "x2" -> 0.75, "x3" -> 0.75, "x4" -> 0.75)

    eval(Assign("x", FuzzySetInstance(setC)))

    // Define a reusable named scope "MyScope" with a body of operations
    eval(CreateScope("MyScope",
      Perform(List(
        Assign("x", FuzzySetInstance(setA)),
        Assign("y", FuzzySetInstance(setB)),
        Assign("unionResult", Union(Variable("x"), Variable("y")))
        // Additional operations can be added in the list 
        // the last operation's (end of list) result will be returned
      ))
    ))


    val something = eval(SummonScope("MyScope"))
    println(s"Result of the scope smth smth blah blah created: $something")
