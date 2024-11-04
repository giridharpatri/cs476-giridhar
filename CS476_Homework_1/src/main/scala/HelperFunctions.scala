import scala.collection.mutable
import Frizzy.*
import Frizzy.ExpOperation.*
import Frizzy.given
object HelperFunctions:
  import Frizzy.*

  def evalClassBody(body: ExpOperation, classInfo: ClassInfo): Unit = body match
    case Perform(ops: List[ExpOperation]) =>
      for op <- ops do
        evalClassBody(op, classInfo)
    case ClassVar(name: String, varType: String) =>
      classInfo.variables.update(name, varType)
    case MethodDef(methodName: String, params: List[Parameter], methodBody: ExpOperation) =>
      classInfo.methods.update(methodName, MethodInfo(methodName, params, methodBody))
    case ClassDef(name: String, body: ExpOperation, parent: Option[String]) =>
      
      val nestedClassInfo = ClassInfo(name, parent, mutable.Map(), mutable.Map(), mutable.Map())
      // Evaluate the nested class body
      evalClassBody(body, nestedClassInfo)
      // Add the nested class to the parent class's nestedClasses map
      classInfo.nestedClasses.update(name, nestedClassInfo)
      // **Register the nested class in the global classRegistry**
      classRegistry.update(name, nestedClassInfo)
    case _ =>
      throw new Exception(s"Unsupported operation in class body: $body")


  def initializeInstanceVariables(classInfo: ClassInfo, instanceEnv: mutable.Map[String, Any]): Unit =
    // Initialize variables from the superclass first
    classInfo.parent.foreach { parentName =>
      val parentClassInfo = classRegistry.getOrElse(parentName, throw new Exception(s"Superclass '$parentName' not found"))
      initializeInstanceVariables(parentClassInfo, instanceEnv)
    }
    // Initialize variables from the current class
    for (varName, varType) <- classInfo.variables do
      if (!instanceEnv.contains(varName)) then
        instanceEnv.update(varName, getDefaultForType(varType))

  def lookupMethod(className: String, methodName: String): MethodInfo =
    val classInfo = classRegistry.getOrElse(className, throw new Exception(s"Class '$className' not found"))
    classInfo.methods.get(methodName) match
      case Some(methodInfo) => methodInfo
      case None =>
        classInfo.parent match
          case Some(parentName) => lookupMethod(parentName, methodName)
          case None => throw new Exception(s"Method '$methodName' not found in class '$className' or its superclasses")


  def getDefaultForType(varType: String): Any = varType match
    case "int" => 0
    case "string" => ""
    case "FuzzySet" => Map.empty[Element, FuzzyValue]
    case _ => null

  def evalFuzzySet(exp: ExpOperation): FuzzySet = eval(exp) match
    case s: FuzzySet => s
    case _ => throw new Exception("Expected FuzzySet")
