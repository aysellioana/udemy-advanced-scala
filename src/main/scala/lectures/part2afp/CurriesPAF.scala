package lectures.part2afp

object CurriesPAF extends App {

  //curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) //Int => Int = y => 3 + y
  println(add3(5))
  println(superAdder(3)(5)) //curried functions

  //METHOD!!
  def curriedAdder(x: Int)(y: Int): Int = x + y //curried method

  val add4: Int => Int = curriedAdder(4)
  //lifting = ETA-EXPANSION

  //functions != mmethods (JVM limitation)
  def inc(x: Int)= x + 1
  List(1,2,3).map(x => inc(x)) //ETA-exapnsion

  //Partial function applications
  val add5 = curriedAdder(5) _ //Int => Int

  //EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  //add7: Int => Int = y => 7 + y
  //as many different implementations of add7 using the above
  //be creative!

  val add7 = (x: Int) => simpleAddFunction(7,x) //simplest
  val add7_2  =simpleAddFunction.curried(7)
  val add7_6 = simpleAddFunction(7, _:Int) //works as well

  val add7_3 = curriedAddMethod(7) _ //PAF
  val add7_4 =curriedAddMethod(7)(_) //PAF =alternative syntax

  val add7_5 = simpleMethod(7, _ :Int) //alternative syntax for turning methods into function values
                // y => simpleMethod(7,y)

  // underscores are powerfull
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, " how are you?") //x: String => concatenator(hello, x, howareyou)
  println(insertName("Daniel"))

  val fillInTheBlanks = concatenator("Hello, ", _:String, _:String) //(x, y) => concatenator("Hello, ", x, y)
  println(fillInTheBlanks("Daniel", " Scala is awesome!"))

  //EXERCISE
  /*
      1. Process a list of numbers and return their string representations with different formats
        Use the %4.2f, %8.6f and %14.12f with a curried  formatter function.
      /*
      2. difference between
          -functions vs methods
          -params: by-name vs 0-lambda
   */
   */
  def curriedFormatter(s: String)(number: Double): String = s.format(number)

  val number = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  val simpleFormat = curriedFormatter("%4.2f") _ //lift
  val seriousFormat = curriedFormatter("8.6f") _
  val preciseFormat = curriedFormatter("%14.12f")

  println(number.map(curriedFormatter("%14.12f"))) //compiler does sweet eta-expansion fos us

  /*
  calling byName and byFunctions
  -int
  -method
  -paranMethod
  -lambda
  -PFA
   */
  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1
  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23) //ok
  byName(method) //ok
  byName(parenMethod())
  //byName(parenMethod)    //beware ==> byName(parenMethod())
  // byName(() => 42) //not ok
  byName((() => 42)()) //ok
  //byName(parenMethod _) //not ok

  // byFunction(45) //not ok
  //byFunction(method)  //not ok !!!  does not do ETA-expansion!
  byFunction(parenMethod) //compiler does ETA-expansion !!!
  byFunction(() => 46) //works
  byFunction(parenMethod _) // also works, but warning -unnecessary
}
