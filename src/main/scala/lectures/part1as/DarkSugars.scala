package lectures.part1as

import scala.util.Try

object DarkSugars extends App{

  //syntax sygar #1: methods with single param
  def singleMethod(arg: Int): String = s"$arg little ducks.."

  val description = singleMethod{
    //write some complex code
    42
  }
  val aTryInstance =Try { //java's tru{..}
    throw new RuntimeException
  }
  List(1,2,3).map{ x =>
    x + 1
  }

  //syntax sugar #2: single abstract method
  trait Action {
    def act(x: Int): Int
  }
  val aInstance: Action =new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunckyInstance: Action = (x: Int) => x + 1  //magic

  //example: Runnable
  val aThread = new Thread(new Runnable{
    override def run(): Unit = println("hello,scala")
  })

  val aSweetThread = new Thread(() => println("sweet, Scala!"))

  abstract class AnAbstractType{
    def implemented: Int = 23
    def f(a: Int):Unit
  }
  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  //syntax sugar #3: the :: and #:: methods are special
  val prependedList =  2 :: List(3,4)
  //2.::(List(3,4))
  //List(3,4).::(2)
  //?!

  //scala spec: last char decides associativity of method
  1 :: 2 :: 3 :: List(4,5)
  List(4,5).::(3).::(2).::(1) //equivalent

  class MyStream[T]{
    def -->:(value: T): MyStream[T] = this //actual implementation here
  }
  val myStream = 1 -->: 2 -->:3  -->: new MyStream[Int]

  //syntax sugar #4: multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "Scalla is so sweet!"

  //syntax sugar #5: infix types
  class Composite[A, B]
  val composite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???

  //syntax sugar #6: update() is very special, mucg like apply()
  val anArray = Array(1,2,3)
  anArray(2) = 7 //rewritten to anArray.update(2,7)
  //used in mutable collections
  //remember apply() AND update()!

  //sytax sugar #7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 //private for OO encapsulation
    def member: Int = internalMember // "getter"
    def member_= (value: Int): Unit =
      internalMember = value // "setter"
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 //rewittern as aMutableContainer.member_=(42)
}
