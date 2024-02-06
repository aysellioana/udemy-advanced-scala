package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean ){
  /*
  Exercise - implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]  //union

  def map[B](f: A => B):MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
  EXERCISE
  -removing an element
  -intersetion with another set
  -difference with another set
   */

  def -(elem: A):MySet[A]
  def --(anotherSet: MySet[A]): MySet[A] //difference
  def &(anotherSer: MySet[A]):MySet[A]  //intersection

  //Exercise #3 - implement a unary_! = NEGATION of a set
  //set[1,2,3] =>
  def unary_! : MySet[A]
}



class EmptySet[A] extends MySet[A]{
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NotEmptySet[A](elem, tail = this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()

  //part 2
  def -(elem: A): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this //difference
  def &(anotherSer: MySet[A]): MySet[A]  = this//intersection
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)

}

class AllInclusiveSet[A] extends MySet[A]{
  override def contains(elem: A): Boolean = true
  override def +(elem: A): MySet[A] = this
  override def ++(anotherSet: MySet[A]): MySet[A] = this

  //naturals = allInclusiveSet[Int] = all the naural numbers
  //naturals.map(x => x % 3) => ???
  // [0 1 2]
  override def map[B](f: A => B): MySet[B] = ???
  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???
  override def filter(predicate: A => Boolean): MySet[A] =  ??? //property-based set

  override def foreach(f: A => Unit): Unit = ???

  override def -(elem: A): MySet[A] = ???

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def &(anotherSer: MySet[A]): MySet[A] = filter(anotherSer)
  override def unary_! : MySet[A] = new EmptySet[A]
}

//elements of type A which satisfy a property
//{ a in A | property(x)
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]{
  def contains(elem: A): Boolean = property(elem)
  //{x in A | property(x) } + element = { x in A | property(x) || x== element}
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

   //x in A | property(x) } ++ set => { x in A | property(x) ||set contains x}
  def ++(anotherSet: MySet[A]): MySet[A] = //union
     new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  //all integers => (_ % 3) => [0 1 2]
  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  def foreach(f: A => Unit): Unit = politelyFail

  def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))

  def -(elem: A): MySet[A] = filter(x => x != elem)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  def &(anotherSer: MySet[A]): MySet[A] = filter(anotherSer)
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))
  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class NotEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  def +(elem: A): MySet[A] =
    if(this contains elem)  this
    else new NotEmptySet[A](elem, this)

  /*
      [1 2 3] ++ [4,5]
      [2 3] ++ [4 5] + 1
      [3] ++ [4 5] + 1 + 2
      [] ++ [4 5] + 1 + 2 + 3
      [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
       */
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if(predicate(head)) filteredTail +  head
    else filteredTail
  }

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  //part 2
  def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  def --(anotherSet: MySet[A]): MySet[A] =filter(!anotherSet)
  def &(anotherSer: MySet[A]): MySet[A] = filter(anotherSer) //intersection =filtering!!!!

  //new operator
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))

}

object MySet {
  /*
  val s = MySet(1,2,3) = buildSet(seq(1,2,3), [])
  =buildSet(seq(2,3), [] + 1)
  =buildSet(seq(3), [1] + 2)
  = buidSet(seq(), [1 2] + 3)
  =[1 2 3]
   */

  def apply[A](values: A*) : MySet[A] ={
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc +  valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }


}

object MySetPlayground extends App{
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1, -2) + 3 flatMap ( x => MySet(x, x * 10)) filter (_ % 2 == 0) foreach println

  val negative = !s //s.unary_! = all the naturals not equal to 1,2,3,4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5 // all the even numbers > 4 + 5
  println(negativeEven5(5))
}