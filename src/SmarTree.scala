

/**
  * Created by marcin on 2/23/17.
  */
abstract class Tree[A]{
  var root: Option[Node[A]]=None
  def size: Int = this match {
    case Node(_,l,r) => 1+l.size+r.size
    case _ => 1
  }
  def learn(newVal:A)(implicit l:Learner[A]): Tree[A] = this match {
    case Leaf(parameters,category) => {
      if( category==l.category(newVal) ){
        val avgParams=(l.parameters(newVal),parameters).zipped.map((x,y) => (x+y)/2)
        Leaf(avgParams,category)
      }
      else {
        val differences=(l.parameters(newVal),parameters).zipped.map((x,y) => Math.abs((x-y)/(x+y)))
        val pivot:Int=maxInd(differences)
        val pivotValue:Double=Math.min(parameters(pivot),l.parameters(newVal)(pivot))+differences(pivot)/2
        def newPredicate:(A => Boolean)={
          x:A => l.parameters(x)(pivot)>=pivotValue
        }
        if(parameters(pivot)>=pivotValue) Node(newPredicate,Leaf(l.parameters(newVal),l.category(newVal)),this)
        else Node(newPredicate,this,Leaf(l.parameters(newVal),l.category(newVal)))
      }
    }
    case Node(predicate, left,right) =>
      if(predicate(newVal)) Node(predicate,left,right.learn(newVal))
      else Node(predicate,left.learn(newVal),right)

  }

  private def maxInd[A<%Ordered[A]](list: List[A])={
    def maxIndHelp(curInd:Int, curMinInd:Int, curMin:A, xs:List[A]):Int = {
      if(xs.isEmpty) curMinInd
      else {
        if(xs.head>curMin) maxIndHelp(curInd+1,curInd, xs.head,xs.tail)
        else maxIndHelp(curInd+1,curMinInd,curMin,xs.tail)
      }
    }
    if(list.isEmpty) -1
    else maxIndHelp(0,0,list.head,list)
  }

}

case class Node[A](predicate:A =>Boolean, left:Tree[A], right: Tree[A]) extends Tree[A]{
}

case class Leaf[A](parameters:List[Double], category: Int) extends Tree[A]{

}


object TreeTest extends App{
  implicit object IntLearnable extends Learner[Int] {
    override def parameters(value:Int): List[Double] = List(value)

    override def category(value:Int): Int = value
  }
  val intLeafFactory=(value:Int) => Leaf[Int](IntLearnable.parameters(value),IntLearnable.category(value))
  var node:Tree[Int]=Node({ x:Int => x>=50},intLeafFactory(40),intLeafFactory(80))
  println(node)
  node=node.learn(50)
  println(node)





}




