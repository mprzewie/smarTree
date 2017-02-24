

/**
  * Created by marcin on 2/23/17.
  */
abstract class Tree{
  var root: Option[Node]=None
  def size: Int = this match {
    case Node(_,l,r) => 1+l.size+r.size
    case _ => 1
  }
  def learn(newValParams:List[Double],newValCategory:Int): Tree=this match{
    case Leaf(parameters,category) => {
      if( category==newValCategory){
        val avgParams=(newValParams,parameters).zipped.map((x,y) => (x+y)/2)
        Leaf(avgParams,category)
      }
      else {
        val differences=(newValParams,parameters).zipped.map((x,y) => Math.abs((x-y)/(x+y)))
        val pivot:Int=maxInd(differences)
        val pivotValue:Double=Math.min(parameters(pivot),newValParams(pivot))+differences(pivot)/2
        def newPredicate:(List[Double] => Boolean)={
          params:List[Double] => params(pivot)>=pivotValue
        }
        if(parameters(pivot)>=pivotValue) Node(newPredicate,Leaf(newValParams,newValCategory),this)
        else Node(newPredicate,this,Leaf(newValParams,newValCategory))
      }
    }
    case Node(predicate, left,right) =>
      if(predicate(newValParams)) Node(predicate,left,right.learn(newValParams,newValCategory))
      else Node(predicate,left.learn(newValParams,newValCategory),right)

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

case class Node(predicate:List[Double] =>Boolean, left:Tree, right: Tree) extends Tree{
}

case class Leaf(parameters:List[Double], category: Int) extends Tree{

}


object TreeTest extends App{
  implicit object IntLearnable extends Learner[Int] {
    override def parameters(value:Int): List[Double] = List(value)

    override def category(value:Int): Int = value
  }
  val intLeafFactory=(value:Int) => Leaf(IntLearnable.parameters(value),IntLearnable.category(value))
  var node:Tree=Node({params:List[Double] => params(0)>=50},intLeafFactory(40),intLeafFactory(80))
  println(node)
  node=node.learn(List(50),50)
  println(node)





}




