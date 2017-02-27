import scala.io.Source
import iris._
import scala.util.Random
/**
  * Created by marcin on 2/23/17.
  */
abstract class Tree {
  var root: Option[Node] = None

  def size: Int = this match {
    case Node(_, l, r) => 1 + l.size + r.size
    case Leaf(_, _) => 1
    case _ => 0
  }

  private def learn(newValParams: List[Double], newValCategory: Int): Tree = this match {
    case EmptyTree() => Leaf(newValParams, newValCategory)
    case Leaf(parameters, category) => {
        val differences = (newValParams, parameters).zipped.map((x, y) => Math.abs((x - y) / (x + y)))
        val pivot: Int = maxInd(differences)
        if(differences(pivot)==0) this
        else{
          val pivotValue: Double = Math.min(parameters(pivot), newValParams(pivot)) + differences(pivot) / 2

          def newPredicate: (List[Double] => Boolean) = {
            params: List[Double] => params(pivot) >= pivotValue
          }

          if (parameters(pivot) >= pivotValue) Node(newPredicate, Leaf(newValParams, newValCategory), this)
          else Node(newPredicate, this, Leaf(newValParams, newValCategory))
        }

    }
    case Node(predicate, left, right) =>
      if (predicate(newValParams)) Node(predicate, left, right.learn(newValParams, newValCategory))
      else Node(predicate, left.learn(newValParams, newValCategory), right)

  }

  private def maxInd[A <% Ordered[A]](list: List[A]) = {
    def maxIndHelp(curInd: Int, curMinInd: Int, curMin: A, xs: List[A]): Int = {
      if (xs.isEmpty) curMinInd
      else {
        if (xs.head > curMin) maxIndHelp(curInd + 1, curInd, xs.head, xs.tail)
        else maxIndHelp(curInd + 1, curMinInd, curMin, xs.tail)
      }
    }

    if (list.isEmpty) -1
    else maxIndHelp(0, 0, list.head, list)
  }

  private def guess(params: List[Double]): Int = this match {
    case Leaf(_, category) => category
    case Node(predicate, left, right) => {
      if (predicate(params)) right.guess(params)
      else left.guess(params)
    }
    case _ => 0
  }

  private def listLearn(paramsList: List[List[Double]], categoryList: List[Int]): Tree = {
    if (paramsList.isEmpty) this
    else listLearn(paramsList.tail, categoryList.tail).learn(paramsList.head, categoryList.head)
  }

  private def trim(): Tree=this match {
    case Node(predicate, left, right) => {
      val leftTrimmed=left.trim()
      val rightTrimmed=right.trim()
      (leftTrimmed, rightTrimmed) match {
        case (Leaf(lParams, lCategory), Leaf(rParams, rCategory)) =>{
          if(lCategory==rCategory){
            Leaf((lParams,rParams).zipped.map((x,y) => (x+y)/2), lCategory)
          }
          else Node(predicate,leftTrimmed,rightTrimmed)
        }
        case _ => Node(predicate,leftTrimmed,rightTrimmed)
      }
  }
    case _ => this
  }

  def fit(paramsList: List[List[Double]], categoryList: List[Int]): Tree = listLearn(paramsList,categoryList).trim()





  def predict(paramsList:List[List[Double]]):List[Int]={
    paramsList.map(params =>guess(params))
  }


}

case class Node(predicate: List[Double] => Boolean, left: Tree, right: Tree) extends Tree

case class Leaf(parameters: List[Double], category: Int) extends Tree

case class EmptyTree() extends Tree


object TreeTest extends App {

  implicit object IntLearnable extends Learner[Int] {
    override def parameters(value: Int): List[Double] = List(value)

    override def category(value: Int): Int = value
  }



  val filename="src/iris/iris"
//  println(System.getProperty("user.dir"))
  val factory=new IrisFactory
  val ires=Source.fromFile(filename).getLines().map(line => factory(line)).toList
  val trainCount=ires.size/2
  val trainingIres=Random.shuffle(ires).take(trainCount)
  val trainingParams=trainingIres.map(ires=> ires.getParams)
  val trainingCategories=trainingIres.map(ires => ires.getSpecies)

  var tree=EmptyTree().fit(trainingParams,trainingCategories)
  println(tree.size)
  println(tree.predict(List(ires(100).getParams)))

  var predictions=tree.predict(ires.map(ires => ires.getParams))
  var actual=ires.map(ires => ires.getSpecies)
  var hits=(predictions,actual).zipped.map((x,y)=> x==y)
  println(hits.count(x => x))





  println(tree)



}




