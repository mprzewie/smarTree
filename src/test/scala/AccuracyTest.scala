import iris.IrisFactory

import scala.io.Source
import scala.util.Random

object AccuracyTest extends App {

  implicit object IntLearnable extends Learner[Int] {
    override def parameters(value: Int): List[Double] = List(value)

    override def category(value: Int): Int = value
  }



  val filename="src/main/scala/iris/iris"
  //  println(System.getProperty("user.dir"))
  val factory=new IrisFactory
  val ires=Source.fromFile(filename).getLines().map(line => factory(line)).toList
  val trainCount=ires.size/2

  val shuffledIres = Random.shuffle(ires)

  val trainingIres=shuffledIres.take(trainCount)
  val trainingParams=trainingIres.map(ires=> ires.getParams)
  val trainingCategories=trainingIres.map(ires => ires.getSpecies)

  val testIres = shuffledIres.filter(iris => !trainingIres.contains(iris))
  val testParams = testIres.map(ires => ires.getParams)
  val testCategories = testIres.map(ires => ires.getSpecies)

  println((trainingIres.size, testIres.size))

  var tree=EmptyTree().fit(trainingParams,trainingCategories)
  println(tree.predict(List(ires(100).getParams)))

  var predictions=tree.predict(testIres.map(ires => ires.getParams))
  var hits=(predictions,testCategories).zipped.map((x,y)=> x==y)
  println(hits.count(x => x).toFloat / testIres.size)

}
