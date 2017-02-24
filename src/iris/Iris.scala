package iris

/**
  * Created by marcin on 2/24/17.
  */
class Iris(sepLength: Double, sepWidth:Double, petLength:Double,petWidth:Double,species:Int){

  def getParams:List[Double]=List(sepLength,sepWidth,petLength,petWidth)
  def getSpecies:Int=species
}

class IrisFactory{
  val names=List("Iris-setosa", "Iris-versicolor","Iris-virginica")
  def apply(line:String):Iris={
    val params=line.split(",")
    val sepLength=params(0).toDouble
    val sepWidth=params(1).toDouble
    val petLength=params(2).toDouble
    val petWidth=params(3).toDouble
    val species=names.indexOf(params(4))
    new Iris(sepLength,sepWidth,petLength,petWidth,species)

  }
}

object IrisTest extends App{
  val factory=new IrisFactory
  println(factory("7.1,3.0,5.9,2.1,Iris-virginica").getParams)

}