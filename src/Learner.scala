/**
  * Created by marcin on 2/23/17.
  */
abstract class Learner[A]{
  def parameters(value: A): List[Double]
  def category(value: A): Int

}
