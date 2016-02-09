package GA

import scala._
import Data._

import scalaz._
import Scalaz._

object GAsolver {

    def main(args: Array[String]): Unit = {
        
        println("hello")

        val vec_a = Vector(1, 0, 0, 1, 0, 0, 1, 1, 0)
        val vec_b = Vector(2, 3, 3, 2, 2, 2, 3, 2, 3)
        
        val slice_start = 3
        val slice_end = 5

        lazy val vec_a_init = vec_a take slice_start
        val vec_a_kodon = vec_a view (slice_start, slice_end)
        lazy val vec_a_rest = vec_a takeRight slice_end 

        println(vec_a_init ++ vec_a_kodon ++ vec_a_rest)
        println(vec_a_kodon.toVector)
        println(vec_a_rest)


        val next_a = Vector(1, 0, 0, 2, 2, 2, 1, 1, 0)
        val next_b = Vector(2, 3, 3, 1, 0, 0, 3, 2, 3)

        // println(vec_a)
    }
}

object OneMax {

    

}
