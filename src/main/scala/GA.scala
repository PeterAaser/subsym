package GA

import scala._
import Data._
import Durp._

import scalaz._
import Scalaz._

object GAsolver {

    def main(args: Array[String]): Unit = {
        
        println("hello")

        val fish = Fish("Jimmy", 2)
        val fish2 = fish.renamed("Bob")

        val kot = Kitty("Ramses", "Grå")
        val notKot = kot.renamed("lal")
        val ramses = esquire(kot)

        println(fish)
        println(fish2)

        println(kot)
        println(notKot)

        println(ramses)

    }
}

object OneMax {

    

}
