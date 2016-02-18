package GA

import scala._
import scalaz._
import Scalaz._

import Data._

object Data {

    trait Gene[A <: Gene[A]] {
        def cross(g2: A): (A, A)
        def mutate: A
    }


    trait Genome[A <: Genome[A]] {
        def cross(gn2: A): (A, A)
        def mutate(rate: Double): A
    }

    case class Phenotype[A <: Genome[A]](
        genome: A,
        relativeFitness: Double, 
        trueFitness: Double,
        age: Int
    ){
        def reset: Phenotype[A] = 
            this.copy(relativeFitness = trueFitness)
    }


    case class Params[A <: Genome[A]](val control: Population[A] => (Population[A] => Population[A]))


    case class Population[A <: Genome[A]](
        val adults: IndexedSeq[Phenotype[A]],
        val evolve: Population[A] => Population[A]
    ){

        def next = evolve(this)

        def run(n: Int): Population[A] = {
            println(this.verbose)
            if(n > 1){
                val evo = next
                evo.run(n - 1)
            }
            else
                this
        }

        def verbose: String = {
            "Population --- \n" +
            adults.mkString("\n") + "\n\n" +
            "Avg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length + 
            "\nBest fit \n: " +
            (if (adults.isEmpty) "n/a" else adults.maxBy(_.trueFitness).trueFitness)
        }

        override def toString: String = {
            "Avg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length +
            "\nBest fit \n: " +
            (if (adults.isEmpty) "n/a" else adults.maxBy(_.trueFitness).trueFitness)
        }
    }
}
