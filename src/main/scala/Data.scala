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

    trait Strategy[A <: Genome[A]] {
        def evolve(p: Population[A]): Population[A]
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

    case class Runner[A <: Genome[A]](
        val initialize: Int => Population[A],
        val done: Population[A] => Boolean,
        val modify: ((Population[A] => Population[A]), Population[A]) => (Population[A] => Population[A]),
        val evolve: Population[A] => Population[A]
    ){
        def run(p: Population[A]): (Runner[A], Population[A]) = {
            val nextEvolve = modify(evolve, p) 
            val nextPop = evolve(p)
            val finished = done(nextPop)
            val nextRunner = this.copy(evolve = nextEvolve)
            if(finished)
                (this, p)
            else
                nextRunner.run(nextPop)
        }
    }

    case class Params[A <: Genome[A]](val control: Population[A] => (Population[A] => Population[A]))

    case class Population[A <: Genome[A]](
        val adults: IndexedSeq[Phenotype[A]],
        val generation: Int
    ){

        def fittest: Phenotype[A] = 
            adults.maxBy(_.trueFitness)

        def averageFitness: Double =
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length

        def verbose: String = {
            "Population --- \n" +
            adults.mkString("\n") + "\n\n" +
            "Avg fitness: " +
            averageFitness.toString +
            "\nBest fit \n: " +
            fittest
        }

        override def toString: String = {
            "Avg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length +
            "\nBest fit \n: " +
            (if (adults.isEmpty) "n/a" else adults.maxBy(_.trueFitness).trueFitness)
        }
    }
}
