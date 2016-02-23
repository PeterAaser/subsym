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


    case class Runner[A <: Genome[A]](
        val initPop: IndexedSeq[Phenotype[A]],
        val done: Population[A] => Boolean,
        val evolve: Population[A] => Population[A]
        // val logger: String => Unit,
        // val writer: java.io.PrintWriter
    ){
        
        def solve: (Population[A], List[(Double, Double)]) =
            run(Population(initPop, 0), List[(Double, Double)]())

        def run(p: Population[A], log: List[(Double, Double)] ): (Population[A], List[(Double, Double)]) = {
            val nextPop = evolve(p)
            val finished = done(nextPop)
            val best = nextPop.fittest.trueFitness
            val avg = nextPop.averageFitness

            if(finished){
                println("YOU'RE WINNER")
                println(nextPop)
                (nextPop.copy(generation = nextPop.generation + 1), (avg, best) :: log)
            }
            else{
                println("generation %d".format(nextPop.generation))
                println(nextPop)
                run(nextPop.copy(generation = nextPop.generation + 1), (avg, best) :: log)
            }
        }
    }

    case class Population[A <: Genome[A]](
        val adults: IndexedSeq[Phenotype[A]],
        val generation: Int
    ){

        def fittest: Phenotype[A] =
            adults.reduceLeft( (l, r) => if (l.trueFitness > r.trueFitness) l else r)

        def averageFitness: Double =
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length


        def verbose: String = {
            "\n\nGeneration: " + generation +
            "\nAvg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length +
            "\nBest fit \n: " +
            fittest +
            "\nPopulation --- \n" +
            adults.mkString("\n") + "\n\n"
        }


        override def toString: String = {
            "\n\nGeneration: " + generation +
            "\nAvg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length +
            "\nBest fit \n: " +
            fittest +
            "\n individuals: %d\n".format(adults.length)
        }
    }
}
