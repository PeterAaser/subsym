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
        val initPop: Int => IndexedSeq[Phenotype[A]],
        val done: Population[A] => Boolean,
        val evolve: Population[A] => Population[A]
    ){
        
        def solve(poolSize: Int): Population[A] =
            run(Population(initPop(poolSize), 0))

        def run(p: Population[A]): Population[A] = {
            val nextPop = evolve(p)
            val finished = done(nextPop)
            if(finished){
                println(nextPop.verbose)
                nextPop.copy(generation = nextPop.generation + 1)
            }
            else{
                println(nextPop)
                run(nextPop.copy(generation = nextPop.generation + 1))
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
            fittest
        }
    }
}
