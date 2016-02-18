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
    )


    case class geneOps[A <: Genome[A]]( 
        grow: A => Phenotype[A],
        childSel: IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]],
        adultSel: (IndexedSeq[Phenotype[A]], IndexedSeq[Phenotype[A]]) => IndexedSeq[Phenotype[A]],
        parentSel: IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]],
        makeChildren: IndexedSeq[Phenotype[A]] => IndexedSeq[A]
    )

    case class Population[A <: Genome[A]](
        val genotypes: IndexedSeq[A],
        val adults: IndexedSeq[Phenotype[A]],
        val config: geneOps[A]
    ){

        override def toString: String = {
            "Population --- \n" +
            genotypes.mkString("\n") + "\n\n" +
            adults.mkString("\n") + "\n\n" +
            "Avg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length
        }
    }

    case object Population {

        def nextAdultPool[A <: Genome[A]](p: Population[A]): Population[A] = {
            val children = p.config.childSel(p.genotypes.map(g => p.config.grow(g)))
            val adults = p.config.adultSel(children, p.adults)
            p.copy(adults = adults)
        }

        def createChildren[A <: Genome[A]](p: Population[A]): Population[A] = {
            val parents = p.config.parentSel(p.adults)
            val children = p.config.makeChildren(parents)
            p.copy(genotypes = children)
        }

        def cycle[A <: Genome[A]](p: Population[A]): Population[A] = {
            val p1 = nextAdultPool(p)
            createChildren(p1)
        }

    }
}
