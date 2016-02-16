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
        def mutate: A
    }

    case class Phenotype[A <: Genome[A]](
        genome: Genome[A], 
        fitness: Double, 
        age: Int
    )


    case class geneOps[A <: Genome[A]]( 
        grow: A => Phenotype[A],
        childSel: IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]],
        adultSel: (IndexedSeq[Phenotype[A]], IndexedSeq[Phenotype[A]]) => IndexedSeq[Phenotype[A]],
        parentSel: IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]]
    )

    case class Population[A <: Genome[A]](
        val genotypes: IndexedSeq[A],
        val adults: IndexedSeq[Phenotype[A]],
        val config: geneOps[A]){

            def growChildren: IndexedSeq[Phenotype[A]] =
                genotypes.map(g => config.grow(g))

            def selectChildren(children: IndexedSeq[Phenotype[A]]): IndexedSeq[Phenotype[A]] =
                config.childSel(children)

            def selectAdults(children: IndexedSeq[Phenotype[A]], adults: IndexedSeq[Phenotype[A]]): IndexedSeq[Phenotype[A]] =
                config.adultSel(children, adults)

            def selectParents(adults: IndexedSeq[Phenotype[A]]): IndexedSeq[Phenotype[A]] =
                config.parentSel(adults)

            def nextGeneration(parents: IndexedSeq[Phenotype[A]]): Population[A] = {
                // val unf =  copy(genotypes = parents.map(_.genome))
                this
            }
    }
}
