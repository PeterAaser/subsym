package GA

import scala._
import scalaz._
import Scalaz._

import Data._
import Operations._

object Data {

    trait Gene[A <: Gene[A]] {
        def cross(g2: A): (A, A)
        def mutate: A
    }


    trait Genome[A <: Genome[A]] {
        def cross(gn2: A): (A, A)
        def mutate: A
    }

    case class Phenotype[A <: Genome[A]](genome: Genome[A], relativeFitness: Double, fitness: Double, age: Int)

    trait geneOps[A <: Genome[A]] {
        def childSel: Seq[Phenotype[A]] => Seq[Phenotype[A]]
        def adultSel: (Seq[Phenotype[A]], Seq[Phenotype[A]]) => Seq[Phenotype[A]]
        def parentSel: Seq[Phenotype[A]] => Seq[Phenotype[A]]
        def grow: Genome[A] => Phenotype[A]
    }

    case class Population[A <: Genome[A]](
        val genotypes: Seq[Genome[A]],
        val adults: Seq[Phenotype[A]],
        val config: geneOps[A]){

            def growChildren: Seq[Phenotype[A]] =
                genotypes.map(config.grow(_))

            def selectChildren(children: Seq[Phenotype[A]]): Seq[Phenotype[A]] =
                config.childSel(children)

            def selectAdults(children: Seq[Phenotype[A]], adults: Seq[Phenotype[A]]): Seq[Phenotype[A]] =
                config.adultSel(children, adults)

            def selectParents(adults: Seq[Phenotype[A]]): Seq[Phenotype[A]] =
                config.parentSel(adults)

            def nextGeneration(parents: Seq[Phenotype[A]]): Population[A] = {
                copy(genotypes = parents.map(_.genome))
            }
    }
}

object Operations {

}
