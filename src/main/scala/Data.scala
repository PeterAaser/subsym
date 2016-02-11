package GA

import scala._
import scalaz._
import Scalaz._

import Data._
import Operations._

object Data {

    trait Gene[A] {
        def +-(g1: A, g2: A): (A, A)
        def ~(g1: A): A
    }

    implicit class GeneOps[A](a: A)(implicit ev: Gene[A]) {
        def +-(g2: A): (A, A) = ev.+-(a, g2)
        def ~(): A = ev.~(a)
    }

    trait Genome[A] {
        def +-(gn1: A, gn2: A): (A, A)
        def ~(gn1: A): A
        // def >=<(gn1: A): Phenotype 
    }

    implicit class GenomeOps[A](a: A)(implicit ev: Genome[A]) {
        def +-(gn2: A): (A, A) = ev.+-(a, gn2)
        def ~(): A = ev.~(a)
    }

    trait Phenotype[A] {
        def genome: Genome[A]
    }

    trait Config[A] {
        def childSel: Seq[Phenotype[A]] => Seq[Phenotype[A]]
        def adultSel: (Seq[Phenotype[A]], Seq[Phenotype[A]]) => Seq[Phenotype[A]]
        def parentSel: Seq[Phenotype[A]] => Seq[Phenotype[A]]
    }

    case class Population[A](
        val genotypes: Seq[Genome[A]],
        val adults: Seq[Phenotype[A]],
        val >=< : Genome[A] => Phenotype[A],
        val eval: Phenotype[A] => Double,
        val config: Config[A] 
    )
    {
        def growChildren: Seq[Phenotype[A]] =
            genotypes map >=<

        def selectChildren(children: Seq[Phenotype[A]]): Seq[Phenotype[A]] =
            config.childSel(children)

        def selectAdults(children: Seq[Phenotype[A]], adults: Seq[Phenotype[A]]): Seq[Phenotype[A]] =
            config.adultSel(children, adults)

        def selectParents(adults: Seq[Phenotype[A]]): Seq[Phenotype[A]] =
            config.parentSel(adults)

        def nextGeneration(parents: Seq[Phenotype[A]]): Population[A] = {
            ???
        }
    }


}

object Operations {

}
