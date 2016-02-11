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

    trait Phenotype

    trait Config {
        def childSel: Seq[Phenotype] => Seq[Phenotype]
        def adultSel: (Seq[Phenotype], Seq[Phenotype]) => Seq[Phenotype]
        def parentSel: Seq[Phenotype] => Seq[Phenotype]
    }

    case class Population[A](
        val genotypes: Seq[Genome[A]],
        val adults: Seq[Phenotype],
        val >=< : Genome[A] => Phenotype,
        val eval: Phenotype => Double,
        val config: Config 
    ){
        def growChildren: Seq[Phenotype] =
            genotypes map >=<

        def selectChildren(children: Seq[Phenotype]): Seq[Phenotype] =
            config.childSel(children)

        def selectAdults(children: Seq[Phenotype], adults: Seq[Phenotype]): Seq[Phenotype] =
            config.adultSel(children, adults)

        def selectParents(adults: Seq[Phenotype]): Seq[Phenotype] =
            config.parentSel(adults)

    }


}

object Operations {

}
