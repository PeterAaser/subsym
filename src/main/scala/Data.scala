package GA

import scala._
import scalaz._
import Scalaz._

object Data {

    trait Gene[A] {
        def +-(g1: A, g2: A): (A, A)
        def ~(g1: A): A
    }

    implicit class GeneOps[A](a: A)(implicit ev: Gene[A]) {
        def +-(g2: A): (A, A) = ev.+-(a, g2)
        def ~(): A = ev.~(a)
    }

    case class BitGene(bits: Vector[Int])
    object BitGene {
        implicit val bitGene = new Gene[BitGene] {
            def +-(g1: BitGene, g2: BitGene): (BitGene, BitGene) = {

                val start = 1
                val end = 2

                val c1 = (g1.bits take start) ++
                    (g2.bits view (start, end)) ++
                    (g1.bits takeRight end)

                val c2 = (g2.bits take start) ++
                    (g1.bits view (start, end)) ++
                    (g2.bits takeRight end)

                (BitGene(c1), BitGene(c2))
            }

            def ~(g1: BitGene): BitGene = {
                g1.copy(g1.bits.updated(1, math.abs(g1.bits(1) - 1)))
            }
        }
    }


    trait Genome[A] {
        def +-(g1: A, g2: A): (A, A)
    }

    implicit class GenomeOps[A](a: A)(implicit ev: Genome[A]) {
        def +-(): (A) = ev.+-(a)
    }

    case class BitGenome(g: List[BitGene])
    object SingleBitGenome {
        implicit val singleBitGenome = new Genome[BitGenome] {
            def +-(
        }

    }


    trait Genotype[Gene] {
        val gene: Gene
        def evaluate
    }


    trait Phenotype {
        def evaluate: Double
    }


    case class Population[A <: Gene[A]](
        adults: List[Phenotype], 
        genotypes: List[Genotype[A]]
    )

    trait Config

    trait GAproblem[A <: Gene[A]] {
        def config: Config
        def +-(g1: A, g2: A): A
        def ^(g1: A): A
    }

}
