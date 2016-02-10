package GA

import scala._
import scalaz._
import Scalaz._

object Data {

    trait Gene[A] {
        def +-(g1: A, g2: A, start: Int, end: Int): (A, A)
    }

    implicit class GeneOps[A](a: A)(implicit ev: Gene[A]) {
        def +-(g2: A, start: Int, end: Int): (A, A) = ev.+-(a, g2, start, end)
    }


    case class BitGene(bits: Vector[Int])
    object BitGene {
        implicit val bitGene = new Gene[BitGene] {
            def +-(g1: BitGene, g2: BitGene, start: Int, end: Int): (BitGene, BitGene) = {

                val c1 = (g1.bits take start) ++
                    (g2.bits view (start, end)) ++
                    (g1.bits takeRight end)

                val c2 = (g2.bits take start) ++
                    (g1.bits view (start, end)) ++
                    (g2.bits takeRight end)

                (BitGene(c1), BitGene(c2))
            }
        }
    }


    // case class BitGene(bits: Vector[Int]) extends Gene {

    //     def +-(g2: BitGene, start: Int, end: Int): (BitGene, BitGene) = {

    //     }
    // }

    // case class BitGene(bits: Vector[Int]) extends Gene {
    //     def +-(g: Gene, start: Int, end: Int): (BitGene, BitGene) = g match {
    //         case g2: BitGene =>
    //             val c1 = (bits take start) ++
    //                 (g2.bits view (start, end)) ++
    //                 (bits takeRight end)

    //             val c2 = (g2.bits take start) ++
    //                 (bits view (start, end)) ++
    //                 (g2.bits takeRight end)

    //             (BitGene(c1), BitGene(c2))
    //     }
    // }

    // trait Chromosome

    // trait Genotype[Gene] {
    //     val genes: List[Gene]
    //     def toPhenotype: Phenotype
    // }


    // trait Phenotype {
    //     def evaluate: Double
    // }


    // case class Population(
    //     adults: List[Phenotype], 
    //     genotypes: List[Genotype[Gene]]
    // )

    // trait Config

    // trait GAproblem {
    //     def config: Config
    //     def +-(g1: Gene, g2: Gene): Gene
    //     def ^(g1: Gene): Gene
    // }


}
