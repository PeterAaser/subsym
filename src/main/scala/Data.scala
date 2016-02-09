package GA

import scala._
import scalaz._
import Scalaz._

object Data {

    trait Gene {
        def +-(g2: Gene, start: Int, end: Int): (Gene, Gene)
    }

    trait Chromosome

    trait Genotype[Gene] {
        val genes: List[Gene]
        def toPhenotype: Phenotype
    }


    trait Phenotype {
        def evaluate: Double
    }


    case class Population(
        adults: List[Phenotype], 
        genotypes: List[Genotype[Gene]]
    )

    trait Config

    trait GAproblem {
        def config: Config
        def +-(g1: Gene, g2: Gene): Gene
        def ^(g1: Gene): Gene
    }

    // case class BitGene(bits: Vector[Int]) extends Gene {

    //     def +-(g2: BitGene, start: Int, end: Int): (BitGene, BitGene) = {

    //         val c1 = (bits take start) ++
    //             (g2.bits view (start, end)) ++
    //             (bits takeRight end)

    //         val c2 = (g2.bits take start) ++
    //             (bits view (start, end)) ++
    //             (g2.bits takeRight end)

    //         (BitGene(c1), BitGene(c2))
    //     }
    // }

    case class BitGene(bits: Vector[Int]) extends Gene {
        def +-(g: Gene, start: Int, end: Int): (BitGene, BitGene) = g match {
            case g2: BitGene =>
                val c1 = (bits take start) ++
                    (g2.bits view (start, end)) ++
                    (bits takeRight end)

                val c2 = (g2.bits take start) ++
                    (bits view (start, end)) ++
                    (g2.bits takeRight end)

                (BitGene(c1), BitGene(c2))
        }
    }
}
