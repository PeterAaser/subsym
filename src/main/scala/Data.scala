package GA

import scala._
import scalaz._
import Scalaz._

object Data {

    trait Gene
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

    case class BitGene(bits: Vector[Int]) extends Gene {
        def +-(other: BitGene, first: Int, last: Int): BitGene = {
            this 
        }
    }

}
