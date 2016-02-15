package GA

import scala._
import scalaz._
import Scalaz._

object testing {
    ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////

    // trait Gene {
    //     def cross(g2: Gene): (Gene, Gene)
    // }

    // case class BitGene(bits: Vector[Int]) extends Gene {

    //     def cross(g2: BitGene): (BitGene, BitGene) =
    //         (this, this)
    // }

    ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////

    // trait Gene {
    //     def cross[A <: Gene](g2: A): (A, A)
    // }

    // case class BitGene(bits: Vector[Int]) extends Gene {
    //     
    //     def cross(g2: BitGene): (BitGene, BitGene) =
    //         (this, this)
    // }

    ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////

    trait Gene[A <: Gene[A]] {
        def cross(g2: A): (A, A)
    }


    case class BitGeneA(bits: Vector[Int]) extends Gene[BitGeneA] {
        def cross(g2: BitGeneA): (BitGeneA, BitGeneA) =
            (this, this)
    }

    case class BitGeneB(bits: Vector[Int]) extends Gene[BitGeneB] {
        def cross(g2: BitGeneB): (BitGeneB, BitGeneB) =
            (this, this)
    }


    trait Genome[A <: Genome[A]] {
        def cross(other: A): (A, A)
    }

    
    case class SimpleGenome(g1: BitGeneA, g2: BitGeneB) {
        def cross(other: SimpleGenome): (SimpleGenome, SimpleGenome) =
            (this, this)
    }


    case class Phenotype[A <: Genome[A]](genome: Genome[A], fitEval: Genome[A] => Double)

    case class Population[A <: Genome[A]](
        val genotypes: Seq[Genome[A]]
        )
    
}
