package GA

import scala._
import scalaz._
import Scalaz._

import Data._

object Common {


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




    case class SingleBitGenome(g: BitGene)
    object BitGenome {
        implicit val singleBitGenome = new Genome[SingleBitGenome] {
            def +-(gn1: SingleBitGenome, gn2: SingleBitGenome): (SingleBitGenome, SingleBitGenome) ={
                val (g1, g2) = gn1.g +- gn2.g
                (SingleBitGenome(g1), SingleBitGenome(g2))
            }
            def ~(gn1: SingleBitGenome): SingleBitGenome =
                gn1.copy(g = gn1.g.~)
        }
    }

}
