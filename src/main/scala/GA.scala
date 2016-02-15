package GA

import scala._
import scalaz._
import Scalaz._

import Data._
import scala.util.Random
import Representations._
import OneMax._

object GAsolver {

    def main(args: Array[String]): Unit = {

        val om = initializeGenome(10)
        // println(om)

    }
}

object OneMax {

    type Pop = IndexedSeq[Phenotype[SingleBitGenome]]

    val problemSize = 30

    def initializeGene(n: Int): BitGene = {
        BitGene(Vector.fill(n)(Random.nextInt(2)))
    }

    val evaluate: (SingleBitGenome => Double) =
        genome => {
            val sum = (0 /: genome.gene.bits)(_+_)
            sum.toDouble/(genome.gene.bits.size - 1).toDouble
        }

    def initializeGenome(size: Int): Vector[SingleBitGenome] =
        Vector.fill(size)(SingleBitGenome(initializeGene(problemSize)))
    

    def oneMaxPhenotype(genome: SingleBitGenome): Phenotype[SingleBitGenome] =
        Phenotype[SingleBitGenome](genome, evaluate(genome), 0) 
     

    def selectChildren(children: IndexedSeq[Phenotype[SingleBitGenome]]): IndexedSeq[Phenotype[SingleBitGenome]] =
        children


    def selectAdults(children: IndexedSeq[Phenotype[SingleBitGenome]], adults: IndexedSeq[Phenotype[SingleBitGenome]]): IndexedSeq[Phenotype[SingleBitGenome]] =
        children

    
    def selectParents(adults: Pop): Pop =
        adults
    

    val conf = geneOps[SingleBitGenome](
        initializeGenome,
        oneMaxPhenotype,
        selectChildren,
        selectAdults,
        selectParents
    )

    

    // val pop = Population[SingleBitGenome](


}
