package GA

import scala._
import scalaz._
import Scalaz._

import Data._
import Selection._
import scala.util.Random
import Representations._
import OneMax._

import reflect.runtime.universe._

object GAsolver {


    implicit class ColonTypeExtender [T : TypeTag] (x : T) {
        def colonType = typeOf[T].toString
    }

    def main(args: Array[String]): Unit = {

        val test1 = OneMax.population
        val test2 = test1.growChildren
        val test3 = normalizer(test2)
        val test4 = test3(test2)
        

        val test5 = test2.head.genome
        val test7 = test2(2).genome match { case s: SingleBitGenome => s }

        println((test5, test7))
        val test8 = test2.head.genome.cross(test7)
        println(test8)
        val test9 = test8._1
        println(test9)
        val test10 = test9.mutate
        println(test10)

    }
}

object OneMax {

    type Pop = IndexedSeq[Phenotype[SingleBitGenome]]

    val problemSize = 10

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
        oneMaxPhenotype,
        selectChildren,
        selectAdults,
        selectParents
    )
    
    val population = Population[SingleBitGenome](
        initializeGenome(20),
        Vector[Phenotype[SingleBitGenome]](),
        conf
    )
}
