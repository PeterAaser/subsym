package GA

import scala._
import scalaz._
import Scalaz._

import Data._
import scala.util.Random
import OneMax._

import Representations._
import Scaling._
import Reproduction._
import ParentSelection._

import reflect.runtime.universe._

object GAsolver {


    implicit class ColonTypeExtender [T : TypeTag] (x : T) {
        def colonType = typeOf[T].toString
    }

    def main(args: Array[String]): Unit = {

        def printList(s: Seq[_]): Unit = {
            println("-----------")
            s.foreach(println(_))
            println()
        }


        // val test1 = OneMax.population
        // print(test1)
        // val test2 = Data.Population.createChildren(test1)
        // val test3 = normalizer(test2)
        // val test4 = test3(test2)

        // val normTest = scale(test2, test3)
        // printList(test2)
        // printList(normTest)

        // val rouletteTest = rouletteScaler(normTest)
        // printList(rouletteTest)

        // val selected = rouletteSelection(rouletteTest, 2)
        // printList(selected)

        // val test5 = test2.head.genome
        // val test7 = test2(2).genome match { case s: SingleBitGenome => s }
        // // println((test5, test7))

        // val test8 = test2.head.genome.cross(test7)
        // // println(test8)

        // val test9 = test8._1
        // // println(test9)

        // val test10 = test9.mutate
        // // println(test10)

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
            sum.toDouble
        }

    def initializeGenome(size: Int): Vector[SingleBitGenome] =
        Vector.fill(size)(SingleBitGenome(initializeGene(problemSize)))
    

    def oneMaxPhenotype(genome: SingleBitGenome): Phenotype[SingleBitGenome] =
        Phenotype[SingleBitGenome](genome, evaluate(genome), 0) 
     

    def selectChildren(children: IndexedSeq[Phenotype[SingleBitGenome]]): IndexedSeq[Phenotype[SingleBitGenome]] =
        children


    def selectAdults(children: IndexedSeq[Phenotype[SingleBitGenome]], adults: IndexedSeq[Phenotype[SingleBitGenome]]): IndexedSeq[Phenotype[SingleBitGenome]] =
        children

    
    def selectParents[SingleBitGenome](adults: Pop): Pop =
        adults
    
    val reproduce = sexualReproduction[SingleBitGenome](0.1)

    def makeChildren(adults: Pop): Vector[SingleBitGenome] =
        reproduce(adults)


    // val conf = geneOps[SingleBitGenome](
    //     oneMaxPhenotype,
    //     selectChildren,
    //     selectAdults,
    //     selectParents,
    //     makeChildren
    // )

    
    // val population = Population[SingleBitGenome](
    //     initializeGenome(20),
    //     Vector[Phenotype[SingleBitGenome]](),
    //     conf
    // )
}
