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


        var testPop = OneMax.population
        println(testPop)

        testPop = Data.Population.nextAdultPool(testPop)
        println(testPop)

        // testPop = Data.Population.cycle(testPop)
        // println(testPop)

        var adults = testPop.adults
        printList(adults)
        println( (0.0 /: adults.map(_.fitness))(_+_) )

        val normalizer = Scaling.normalizer(adults)
        println(normalizer.colonType)

        val scaled = Scaling.scale(adults, normalizer)
        printList(scaled)

        val rScaled = Scaling.rouletteScaler(adults)
        printList(rScaled)

        val parents = ParentSelection.rouletteSelection(rScaled, rScaled.length)
        printList(parents)

        def refit(p: Phenotype[SingleBitGenome]) : Phenotype[SingleBitGenome] =
            p.copy(fitness = OneMax.oneMaxPhenotype(p.genome).fitness)

        val nParents = parents.map(refit(_))
        println( (0.0 /: nParents.map(_.fitness))(_+_) )
        printList(nParents)
        


    }
}

object OneMax {

    type Pheno = Phenotype[SingleBitGenome]
    type Phenos = IndexedSeq[Pheno]

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
    
    def oneMaxPhenotype(genome: SingleBitGenome): Pheno =
        Phenotype[SingleBitGenome](genome, evaluate(genome), 0) 
     
    def selectChildren(children: Phenos): Phenos =
        children

    def selectAdults(children: Phenos, adults: Phenos): Phenos =
        children
    
    def selectParents(adults: Phenos): Phenos = {
        val normalizer = Scaling.normalizer(adults)
        val scaled = Scaling.scale(adults, normalizer)
        val rScaled = Scaling.rouletteScaler(adults)
        ParentSelection.rouletteSelection(rScaled, rScaled.length)
        // adults
    }

    def makeChildren(adults: Phenos): Vector[SingleBitGenome] =
        sexualReproduction(0.1)(adults).toVector

    val ops = geneOps[SingleBitGenome](
        oneMaxPhenotype,
        selectChildren,
        selectAdults,
        selectParents,
        makeChildren
    )
    
    val population = Population[SingleBitGenome](
        initializeGenome(20),
        Vector[Phenotype[SingleBitGenome]](),
        ops
    )
}
