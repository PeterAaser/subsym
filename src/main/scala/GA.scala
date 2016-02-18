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

        val testPop = OneMax.population
        val done = testPop.run(50)

        val sScale = done.adults
        // printList(sScale)
        // printList(Scaling.scale(sScale, Scaling.sigma[SingleBitGenome]))

        // val next = Data.Population.run(40, testPop)

    }
}

object OneMax {

    def printList(s: Seq[_]): Unit = {
        println("-----------")
        s.foreach(println(_))
        println()
    }

    type Pheno = Phenotype[SingleBitGenome]
    type Phenos = IndexedSeq[Pheno]

    val problemSize = 30
    val adults = 20
    val children = 20
    val crossrate = 0.7
    val mutationRate = 0.1
    val mutationSeverity = 0.2

    def initializeGene(n: Int): BitGene = {
        BitGene(Vector.fill(n)(Random.nextInt(2)), crossrate, mutationSeverity)
    }

    def grow(genome: SingleBitGenome): Pheno =
        Phenotype[SingleBitGenome](genome, evaluate(genome), evaluate(genome), 0) 

    val evaluate: (SingleBitGenome => Double) =
        genome => {
            val sum = (0 /: genome.gene.bits)(_+_)
            sum.toDouble
        }

    def initGenome: SingleBitGenome =
        SingleBitGenome(initializeGene(problemSize))

    def initPhenotype: Pheno =
        grow(initGenome)

    def initPop: Phenos = 
        Vector.fill(adults)(initPhenotype)

    val selectParents1: ( Int => ( Phenos => Phenos )) = 
        p => (adults => {
            val normalizer = Scaling.normalizer[SingleBitGenome](_)
            val sigma = Scaling.sigma[SingleBitGenome](_)
            val sScaled = Scaling.scale(adults, sigma)
            val nScaled = Scaling.scale(sScaled, normalizer)
            val rScaled = Scaling.rouletteScaler(nScaled)
            val rouletted = ParentSelection.rouletteSelection(rScaled)(p)

            // println("sScaled")
            // printList(sScaled)
            // printList(adults)

            // println("nScaled")
            // printList(nScaled)

            // println("rScaled")
            // printList(rScaled)
            // println("rouletted")
            // printList(rouletted)
            // println(adults.map(_.trueFitness).sum)
            // println(rouletted.map(_.trueFitness).sum)
            rouletted
        })

    val selectParents2: ( Int => ( Phenos => Phenos)) = 
        p => (adults => 
            ParentSelection.tournamentSelection(adults, adults.length, 0.2, 4))
        

    def reproduce(adults: Phenos): Vector[SingleBitGenome] =
         sexualReproduction(mutationRate)(adults).toVector

    val evolutionStrategy = AdultSelection.full[SingleBitGenome](
        adults,
        selectParents1,
        reproduce,
        genomes => genomes.map(grow(_))
    )

    val population = Population[SingleBitGenome](
        initPop,
        evolutionStrategy,
        Controllers.Normal[SingleBitGenome]
    )
}
