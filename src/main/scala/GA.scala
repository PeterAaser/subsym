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
        testPop.run(50)

        // val next = Data.Population.run(40, testPop)

    }
}

object OneMax {

    type Pheno = Phenotype[SingleBitGenome]
    type Phenos = IndexedSeq[Pheno]

    val problemSize = 30
    val adults = 20
    val children = 20
    val crossrate = 0.2
    val mutationRate = 0.3
    val mutationSeverity = 0.3

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

    val selectParents: ( Int => ( Phenos => Phenos )) = 
        p => (adults => {
            val normalizer = Scaling.normalizer(adults)
            val scaled = Scaling.scale(adults, normalizer)
            val rScaled = Scaling.rouletteScaler(adults)
            ParentSelection.rouletteSelection(rScaled)(p)
        })

    val selectParents2: ( Int => ( Phenos => Phenos)) = 
        p => (adults => 
            ParentSelection.tournamentSelection(adults, adults.length, 0.2, 4))
        

    def reproduce(adults: Phenos): Vector[SingleBitGenome] =
         sexualReproduction(mutationRate)(adults).toVector

    val evolutionStrategy = AdultSelection.full[SingleBitGenome](
        adults,
        selectParents2,
        reproduce,
        genomes => genomes.map(grow(_))
    )

    val population = Population[SingleBitGenome](
        initPop,
        evolutionStrategy
    )
}
