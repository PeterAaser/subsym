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
import BitVecProblems._


import reflect.runtime.universe._

object OneMax {

    val evaluate: (SingleBitGenome => Double) =
        genome => {
            val sum = (0 /: genome.gene.bits)(_+_)
            sum.toDouble
        }
}


object LOLZ {

    val z = 15

    def count(v: Vector[Int], target: Int): Int = {
        v match {
            case h +: t if(h == target) => 1 + count(t, target)
            case _ => 0
        }
    }

    val evaluate: (SingleBitGenome => Double) =
        genome => {
            val leading = count(genome.gene.bits, genome.gene.bits.head)
            if(genome.gene.bits.head == 0)
                if (leading > z) z.toDouble else leading.toDouble
            else
                leading.toDouble
        }

}

object Suprising {

    case class SubSeq(s1: Int, s2: Int, d: Int)

    def maxUnique(length: Int, distance: Int): Int =
        distance*(length - distance) + (distance*(distance - 1))/2


    def collect(h: SymbolGene, t: IndexedSeq[SymbolGene], d: Int): Set[SubSeq] = {
        val successors = (t take d).zipWithIndex
        val subSequences = successors.map( { case (s, i) => SubSeq(h.symbol, s.symbol, i) } )
        (Set[SubSeq]() /: subSequences)(_+_)
    }

    def evaluator(s: Int): (SymbolGenome => Double) = {
        def collectAll(g: IndexedSeq[SymbolGene]): Set[SubSeq] = g match {
            case h +: t => collect(h, t, s) ++ collectAll(t)
            case _ => Set[SubSeq]()
        }
        candidate => (maxUnique(candidate.genome.length, s) - collectAll(candidate.genome).size).toDouble
    }
}


object Symbol {

    def printList(s: Seq[_]): Unit = {
        println("-----------")
        s.foreach(println(_))
        println()
    }

    type Pheno = Phenotype[SymbolGenome]
    type Phenos = IndexedSeq[Pheno]

    val distance = 1
    val symbols = 3
    val length = 10

    val adults = 20
    val children = 20
    val crossRate = 0.2
    val mutationRate = 0.4
    val mutationSeverity = 0.3

    def evaluate(genome: SymbolGenome) = 
        Suprising.evaluator(distance)(genome)

    def initializeGene(s: Int): SymbolGene =
        SymbolGene(Random.nextInt(s), s)
    
    def grow(genome: SymbolGenome): Pheno =
        Phenotype[SymbolGenome](genome, evaluate(genome), evaluate(genome), 0)

    def initGenome: SymbolGenome =
        SymbolGenome(Vector.fill(length)(initializeGene(symbols)), crossRate, mutationSeverity)

    def initPhenotype: Pheno =
        grow(initGenome)

    def initPop: Phenos =
        Vector.fill(adults)(initPhenotype)


    val selectParents1: ( Int => ( Phenos => Phenos )) = 
        p => (adults => {
            val normalizer = Scaling.badnessNormalizer[SymbolGenome](_)
            val sigma = Scaling.sigma[SymbolGenome](_)
            val sScaled = Scaling.scale(adults, sigma)
            val nScaled = Scaling.scale(sScaled, normalizer)
            val rScaled = Scaling.rouletteScaler(nScaled)
            val rouletted = ParentSelection.rouletteSelection(rScaled)(p)

            println("Adults")
            printList(adults)

            println("sigma")
            printList(sScaled)

            println("normalized")
            printList(nScaled)

            println("roultte scaled")
            printList(rScaled)

            println("roulette winners")
            printList(rouletted)

            rouletted
        })


    def reproduce(adults: Phenos): Vector[SymbolGenome] =
         sexualReproduction(mutationRate)(adults).toVector


    val evolutionStrategy = AdultSelection.full[SymbolGenome](
        adults,
        selectParents1,
        reproduce,
        genomes => genomes.map(grow(_))
    )

    val population = Population[SymbolGenome](
        initPop,
        evolutionStrategy,
        Controllers.Normal[SymbolGenome]
    )
}



object BitVecProblems {


    type Pheno = Phenotype[SingleBitGenome]
    type Phenos = IndexedSeq[Pheno]

    val problemSize = 30
    val adults = 20
    val children = 20
    val crossrate = 0.2
    val mutationRate = 0.4
    val mutationSeverity = 0.3

    def initializeGene(n: Int): BitGene = {
        BitGene(Vector.fill(n)(Random.nextInt(2)), crossrate, mutationSeverity)
    }

    def grow(genome: SingleBitGenome): Pheno =
        Phenotype[SingleBitGenome](genome, LOLZ.evaluate(genome), LOLZ.evaluate(genome), 0) 

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

