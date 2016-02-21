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

    def maxUnique(length: Int, distance: Int): Int = {
        val d = if(distance > length) length else distance 
        d*( (length - 1) - d) + (d*(d + 1))/2
    }


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
        candidate => (collectAll(candidate.genome).size.toDouble)/(maxUnique(candidate.genome.length, s))
    }
}


object SymbolProblems {

    def printList(s: Seq[_]): Unit = {
        println("-----------")
        s.foreach(println(_))
        println()
    }

    type Pheno = Phenotype[SymbolGenome]
    type Phenos = IndexedSeq[Pheno]

    val distance = 30
    val symbols = 10
    val length = 25

    val adults = 100
    val children = 20
    val crossRate = 0.2
    val mutationRate = 0.6
    val mutationSeverity = 0.6

    def evaluate(genome: SymbolGenome) = 
        Suprising.evaluator(distance)(genome)

    def grow(genome: SymbolGenome): Pheno =
        Phenotype[SymbolGenome](genome, evaluate(genome), evaluate(genome), 0)

    def reproduce(adults: Phenos): Vector[SymbolGenome] =
         sexualReproduction(mutationRate)(adults).toVector

    val evolutionStrategy = AdultSelection.full[SymbolGenome](
        adults,
        ParentSelection.tournamentStrat(_, 0.2, 9),
        reproduce,
        genomes => genomes.map(grow(_))
    )

    val runner = Runner[SymbolGenome](
        poolSize => SymbolGenome.initPool(poolSize, length, symbols, crossRate, mutationSeverity).map(grow(_)),
        p => ( (p.fittest.trueFitness == 1.0) || (p.generation > 2000)),
        evolutionStrategy
    )
}



object BitVecProblems {

    type Pheno = Phenotype[SingleBitGenome]
    type Phenos = IndexedSeq[Pheno]

    val problemSize = 30
    val adults = 30
    val children = 20
    val crossRate = 0.2
    val mutationRate = 0.4
    val mutationSeverity = 0.3

    def grow(genome: SingleBitGenome): Pheno =
        Phenotype[SingleBitGenome](genome, LOLZ.evaluate(genome), LOLZ.evaluate(genome), 0) 

    def reproduce(adults: Phenos): Vector[SingleBitGenome] =
         sexualReproduction(mutationRate)(adults).toVector

    val evolutionStrategy = AdultSelection.full[SingleBitGenome](
        adults,
        ParentSelection.tournamentStrat(_, 0.2, 4),
        reproduce,
        genomes => genomes.map(grow(_))
    )

    val runner = Runner[SingleBitGenome](
        poolSize => SingleBitGenome.initPool(adults, poolSize, crossRate, mutationSeverity).map(grow(_)),
        p => ( (p.fittest.trueFitness > 19.0) || (p.generation > 100)),
        evolutionStrategy
    )
}

