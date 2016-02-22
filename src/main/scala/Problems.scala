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

    import scala.collection.mutable.HashSet

    case class SubSeq(s1: Int, s2: Int, d: Int)

    // I think this caches
    def maxUnique(length: Int, distance: Int): Int = {
        val d = if(distance > length) length else distance 
        d*( (length - 1) - d) + (d*(d + 1))/2
    }

    def collect(h: SymbolGene, t: IndexedSeq[SymbolGene], d: Int, found: HashSet[SubSeq]){
        val successors = (t take d).zipWithIndex
        successors.toList.foreach( { case (s, i) => found += SubSeq(h.symbol, s.symbol, i) } )
    }


    def evaluator(s: Int): (SymbolGenome => Double) = {
        def collectAll(g: IndexedSeq[SymbolGene], found: HashSet[SubSeq]): Unit = { 
            g match {
                case h +: t => collect(h, t, s, found); collectAll(t, found)
                case _ => ()
            }
        }
        val found = HashSet[SubSeq]()
        candidate => {
            collectAll(candidate.genome, found)
            found.size.toDouble/(maxUnique(candidate.genome.length, s))
        }
    }

    // Immutable version
    // 
    // def collect(h: SymbolGene, t: IndexedSeq[SymbolGene], d: Int): List[SubSeq] = {
    //     val successors = (t take d).zipWithIndex
    //     successors.toList.map( { case (s, i) => SubSeq(h.symbol, s.symbol, i) } )
    // }


    // def evaluator(s: Int): (SymbolGenome => Double) = {
    //     def collectAll(g: IndexedSeq[SymbolGene]): List[SubSeq] = g match {
    //         case h +: t => collect(h, t, s) ::: collectAll(t)
    //         case _ => List[SubSeq]()
    //     }
    //     candidate => (collectAll(candidate.genome).toSet.size.toDouble)/(maxUnique(candidate.genome.length, s))
    // }
}

object ParamSearch {

    // val adults = 20
    // val symbols = 10
    // val length = 21
    // val generations = 100

    // def trial(runner: Runner[SymbolGenome]): Double = {
    //     println("Trial commencing")
    //     val result = runner.solve(20)
    //     if(result._1.generation < generations) {
    //         1.0
    //     }
    //     else{
    //         val metrics = result._2.unzip
    //         val average = metrics._1
    //         val best = metrics._2
    //         
    //         ((average.sum/generations) + (best.sum/generations))/2.0
    //     }
    // }

    // def evaluate: (ParamGenome => Double) = 
    //     params => {
    //         
    //         val testRunner = Suprise.symbolRunner(30, 10, 23, 100, 
    //             params.genome(0).real,
    //             params.genome(1).real,
    //             params.genome(2).real
    //         )

    //         Vector.fill(3)(trial(testRunner)).sum
    //     }

}

object BitVecProblems {

    // type Pheno = Phenotype[SingleBitGenome]
    // type Phenos = IndexedSeq[Pheno]

    // val problemSize = 30
    // val adults = 30
    // val children = 20
    // val crossRate = 0.2
    // val mutationRate = 0.4
    // val mutationSeverity = 0.3

    // def grow(genome: SingleBitGenome): Pheno =
    //     Phenotype[SingleBitGenome](genome, LOLZ.evaluate(genome), LOLZ.evaluate(genome), 0) 

    // def reproduce(adults: Phenos): Vector[SingleBitGenome] =
    //      sexualReproduction(mutationRate)(adults).toVector

    // val evolutionStrategy = AdultSelection.full[SingleBitGenome](
    //     adults,
    //     ParentSelection.tournamentStrat(_, 0.2, 4),
    //     reproduce,
    //     genomes => genomes.map(grow(_))
    // )

    // val runner = Runner[SingleBitGenome](
    //     poolSize => SingleBitGenome.initPool(adults, poolSize, crossRate, mutationSeverity).map(grow(_)),
    //     p => ( (p.fittest.trueFitness > 19.0) || (p.generation > 100)),
    //     evolutionStrategy
    // )
}

