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
}

