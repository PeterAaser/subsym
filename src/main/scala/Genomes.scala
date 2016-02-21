package GA
import scala._
import scalaz._
import Scalaz._

import scala.util.Random
import scala.language.postfixOps

import Data._

import Representations._
import Scaling._
import Reproduction._
import ParentSelection._
import AdultSelection._

object Representations {

    case class BitGene(bits: Vector[Int], crossRate: Double, mutationSeverity: Double) extends Gene[BitGene] {

        def cross(g2: BitGene): (BitGene, BitGene) = {
            val (t1, t2) = (bits.toArray, g2.bits.toArray)

            for(i <- 0 until (bits.length*crossRate).toInt){
                val crossPoint = Random.nextInt(bits.length - 1)
                val temp = t1(crossPoint)
                t1(crossPoint) = t2(crossPoint)
                t2(crossPoint) = temp
            }
            (copy(bits=t1.toVector), g2.copy(bits=t2.toVector))
        }

        def mutate: BitGene = {
            val t1 = bits.toArray

            for(i <- 0 until (bits.length*crossRate).toInt){
                val mPoint = Random.nextInt(bits.length) 
                t1(mPoint) = t1(mPoint) ^ 1
            }
            copy(bits=t1.toVector)
        }
    }
    object BitGene {
        
        def init(n: Int, crossRate: Double, mutationSeverity: Double): BitGene = 
            BitGene(Vector.fill(n)(Random.nextInt(2)), crossRate, mutationSeverity)
    }


    case class SingleBitGenome(gene: BitGene) extends Genome[SingleBitGenome] {

        def cross(genome2: SingleBitGenome): (SingleBitGenome, SingleBitGenome) = {
            val (gene1, gene2) = gene.cross(genome2.gene)
            (SingleBitGenome(gene1), SingleBitGenome(gene2))
        }

        def mutate(rate: Double): SingleBitGenome = {
            if (Random.nextDouble < rate){
                copy(gene = gene.mutate)
            }
            else this
        }
    }
    object SingleBitGenome {

        def initPool(poolSize: Int, genomeSize: Int, crossRate: Double, mutationSeverity: Double): IndexedSeq[SingleBitGenome] = 
            Vector.fill(poolSize)(SingleBitGenome(BitGene.init(genomeSize, crossRate, mutationSeverity)))
    }





    case class SymbolGene(symbol: Int, s: Int) extends Gene[SymbolGene] {
        def cross(g2: SymbolGene): (SymbolGene, SymbolGene) =
            (g2, this)
        
        def mutate: SymbolGene = copy(symbol = Random.nextInt(s))
    }
    object SymbolGene {

        def init(s: Int): SymbolGene =
            SymbolGene(Random.nextInt(s), s)
    }




    case class SymbolGenome(genome: IndexedSeq[SymbolGene], crossRate: Double, mutationSeverity: Double) extends Genome[SymbolGenome] {

        def cross(genome2: SymbolGenome): (SymbolGenome, SymbolGenome) = {
            val (t1, t2) = (genome.toArray, genome2.genome.toArray)
            for(i <- 0 until(genome.length*crossRate).toInt){
                val crossPoint = Random.nextInt(genome.length - 1)
                val (g1, g2) = t1(crossPoint).cross(t2(crossPoint))
                t1(crossPoint) = g2
                t2(crossPoint) = g1
            }
            (copy(genome=t1.toVector), genome2.copy(genome=t2.toVector))
        }

        def mutate(rate: Double): SymbolGenome = {
            val t1 = genome.toArray

            for(i <- 0 until (genome.length*crossRate).toInt){
                val mPoint = Random.nextInt(genome.length)
                t1(mPoint) = t1(mPoint).mutate
            }
            copy(genome=t1.toVector)
        }

        override def toString: String = genome.map(_.symbol).mkString("[", "][", "]")
    }
    object SymbolGenome {

        def initGenome(genomeSize: Int, symbols: Int, crossRate: Double, mutationSeverity: Double): SymbolGenome = {
            SymbolGenome(Vector.fill(genomeSize)(SymbolGene.init(symbols)), crossRate, mutationSeverity)
        }

        def initPool(poolSize: Int, genomeSize: Int, symbols: Int, crossRate: Double, mutationSeverity: Double): IndexedSeq[SymbolGenome] = {
            Vector.fill(poolSize)(SymbolGenome.initGenome(genomeSize, symbols, crossRate, mutationSeverity))
        }
    }
}
