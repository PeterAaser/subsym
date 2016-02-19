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

            // local effect only
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

    case class SymbolGene(symbol: Int, s: Int) extends Gene[SymbolGene] {
        def cross(g2: SymbolGene): (SymbolGene, SymbolGene) =
            (g2, this)
        
        def mutate: SymbolGene = copy(symbol = Random.nextInt(s))
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
}


object Scaling {

    // Scales a population of candidates using some function that can be tailored to populations
    def scale[A <: Genome[A]](
        candidates: IndexedSeq[Phenotype[A]], 
        scaler: IndexedSeq[Phenotype[A]] => (Double => Double))
    : IndexedSeq[Phenotype[A]] = {

            val scalingFun = scaler(candidates)
            candidates.map(c => c.copy(relativeFitness=scalingFun(c.relativeFitness)))
        }

    def sigma[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]]): (Double => Double) = {
        val mean = (0.0 /: candidates.map(_.relativeFitness))(_+_)/(candidates.length.toDouble)
        if(mean == 0.0)
            (relativeFitness => relativeFitness)
        else{
            val stddev = (0.0 /: candidates.map(p => math.pow(p.relativeFitness - mean, 2)))(_+_)/(candidates.length.toDouble)
            (relativeFitness => relativeFitness*(1.0 + ((relativeFitness - mean) / (2.0 * stddev))))
        }
    }


    // creates a fitness normalizing function from a list of candidates such that the highest has
    // fitness 1.0
    def normalizer[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]]): (Double => Double) = {

        val fittest = candidates.reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)
        (relativeFitness => relativeFitness/(fittest.relativeFitness))
    }


    // when we measure unfitness we want to promote the least unfit
    def badnessNormalizer[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]]): (Double => Double) = {

        val unfittest = candidates.reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)
        (relativeFitness => 1.0 - relativeFitness/(unfittest.relativeFitness))
    }


    // creates a roulette scaled, normalized list of candidates
    def rouletteScaler[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]]): IndexedSeq[Phenotype[A]] = {

        val fitnessSum = (0.0 /: candidates.map(_.relativeFitness))(_+_)

        def stackingSum(ps: IndexedSeq[Phenotype[A]], stack: Double): IndexedSeq[Phenotype[A]] = ps match {
            case h +: t => h.copy( relativeFitness = (h.relativeFitness + stack)/fitnessSum) +: stackingSum(t, stack + h.relativeFitness)
            case _ => Vector[Phenotype[A]]()
        }
        stackingSum(candidates, 0.0)
    }

}

object ParentSelection {

    // Roulette selection expects a roulette scaled population
    def rouletteSelection[A <: Genome[A]](
        candidates: IndexedSeq[Phenotype[A]]
    ): Int => IndexedSeq[Phenotype[A]] = spins => {

        // hastily clobbered together
        def search(low: Int, high: Int, target: Double): Phenotype[A] = {
            if (low == high - 1){
                candidates(high) 
            }
            else (low + high)/2 match {
                case mid if candidates(mid).relativeFitness > target => search(low, mid, target)
                case mid if candidates(mid).relativeFitness < target => search(mid, high, target)
                case _ => candidates(low)
            }
        }
        Vector.fill(spins)(search(0, candidates.size, Random.nextDouble))
    }


    def tournamentSelection[A <: Genome[A]](
        candidates: IndexedSeq[Phenotype[A]], 
        winners: Int, epsilon: Double, contestants: Int)
    : IndexedSeq[Phenotype[A]] = {

        def select: Phenotype[A] = {
            if(Random.nextDouble < epsilon){
                candidates(Random.nextInt(candidates.size - 1))
            }
            else{
                tournament(candidates, contestants)
            }
        }

        def tournament[A <: Genome[A]](
            candidates: IndexedSeq[Phenotype[A]], 
            contestants: Int
        ): Phenotype[A] = {

            val chosen = sample(0 to candidates.size - 1 toList, contestants).map(candidates(_))
            chosen.reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)
        }
        Vector.fill(winners)(select)
    }

    // https://stackoverflow.com/questions/14862602/scala-java-generating-a-set-of-non-repeating-random-numbers
    def sample[A](items: List[A], sampleSize: Int) = {
        def collect(vect: Vector[A], sampleSize: Int, acc: List[A]): List[A] = {
            if(sampleSize == 0) acc
            else {
                val index = Random.nextInt(vect.size)
                collect( vect.updated(index, vect(0)) tail, sampleSize - 1, vect(index) :: acc)
            }
        }
        collect(items toVector, sampleSize, Nil)
    }
}


object Reproduction {
        
    def asexual[A <: Genome[A]](p: Phenotype[A], mutationRate: Double): Genome[A] = {
        p.genome.mutate(mutationRate)
    }


    def sexual[A <: Genome[A]](
        p1: Phenotype[A], 
        p2: Phenotype[A], mutationRate: Double
        ): (A, A) = {

            val children = p1.genome.cross(p2.genome)
            (children._1.mutate(mutationRate), children._2.mutate(mutationRate))
    }


    def sexualReproduction[A <: Genome[A]](
        mutationRate: Double
        ): (IndexedSeq[Phenotype[A]] => IndexedSeq[A]) = {

            def reproduce(parents: IndexedSeq[Phenotype[A]]): IndexedSeq[A] = {
                parents match {
                    case p1 +: p2 +: t => 
                        val children = sexual(p1, p2, mutationRate)
                        children._1 +: children._2 +: reproduce(t)
                    case _ => Vector[A]()
                }
            }
            parents => reproduce(parents)
    }


    def asexualReproduction[A <: Genome[A]](mutationRate: Double): (IndexedSeq[Phenotype[A]] => IndexedSeq[Genome[A]]) = {
        parents => parents.map(asexual(_, mutationRate))
    }
}


object AdultSelection {
    
    def full[A <: Genome[A]](
        µ: Int,
        parentSel: Int => (IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]]),
        reproductionScheme: IndexedSeq[Phenotype[A]] => IndexedSeq[A],
        grow: IndexedSeq[A] => IndexedSeq[Phenotype[A]]
    ): Population[A] => Population[A] =
        pop => {
            val parents = parentSel( µ )(pop.adults)
            val children = grow(reproductionScheme(parents))
            pop.copy(adults = children)
        }
    

    // mu: adult, lambda: child
    def overProduction[A <: Genome[A]](
        µ: Int,
        λ: Int,
        parentSel: Int => (IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]]),
        reproductionScheme: IndexedSeq[Phenotype[A]] => IndexedSeq[A],
        adultSel: Int => (IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]]),
        grow: IndexedSeq[A] => IndexedSeq[Phenotype[A]]
    ): Population[A] => Population[A] =
        pop => {
            val parents = parentSel( λ )(pop.adults)
            val children = grow(reproductionScheme(parents))
            val survivors = adultSel( µ )(children)
            pop.copy(adults = survivors)
        }


    def mixin[A <: Genome[A]](
        µ: Int,
        λ: Int,
        parentSel: Int => (IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]]),
        reproductionScheme: IndexedSeq[Phenotype[A]] => IndexedSeq[A],
        adultSel: Int => (IndexedSeq[Phenotype[A]], IndexedSeq[Phenotype[A]]) => IndexedSeq[Phenotype[A]],
        grow: IndexedSeq[A] => IndexedSeq[Phenotype[A]]
    ): Population[A] => Population[A] =
        pop => {
            val parents = parentSel( µ )(pop.adults)
            val children = grow(reproductionScheme(parents))
            val survivors = adultSel( λ )(children, pop.adults)
            pop.copy(adults = survivors)
        }
}

object Controllers {

    case class Normal[A <: Genome[A]]() extends Strategy[A] {
        def evolve(p: Population[A]): Population[A] = p
    }

}
