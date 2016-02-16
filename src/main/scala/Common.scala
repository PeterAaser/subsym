package GA

import scala._
import scalaz._
import Scalaz._

import scala.util.Random

import Data._

object Representations {

    // TODO more intelligent selection.
    case class BitGene(bits: Vector[Int]) extends Gene[BitGene] {
        def cross(g2: BitGene): (BitGene, BitGene) = {

            val start = Random.nextInt(bits.length - 2)
            val end = start + 1

            println("Crossing at %d ".format(start))

            val c1 = (bits take start) ++
                (g2.bits slice (start, end)) ++
                (bits takeRight (bits.length - end))

            val c2 = (g2.bits take start) ++
                (bits slice (start, end)) ++
                (g2.bits takeRight (bits.length - end))

            (BitGene(c1), BitGene(c2))
        }

        def mutate: BitGene = {
            val point = Random.nextInt(bits.length - 1)
            copy(bits.updated(point, math.abs(bits(point) - 1)))
        }
    }


    case class SingleBitGenome(gene: BitGene) extends Genome[SingleBitGenome] {

        def cross(genome2: SingleBitGenome): (SingleBitGenome, SingleBitGenome) = {
            val (gene1, gene2) = gene.cross(genome2.gene)
            (SingleBitGenome(gene1), SingleBitGenome(gene2))
        }

        def mutate: SingleBitGenome =
            copy(gene = gene.mutate)
    }
}


object Selection {

    // Scales a population of candidates using some function that can be tailored to populations
    def scale[A <: Genome[A]](
        candidates: IndexedSeq[Phenotype[A]], 
        scaler: IndexedSeq[Phenotype[A]] => (Double => Double))
    : IndexedSeq[Phenotype[A]] = {

            val scalingFun = scaler(candidates)
            candidates.map(c => c.copy(fitness=scalingFun(c.fitness)))
        }


    // creates a fitness normalizing function from a list of candidates
    def normalizer[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]])
    : IndexedSeq[Phenotype[A]] => (Double => Double) = {

        val fittest = candidates.reduceLeft( (l, r) => if (l.fitness > r.fitness) l else r)
        candidates => (fitness => fitness/(fittest.fitness))
    }


    // creates a roulette scaled, normalized list of candidates
    def rouletteScaler[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]])
    : IndexedSeq[Phenotype[A]] = {

        val fitnessSum = (0.0 /: candidates.map(_.fitness))(_+_)

        def stackingSum(ps: IndexedSeq[Phenotype[A]], stack: Double): IndexedSeq[Phenotype[A]] = ps match {
            case h +: t => h.copy( fitness = (h.fitness + stack)/fitnessSum) +: stackingSum(t, stack + h.fitness)
            case _ => Vector[Phenotype[A]]()
        }
        stackingSum(candidates, 0.0)
    }

    // Roulette selection expects a roulette scaled population
    def roulettSelection[A <: Genome[A]](
        candidates: IndexedSeq[Phenotype[A]], 
        spins: Int)
    : IndexedSeq[Phenotype[A]] = {

        def search(low: Int, high: Int, target: Double): Phenotype[A] = {
            if (low == high)
                candidates(low) 
            else (low + high)/2 match {
                case mid if candidates(mid).fitness > target => search(low, mid, target)
                case mid if candidates(mid).fitness < target => search(mid, high, target)
                case _ => candidates(low)
            }
        }
        Vector.fill(spins)(search(0, candidates.size, Random.nextDouble))
    }


    // creates random samples and matches them when it feels like it, else selects a random specimen
    def tournamentSelection[A <: Genome[A]](
        candidates: IndexedSeq[Phenotype[A]], 
        winners: Int, epsilon: Double, contestants: Int)
    : IndexedSeq[Phenotype[A]] = {

        def select(remaining: Int): IndexedSeq[Phenotype[A]] = {
            if (remaining < 1){ IndexedSeq[Phenotype[A]]() }
            else{
                if(Random.nextDouble > epsilon)
                    candidates(Random.nextInt(candidates.size - 1)) +:
                    tournamentSelection(candidates, winners - 1, epsilon, contestants)
                
                else
                    tournament(candidates, contestants) +:
                    tournamentSelection(candidates, winners - 1, epsilon, contestants)
            }
        }

        def tournament[A <: Genome[A]](
            candidates: IndexedSeq[Phenotype[A]], 
            contestants: Int
        ): Phenotype[A] = {

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

            val chosen = sample(0 to candidates.size - 1 toList, contestants).map(candidates(_))
            chosen.reduceLeft( (l, r) => if (l.fitness > r.fitness) l else r)
        }

        select(winners)
    }
}

object ParentSelection {

}
