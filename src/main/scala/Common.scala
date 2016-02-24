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


    def normalizer[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]]): (Double => Double) = {
        val fittest = candidates.reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)
        (relativeFitness => relativeFitness/(fittest.relativeFitness))
    }


    def badnessNormalizer[A <: Genome[A]](candidates: IndexedSeq[Phenotype[A]]): (Double => Double) = {
        val unfittest = candidates.reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)
        (relativeFitness => 1.0 - relativeFitness/(unfittest.relativeFitness))
    }


    def rankScale[A <: Genome[A]](generation: Int, candidates: IndexedSeq[Phenotype[A]]): (IndexedSeq[Phenotype[A]]) = {
        val sorted = candidates.sortBy(_.relativeFitness).zipWithIndex
        val min = sorted.head._1.relativeFitness 
        val max = sorted.last._1.relativeFitness 
        val n = sorted.length

        def rscaler(rank: Int, fitness: Double): Double = {
            (min + (max - min)*(rank - 1)/(sorted.length - 1))
        }
        sorted.map(t => t._1.copy(relativeFitness = rscaler(t._2, t._1.relativeFitness)))
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

object Selection {

    def proportional[A <: Genome[A]](
        candidates: IndexedSeq[Phenotype[A]],
        spots: Int): IndexedSeq[Phenotype[A]] =

            candidates.sortBy(_.relativeFitness) takeRight spots


    def proportionalMixin[A <: Genome[A]](
        children: IndexedSeq[Phenotype[A]],
        adults: IndexedSeq[Phenotype[A]],
        spots: Int): IndexedSeq[Phenotype[A]] =

            (adults ++ children).sortBy(_.relativeFitness) takeRight spots

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

    def sigmaSelect[A <: Genome[A]](winners: Int): IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]] =
        adults => {
            val Snormalizer = normalizer[A](_)
            val Ssigma = sigma[A](_)
            val sScaled = scale(adults, Ssigma)
            val nScaled = scale(sScaled, Snormalizer)
            val rScaled = rouletteScaler(nScaled)
            val rouletted = ParentSelection.rouletteSelection(rScaled)(winners)
            rouletted
        }

    def tournamentStrat[A <: Genome[A]](winners: Int, epsilon: Double, contestants: Int): (IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]]) =
        adults => tournamentSelection(adults, winners, epsilon, contestants)

    def rouletteStrat[A <: Genome[A]](winners: Int): IndexedSeq[Phenotype[A]] => IndexedSeq[Phenotype[A]] =
        adults => 
            ParentSelection.rouletteSelection(rouletteScaler(adults))(winners)
        

    // def rankSelection[A <: Genome[A]](winners

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

    // µ child pool size
    // λ adult pool size
    
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
            println("Got %d parents".format(λ))
            val parents = parentSel( λ )(pop.adults)

            val children = grow(reproductionScheme(parents))

            println("there were %d survivors for the next generation".format( µ ))
            val survivors = adultSel( λ )(children, pop.adults)
            pop.copy(adults = survivors)
        }
}
