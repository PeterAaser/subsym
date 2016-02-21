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
import Suprise._


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

        val go = ParamRun.paramRunner(
            30,
            30,
            0.2,
            0.2,
            0.2
        )

        println("runner acquired")

        val done = go.solve(20)
        println(done._2.head)
        println(done._2(5))

    }
}

object Suprise {

    import Suprising._

    type Pheno = Phenotype[SymbolGenome]
    type Phenos = IndexedSeq[Pheno]

    def symbolRunner(
        distance: Int,
        symbols: Int,
        length: Int,

        adults: Int,
        children: Int,
        crossRate: Double,
        mutationRate: Double,
        mutationSeverity: Double): Runner[SymbolGenome] = {

            def evaluate(genome: SymbolGenome) = 
                Suprising.evaluator(distance)(genome)

            def grow(genome: SymbolGenome): Pheno =
                Phenotype[SymbolGenome](genome, evaluate(genome), evaluate(genome), 0)

            def reproduce(adults: Phenos): Vector[SymbolGenome] =
                 sexualReproduction(mutationRate)(adults).toVector

            val evolutionStrategy = AdultSelection.full[SymbolGenome](
                adults,
                ParentSelection.rouletteStrat(_),
                reproduce,
                genomes => genomes.map(grow(_))
            )

            val runner = Runner[SymbolGenome](
                poolSize => SymbolGenome.initPool(poolSize, length, symbols, crossRate, mutationSeverity).map(grow(_)),
                p => ( (p.fittest.trueFitness == 1.0) || (p.generation > 50)),
                evolutionStrategy
            )
            
            runner
    }
}

object ParamRun {

    import ParamSearch._

    type Pheno = Phenotype[ParamGenome]
    type Phenos = IndexedSeq[Pheno]

    def paramRunner(
        adults: Int,
        children: Int,
        crossRate: Double,
        mutationRate: Double,
        mutationSeverity: Double): Runner[ParamGenome] = {

            def evaluate(genome: ParamGenome) = 
                ParamSearch.evaluate(genome)

            def grow(genome: ParamGenome): Pheno = {
                val v = evaluate(genome)
                Phenotype[ParamGenome](genome, v, v, 0)
            }

            def reproduce(adults: Phenos): Vector[ParamGenome] =
                 sexualReproduction(mutationRate)(adults).toVector

            val evolutionStrategy = AdultSelection.full[ParamGenome](
                adults,
                ParentSelection.tournamentStrat(_, 0.2, 9),
                reproduce,
                genomes => genomes.map(grow(_))
            )

            val runner = Runner[ParamGenome](
                poolSize => ParamGenome.initPool(poolSize, 3, crossRate, mutationSeverity).map(grow(_)),
                p => ( (p.fittest.trueFitness == 1.0) || (p.generation > 40)),
                evolutionStrategy
            )
            
            runner
    }
}


