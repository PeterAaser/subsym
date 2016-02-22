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

import scala.io.Source
import org.sameersingh.scalaplot._
import gnuplot.GnuplotPlotter
import jfreegraph.JFGraphPlotter

import reflect.runtime.universe._

import spray.json._
import DefaultJsonProtocol._

object GAsolver {


    implicit class ColonTypeExtender [T : TypeTag] (x : T) {
        def colonType = typeOf[T].toString
    }

    def main(args: Array[String]): Unit = {

        val problemString = Source.fromFile("presets/problem.json").getLines.mkString
        val problem = problemString.parseJson.convertTo[Map[String, JsValue]]

        def parseProblem(p: String) = {
            val problemString = Source.fromFile("presets/" + p + ".json").getLines.mkString 
            val problem = problemString.parseJson.convertTo[Map[String, JsValue]]
            val problemType = ((problem get "problem") match { case Some(JsString(j)) => j })

            problemType match {
                case "symbol" => parseSymbol(problem)
                case "LOLZ" => None
                case _ => None
            }
        }

        def parseSymbol(p: Map[String, JsValue]): Runner[SymbolGenome] = {
            val length = ((p get "length") match { case Some(JsNumber(v)) => v })
            val distance = ((p get "distance") match { 
                case Some(JsNumber(v)) => v
                case Some(JsString(s)) => length
            })
            val symbols = ((p get "symbols") match { case Some(JsNumber(v)) => v })
            val adults = ((p get "adults") match { case Some(JsNumber(v)) => v })
            val crossRate = ((p get "crossRate") match { case Some(JsNumber(v)) => v })
            val mutationRate = ((p get "mutationRate") match { case Some(JsNumber(v)) => v })
            val mutationSeverity = ((p get "mutationSeverity") match { case Some(JsNumber(v)) => v })
            
            Suprise.symbolRunner(
                distance.toInt,
                symbols.toInt,
                length.toInt,
                adults.toInt,
                crossRate.toDouble,
                mutationRate.toDouble,
                mutationSeverity.toDouble)
        }

        println(parseProblem("problem"))
        val runner = parseProblem("problem") match {
            case f: Runner[SymbolGenome] => f.solve(30) 
            case f: Runner[SingleBitGenome] => f.solve(30)
        }
        
        
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
                ParentSelection.tournamentStrat(_, 0.9, 8),
                reproduce,
                genomes => genomes.map(grow(_))
            )

            val runner = Runner[SymbolGenome](
                poolSize => SymbolGenome.initPool(poolSize, length, symbols, crossRate, mutationSeverity).map(grow(_)),
                p => ( (p.fittest.trueFitness == 1.0) || (p.generation > 500)),
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


