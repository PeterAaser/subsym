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

import spray.json._
import DefaultJsonProtocol._

import java.io._

object GAsolver {

    def main(args: Array[String]): Unit = {

        val problems = {
            val mutationRates = List.fill(200)(Random.nextDouble*0.9 + 0.05)
            val severity = List.fill(200)(Random.nextDouble*0.9 + 0.05)
            val cross = List.fill(200)(Random.nextDouble*0.9 + 0.05)
            val contestants = List.fill(200)( (Random.nextInt(95) + 5) )
            val epsilon = List.fill(200)(Random.nextDouble*0.9 + 0.05)

            def makeProblem(i: Int): String = { """
            
                {
                    "problem": "symbol",
                    "distance": "global",
                    "symbols": 26,
                    "length": 53,
                    "adults": 2000,
                    "crossRate": %1.2f,
                    "mutationRate": %1.2f,
                    "mutationSeverity": %1.2f,  
                    "generations": 800,
                    "contestants": %d,
                    "epsilon": %1.2f
                }

            """.format(cross(i), mutationRates(i), severity(i), contestants(i), epsilon(i))
            }

            List.tabulate(200)(n => makeProblem(n))
        }

        val jasons = problems.map(_.parseJson.convertTo[Map[String, JsValue]])

        jasons.foreach(p => parseSymbol(p) match { case f: Runner[SymbolGenome] => f.solve })

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
            val generations = ((p get "generations") match { case Some(JsNumber(v)) => v })
            val contestants = ((p get "contestants") match { case Some(JsNumber(v)) => v })
            val epsilon = ((p get "epsilon") match { case Some(JsNumber(v)) => v })
            

            val logfile = "symbol_cross_%1.2f_mrate_%1.2f_msev_%1.2f.txt".format(crossRate, mutationRate, mutationSeverity)
            val writer = new PrintWriter(new File(logfile))
            val logger = { 
                pop: String => {
                    writer.write(pop)
                }
            }

            println("Rollin")
            
            Suprise.symbolRunner(
                distance.toInt,
                symbols.toByte,
                length.toInt,
                adults.toInt,
                crossRate.toDouble,
                mutationRate.toDouble,
                mutationSeverity.toDouble,
                generations.toInt,
                contestants.toInt,
                epsilon.toDouble,
                logger,
                writer)
        }

        // val runner = parseProblem("problem") match {
        //     case f: Runner[SymbolGenome] => f.solve
        //     case f: Runner[SingleBitGenome] => f.solve
        // }
        
        
    }
}

object Suprise {

    import Suprising._

    type Pheno = Phenotype[SymbolGenome]
    type Phenos = IndexedSeq[Pheno]

    def symbolRunner(
        distance: Int,
        symbols: Byte,
        length: Int,

        adults: Int,
        crossRate: Double,
        mutationRate: Double,
        mutationSeverity: Double,
        generations: Int,
        contestants: Int,
        epsilon: Double,
        logger: String => Unit,
        writer: java.io.PrintWriter): Runner[SymbolGenome] = {

            def evaluate(genome: SymbolGenome) = 
                Suprising.evaluator(distance)(genome)

            def grow(genome: SymbolGenome): Pheno =
                Phenotype[SymbolGenome](genome, evaluate(genome), evaluate(genome), 0)

            def reproduce(adults: Phenos): Vector[SymbolGenome] =
                 sexualReproduction(mutationRate)(adults).toVector

            val evolutionStrategy = AdultSelection.full[SymbolGenome](
                adults,
                ParentSelection.tournamentStrat(_, epsilon, contestants),
                reproduce,
                genomes => {
                    val a = genomes.par.map(grow(_))
                    a.toVector
                }
            )

            val runner = Runner[SymbolGenome](
                SymbolGenome.initPool(adults, length, symbols, crossRate, mutationSeverity).map(grow(_)),
                p => ( (p.fittest.trueFitness == 1.0) || (p.generation > (generations - 1))),
                evolutionStrategy,
                logger,
                writer
            )
            
            runner
    }
}
