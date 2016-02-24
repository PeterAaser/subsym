package GA

import scala._
import scalaz._
import Scalaz._

import scala.io.Source
import spray.json._
import DefaultJsonProtocol._

import Representations._
import Scaling._
import Reproduction._
import ParentSelection._

import Data._


object JParse {

    object MyJsonProtocol extends DefaultJsonProtocol {
        implicit val commonParamFormat = jsonFormat11(CommonParams)
        implicit val supFormat = jsonFormat3(SupParams)
        implicit val bitFormat = jsonFormat2(BitParams)
    }

    import MyJsonProtocol._

    case class CommonParams(
        problem: String, 
        adultPool: Int, 
        childPool: Option[Int],
        crossRate: Double, 
        mutationRate: Double,
        mutationSeverity: Double,
        contestants: Option[Int],
        epsilon: Option[Double],
        generations: Int,
        reproductionStrat: String,
        populationStrat: String
    )

    case class SupParams(
        symbols: Int,
        length: Int, 
        distance: Either[Int, String]
    )

    case class BitParams(
        length: Int,
        cutoff: Option[Int]
    )

    def parseProblem(p: String): Runner[_] = {
        val problemString = Source.fromFile("presets/" + p + ".json").getLines.mkString 
        val problem = problemString.parseJson.convertTo[CommonParams]

        problem.problem match {

            case "symbol" => {
                val suprisingParams = problemString.parseJson.convertTo[SupParams]
                symRunner(problem, suprisingParams)
            }

            case "LOLZ" | "OneMax" => {
                val bitParams = problemString.parseJson.convertTo[BitParams]
                bitRunner(problem, bitParams)
            }
        }
    }

    def symRunner(p: CommonParams, s: SupParams): Runner[SymbolGenome] = {

        type Pheno = Phenotype[SymbolGenome]
        type Phenos = IndexedSeq[Pheno]

        val distance = s.distance match {
            case Left(x) => x
            case Right(_) => s.length 
        }
        
        def evaluate(genome: SymbolGenome) = {
            Suprising.evaluator(distance)(genome)

        }

        def grow(genome: SymbolGenome): Pheno =
            Phenotype[SymbolGenome](genome, evaluate(genome), evaluate(genome), 0)

        def reproduce(adults: Phenos): Vector[SymbolGenome] =
            sexualReproduction(p.mutationRate)(adults).toVector

        val reproductionStrat: (Int, Int, Phenos) => Phenos = (p.reproductionStrat, p.epsilon, p.contestants) match {
            case ( "tournament", Some(epsilon), Some(contestants) ) => ParentSelection.tournamentStrat(_, _, _, epsilon, contestants)
            case ( "roulette", _, _) => rouletteStrat
            case ( "sigma", _, _) => sigmaSelect
            case _ => rouletteStrat
        }

        def populationStrat = (p.populationStrat, p.childPool) match {
            
            case ("mixin", Some(childPool)) => AdultSelection.mixin[SymbolGenome](
                p.adultPool,
                childPool,
                reproductionStrat,
                reproduce,
                Selection.proportionalMixin,
                genomes => genomes.par.map(grow(_)).toVector)

            case ("full", _) => AdultSelection.full[SymbolGenome](
                p.adultPool,
                reproductionStrat,
                reproduce,
                genomes => genomes.par.map(grow(_)).toVector)

            case ("over", _) => ???
        }

        Runner[SymbolGenome](
            SymbolGenome.initPool(p.adultPool, s.length, s.symbols.toByte, p.crossRate, p.mutationSeverity).map(grow(_)),
            pop => ( (pop.fittest.trueFitness >= 1.0) || (pop.generation > (p.generations - 1) ) ),
            populationStrat
        )
    }

    def bitRunner(p: CommonParams, b: BitParams): Runner[SingleBitGenome] = {

        type Pheno = Phenotype[SingleBitGenome]
        type Phenos = IndexedSeq[Pheno]
        
        val evaluate: (SingleBitGenome => Double) = (p.problem, b.cutoff) match {

            // case ("OneMax", _) => OneMax.randomEvaluator(b.length)
            case ("OneMax", _) => OneMax.evaluate
            case ("LOLZ", Some(cutoff)) => LOLZ.evaluator(cutoff)
        }

        def grow(genome: SingleBitGenome): Pheno =
            Phenotype[SingleBitGenome](genome, evaluate(genome), evaluate(genome), 0)

        def reproduce(adults: Phenos): Vector[SingleBitGenome] =
            sexualReproduction(p.mutationRate)(adults).toVector

        val reproductionStrat: (Int, Int, Phenos) => Phenos = (p.reproductionStrat, p.epsilon, p.contestants) match {
            case ( "tournament", Some(epsilon), Some(contestants) ) => ParentSelection.tournamentStrat(_, _, _, epsilon, contestants)
            case ( "roulette", _, _) => rouletteStrat
            case ( "sigma", _, _) => sigmaSelect
            case _ => rouletteStrat
        }

        def populationStrat = (p.populationStrat, p.childPool) match {
            
            case ("mixin", Some(childPool)) => AdultSelection.mixin[SingleBitGenome](
                p.adultPool,
                childPool,
                reproductionStrat,
                reproduce,
                Selection.proportionalMixin,
                genomes => genomes.par.map(grow(_)).toVector)

            case ("full", _) => AdultSelection.full[SingleBitGenome](
                p.adultPool,
                reproductionStrat,
                reproduce,
                genomes => genomes.par.map(grow(_)).toVector)

            case ("over", _) => ???
        }

        Runner[SingleBitGenome](
            SingleBitGenome.initPool(p.adultPool, b.length, p.crossRate, p.mutationSeverity).map(grow(_)),
            pop => ( (pop.fittest.trueFitness >= b.length) || (pop.generation > (p.generations - 1) ) ),
            populationStrat
        )
    }
}
