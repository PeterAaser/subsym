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

import scala.io.Source
import org.sameersingh.scalaplot._
import gnuplot.GnuplotPlotter
import jfreegraph.JFGraphPlotter
import java.io._

object GAsolver {

    def main(args: Array[String]): Unit = {
        
        println("Input problem name")
        val problem = scala.io.StdIn.readLine()

        println("running: " + problem)
        val p = JParse.parseProblem(problem)

        val bestData = new XYData()
        val avgData = new XYData()

        for (i <- 0 until 1) {
            val logs = p.solve
            val (avg, best) = logs._2.reverse.unzip

            avgData += new MemXYSeries((0 until avg.length).map(_.toDouble), avg, "average")
            bestData += new MemXYSeries((0 until best.length).map(_.toDouble), best, "best")
        }

        val bestChart = new XYChart("One Max", bestData)
        val avgChart = new XYChart("One Max", avgData)
        val bestPlotter = new JFGraphPlotter(bestChart)
        val avgPlotter = new JFGraphPlotter(avgChart)
        bestPlotter.gui()
        avgPlotter.gui()
    }
}

