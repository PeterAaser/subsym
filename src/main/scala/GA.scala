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

        val p = JParse.parseProblem("om")
        val logs = p.solve

        val (avg, best) = logs._2.reverse.unzip

        val series = new MemXYSeries((0 until avg.length).map(_.toDouble), avg, "average")
        val data = new XYData(series)
        data += new MemXYSeries((0 until best.length).map(_.toDouble), best, "best")
        val chart = new XYChart("One Max", data)
        chart.showLegend = true
        val plotter = new JFGraphPlotter(chart)
        plotter.gui()
    }
}

