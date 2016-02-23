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

        val p = JParse.parseProblem("lolz")
        p.solve
    }
}

