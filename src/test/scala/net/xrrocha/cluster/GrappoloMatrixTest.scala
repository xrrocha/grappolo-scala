package net.xrrocha.cluster

import java.io.{PrintWriter, FileWriter}

import com.typesafe.scalalogging.LazyLogging
import net.xrrocha.test.BaseTest
import net.xrrocha.util.TimeUtils._
import org.apache.lucene.search.spell.LevensteinDistance

import scala.io.Source

class GrappoloMatrixTest extends BaseTest {
  import GrappoloMatrixTest._

  test("All element indexes are present") {
    assert(elements.indices.forall(matrix.contains))
    elements.indices.forall { i =>
      elements.indices.forall(matrix(i).contains)
    }
  }

  test("Only element indexes are present") {
    assert(!matrix.contains(elements.length))
    elements.indices.forall { i =>
      !matrix(i).contains(elements.length)
    }
  }

  test("All diagonal elements are 1") {
    assert(elements.indices.forall(i => matrix(i)(i) == 1d))
  }

  test("Returns value for every row and column") {
    val cells = for(i <- elements.indices; j <- elements.indices) yield matrix(i)(j)
    assert(cells.forall(s => s >= 0d && s <= 1d))
  }

  test("Every row is equal to its corresponding column") {
    assert(elements.indices.forall { i =>
      val row = matrix(i)
      val column = elements.indices.map(j => (j, matrix(j)(i))).toMap
      elements.indices.map(row) == elements.indices.map(column)
    })
  }
}

object GrappoloMatrixTest extends LazyLogging {
  import BaseTest._

  def loadMatrix(filename: String) = {
    Source.fromFile(filename).getLines().zipWithIndex.map { case(line, index) =>
      val inFields = line.split(",")
      val vector = inFields.
        map { field =>
        val pair = field.split("/")
        (pair(0).toInt, pair(1).toDouble)
      }.
        toMap.
        withDefaultValue(0d)
      (index, vector)
    }.
      toMap.withDefaultValue(Map.empty.withDefaultValue(0d))
  }

  val elements = load("data/surnames.txt")

  val scoringThreshold = 0.5
  val clusteringThreshold = 0.7
  val scorer = new LevensteinDistance

  val count = elements.length

  logger.info(s"$count elements, clustering threshold $clusteringThreshold, scoring threshold $scoringThreshold")
  implicit val matrix = {
    val (matrix, matrixTime) = time {
      loadMatrix("matrix.dat")
      //Grappolo.buildMatrix(count, scoringThreshold) { (i, j) =>
      //  scorer.getDistance(elements(i), elements(j))
      //}
    }
    logger.info(s"Created matrix for $count elements in ${sayTimeInMillis(matrixTime)}")
    matrix
  }

  //val matrixOut = new PrintWriter(new FileWriter("matrix.dat"), true)
  //val namesOut = new PrintWriter(new FileWriter("matrix-names.dat"), true)
  //for {
  //  i <- matrix.keySet.toSeq.sorted
  //  v = matrix(i).toSeq.sortWith { (p1, p2) =>
  //    val (index1, score1) = p1
  //    val (index2, score2) = p2
  //
  //    (score2 < score1) || {
  //      (score2 == score1) && (index1 < index2)
  //    }
  //  }
  //} {
  //  matrixOut.println(v.map { case (index, score) => s"$index/$score" }.mkString(","))
  //  namesOut.println(v.map { case (index, score) => s"${elements(index)}/$score" }.mkString(","))
  //}
}
