package net.xrrocha.grappolo

import java.io.{FileWriter, PrintWriter}

import org.apache.lucene.search.spell._
import org.scalatest.FunSuite

import scala.io.Source

class GrappoloTest extends FunSuite {
  test("Clusters names") {
    val names = Seq(
      "alejandro", "aleajndro", "laejandro",
      "malrene", "marlen", "marlene", "marleny", "marleney",
      "marta", "martha",
      "ricardo")

    val distance = new JaroWinklerDistance//LevensteinDistance
    val matrix = Matrix(names.length, .6d)((i, j) => distance.getDistance(names(i), names(j)))

    val (clusters, similarity, dunnIndex) =  Test.bestCluster(matrix, .7d, .9d)
    println(s"${clusters.length} clusters at $similarity. Dunn index: $dunnIndex")

    clusters.sortBy(-_.length).zipWithIndex.foreach { case(cluster, index) =>
      println(s"${index + 1}: ${cluster.length} - ${cluster.map(names).sorted.mkString(", ")}")
    }
  }
}

object Test extends Grappolo {
  import Types._

  def main(args: Array[String]): Unit = {
    val names = Source.fromFile("data/surnames.txt").getLines().toSeq
    val distance = new LevensteinDistance

    val count = 10000
    val matrix =  Matrix.load("other/data/matrix.dat").
      filterKeys(_ < count).
      mapValues(_.filterKeys(_ < count).withDefaultValue(0d)).
      withDefaultValue(Map().withDefaultValue(0d))

    val (clusters, similarity, dunnIndex) =  bestCluster(matrix, .65, .75)

    val out = new PrintWriter(new FileWriter(s"other/data/clusters-$similarity-$dunnIndex.dat"), true)
    clusters.sortBy(-_.length).zipWithIndex.foreach { case(cluster, index) =>
      out.println(s"${index + 1}: ${cluster.length} - ${cluster.map(names).sorted.mkString(", ")}")
    }
  }

  def bestCluster(matrix: Matrix, low: Similarity, high: Similarity) = {
    val scores = matrix.toSeq.flatMap(_._2.values).distinct.filter(s => s >= low && s <= high).sorted
    scores.foldLeft(Seq[Seq[Int]](), 0d, Double.MaxValue) { (accum, similarity) =>
      val (_, _, minDunnIndex) = accum

      logger.debug(s"Clustering with similarity $similarity")
      val clusters = cluster(matrix, similarity)
      val dunnIndex = Cluster.dunnIndex(clusters.map(Cluster(_)(matrix)))
      logger.debug(s"${clusters.length} clusters with similarity $similarity and Dunn index $dunnIndex")

      if (dunnIndex < minDunnIndex) (clusters, similarity, dunnIndex)
      else accum
    }
  }

  def extractCluster(element: Int, matrix: Matrix, threshold: Similarity): Seq[Int] = {
    val neighbors = matrix(element).filter(p => p._2 >= threshold && p._2 < 1d)
    if (neighbors.isEmpty) Seq(element)
    else element +: {
      val maxScore = neighbors.values.max
      neighbors.filter(_._2 == maxScore).keys.toSeq
    }
  }

  // TODO Replace with cluster's intra-similarity
  def clusterQuality(cluster: Seq[Int], matrix: Matrix): Similarity = {
    assert(cluster.nonEmpty)
    if (cluster.length == 1) 1d
    else {
      val scores = for {
        i <- cluster.indices
        j <- i + 1 until cluster.length
        similarity = matrix(i)(j)
      } yield similarity
      scores.sum / scores.length
    }
  }

  def clusterOrdering(left: (Seq[Int], Int, Similarity), right: (Seq[Int], Int, Similarity)): Boolean = {
    val (cluster1, occurrences1, quality1) = left
    val (cluster2, occurrences2, quality2) = right

    occurrences2 < occurrences1 || {
      occurrences2 == occurrences1 && {
        quality2 < quality1 || {
          quality2 == quality1 && cluster2.length < cluster1.length
        }
      }
    }
  }
}

