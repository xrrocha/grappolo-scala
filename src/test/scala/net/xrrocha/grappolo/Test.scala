package net.xrrocha.grappolo

import java.io.{FileWriter, PrintWriter}

import com.typesafe.scalalogging.StrictLogging
import net.xrrocha.grappolo.Types._
import org.apache.lucene.search.spell.LevensteinDistance

import scala.io.Source

object Test extends App with Grappolo with StrictLogging {
  val names = Source.fromFile("data/surnames.txt", "UTF-8").getLines().toSeq
  val distance = new LevensteinDistance

  val count = 10000
  val matrix = Matrix.load("other/data/matrix.dat").
    filterKeys(_ < count).
    mapValues(_.filterKeys(_ < count).withDefaultValue(0d)).
    withDefaultValue(Map().withDefaultValue(0d))

  //val matrix = Matrix(names.length, .6d)((i, j) => distance.getDistance(names(i), names(j)))

  val scores = matrix.toSeq.flatMap(_._2.values).distinct.filter(s => s >= .69 && s <= .72).sorted
  val (clusters, similarity, dunnIndex) = scores.foldLeft(Seq[Seq[Int]](), 0d, Double.MaxValue) { (accum, similarity) =>
    val (_, _, minDunnIndex) = accum

    logger.info(s"Clustering with similarity $similarity")
    val clusters = cluster(matrix, similarity)
    val dunnIndex = Cluster.dunnIndex(clusters.map(Cluster(_)(matrix)))
    logger.info(s"${clusters.length} clusters with similarity $similarity and Dunn index $dunnIndex")

    if (dunnIndex < minDunnIndex) (clusters, similarity, dunnIndex)
    else accum
  }
  logger.info(s"${clusters.length} best clusters selected with similarity $similarity and lowest Dunn index $dunnIndex")

  val out = new PrintWriter(new FileWriter(s"other/data/test-clusters-$similarity-$dunnIndex.dat"), true)
  clusters.sortBy(-_.length).zipWithIndex.foreach { case (cluster, index) =>
    out.println(s"${index + 1}: ${cluster.length} - ${cluster.map(names).sorted.mkString(", ")}")
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