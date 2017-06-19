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

    val distance = new JaroWinklerDistance
    //LevensteinDistance
    val matrix = Matrix(names.length, .6d)((i, j) => distance.getDistance(names(i), names(j)))

    val (clusters, similarity, dunnIndex) = GrappoloTest.bestCluster(matrix, .7d, .9d)
    println(s"${clusters.length} clusters at $similarity. Dunn index: $dunnIndex")

    clusters.sortBy(-_.length).zipWithIndex.foreach { case (cluster, index) =>
      println(s"${index + 1}: ${cluster.length} - ${cluster.map(names).sorted.mkString(", ")}")
    }
  }
}

object GrappoloTest extends Grappolo {

  import Types._
  import net.xrrocha.util.TimeUtils._

  def main(args: Array[String]): Unit = {
    val names = Source.fromFile("data/surnames.txt").getLines().toSeq
    val distance = new LevensteinDistance

    val count = 10000
    val lowSimilarity = .65d
    val highSimilarity = .77d
    val matrixFilename = "other/data/matrix.dat"

    val matrix = Matrix.load(matrixFilename).
      filterKeys(_ < count).
      mapValues(_.filterKeys(_ < count).withDefaultValue(0d)).
      withDefaultValue(Map().withDefaultValue(0d))

    val ((clusters, similarity, dunnIndex), elapsedTime) = time {
      bestCluster(matrix, lowSimilarity, highSimilarity)
    }
    logger.info(s"Found ${clusters.length} clusters in ${sayTimeInMillis(elapsedTime)}")

    val out = new PrintWriter(new FileWriter(s"other/data/grappolo-test-clusters-$similarity-$dunnIndex.dat"), true)
    clusters.sortBy(-_.length).foreach { cluster =>
      out.println(s"${cluster.length}\t${cluster.map(names).sorted.mkString(",")}")
    }
  }

  def bestCluster(matrix: Matrix, low: Similarity, high: Similarity): (Seq[Seq[Index]], Similarity, Similarity) = {
    val scores = matrix.toSeq.flatMap(_._2.values).distinct.filter(s => s >= low && s <= high).sorted
    scores.foldLeft(Seq[Seq[Index]](), 0d, Double.MaxValue) { (accum, similarity) =>
      val (_, _, minDunnIndex) = accum

      logger.debug(s"Clustering with similarity $similarity")
      val clusters = cluster(matrix, similarity)
      val dunnIndex = Cluster.dunnIndex(clusters.map(Cluster(_)(matrix)))
      logger.debug(s"${clusters.length} clusters with similarity $similarity and Dunn index $dunnIndex")

      if (dunnIndex < minDunnIndex) (clusters, similarity, dunnIndex)
      else accum
    }
  }

  //  override def extractCluster(element: Index, matrix: Matrix, threshold: Similarity): Seq[Int] = {
  //
  //    def similarityScore(pairs: Seq[IndexScore]): Similarity =
  //      if (pairs.isEmpty) 0d
  //      else pairs.map(_._2).sum / pairs.length
  //
  //    def pairsFromIndices(indices: Seq[Index]): Seq[IndexScore] = {
  //      indices.
  //        map { i =>
  //          val scores = indices.map(j => matrix(i)(j)).filter(_ >= threshold)
  //          i -> scores.sum / scores.size
  //        }.
  //        sortBy(-_._2)
  //    }
  //
  //    val pairs: Seq[IndexScore] = pairsFromIndices(matrix(element).filter(_._2 >= threshold).keySet.toSeq)
  //
  //    val initialScore = similarityScore(pairs)
  //    val initialPairs = pairs
  //    val initialMaxDelta = 0d
  //    val initialBestCluster = pairs
  //
  //    val (_, _, _, clusterPairs) =
  //      Iterator.iterate(
  //        initialScore,
  //        initialPairs,
  //        initialMaxDelta,
  //        initialBestCluster) { case (previousScore, previousPairs, previousMaxDelta, previousBestCluster) =>
  //
  //        val nextScore = similarityScore(previousPairs.init)
  //
  //        val nextPairs = {
  //          val indices = previousPairs.init.map(_._1)
  //          pairsFromIndices(indices)
  //        }
  //
  //        val delta = nextScore - previousScore
  //
  //        val (nextMaxDelta, nextBestCluster) =
  //          if (delta > previousMaxDelta) (delta, nextPairs)
  //          else (previousMaxDelta, previousBestCluster)
  //
  //        (nextScore, nextPairs, nextMaxDelta, nextBestCluster)
  //      }.
  //        dropWhile(_._2.nonEmpty).
  //        next
  //
  //    clusterPairs.map(_._1)
  //  }

  override def extractCluster(element: Index, matrix: Matrix, threshold: Similarity): Seq[Index] = {
    val neighbors = matrix(element).filter(p => p._2 >= threshold && p._2 < 1d)
    if (neighbors.isEmpty) Seq(element)
    else element +: {
      val maxScore = neighbors.values.max
      neighbors.filter(_._2 == maxScore).keys.toSeq
    }
  }

  // TODO Replace with cluster's intra-similarity
  override def clusterQuality(cluster: Seq[Index], matrix: Matrix): Similarity = {
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

  override def clusterOrdering(left: (Seq[Index], Index, Similarity), right: (Seq[Index], Index, Similarity)): Boolean = {
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

