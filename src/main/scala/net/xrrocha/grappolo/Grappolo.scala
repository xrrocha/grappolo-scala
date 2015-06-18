package net.xrrocha.grappolo

import java.io.{FileWriter, PrintWriter}

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.search.spell.LevensteinDistance

import scala.io.Source

object Types {
  type Similarity = Double
  type Matrix = Map[Int, Map[Int, Similarity]]
}

import Types._

object Test extends App with Grappolo with LazyLogging {
  val names = Source.fromFile("data/surnames.txt").getLines().toSeq
  val distance = new LevensteinDistance

  val count = 10000
  val matrix =  Matrix.load("other/data/matrix.dat").
    filterKeys(_ < count).
    mapValues(_.filterKeys(_ < count).withDefaultValue(0d)).
    withDefaultValue(Map().withDefaultValue(0d))

  //val matrix = Matrix(names.length, .6d)((i, j) => distance.getDistance(names(i), names(j)))

  val scores = matrix.toSeq.flatMap(_._2.values).distinct.filter(s => s >= .69 && s <= .72).sorted
  val (clusters, similarity, dunnIndex) = scores.foldLeft(Seq[Seq[Int]](), 0d, Double.MaxValue) { (accum, similarity) =>
    val (_, _, minDunnIndex) = accum

    logger.info(s"Clustering with similarity $similarity")
    val clusters = agglomerate(matrix, similarity)
    val dunnIndex = Cluster.dunnIndex(clusters.map(Cluster(_)(matrix)))
    logger.info(s"${clusters.length} clusters with similarity $similarity and Dunn index $dunnIndex")

    if (dunnIndex < minDunnIndex) (clusters, similarity, dunnIndex)
    else accum
  }
  logger.info(s"${clusters.length} best clusters selected with similarity $similarity and lowest Dunn index $dunnIndex")

  val out = new PrintWriter(new FileWriter(s"other/data/clusters-$similarity-$dunnIndex.dat"), true)
  clusters.sortBy(-_.length).zipWithIndex.foreach { case(cluster, index) =>
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

case class Cluster(members: Seq[Int])(implicit matrix: Matrix) {
  val (centroids, intraSimilarity) = {
    val triplets = for {
      i <- members
      j <- members
      similarity = matrix(i)(j)
    } yield (i, j, similarity)
    val similarities = triplets.groupBy(_._1).mapValues(vs => vs.map(_._3).sum / vs.length)

    val maxSimilarity = similarities.values.max
    val centroids = similarities.filter(_._2 == maxSimilarity).keys.toSeq

    val intraSimilarity = similarities.values.sum / similarities.size

    (centroids, intraSimilarity)
  }

  def similarityWith(other: Cluster) = {
    val scores = for {
      i <- members
      j <- other.members
      similarity = matrix(i)(j)
    } yield similarity
    scores.sum / scores.length
  }
}

object Cluster {

  def dunnIndex(clusters: Seq[Cluster]) = {
    val minIntraSimilarity = Cluster.minIntraSimilarity(clusters)
    val maxInterSimilarity = Cluster.maxInterSimilarity(clusters)
    (1d - maxInterSimilarity) / (1d - minIntraSimilarity)
  }

  def maxInterSimilarity(clusters: Seq[Cluster]) = {
    if (clusters.length == 1) 0d
    else {
      val scores = for {
        i <- clusters.indices
        j <- i + 1 until clusters.length
        similarity = clusters(i).similarityWith(clusters(j))
      } yield similarity
      scores.max
    }
  }

  def minIntraSimilarity(clusters: Seq[Cluster]) = clusters.map(_.intraSimilarity).min
}

trait Grappolo extends LazyLogging {
  type Matrix = Map[Int, Map[Int, Similarity]]

  def extractCluster(element: Int, matrix: Matrix, threshold: Similarity): Seq[Int]
  def clusterQuality(cluster: Seq[Int], matrix: Matrix): Similarity
  def clusterOrdering(left: (Seq[Int], Int, Similarity), right: (Seq[Int], Int, Similarity)): Boolean

  def toMatrix(map: Matrix) =
    map
      .mapValues(_.withDefaultValue(0d))
      .withDefaultValue(Map().withDefaultValue(0d))

  def agglomerate(matrix: Matrix, threshold: Similarity): Seq[Seq[Int]] = {

    def clusterSplit(matrix: Map[Int, Map[Int, Similarity]]) = {
      val singletons = matrix.
        mapValues(_.filter(_._2 >= threshold)).
        filter(_._2.size == 1).
        keys.
        toSeq

      val reducedMatrix = (matrix -- singletons).
        mapValues(m => (m -- singletons).withDefaultValue(0d)).
        withDefaultValue(Map().withDefaultValue(0d))
      logger.info(s"${singletons.size} singletons, ${reducedMatrix.size} other elements")

      val clusters = cluster(toMatrix(reducedMatrix), threshold)
      logger.info(s"${clusters.length} clusters, ${clusters.map(_.length).sum} elements")

      (singletons.map(Seq(_)), clusters)
    }

    Stream.iterate(clusterSplit(matrix)) { case (singletons, clusters) =>
      logger.info(s"Agglomerating ${clusters.length} clusters")

      val newMatrix = Matrix(clusters.length, threshold) { (l, r) =>
        val scores = for {
          i <- clusters(l)
          j <- clusters(r)
          similarity = matrix(i)(j)
        } yield similarity
        scores.sum / scores.length
      }

      val (nextSingletons, nextClusters) = clusterSplit(newMatrix)

      def flatten(newClusters: Seq[Seq[Int]]) = newClusters.map(_.flatMap(clusters))

      (singletons ++ flatten(nextSingletons), flatten(nextClusters))
    }.
    dropWhile(_._2.nonEmpty).
    head.
    _1
  }

  def cluster(matrix: Matrix, threshold: Similarity): Seq[Seq[Int]] = {
    val elements = matrix.keySet.toSeq

    def getCluster(element: Int) = {
      val cluster = extractCluster(element, matrix, threshold)
      assert(cluster.nonEmpty, s"Cluster empty for element $element")
      cluster
    }

    val candidateClusters = elements.
      map(getCluster(_).sorted).
      groupBy(c => c).
      mapValues(_.length).
      toSeq.
      map { case(cluster, occurrences) => (cluster, occurrences, clusterQuality(cluster, matrix)) }.
      sortWith(clusterOrdering).
      map(_._1)

    val (clusters, clustered) = (candidateClusters  ++ elements.map(Seq(_)))
      .foldLeft(Seq[Seq[Int]](), Set[Int]()) { (accum, candidateCluster) =>
        val (clusters, clustered) = accum

        if (candidateCluster.exists(clustered.contains)) (clusters, clustered)
        else (clusters :+ candidateCluster, clustered ++ candidateCluster)
      }

    clusters
  }
}

object Matrix {
  def apply(size: Int, threshold: Similarity)(scorer: (Int, Int) => Similarity): Matrix = {
    val scores = for {
      i <- 0 until size
      j <- i + 1 until size
      similarity = scorer(i, j)
      if similarity >= threshold
    } yield (i, j, similarity)

    buildMatrix(size, scores)
  }

  def apply(size: Int, pairs: Iterable[(Int, Int)])(scorer: (Int, Int) => Similarity): Matrix = {
    val scores = for {
      (i, j) <- pairs
      similarity = scorer(i, j)
    } yield (i, j, similarity)

    buildMatrix(size, scores)
  }

  def buildMatrix(size: Int, scores: Iterable[(Int, Int, Similarity)]) = {
    val allScores = scores ++ scores.map(t => (t._2, t._1, t._3)) ++ (0 until size).map(i => (i, i, 1d))

    val emptyVector = Map[Int, Similarity]().withDefaultValue(0d)

    allScores.
      groupBy(_._1).
      mapValues(_.map(t => (t._2, t._3)).toMap.withDefaultValue(0d)).
      withDefaultValue(emptyVector)
  }

  def save(matrix: Matrix, filename: String): Unit = {
    val out = new PrintWriter(new FileWriter(filename), true)
    matrix.foreach { case(index, vector) =>
      out.println(vector.toSeq.sortBy(_._1).map(p => s"${p._1}/${p._2}").mkString(","))
    }
  }

  def load(filename: String): Matrix = {
    Source.fromFile(filename).
      getLines().
      zipWithIndex.
      map { case(line, index) =>
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
}
