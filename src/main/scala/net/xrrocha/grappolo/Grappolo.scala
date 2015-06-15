package net.xrrocha.grappolo

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.search.spell.LevensteinDistance

import scala.io.Source

object Test extends App with Grappolo with LazyLogging {
  val names = Source.fromFile("data/surnames.txt").getLines().toSeq
  val distance = new LevensteinDistance

  val matrix = Matrix("other/data/matrix.dat")
  //val matrix = Matrix(1000, .6d)((i, j) => distance.getDistance(names(i), names(j)))

  val clusters = agglomerate(matrix, .7d)
  logger.info(s"clustered elements: ${clusters.map(_.length).sum}")
  assert(clusters.map(_.length).sum == matrix.size)

  clusters.sortBy(_.length).zipWithIndex.foreach { case(cluster, index) =>
    logger.debug(s"${index + 1}: ${cluster.length} - ${cluster.map(names).sorted.mkString(", ")}")
  }

  def extractCluster(element: Int, matrix: Map[Int, Map[Int, Double]], threshold: Double): Seq[Int] = {
    val vector = matrix(element).filter(p => p._2 >= threshold && p._2 < 1d)
    if (vector.isEmpty) Seq(element)
    else element +: {
      val maxScore = vector.values.max
      vector.filter(_._2 == maxScore).keys.toSeq
    }
  }

  def clusterQuality(cluster: Seq[Int], matrix: Map[Int, Map[Int, Double]]): Double = {
    val scores = for {
      i <- cluster.indices
      j <- i + 1 until cluster.length
      score = matrix(i)(j)
    } yield score
    scores.sum / scores.length
  }

  def clusterOrdering(left: (Seq[Int], Int, Double), right: (Seq[Int], Int, Double)): Boolean = {
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

trait Grappolo extends LazyLogging {
  def extractCluster(element: Int, matrix: Map[Int, Map[Int, Double]], threshold: Double): Seq[Int]
  def clusterQuality(cluster: Seq[Int], matrix: Map[Int, Map[Int, Double]]): Double
  def clusterOrdering(left: (Seq[Int], Int, Double), right: (Seq[Int], Int, Double)): Boolean

  def agglomerate(matrix: Map[Int, Map[Int, Double]], threshold: Double): Seq[Seq[Int]] = {
    logger.info("Ora jue")
    Stream.iterate((cluster(matrix, threshold), false)) { case (clusters, done) =>
      logger.info(s"Agglomerating ${clusters.length} clusters")

      val newMatrix = Matrix(clusters.length, threshold) { (l, r) =>
        val scores = for {
          i <- clusters(l)
          j <- clusters(r)
          score = matrix(i)(j)
        } yield score
        scores.sum / scores.length
      }

      val newClusters = cluster(newMatrix, threshold)

      val nextClusters = newClusters.map(_.flatMap(clusters))

      (nextClusters, nextClusters.length == clusters.length)
    }.
    dropWhile(!_._2).
    head.
    _1
  }

  def cluster(matrix: Map[Int, Map[Int, Double]], threshold: Double): Seq[Seq[Int]] = {
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
    logger.debug(s"${candidateClusters.length} candidateClusters")

    val (clusters, clustered) = (candidateClusters  ++ elements.map(Seq(_)))
      .foldLeft(Seq[Seq[Int]](), Set[Int]()) { (accum, candidateCluster) =>
        val (clusters, clustered) = accum

        if (candidateCluster.exists(clustered.contains)) (clusters, clustered)
        else (clusters :+ candidateCluster, clustered ++ candidateCluster)
      }
    logger.debug(s"${clusters.length} clusters")

    clusters
  }
}

object Matrix {
  def apply(size: Int, threshold: Double)(scorer: (Int, Int) => Double): Map[Int, Map[Int, Double]] = {
    val scores = for {
      i <- 0 until size
      j <- i + 1 until size
      score = scorer(i, j)
      if score >= threshold
    } yield (i, j, score)

    val allScores = scores ++ scores.map(t => (t._2, t._1, t._3)) ++ (0 until size).map(i => (i, i, 1d))

    val emptyVector = Map[Int, Double]().withDefaultValue(0d)

    allScores.
      groupBy(_._1).
      mapValues(_.map(t => (t._2, t._3)).toMap.withDefaultValue(0d)).
      withDefaultValue(emptyVector)
  }

  def apply(filename: String): Map[Int, Map[Int, Double]] = {
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
