package net.xrrocha.cluster

import java.io.FileOutputStream

import com.typesafe.scalalogging.LazyLogging
import net.xrrocha.grappolo.Matrix
import org.slf4j.LoggerFactory
;

object Clusterer extends LazyLogging {

  lazy val log = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {

    implicit val elements =
      io.Source.
        fromFile("data/schoolNames.txt").
        getLines().
        take(100).
        toSeq
    val size = elements.length
    logger.info(s"Loaded $size elements")

    logger.info(s"Creating scorer")
    // val (scorer, minSimilarity) = (new LevensteinDistance, 0.7d)
    val (scorer, minSimilarity) = {
      import com.wcohen.ss._
      val distance = new JaroWinkler
      val scorer = new SoftTFIDF(distance, .875d)
      val iterator = new BasicStringWrapperIterator(elements.iterator.map(new BasicStringWrapper(_)))
      scorer.train(iterator)

      (scorer, 0.475d)
    }

    logger.info(s"Creating similarity matrix")
    implicit val simMatrix = {
      val (baseMatrix, elapsedTime) = time(Matrix(size, (i, j) => scorer.score(elements(i), elements(j))))
      logger.info(s"Created similarity matrix in $elapsedTime milliseconds")

      baseMatrix.dump(new FileOutputStream("target/matrix_1.csv"), elements)

      baseMatrix
    }

    val clusters = {
      val (clusters, elapsedTime) = time(clusterAndAgglomerate(simMatrix, minSimilarity))

      val clusteredCount = clusters.map(_.members.length).sum
      logger.info(s"Built ${clusters.length} clusters with $clusteredCount elements in $elapsedTime milliseconds")

      assert(size == clusteredCount)

      clusters.sortBy(-_.members.length)
    }

    for {
      i <- clusters.indices
      j <- clusters(i).members.indices
      elementIndex = clusters(i).members(j)
      element = elements(elementIndex)
    } {
      println(s"${i + 1}\t${j + 1}\t$element")
    }

    val nonSingletonClusters = clusters.filter(_.members.size > 1)
    val averageIntraSimilarity = nonSingletonClusters.map(_.intraSimilarity).sum / nonSingletonClusters.length
    logger.info(s"Average intra-similarity: $averageIntraSimilarity")
  }

  def clusterAndAgglomerate(simMatrix: Matrix, minSimilarity: Double)(implicit toString: Int => String = _.toString) = {
    val seedClusters = cluster(simMatrix, minSimilarity)

    agglomerate(seedClusters, simMatrix, minSimilarity)
  }

  def cluster(simMatrix: Matrix, minSimilarity: Double)(implicit toString: Int => String = _.toString) =
    Stream.iterate((simMatrix, Seq[Cluster]())) { case (matrix, clusters) =>

      // TODO There can be multiple "best" clusters; account for that: capture all non-overlapping clusters
      val (cluster, _) = matrix.bestCluster(minSimilarity)

      // FIXME Subtract cluster from values as well!
      (matrix -- cluster, clusters :+ Cluster(cluster.toSeq)(matrix, toString))
    }
      .dropWhile { case (matrix, _) => matrix.nonEmpty }
      .head
      ._2

  // TODO Pass cluster comparison lambda as argument to agglomerate
  def agglomerate(seedClusters: Seq[Cluster], simMatrix: Matrix, minSimilarity: Double)(implicit toString: Int => String = _.toString) = {
    Stream.iterate((seedClusters, true)) { case (clustersSoFar, change) =>
      logger.info(s"Agglomerating ${clustersSoFar.length} clusters")
      val newMatrix = Matrix(clustersSoFar.length,
        //(i, j) => scoreClusters(clustersSoFar(i).members, clustersSoFar(j).members, simMatrix))
        (i, j) => compareClusters(clustersSoFar(i).centroids, clustersSoFar(j).centroids, simMatrix))

      val newClusters = cluster(newMatrix, minSimilarity)

      val nextClusters = newClusters.map { newCluster =>
        Cluster(newCluster.members.flatMap(clustersSoFar(_).members))(simMatrix, toString)
      }

      (nextClusters, nextClusters.length != clustersSoFar.length)
    }
      .dropWhile(_._2)
      .head
      ._1
  }

  def compareClusters(leftCluster: Seq[Int], rightCluster: Seq[Int], matrix: Matrix) = {
    val scores = for {
      i <- leftCluster
      j <- rightCluster
      score = matrix(i)(j)
    } yield score
    scores.sum / scores.length
  }

  def compareElementToCluster(elementIndex: Int, cluster: Seq[Int], matrix: Matrix) = {
    val scores = for (i <- cluster.indices) yield matrix(elementIndex)(cluster(i))
    scores.sum / scores.length
  }

  case class Cluster(members: Seq[Int])(implicit matrix: Matrix, toString: Int => String) {
    lazy val centroids = {
      if (members.length <= 2) members
      else {
        val scoreTriplets = for {
          i <- members.indices
          j <- i + 1 until members.size
          score = matrix(members(i))(members(j))
        } yield (members(i), members(j), score)

        val scorePairs =
          (scoreTriplets ++ scoreTriplets.map(t => (t._2, t._1, t._3))).
            groupBy(_._1).
            mapValues(_.map(_._3).sum)

        val maxScore = scorePairs.values.max

        scorePairs.filter(_._2 == maxScore).keySet.toSeq
      }
    }

    lazy val intraSimilarity = matrix.intraSimilarity(members)
  }

}
