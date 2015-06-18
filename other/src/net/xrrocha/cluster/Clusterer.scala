package net.xrrocha.cluster

import com.typesafe.scalalogging.LazyLogging

class Clusterer(size: Int, threshold: Double, matrix: Map[Int, Map[Int, Double]]) {
  def cluster(): Seq[Seq[Int]] = {

    val candidateClusters = for (element <- matrix.keys) yield {
      val siblings = matrix(element).filter(_._2 >= threshold).keySet

      val extendedSiblings = siblings.flatMap(matrix).filter(_._2 >= threshold).map(_._1)

      val seedCluster = extendedSiblings.filter { i =>
        val scores = siblings.toSeq.map(s => matrix(s)(i))
        scores.sum / scores.length > threshold
      }

      def closestSiblings(elementIndex: Int): Set[Int] = {
        val siblings = (matrix(elementIndex) - elementIndex).filter(_._2 >= threshold)
        if (siblings.isEmpty) Set()
        else {
          val maxScore = siblings.values.max
          siblings.filter(_._2 == maxScore).keySet
        }
      }

      val cluster = seedCluster.filter { index =>
        val closeSiblings = closestSiblings(index).filter(cs => closestSiblings(cs).contains(index))

        closeSiblings.isEmpty || seedCluster.intersect(closeSiblings).nonEmpty
      }

      cluster.toSeq.sorted
    }

    def intraSimilarity(elements: Seq[Int]) = {
      if (elements.isEmpty) 0d
      else if (elements.length == 1) 1d
      else {
        val scores = for {
          i <- elements.indices
          j <- i + 1 until elements.length
          score = matrix(elements(i))(elements(j))
        } yield score
        scores.sum / scores.length
      }
    }

    val orderedCandidateClusters =
      candidateClusters.
        toSeq.
        groupBy(p => p).
        mapValues(v => v.length).
        toSeq.
        map(p => (p._2, intraSimilarity(p._1), p._1)).
        sortWith { (c1, c2) =>
          val (votes1, similarity1, elements1) = c1
          val (votes2, similarity2, elements2) = c2
          (votes2 < votes1) || {
            (votes2 == votes1) &&
              (similarity2 < similarity1) || {
              (similarity2 == similarity1) && {
                elements2.size < elements1.size
              }
            }
          }
        }.
        map(_._3)

    val allCandidates = orderedCandidateClusters ++ (0 until size).map(Seq(_))
    val (clusters, _) = allCandidates.foldLeft(Seq[Seq[Int]](), Set[Int]()) { (accum, candidateCluster) =>
      val (clusters, clustered) = accum
      val nextClustered = clustered ++ candidateCluster
      if (!candidateCluster.exists(clustered.contains)) (clusters :+ candidateCluster, nextClustered)
      else accum
    }

    clusters
  }
}

object Clusterer extends LazyLogging {

  def cluster(size: Int, clusteringThreshold: Double, matrix: Map[Int, Map[Int, Double]]) = {
    val clusterer = new Clusterer(size, clusteringThreshold, matrix)
    clusterer.cluster()
  }

  def cluster(size: Int, scoringThreshold: Double, clusteringThreshold: Double)(score: (Int, Int) => Double): Seq[Seq[Int]] = {
    val matrix = buildMatrix(size, scoringThreshold)(score)
    val clusterer = new Clusterer(size, clusteringThreshold, matrix)
    clusterer.cluster()
  }

  object Cluster {
    def apply(element: Int, threshold: Double)(implicit matrix: Map[Int, Map[Int, Double]]) = {
      val members = {


        val siblings = matrix(element).filter(_._2 >= threshold).keySet

        val extendedSiblings = siblings.flatMap(matrix).filter(_._2 >= threshold).map(_._1)

        val seedCluster = extendedSiblings.filter { i =>
          val scores = siblings.toSeq.map(s => matrix(s)(i))
          scores.sum / scores.length > threshold
        }

        val cluster = seedCluster.filter { elementIndex =>
          val closeSiblings = findClosestSiblings(elementIndex, threshold).
            filter(cs => findClosestSiblings(cs, threshold).contains(elementIndex))

          closeSiblings.isEmpty || seedCluster.intersect(closeSiblings).nonEmpty
        }

        if (cluster.isEmpty) Set(element)
        else cluster
      }

      new Cluster(members.toSeq.sorted)
    }

    def prune(seedScores: Seq[(Int, Double)], threshold: Double)(implicit matrix: Map[Int, Map[Int, Double]]) = {
      Stream.iterate(seedScores) { scores =>
        computeSimilarities(scores.tail.map(_._1))
      }
        .dropWhile(p => p.nonEmpty && p.head._2 < threshold)
        .head
        .map(_._1)
        .toSet
    }

    def computeSimilarities(seq: Seq[Int])(implicit matrix: Map[Int, Map[Int, Double]]): Seq[(Int, Double)] = {
      if (seq.isEmpty) Seq()
      else {
        val pairs = for {
          i <- seq.indices.par
          j <- seq.indices.par
          if i != j
          score = matrix(seq(i))(seq(j))
        } yield (seq(i), score)
        pairs
          .groupBy(_._1)
          .mapValues(ps => ps.map(_._2).sum / ps.length)
          .toSeq
          .seq
          .sortBy(_._2)
      }
    }

    def findClosestSiblings(elementIndex: Int, threshold: Double)(implicit matrix: Map[Int, Map[Int, Double]]): Set[Int] = {
      val siblings = (matrix(elementIndex) - elementIndex).filter(_._2 >= threshold)
      if (siblings.isEmpty) Set()
      else {
        val maxScore = siblings.values.max
        siblings.filter(_._2 == maxScore).keySet
      }
    }
  }

  case class Cluster(members: Seq[Int])(implicit matrix: Map[Int, Map[Int, Double]]) {
    lazy val (centroids, quality) =
      if (members.isEmpty) (Seq(), 0d)
      else if (members.length == 1) (members, 1d)
      else {
        val scoreTriplets = for {
          i <- members.indices
          j <- i + 1 until members.size
          score = matrix(members(i))(members(j))
        } yield (members(i), members(j), score)

        val scoreMap =
          (scoreTriplets ++ scoreTriplets.map(t => (t._2, t._1, t._3))).
            groupBy(_._1).
            mapValues(s => s.map(_._3).sum / s.length)

        val maxScore = scoreMap.values.max
        val centroids = scoreMap.filter(_._2 == maxScore).keySet.toSeq

        val avgCentroidSimilarity = {
          val baseScores = for {
            centroid <- centroids
            nonCentroid <- scoreMap.keySet -- centroids
          } yield matrix(nonCentroid)(centroid)

          val centroidScores = if (baseScores.nonEmpty) baseScores else centroids.map(scoreMap)

          centroidScores.sum / centroidScores.length
        }

        (centroids.sorted, avgCentroidSimilarity)
      }

    def toString(toString: Int => String) =
      s"${centroids.map(toString).mkString(", ")} (${members.length})($quality): ${members.map(toString).mkString(", ")}"

  }

  def buildMatrix[A](size: Int, scoringThreshold: Double)(score: (Int, Int) => Double): Map[Int, Map[Int, Double]] = {
    val baseScores = for {
      i <- 0 until size
      j <- i + 1 until size
      similarity = score(i, j)
      if similarity >= scoringThreshold
    } yield (i, j, similarity)

    val allScores = {
      baseScores ++
        baseScores.map(t => (t._2, t._1, t._3)) ++
        (for (i <- 0 until size) yield (i, i, 1d))
    }

    allScores
      .groupBy(_._1)
      .mapValues(_.map(t => (t._2, t._3)).toMap.withDefaultValue(0d))
      .withDefaultValue(Map.empty.withDefaultValue(0d))
  }

}
