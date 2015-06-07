package net.xrrocha.cluster

import com.typesafe.scalalogging.LazyLogging

object Grappolo extends LazyLogging {
  type Score = Double
  type Vector = Map[Int, Score]
  type Matrix = Map[Int, Vector]

  def cluster(size: Int, scoringThreshold: Score, clusteringThreshold: Score)(score: (Int, Int) => Score): Seq[Seq[Int]] = {
    val matrix = buildMatrix(size, scoringThreshold)(score)

    ???
  }

  object Cluster {
    def apply(element: Int, threshold: Score)(implicit matrix: Matrix) = {
      val members = {

        val siblings = matrix(element).filter(_._2 >= threshold).keySet

        if (siblings.size == 1) siblings
        else {
          val seedCluster = siblings ++ siblings.flatMap(findClosestSiblings(_, threshold))

          val seedSimilarities = computeSimilarities(seedCluster.toSeq)

          val initialCluster = prune(seedSimilarities, threshold)

          val intermediateCluster = initialCluster.filter { elementIndex =>
            val closeSiblings = findClosestSiblings(elementIndex, threshold).
              filter(cs => findClosestSiblings(cs, threshold).contains(elementIndex))

            closeSiblings.isEmpty || initialCluster.intersect(closeSiblings).nonEmpty
          }

          val cluster = prune(computeSimilarities(intermediateCluster.toSeq), threshold)

          cluster
        }
      }

      new Cluster(members.toSeq.sorted)
    }

    def prune(seedScores: Seq[(Int, Score)], threshold: Score)(implicit matrix: Matrix) = {
      Stream.iterate(seedScores) { scores =>
        computeSimilarities(scores.tail.map(_._1))
      }
        .dropWhile(p => p.nonEmpty && p.head._2 < threshold)
        .head
        .map(_._1)
        .toSet
    }

    def computeSimilarities(seq: Seq[Int])(implicit matrix: Matrix): Seq[(Int, Double)] = {
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

    def findClosestSiblings(elementIndex: Int, threshold: Double)(implicit matrix: Matrix): Set[Int] = {
      val siblings = (matrix(elementIndex) - elementIndex).filter(_._2 >= threshold)
      if (siblings.isEmpty) Set()
      else {
        val maxScore = siblings.values.max
        siblings.filter(_._2 == maxScore).keySet
      }
    }
  }

  case class Cluster(members: Seq[Int])(implicit matrix: Matrix) {
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

  def buildMatrix[A](size: Int, scoringThreshold: Score)(score: (Int, Int) => Score): Matrix = {
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
