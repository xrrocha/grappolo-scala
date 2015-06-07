package net.xrrocha.cluster

import java.io._

object Matrix {
  def apply(size: Int, scorer: (Int, Int) => Double)(implicit toString: Int => String = _.toString) = {

    val scores = for {
      i <- 0 until size
      j <- i + 1 until size
      score = scorer(i, j)
    } yield (i, j, score)

    val map = {
      scores ++
        scores.map(t => (t._2, t._1, t._3)) ++
        (for (i <- 0 until size) yield (i, i, 1d))
    }
      .groupBy(_._1)
      .mapValues(_.map(t => (t._2, t._3)).toMap)

    new Matrix(map)
  }

  def cosineSimilarity(left: Map[Int, Double], right: Map[Int, Double]): Double = {

    def dotProduct(left: Map[Int, Double], right: Map[Int, Double]): Double =
      left.keySet.union(right.keySet).map(i => left.getOrElse(i, 0d) * right.getOrElse(i, 0d)).sum

    def magnitude(vector: Map[Int, Double]): Double = math.sqrt(vector.values.map(v => v * v).sum)

    dotProduct(left, right) / (magnitude(left) * magnitude(right))
  }
}

class Matrix(map: Map[Int, Map[Int, Double]]) {
  lazy val isEmpty = map.isEmpty
  lazy val nonEmpty = map.nonEmpty

  def apply(index: Int): Map[Int, Double] = {
    map.getOrElse(index, Map())
  }

  def --(indices: Set[Int]): Matrix = new Matrix((map -- indices).mapValues(_ -- indices))

  def bestCluster(minSimilarity: Double)(implicit toString: Int => String = _.toString): (Set[Int], Double) = {
    def simScore(set: Set[Int]) = if (set.size == 1) 0d else intraSimilarity(set)

    val startIndex = map.keySet.head
    val startCluster = buildCluster(startIndex, minSimilarity)
    val startIntraSimilarity = simScore(startCluster)

    map.keySet.tail.foldLeft(startCluster, startIntraSimilarity) { (accum, currentIndex) =>
      val (_, previousIntraSimilarity) = accum
      val currentCluster = buildCluster(currentIndex, minSimilarity)
      val currentIntraSimilarity = simScore(currentCluster)
      if (currentIntraSimilarity > previousIntraSimilarity) (currentCluster, currentIntraSimilarity)
      else accum
    }
  }

  def intraSimilarity(set: Set[Int]): Double = intraSimilarity(set.toSeq)

  def intraSimilarity(seq: Seq[Int]): Double = {
    if (seq.size == 1) 1d
    else {
      val scores = for {
        i <- seq
        j <- seq
        if i != j
        score = map.getOrElse(i, Map()).getOrElse(j, 0d)
      } yield score
      scores.sum / scores.length
    }
  }

  def buildCluster(elementIndex: Int, minSimilarity: Double)(implicit toString: Int => String = _.toString): Set[Int] = {

    val siblings = map(elementIndex).filter(_._2 >= minSimilarity).keySet

    if (siblings.size == 1) siblings
    else {
      val seedCluster = siblings.flatMap(map(_).filter(_._2 >= minSimilarity).keySet)
      val seedScores = scoresFor(seedCluster.toSeq)

      val initialCluster = Stream.iterate(seedScores) { scores =>
        scoresFor(scores.tail.map(_._1))
      }
        .dropWhile(p => p.head._2 < minSimilarity)
        .head
        .map(_._1)
        .toSet

      // TODO Re-prune cluster after removing members whose most similar siblings are not in cluster
      val cluster = initialCluster.filter { elementIndex =>
        val closeSiblings = findClosestSiblings(elementIndex, minSimilarity).
          filter(cs => findClosestSiblings(cs, minSimilarity).contains(elementIndex))

        closeSiblings.isEmpty || initialCluster.intersect(closeSiblings).nonEmpty
      }

      cluster
    }
  }

  def findClosestSiblings(elementIndex: Int, minSimilarity: Double): Set[Int] = {
    val siblings = (map(elementIndex) - elementIndex).filter(_._2 >= minSimilarity)
    if (siblings.isEmpty) Set()
    else {
      val maxScore = siblings.values.max
      siblings.filter(_._2 == maxScore).keySet
    }
  }

  def scoresFor(seq: Seq[Int]): Seq[(Int, Double)] = {
    if (seq.isEmpty) Seq()
    else {
      val pairs = for {
        i <- seq.indices.par
        j <- seq.indices.par
        if i != j
        score = apply(seq(i), seq(j))
      } yield (seq(i), score)
      pairs
        .groupBy(_._1)
        .mapValues(ps => ps.map(_._2).sum / ps.length)
        .toSeq
        .seq
        .sortBy(_._2)
    }
  }

  def apply(i: Int, j: Int) = {
    map.getOrElse(i, Map()).getOrElse(j, 0d)
  }

  def dump(out: OutputStream, toString: Int => String): Unit = {
    dump(new PrintWriter(new OutputStreamWriter(out), true), toString)
  }

  def dump(out: PrintWriter, toString: Int => String): Unit = {
    val size = map.keySet.union(map.flatMap(_._2.keySet).toSet).max
    out.println(s"\t${(0 until size).map(toString).mkString("\t")}")
    for (i <- 0 until size) {
      out.println(s"${toString(i)}\t${(0 until size).map(map(i)).mkString("\t")}")
    }
  }
}
