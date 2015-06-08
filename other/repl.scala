import net.xrrocha.cluster.Grappolo._

import scala.io.Source

val scoringThreshold = .6
val threshold = .7

def loadElements(filename: String, count: Int) = Source.fromFile(filename).getLines().take(count).toSeq

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

val elements = loadElements("data/surnames.txt", Int.MaxValue)
val name2index = elements.indices.map(i => (elements(i), i)).toMap

implicit val matrix = loadMatrix("other/data/matrix.dat")

trait Show { def show: String }

implicit def int2show(index: Int) = new Show {
  def show = elements(index)
}

implicit def pairs2show(pairs: Iterable[(Int, Double)]) = new Show {
  def show = pairs.map { case(i, s) => (elements(i), s) }.mkString(", ")
}

implicit def ints2show(ints: Iterable[Int]) = new Show {
  def show = ints.map(elements).mkString(", ")
}

implicit def string2int(name: String) = name2index(name)

def vector(elementIndex: Int) = matrix(elementIndex).toSeq.sortBy(-_._2)

def siblings(elementIndex: Int) = matrix(elementIndex).toSeq.filter(_._2 >= threshold).sortBy(-_._2)

def extSiblings(elementIndex: Int) =
  siblings(elementIndex).map(_._1).
    flatMap(matrix(_).filter(_._2 >= threshold).keySet).
    distinct.
    map(i => (i, matrix(elementIndex)(i))).
    sortBy(-_._2)

def computeSimilarities(seq: Seq[Int]): Seq[(Int, Double)] = {
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

def prune(seedScores: Seq[(Int, Score)]) = {
  Stream.iterate(seedScores) { scores =>
    println(scores.show)
    computeSimilarities(scores.tail.map(_._1))
  }
    .dropWhile(p => p.nonEmpty && p.head._2 < threshold)
    .head
    .map(_._1)
    .toSet
}


def intraSimilarity(members: Iterable[Int]) = {
  val seq = members.toSeq
  if (seq.isEmpty) 0d
  else if (seq.length == 1) 1d
  else {
    val scores = for {
      i <- seq.indices
      j <- i + 1 until seq.length
    } yield matrix(seq(i))(seq(j))
    scores.sum / scores.length
  }
}


def xprune(members: Iterable[Int]) = {
  Stream.iterate((members, intraSimilarity(members))) { case(members, similarity) =>
    println(members.show)
    (members.tail, intraSimilarity(members.tail))
  }
    .dropWhile(p => p._1.nonEmpty && p._2 < threshold)
    .head
    ._1
    .toSet
}

def findClosestSiblings(elementIndex: Int): Set[Int] = {
  val siblings = (matrix(elementIndex) - elementIndex).filter(_._2 >= threshold)
  if (siblings.isEmpty) Set()
  else {
    val maxScore = siblings.values.max
    siblings.filter(_._2 == maxScore).keySet
  }
}

val element = name2index("lina")

val siblings = matrix(element).filter(_._2 >= threshold).keySet

val extendedSiblings = siblings.flatMap(matrix).filter(_._2 >= threshold).map(_._1)

val seedCluster = extendedSiblings.filter { i =>
    val scores = siblings.toSeq.map(s => matrix(s)(i))
    scores.sum / scores.length > threshold
  }

val intermediateCluster = seedCluster.filter { elementIndex =>
  val closeSiblings = findClosestSiblings(elementIndex).
    filter(cs => findClosestSiblings(cs).contains(elementIndex))

  closeSiblings.isEmpty || seedCluster.intersect(closeSiblings).nonEmpty
}

