package net.xrrocha.util

object TimeUtils {
  def time[A](action: => A): (A, Int) = {
    val startTime = System.currentTimeMillis()
    val result = action
    val endTime = System.currentTimeMillis()
    (result, (endTime - startTime).toInt)
  }
  val timeParts = {
    val timeParts = Seq(("millisecond", 1), ("second", 1000), ("minute", 60), ("hour", 60), ("day", 24))
    val (_, triplets) = timeParts.foldLeft(1, Seq[(String, Int, Int)]()) { (accum, pair) =>
      val (accumFactor, triplets) = accum
      val (name, factor) = pair
      (accumFactor * factor, triplets :+(name, factor, accumFactor * factor))
    }
    triplets.reverse
  }

  def sayTimeInMillis(millis: Int) = {
    val (remainder, pairs) = timeParts.foldLeft(millis, Seq[(String, Int)]()) { case (accum, triplet) =>
      val (millis, pairs) = accum
      val (name, factor, accumFactor) = triplet

      val count = millis / accumFactor
      val remainder = millis % accumFactor

      (remainder, pairs :+ (name, count))
    }

    val parts = pairs.filter(_._2 != 0)

    def toString(pair: (String, Int)) = {
      val (name, count) = pair
      s"$count ${s"$name${if (count > 1) "s" else ""}"}"
    }

    if (parts.length == 1) toString(parts.head)
    else {
      s"${parts.init.map(toString).mkString(", ")} and ${toString(parts.last)}"
    }
  }
}
