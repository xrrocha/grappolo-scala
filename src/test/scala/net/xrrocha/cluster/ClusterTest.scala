package net.xrrocha.cluster

import java.io.{FileWriter, PrintWriter}

import net.xrrocha.cluster.Grappolo.Cluster
import net.xrrocha.test.BaseTest
import net.xrrocha.util.TimeUtils._

class ClusterTest extends BaseTest {

  import GrappoloMatrixTest._

  test("Generates all clusters") {
    val votesOut = new PrintWriter(new FileWriter("other/data/cluster-votes.dat"), true)
    val (clusters, clusteringTime) = time {
      matrix.par
        .map { case (element, vector) =>
          val cluster = Cluster(element, clusteringThreshold)
          votesOut.println(s"${elements(element)}\t${cluster.toString(elements)}")
          cluster
        }.
        seq.
        groupBy(_.members).
        toSeq.
        map { case (_, clusters) => (clusters.head, clusters.size) }.
        sortWith { (c1, c2) =>
          val (cluster1, votes1) = c1
          val (cluster2, votes2) = c2

          (votes2 < votes1) || {
            (votes2 == votes1) &&
              (cluster2.quality < cluster1.quality) || {
              (cluster2.quality == cluster1.quality) && {
                cluster2.members.size < cluster1.members.size
              }
            }
          }
        }
      }
    logger.info(s"Clustering time: ${sayTimeInMillis(clusteringTime)}")

    val out = new PrintWriter(new FileWriter("other/data/cluster-candidates.dat"), true)
    clusters.foreach { case (cluster, count) =>
      val fields = Seq(
        count,
        cluster.quality,
        cluster.members.size,
        s"(${cluster.centroids.map(elements).mkString(",")})",
        cluster.members.map(elements).mkString(",")
      )
      out.println(fields.mkString("\t"))
    }
  }

}
