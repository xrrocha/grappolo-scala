package net.xrrocha.cluster

import java.io.{FileWriter, PrintWriter}

import net.xrrocha.cluster.Clusterer.Cluster
import net.xrrocha.test.BaseTest
import net.xrrocha.util.TimeUtils._

class ClusterTest extends BaseTest {

  import ClustererMatrixTest._

  test("Generates all clusters") {
    val clusters = Clusterer.cluster(elements.size, clusteringThreshold, matrix)
    assert(elements.length == clusters.map(_.length).sum)
    clusters.sortBy(_.length).foreach { cluster =>
      println(s"${cluster.length} ${cluster.map(elements).mkString(",")}")
    }
  }

}
