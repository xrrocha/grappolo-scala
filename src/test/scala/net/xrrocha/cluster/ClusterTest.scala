package net.xrrocha.cluster

import java.io.{FileWriter, PrintWriter}

import net.xrrocha.cluster.Grappolo.Cluster
import net.xrrocha.test.BaseTest
import net.xrrocha.util.TimeUtils._

class ClusterTest extends BaseTest {

  import GrappoloMatrixTest._

  test("Generates all clusters") {
    val clusters = Grappolo.cluster(elements.size, clusteringThreshold, matrix)
    assert(elements.length == clusters.map(_.length).sum)
    clusters.sortBy(_.length).foreach { cluster =>
      println(s"${cluster.length} ${cluster.map(elements).mkString(",")}")
    }
  }

}
