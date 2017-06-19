package net.xrrocha.grappolo

import java.io.{File, FileOutputStream}

import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartUtilities}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

trait Plotter {
  val Legend = true
  val Tooltips = true
  val Urls = false

  def topTitle: String

  def xTitle: String

  def yTitle: String

  def width = 800

  def height = 800

}

case class XYSeriesConf(topTitle: String, xTitle: String, yTitle: String, width: Int = 500, height: Int = 500)

trait XYSeriesPlotter extends Plotter {
  def plot(filename: String, vectors: Seq[Array[Double]]): Unit = {

    val dataset = new XYSeriesCollection

    val series = new XYSeries(seriesTitle(vectors))
    dataset.addSeries(series)
    vectors.foreach { vector =>
      series.add(vector(0), vector(1))
    }

    val chart = ChartFactory.createXYLineChart(
      topTitle, xTitle, yTitle,
      dataset, PlotOrientation.VERTICAL,
      true, true, false)

    val outputFile = new File(filename)
    val outputStream = new FileOutputStream(outputFile)
    ChartUtilities.writeChartAsPNG(outputStream, chart, width, height)
    outputStream.flush()
    outputStream.close()
  }

  def seriesTitle(vectors: Seq[Array[Double]]) = s"${vectors.length} vectors"
}

trait ClusterPlotter extends Plotter {
  def plot(filename: String, clusters: Seq[Seq[Int]], exclusions: Set[Int] = Set.empty)(extractData: Int => (Double, Double)): File = {
    val dataset = new XYSeriesCollection
    clusters.zipWithIndex.
      filterNot(p => exclusions.contains(p._2)).
      foreach { case (cluster, index) =>
        val series = new XYSeries(clusterLabel(cluster, index))
        dataset.addSeries(series)

        cluster.foreach { member =>
          val (x, y) = extractData(member)
          series.add(x, y)
        }
      }

    val chart = ChartFactory.createScatterPlot(
      topTitle, xTitle, yTitle,
      dataset, PlotOrientation.VERTICAL,
      Legend, Tooltips, Urls)

    val outputFile = new File(filename)
    outputFile.getParentFile.mkdirs()

    val outputStream = new FileOutputStream(outputFile)

    ChartUtilities.writeChartAsPNG(outputStream, chart, width, height)

    outputStream.flush()
    outputStream.close()

    //java.awt.Desktop.getDesktop.open(outputFile)

    outputFile
  }

  def clusterLabel(cluster: Seq[Int], index: Int) = s"Cluster #$index (${cluster.length})"
}
