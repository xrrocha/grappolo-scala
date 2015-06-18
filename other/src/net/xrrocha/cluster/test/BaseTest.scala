package net.xrrocha.test

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.FunSuite

abstract class BaseTest extends FunSuite with LazyLogging

object BaseTest {
  def load(filename: String, count: Int = Int.MaxValue): Seq[String] =
    io.Source.fromFile(filename).getLines().take(count).toSeq
}
