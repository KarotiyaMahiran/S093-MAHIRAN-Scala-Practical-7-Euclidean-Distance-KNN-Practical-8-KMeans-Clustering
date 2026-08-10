import breeze.linalg.DenseVector
import scala.io.Source

import javax.swing.{JFrame, JPanel, WindowConstants}
import java.awt.{Color, Graphics, Graphics2D}

object KMeansExample:

  case class Point(
                    id: Int,
                    x: Double,
                    y: Double
                  )

  // Euclidean Distance
  def distance(
                p1: Point,
                cx: Double,
                cy: Double
              ): Double =
    math.sqrt(
      math.pow(p1.x - cx, 2) +
        math.pow(p1.y - cy, 2)
    )

  def main(args: Array[String]): Unit =

    // =========================
    // READ CSV
    // =========================

    val source = Source.fromFile("data.csv")

    val data =
      source
        .getLines()
        .drop(1)
        .map { line =>
          val v = line.split(",")

          Point(
            v(0).toInt,
            v(1).toDouble,
            v(2).toDouble
          )
        }
        .toSeq

    source.close()

    println("K-MEANS CLUSTERING")
    println("==================")

    data.foreach(println)

    // =========================
    // INITIAL CENTROIDS
    // =========================

    var c1x = data.head.x
    var c1y = data.head.y

    var c2x = data(10).x
    var c2y = data(10).y

    var changed = true
    var iteration = 0

    // =========================
    // K-MEANS
    // =========================

    while changed && iteration < 100 do

      iteration += 1

      val cluster1 =
        data.filter { p =>
          distance(p, c1x, c1y) <=
            distance(p, c2x, c2y)
        }

      val cluster2 =
        data.filter { p =>
          distance(p, c2x, c2y) <
            distance(p, c1x, c1y)
        }

      val newC1x =
        cluster1.map(_.x).sum / cluster1.size

      val newC1y =
        cluster1.map(_.y).sum / cluster1.size

      val newC2x =
        cluster2.map(_.x).sum / cluster2.size

      val newC2y =
        cluster2.map(_.y).sum / cluster2.size

      changed =
        math.abs(c1x - newC1x) > 0.001 ||
          math.abs(c1y - newC1y) > 0.001 ||
          math.abs(c2x - newC2x) > 0.001 ||
          math.abs(c2y - newC2y) > 0.001

      c1x = newC1x
      c1y = newC1y

      c2x = newC2x
      c2y = newC2y

      println()
      println(s"Iteration $iteration")
      println(f"Centroid 1: ($c1x%.2f, $c1y%.2f)")
      println(f"Centroid 2: ($c2x%.2f, $c2y%.2f)")

    // =========================
    // FINAL CLUSTERS
    // =========================

    val cluster1 =
      data.filter { p =>
        distance(p, c1x, c1y) <=
          distance(p, c2x, c2y)
      }

    val cluster2 =
      data.filter { p =>
        distance(p, c2x, c2y) <
          distance(p, c1x, c1y)
      }

    println()
    println("========================")
    println("FINAL RESULT")
    println("========================")

    println(f"Centroid 1 = ($c1x%.2f, $c1y%.2f)")
    println(f"Centroid 2 = ($c2x%.2f, $c2y%.2f)")

    println()
    println("Cluster 1:")

    cluster1.foreach { p =>
      println(s"ID ${p.id}: (${p.x}, ${p.y})")
    }

    println()
    println("Cluster 2:")

    cluster2.foreach { p =>
      println(s"ID ${p.id}: (${p.x}, ${p.y})")
    }

    // =========================
    // GRAPH
    // =========================

    val frame =
      new JFrame("K-Means Clustering")

    frame.setDefaultCloseOperation(
      WindowConstants.EXIT_ON_CLOSE
    )

    frame.setSize(800, 600)

    frame.add(
      new GraphPanel(
        cluster1,
        cluster2,
        c1x,
        c1y,
        c2x,
        c2y
      )
    )

    frame.setLocationRelativeTo(null)
    frame.setVisible(true)


class GraphPanel(
                  cluster1: Seq[KMeansExample.Point],
                  cluster2: Seq[KMeansExample.Point],
                  c1x: Double,
                  c1y: Double,
                  c2x: Double,
                  c2y: Double
                ) extends JPanel:

  override def paintComponent(
                               g0: Graphics
                             ): Unit =

    super.paintComponent(g0)

    val g =
      g0.asInstanceOf[Graphics2D]

    val left = 70
    val right = getWidth - 50
    val top = 60
    val bottom = getHeight - 70

    def sx(x: Double): Int =
      (left + x / 10.0 * (right - left)).toInt

    def sy(y: Double): Int =
      (bottom - y / 10.0 * (bottom - top)).toInt

    // Grid

    g.setColor(
      new Color(230, 230, 230)
    )

    for i <- 0 to 10 do

      g.drawLine(
        sx(i),
        top,
        sx(i),
        bottom
      )

      g.drawLine(
        left,
        sy(i),
        right,
        sy(i)
      )

    // Axes

    g.setColor(Color.BLACK)

    g.drawLine(
      left,
      bottom,
      right,
      bottom
    )

    g.drawLine(
      left,
      top,
      left,
      bottom
    )

    // Title

    g.drawString(
      "K-Means Clustering (K = 2)",
      300,
      30
    )

    // Cluster 1 - RED

    g.setColor(Color.RED)

    cluster1.foreach { p =>

      val x = sx(p.x)
      val y = sy(p.y)

      g.fillOval(
        x - 6,
        y - 6,
        12,
        12
      )

      g.setColor(Color.BLACK)

      g.drawString(
        p.id.toString,
        x + 8,
        y
      )

      g.setColor(Color.RED)
    }

    // Cluster 2 - BLUE

    g.setColor(Color.BLUE)

    cluster2.foreach { p =>

      val x = sx(p.x)
      val y = sy(p.y)

      g.fillOval(
        x - 6,
        y - 6,
        12,
        12
      )

      g.setColor(Color.BLACK)

      g.drawString(
        p.id.toString,
        x + 8,
        y
      )

      g.setColor(Color.BLUE)
    }

    // Centroid 1

    g.setColor(Color.GREEN.darker)

    val x1 = sx(c1x)
    val y1 = sy(c1y)

    g.fillRect(
      x1 - 8,
      y1 - 8,
      16,
      16
    )

    g.setColor(Color.BLACK)

    g.drawString(
      "C1",
      x1 + 12,
      y1
    )

    // Centroid 2

    g.setColor(Color.GREEN.darker)

    val x2 = sx(c2x)
    val y2 = sy(c2y)

    g.fillRect(
      x2 - 8,
      y2 - 8,
      16,
      16
    )

    g.setColor(Color.BLACK)

    g.drawString(
      "C2",
      x2 + 12,
      y2
    )

    // Axis labels

    g.drawString(
      "Feature X",
      380,
      bottom + 40
    )

    g.drawString(
      "Feature Y",
      15,
      70
    )