import breeze.linalg.{DenseVector, euclideanDistance}
import scala.io.Source

import javax.swing.{JFrame, JPanel, WindowConstants}
import java.awt.{Color, Graphics, Graphics2D, BasicStroke}
import java.awt.BasicStroke


object Euclideananddistance:

  // ==========================================
  // DATA POINT
  // ==========================================

  case class DataPoint(
                        features: DenseVector[Double],
                        label: String
                      )


  // ==========================================
  // MAIN
  // ==========================================

  def main(args: Array[String]): Unit =

    // ==========================================
    // READ CSV
    // ==========================================

    val source = Source.fromFile("data.csv")

    val dataset =
      source
        .getLines()
        .drop(1)
        .map { line =>

          val values =
            line.split(",").map(_.trim)

          val x =
            values(1).toDouble

          val y =
            values(2).toDouble

          val label =
            values(3)

          DataPoint(
            DenseVector(x, y),
            label
          )
        }
        .toSeq

    source.close()


    // ==========================================
    // DISPLAY DATA
    // ==========================================

    println()
    println("======================================")
    println("          TRAINING DATA")
    println("======================================")

    dataset.foreach { point =>

      println(
        f"X = ${point.features(0)}%.1f   " +
          f"Y = ${point.features(1)}%.1f   " +
          s"Label = ${point.label}"
      )
    }


    // ==========================================
    // NEW POINT
    // ==========================================

    val newPoint =
      DenseVector(
        2.1,
        2.2
      )

    println()
    println("======================================")
    println("             NEW POINT")
    println("======================================")

    println(
      f"X = ${newPoint(0)}%.1f   " +
        f"Y = ${newPoint(1)}%.1f"
    )


    // ==========================================
    // 1-NN CLASSIFICATION
    // ==========================================

    var minDistance =
      Double.MaxValue

    var predictedLabel =
      ""

    var nearestPoint =
      DenseVector(0.0, 0.0)


    println()
    println("======================================")
    println("        EUCLIDEAN DISTANCES")
    println("======================================")


    for point <- dataset do

      val distance =
        euclideanDistance(
          newPoint,
          point.features
        )

      println(
        f"Point (${point.features(0)}%.1f, " +
          f"${point.features(1)}%.1f)   " +
          f"Label = ${point.label}%-6s   " +
          f"Distance = $distance%.4f"
      )


      if distance < minDistance then

        minDistance =
          distance

        predictedLabel =
          point.label

        nearestPoint =
          point.features


    // ==========================================
    // RESULT
    // ==========================================

    println()
    println("======================================")
    println("        CLASSIFICATION RESULT")
    println("======================================")

    println(
      f"Nearest Distance = $minDistance%.4f"
    )

    println(
      s"Predicted Label  = $predictedLabel"
    )

    println(
      s"Nearest Point    = $nearestPoint"
    )

    println("======================================")


    // ==========================================
    // GRAPH WINDOW
    // ==========================================

    val frame =
      new JFrame(
        "Euclidean Distance - KNN"
      )

    frame.setDefaultCloseOperation(
      WindowConstants.EXIT_ON_CLOSE
    )

    frame.setSize(
      900,
      700
    )

    frame.add(
      new KNNGraphPanel(
        dataset,
        newPoint,
        nearestPoint,
        predictedLabel,
        minDistance
      )
    )

    frame.setLocationRelativeTo(null)

    frame.setVisible(true)



// ==================================================
// GRAPH PANEL
// ==================================================

class KNNGraphPanel(
                     dataset: Seq[Euclideananddistance.DataPoint],
                     newPoint: DenseVector[Double],
                     nearestPoint: DenseVector[Double],
                     predictedLabel: String,
                     minDistance: Double
                   ) extends JPanel:


  override def paintComponent(
                               graphics: Graphics
                             ): Unit =

    super.paintComponent(graphics)

    val g =
      graphics.asInstanceOf[Graphics2D]


    // ==========================================
    // GRAPH AREA
    // ==========================================

    val left =
      90

    val right =
      getWidth - 70

    val top =
      90

    val bottom =
      getHeight - 100


    // ==========================================
    // TITLE
    // ==========================================

    g.setColor(Color.BLACK)

    g.drawString(
      "KNN - Euclidean Distance Classification",
      300,
      35
    )


    g.drawString(
      "Training Points and New Point",
      330,
      55
    )


    // ==========================================
    // SCALE
    // ==========================================

    val minX =
      0.0

    val maxX =
      10.0

    val minY =
      0.0

    val maxY =
      10.0


    def screenX(
                 x: Double
               ): Int =

      (
        left +
          ((x - minX) /
            (maxX - minX)) *
            (right - left)
        ).toInt


    def screenY(
                 y: Double
               ): Int =

      (
        bottom -
          ((y - minY) /
            (maxY - minY)) *
            (bottom - top)
        ).toInt


    // ==========================================
    // GRID
    // ==========================================

    g.setColor(
      new Color(
        225,
        225,
        225
      )
    )


    for i <- 0 to 10 do

      val x =
        screenX(i.toDouble)

      val y =
        screenY(i.toDouble)


      // Vertical grid

      g.drawLine(
        x,
        top,
        x,
        bottom
      )


      // Horizontal grid

      g.drawLine(
        left,
        y,
        right,
        y
      )


    // ==========================================
    // AXIS
    // ==========================================

    g.setColor(Color.BLACK)

    g.setStroke(
      new BasicStroke(2)
    )


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


    // ==========================================
    // AXIS NUMBERS
    // ==========================================

    for i <- 0 to 10 do

      val x =
        screenX(i.toDouble)

      val y =
        screenY(i.toDouble)


      g.drawString(
        i.toString,
        x - 4,
        bottom + 22
      )


      g.drawString(
        i.toString,
        left - 25,
        y + 5
      )


    // ==========================================
    // AXIS LABELS
    // ==========================================

    g.drawString(
      "X Feature",
      (left + right) / 2,
      bottom + 55
    )


    g.drawString(
      "Y Feature",
      20,
      top - 20
    )


    // ==========================================
    // CLASS 1 - RED
    // ==========================================

    dataset
      .filter(
        _.label == "Class1"
      )
      .foreach { point =>

        val x =
          screenX(
            point.features(0)
          )

        val y =
          screenY(
            point.features(1)
          )


        g.setColor(
          new Color(
            220,
            50,
            50
          )
        )


        g.fillOval(
          x - 8,
          y - 8,
          16,
          16
        )


        // Point label

        g.setColor(Color.BLACK)

        g.drawString(
          "Class1",
          x + 10,
          y
        )
      }


    // ==========================================
    // CLASS 2 - BLUE
    // ==========================================

    dataset
      .filter(
        _.label == "Class2"
      )
      .foreach { point =>

        val x =
          screenX(
            point.features(0)
          )

        val y =
          screenY(
            point.features(1)
          )


        g.setColor(
          new Color(
            50,
            80,
            220
          )
        )


        g.fillOval(
          x - 8,
          y - 8,
          16,
          16
        )


        // Point label

        g.setColor(Color.BLACK)

        g.drawString(
          "Class2",
          x + 10,
          y
        )
      }


    // ==========================================
    // LINE TO NEAREST NEIGHBOR
    // ==========================================

    val newX =
      screenX(
        newPoint(0)
      )

    val newY =
      screenY(
        newPoint(1)
      )


    val nearestX =
      screenX(
        nearestPoint(0)
      )

    val nearestY =
      screenY(
        nearestPoint(1)
      )


    // Dashed green line

    g.setColor(
      new Color(
        0,
        150,
        0
      )
    )


    g.setStroke(
      new BasicStroke(
        2,
        BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_BEVEL,
        0,
        Array(8.0f, 8.0f),
        0
      )
    )


    g.drawLine(
      newX,
      newY,
      nearestX,
      nearestY
    )


    // ==========================================
    // NEW POINT - GREEN STAR
    // ==========================================

    g.setColor(
      new Color(
        0,
        180,
        0
      )
    )


    g.setStroke(
      new BasicStroke(4)
    )


    // Horizontal

    g.drawLine(
      newX - 14,
      newY,
      newX + 14,
      newY
    )


    // Vertical

    g.drawLine(
      newX,
      newY - 14,
      newX,
      newY + 14
    )


    // Diagonal

    g.drawLine(
      newX - 10,
      newY - 10,
      newX + 10,
      newY + 10
    )


    g.drawLine(
      newX - 10,
      newY + 10,
      newX + 10,
      newY - 10
    )


    // New point text

    g.setColor(Color.BLACK)

    g.drawString(
      "NEW POINT (2.1, 2.2)",
      newX + 15,
      newY - 15
    )


    // ==========================================
    // LEGEND
    // ==========================================

    val legendX =
      right - 170

    val legendY =
      top + 20


    // Class1

    g.setColor(
      new Color(
        220,
        50,
        50
      )
    )

    g.fillOval(
      legendX,
      legendY,
      14,
      14
    )

    g.setColor(Color.BLACK)

    g.drawString(
      "Class1",
      legendX + 22,
      legendY + 12
    )


    // Class2

    g.setColor(
      new Color(
        50,
        80,
        220
      )
    )

    g.fillOval(
      legendX,
      legendY + 30,
      14,
      14
    )

    g.setColor(Color.BLACK)

    g.drawString(
      "Class2",
      legendX + 22,
      legendY + 42
    )


    // New point

    g.setColor(
      new Color(
        0,
        180,
        0
      )
    )

    g.fillOval(
      legendX,
      legendY + 60,
      14,
      14
    )

    g.setColor(Color.BLACK)

    g.drawString(
      "New Point",
      legendX + 22,
      legendY + 72
    )


    // ==========================================
    // PREDICTION
    // ==========================================

    g.setColor(Color.BLACK)

    g.drawString(
      s"Prediction: $predictedLabel",
      legendX,
      legendY + 110
    )


    g.drawString( 
      f"Distance: $minDistance%.4f",
      legendX,
      legendY + 130
    )