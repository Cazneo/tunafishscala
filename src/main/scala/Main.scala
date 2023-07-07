import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.util.Duration
import scala.collection._

abstract class Fish(val energy: Int, val breedTime: Int, val color: Color) {
  def copy(energy: Int): Fish
}

case class Tuna(override val energy: Int, tBreed: Int) extends Fish(energy, tBreed, Color.Blue) {
  def copy(energy: Int): Fish = Tuna(energy, tBreed)
}

case class Shark(override val energy: Int, sBreed: Int, var starvationTime: Int) extends Fish(energy, sBreed, Color.Red) {
  def copy(energy: Int): Fish = Shark(energy, sBreed, starvationTime)
}

object Main extends JFXApp3 {
  val totalTunas = 100
  val totalSharks = 50
  val sharkEnergy = 15
  val sharkBreed = 20
  val tunaBreed = 15
  var starvationTime = 25

  val grid = Array.fill[Option[Fish]](50, 50)(None)
  var rectangles = Array.ofDim[Rectangle](50, 50)

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title.value = "WaTor"
      scene = new Scene(500, 500) {
        content = for (i <- 0 until 50; j <- 0 until 50) yield {
          val rectangle = new Rectangle {
            x = j * 10
            y = i * 10
            width = 10
            height = 10
            fill = Color.White
          }
          rectangles(i)(j) = rectangle
          rectangle
        }
      }
    }

    for (counter <- 1 to totalTunas) {
      var placed = false
      while (!placed) {
        val i = scala.util.Random.nextInt(50)
        val j = scala.util.Random.nextInt(50)
        if (grid(i)(j).isEmpty) {
          grid(i)(j) = Some(Tuna(0, tunaBreed))
          rectangles(i)(j).fill = Color.Blue
          placed = true
        }
      }
    }

    for (counter <- 1 to totalSharks) {
      var placed = false
      while (!placed) {
        val i = scala.util.Random.nextInt(50)
        val j = scala.util.Random.nextInt(50)
        if (grid(i)(j).isEmpty) {
          grid(i)(j) = Some(Shark(sharkEnergy, sharkBreed, starvationTime))
          rectangles(i)(j).fill = Color.Red
          placed = true
        }
      }
    }

    val timeline = new Timeline {
      cycleCount = Timeline.Indefinite
      keyFrames = KeyFrame(Duration(500), onFinished = event => update())
    }

    timeline.play()
  }

  def update(): Unit = {
    val oldGrid = grid.map(array => array.clone())

    oldGrid.indices.foreach { index1 =>
      oldGrid(index1).indices.foreach { index2 =>
        oldGrid(index1)(index2) match {
          case Some(tuna: Tuna) =>
            updateTuna(tuna, index1, index2)
          case Some(shark: Shark) =>
            updateShark(shark, index1, index2)
          case None =>
        }
      }
    }
  }

  def updateTuna(tuna: Tuna, i: Int, j: Int): Unit = {
    val possibleMoves = List((i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j + 1), (i - 1, j - 1), (i + 1, j + 1), (i + 1, j - 1))
      .filter { case (x, y) => x >= 0 && x < 50 && y >= 0 && y < 50 && grid(x)(y).isEmpty }

    if (possibleMoves.nonEmpty) {
      val (newI, newJ) = possibleMoves(scala.util.Random.nextInt(possibleMoves.length))
      grid(newI)(newJ) = Some(tuna.copy(energy = tuna.energy + 1))
      rectangles(newI)(newJ).fill = Color.Blue

      if (tuna.energy >= tuna.tBreed) {
        grid(i)(j) = Some(tuna.copy(energy = 0))
        rectangles(i)(j).fill = Color.Blue
      } else {
        grid(i)(j) = None
        rectangles(i)(j).fill = Color.White
      }
    }
  }

  def updateShark(shark: Shark, i: Int, j: Int): Unit = {
    val possibleMoves = List((i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j + 1), (i - 1, j - 1), (i + 1, j + 1), (i + 1, j - 1))
      .filter { case (x, y) => x >= 0 && x < 50 && y >= 0 && y < 50 }
    val tunaMoves = possibleMoves.filter { case (x, y) => grid(x)(y).exists(_.isInstanceOf[Tuna]) }
    val emptyMoves = possibleMoves.filter { case (x, y) => grid(x)(y).isEmpty }

    if (tunaMoves.nonEmpty) {
      val (newI, newJ) = tunaMoves(scala.util.Random.nextInt(tunaMoves.length))
      grid(newI)(newJ) = Some(shark.copy(energy = shark.energy + 1, starvationTime = starvationTime))
      rectangles(newI)(newJ).fill = Color.Red
      grid(i)(j) = Some(shark.copy(energy = 0))
      rectangles(i)(j).fill = Color.Red
    } else if (emptyMoves.nonEmpty) {
      val (newI, newJ) = emptyMoves(scala.util.Random.nextInt(emptyMoves.length))
      if (shark.starvationTime <= 0) {
        grid(i)(j) = None
        rectangles(i)(j).fill = Color.White
      } else {
        grid(newI)(newJ) = Some(shark.copy(starvationTime = shark.starvationTime - 1))
        rectangles(newI)(newJ).fill = Color.Red
        grid(i)(j) = None
        rectangles(i)(j).fill = Color.White
      }
    } else if (shark.energy >= shark.sBreed) {
      grid(i)(j) = Some(shark.copy(energy = 0))
      rectangles(i)(j).fill = Color.Red
    } else if (shark.starvationTime <= 0) {
      grid(i)(j) = None
      rectangles(i)(j).fill = Color.White
    } else {
      grid(i)(j) = Some(shark.copy(starvationTime = shark.starvationTime - 1))
      rectangles(i)(j).fill = Color.Red
    }
  }
}
