package o1.TETRIS

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import java.util.concurrent._
import scala.math._


class LetterCanvas(private val board: Board, size: Int) extends TextArea {
  this.editable = false
  focusable = false
  this.font = new Font("Arial", 0, size)
  this.background = new Color(220, 230, 240)
  this.border = Swing.LineBorder(new Color(10,10,0), size / 10)
  
  private def modify(grid: Array[Array[Int]], between: String, block: String): String = {
    val textArray: Array[Array[String]] = Array.ofDim[String](board.h, board.w)
    
      for (i <- 0 until textArray.size) {
        for (k <- 0 until textArray(0).size) {
          if  (grid(i)(k) % 2 == 0) {
            textArray(i)(k) = "  "
          }
          else textArray(i)(k) = block
        }
      }
    textArray.map(_.mkString(between)).mkString("\n")
  }
  def updateCanvas() = {
    this.text = modify(board.gameGrid, "|", "[]")
  }
  def pauseCanvas() = {
    this.text = modify(board.gameGrid, " ", "  ")
  }
  updateCanvas()
}

object Game extends SimpleSwingApplication {

  
  // Components:
  //Main panel
  protected val panel = new BoxPanel(Orientation.Vertical)
 
  //Top panel
  var topPanel: BoxPanel = null
  var nimike: Label = null
  var incSize: Button = null
  var decSize: Button = null
  def formTopPanel() = {
    nimike = new Label {
    text = "  ( ͡° ͜ʖ ͡°) <(Hey)  "
    background = new Color(10, 100, 100)
    focusable = true
  }
   incSize = new Button("+")
  incSize.focusable = false
   decSize = new Button("-")
  decSize.focusable = false
  topPanel = new BoxPanel(Orientation.Horizontal)
  topPanel.contents += incSize
  topPanel.contents += nimike
  topPanel.contents += decSize
  }
  formTopPanel()
  //Bottom panel
  var inputText: TextArea = null
  var quitButton: Button = null
  def formBottomPanel() = {
  inputText = new TextArea("Here we go!", 4, 1)
  inputText.editable = false
  inputText.border = Swing.LineBorder(new Color(10,10,0), 2)
  inputText.background = new Color(150, 200, 230)
  quitButton = Button("Quit") { sys.exit(0) }

  inputText.lineWrap = true
  inputText.wordWrap = true
  }
  formBottomPanel()
  // Game setup!
  private var canvasSize = 20
  private val board = new Board(19, 10)
  private var canvas = new LetterCanvas(board, canvasSize)
  private val session = new Tetris(board)
  
  def updateView(sana: Option[String]) = {
    if (!isLost) {
      inputText.text = sana.getOrElse(inputText.text)
    } else inputText.text = "Game over. Your score is " + score.toString + "\nWrite it down, as I don't do that!"
    
    canvas.updateCanvas()
  }
  def pauseView(sana: Option[String]) = {
    if (!isLost) {
      inputText.text = sana.getOrElse(inputText.text)
    } else inputText.text = "Game over. Your score is " + score.toString + "\nWrite it down, as I don't do that!"
    
    canvas.pauseCanvas()    
  }
  
  protected val buttonPanel = new BoxPanel(Orientation.Horizontal)
  val restart = new Button("Restart")
  restart.focusable = false
  buttonPanel.contents += quitButton
  buttonPanel.contents += restart
  
  // Layout: 
  def formPanel() = {
    panel.contents.clear()
    panel.contents += topPanel
    panel.contents += canvas
    panel.contents += inputText
    panel.contents += buttonPanel
    panel.border = Swing.EmptyBorder(10, 30, 10, 30)
  }
  formPanel()
  val gameWindow = new MainFrame
  gameWindow.title = "DOOMSDAY TETRIS"
  gameWindow.resizable = false
  gameWindow.contents = panel
  

  def top = this.gameWindow
  
  //Toiminta
  def listen() = {
  listenTo(panel.keys)
  this.listenTo(restart)
  this.listenTo(incSize)
  this.listenTo(decSize)
  }
  listen()
  panel.focusable = true
  /*this.reactions += {
    case KeyPressed(_, c, _, _) =>
      if (c == Key.Up)  {
        session.rotate()
      }
      if (c == Key.Down) {
        session.dropOne()
      }
      if (c == Key.Left) {
        session.left()
      }
      if (c == Key.Right) {
        session.right()
      }
      if (c == Key.Space) {
        session.wholeDrop()
      }
      bonusScore = session.bonusScore
      updateView(Some(c.toString + "\n" + score))
  }*/
  // To counter multiple schedules of same instance
  private var spinEngaged = false
  private var fastEngaged = false
  private var leftEngaged = false
  private var rightEngaged = false
  private var spaceHeld = false
  private var pause = false
  private var gameStarted = false
  // Better reactions
  this.reactions += {
    case action: ButtonClicked => 
      if (action.source == restart) {
      haltAll()
      session.reset()
      time = 700
      level = 1
      dropScore = 0
      isLost = false
      pause = false
      continue()
      updateView(Some("Here we go, again!"))

      panel.requestFocus()
      listenTo(panel.keys)
    }
      if (action.source == incSize) {
        canvasSize += 2
        canvas = new LetterCanvas(board, canvasSize)
        formPanel()
        gameWindow.contents = panel
        if (pause) {
        pauseView(Some("Better?"))
        } else updateView(Some("Better?"))
        panel.requestFocus
      }
      if (action.source == decSize) {
        canvasSize -= 2
        canvas = new LetterCanvas(board, canvasSize)
        formTopPanel()
        formBottomPanel()
        formPanel()
        gameWindow.size = new Dimension(1, 1)
        gameWindow.contents = panel
        listen()
        if (pause) {
        pauseView(Some("Better?"))
        } else updateView(Some("Better?"))
        panel.requestFocus
      }
    case KeyPressed(_, c, _, _) =>
      if (!gameStarted) {
        continue()
        gameStarted = true
      }
      if (c == Key.Up && !pause)  {
        if(!spinEngaged && !spaceHeld) {
          spinThing = ex.scheduleAtFixedRate(spinTask, 0, 6 * quickTime, TimeUnit.MILLISECONDS) 
          spinEngaged = true
        }
      }
      if (c == Key.Down && !pause) {
        if (!spaceHeld) {
        haltDrop()
          if(!fastEngaged) {
            fastEngaged = true
            fastDrop = ex.scheduleAtFixedRate(dropTask, 0, min(quickTime, time), TimeUnit.MILLISECONDS)
          }
        }
      }
      if (c == Key.Left && !pause) {
        if (!leftEngaged && !spaceHeld) {
          session.left()
          updateView(Some("\n" + score.toString))
          leftThing = ex.scheduleAtFixedRate(leftTask, 250, (1 * quickTime).toInt, TimeUnit.MILLISECONDS)
          leftEngaged = true
        }
        
      }
      if (c == Key.Right && !pause) {
        if (!rightEngaged && !spaceHeld) {
          session.right()
          updateView(Some("\n" + score.toString))
          rightThing = ex.scheduleAtFixedRate(rightTask, 250, (1 * quickTime).toInt, TimeUnit.MILLISECONDS)
          rightEngaged = true
        }
      }
      if (c == Key.Space && !pause) {
        spaceHeld = true
        haltAll()
        session.wholeDrop()
        bonusScore = session.bonusScore
        updateView(Some("Whoa!\n" + score))
      }
      if (c == Key.P) {
        if (!pause) {
        haltAll()
        spaceHeld = false
        pause = true
        pauseView(Some("Paused\n" + score))
        }
        else {
          session.dropOne()
          continue()
          pause = false
          updateView(Some("Resumed\n" + score))
        }
      }

    case KeyReleased(_, c, _, _) => 
      if (c == Key.Up && spinEngaged) {
        haltSpin()
        spinEngaged = false
      }
      if (c == Key.Down && fastEngaged) {
        if (!spaceHeld) {
        haltFast()
        fastEngaged = false
        continue()
        }
      }
      if (c == Key.Left && leftEngaged) {
        leftEngaged = false
        haltLeft()
      }
      if (c == Key.Right && rightEngaged) {
        rightEngaged = false
        haltRight()
      }
      if (c == Key.Space && spaceHeld) {
        spaceHeld = false
        continue()
      }
  }
  
  // Pisteytys
    def score: Long = bonusScore
    private var dropScore: Long = 0
    private var bonusScore: Long = 0
    private var isLost = false
    private var level: Long = 1
    
       //LETS GO
      session.start
      updateView(Some("The controls are up, down, left, right, space and p\nPress anything to start"))
    
  // Ajastettu toiminta
    def haltAll(): Unit = {
      haltDrop()
      haltFast()
      haltSpin()
      haltLeft()
      haltRight()
      spinEngaged = false
      fastEngaged = false
      leftEngaged = false
      rightEngaged = false
      
    }
    def haltDrop(): Unit = {
      normalDrop.cancel(false)
      if (isLost) {
        updateView(Some("Losted!\n" + score.toString ))
      }
    }
    def haltFast(): Unit = {
      fastDrop.cancel(false)
      if (isLost) {
        updateView(Some("Losted!\n" + score.toString ))
      }
    }
    def haltSpin(): Unit = {
      spinThing.cancel(false)
    }
    def haltLeft(): Unit = {
      leftThing.cancel(true)
    }
    def haltRight(): Unit = {
      rightThing.cancel(true)
    }
    def continue() = {
      if (!isLost) {
        haltDrop()
        normalDrop = ex.scheduleAtFixedRate(dropTask, time, time, TimeUnit.MILLISECONDS)
      }
    }
    // Includes the parameter for how fast the speed grows!
    def faster(): Unit = {
      time = (time * 0.88).toInt
      level += 1
      normalDrop.cancel(false)
      normalDrop = ex.scheduleAtFixedRate(dropTask, time, time, TimeUnit.MILLISECONDS)
    }
    
    val ex = new ScheduledThreadPoolExecutor(1)
    var time = 700
    val quickTime = 55
    // Tasks
    val dropTask = new Runnable { 
      def run() = {
        if(!session.dropOne()) {
          deafTo(panel.keys)
          isLost = true
          haltAll()
        } 
        dropScore += 1
        bonusScore = session.bonusScore
        updateView(Some("\n" + score.toString))
        if (score > level * 1000) {
          faster()
        }
        
      }
    }
    val spinTask = new Runnable {
      def run() = {
       session.rotate()
       bonusScore = session.bonusScore
       updateView(Some("\n" + score.toString))
      }
    }
    val leftTask = new Runnable {
      def run() = {
        if (leftEngaged) session.left()
        bonusScore = session.bonusScore
        updateView(Some("\n" + score.toString))
      }
    }
    val rightTask = new Runnable {
      def run() = {
        if (rightEngaged) session.right()
        bonusScore = session.bonusScore
        updateView(Some("\n" + score.toString))
      }
    }
    // Schedules
    var fastDrop: ScheduledFuture[_] = ex.scheduleAtFixedRate(dropTask, 0, quickTime, TimeUnit.MILLISECONDS)
    var spinThing: ScheduledFuture[_] = ex.scheduleAtFixedRate(spinTask, 0, quickTime, TimeUnit.MILLISECONDS)
    var leftThing: ScheduledFuture[_] = ex.scheduleAtFixedRate(leftTask, 0, quickTime, TimeUnit.MILLISECONDS)
    var rightThing: ScheduledFuture[_] = ex.scheduleAtFixedRate(rightTask, 0, quickTime, TimeUnit.MILLISECONDS)
    var normalDrop = ex.scheduleAtFixedRate(dropTask, time, time, TimeUnit.MILLISECONDS)
    haltAll()

 
}

