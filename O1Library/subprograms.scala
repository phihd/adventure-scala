

// At this stage, you don’t have to worry about the definitions immediately below this text. 
// They have to do with the way these functions have been placed within the package; more on that in Chapter 4.5.   
package o1
object subprograms {
  
  
  
  // WRITE YOUR OWN FUNCTIONS BELOW THESE IMPORT COMMANDS:  
  import scala.collection.mutable.Buffer
  import scala.math._


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  // Here is an buggy piece of code for you to fix in one of the assignments in Chapter 1.7.
  def onTwoInstruments(melody: String, first: Int, second: Int, lengthOfPause: Int) = {
    val melodyUsingFirst  = "[" + first  + "]" + melody
    val melodyUsingSecond = "[" + second + "]" + melody
    val pause = " " * lengthOfPause
    val playedTwice = melodyUsingFirst + pause + melodyUsingSecond
  }
   
  // Here is a template for one of the assignments in Chapter 1.8. 
  // Note: this template is missing the core of the solution. It also contains a small but significant error. 
  def verbalEvaluation(projectGrade: Int, examBonus: Int, participationBonus: Int) {
    val descriptions = Buffer("failed", "acceptable", "satisfactory", "good", "very good", "excellent")
    // PLEASE FILL OUT THIS PART OF THE SOLUTION. YOU CAN REMOVE THIS COMMENT.
  }

  
  
 
  
  // HERE ARE SOME EXAMPLE FUNCTIONS WHOSE IMPLEMENTATIONS WE EXAMINE IN CHAPTERS 1.7 AND 1.8.
  // PLEASE SEE THE CHAPTER TEXTS FOR DETAILED EXPLANATIONS.

  def average(first: Double, second: Double) = (first + second) / 2 

  def yell(phrase: String) = phrase + "!"

  def careerAdvicePlease(addendum: String) = {
    println("Oppenheimer built the bomb, but now he's dead. (Dead!)")
    println("Einstein was very, very smart, but not enough not to be dead. (Dead!)")
    println("So don't go into science; you'll end up dead.")
    println("Don't go into science; you'll end up dead.")
    println(addendum)
  }
  
  
  def distance(x1: Double, y1: Double, x2: Double, y2: Double) = hypot(x2 - x1, y2 - y1)
  
  def redBall(size: Int) = circle(size, Red)
  
  def greatestDistance(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) = {
    val first  = distance(x1, y1, x2, y2)
    val second = distance(x1, y1, x3, y3)
    val third  = distance(x2, y2, x3, y3)
    max(max(first, second), third)
  }
  
  def incomeTax(income: Double, thresholdIncome: Double, baseRate: Double, additionalRate: Double) = {
    val baseTax = min(thresholdIncome, income)
    val additionalTax = max(income - thresholdIncome, 0)
    baseTax * baseRate + additionalTax * additionalRate
  } 

  def experiment1(number: Int) = {
    println("The number is: " + number)
  }

  def experiment2(numbers: Buffer[Int]) = {
    numbers(0) = 100
  }
  
  def experiment3(number: Int) = {
    println("The number is: " + number)
    number + 1
  }

  def experiment4(word: String) = {
    var number = 1
    println(word + ": " + number)
    number = number + 1
    println(word + ": " + number)
    number = number + 1
    println(word + ": " + number)
    number
  }
  
  def experiment5(initial: Int) = {
    var number = initial
    number = number + 1
    number = number + 1
    number = number + 1
    number
  }
    
  def test1(text: String) = {
    println(text)
    "always the same string"
  }

  def test2(text: String) = {
    println(text)
    val response = test1(yell(text))
    test1(text)
    println("we got:")
    println(response)
  }
  
  
  
  
  
  
  // HERE ARE SOME FUNCTIONS WHICH WE MAKE USE OF IN CHAPTER 1.6. 
  // YOU DON’T NEED TO UNDERSTAND THEIR INTERNAL WORKINGS AT THAT STAGE. 
  // The program code below has not been written in a beginner-friendly style.
  
  
  def removeNegatives(numbers: Buffer[Int]): Unit = {
    numbers --= numbers.filter( _ < 0 )
  }
  
  
  def imdbMovie(rank: Int) = movieData.sortBy( _._3 ).apply(rank - 1)._1     
    
  def imdbBestBetween(startYear: Int, endYear: Int) = 
    movieData
      .filter( movie => movie._2 >= startYear && movie._2 <= endYear )
      .sortBy( _._3 )
      .apply(0)._1
    
  def imdbBestDirectors(minMovieCount: Int) =  
    movieData
      .flatMap { case (_, _, _, _, directors) => directors.toList }                                        
      .groupBy(identity).mapValues( _.size )
      .filter( _._2 >= minMovieCount )
      .toList.sortBy( -_._2 ) 
      .map { case (director, movieCount) => director + " (" + movieCount + ")" }
      .mkString(", ")
      
  private lazy val movieData = { 
    val Subdir   = "top_movies" 
    val FileName = "omdb_movies_2015.txt" 
    val rawLines = o1.util.readFileLines(s"$Subdir/$FileName").getOrElse( throw new Exception(s"Could not read the file $FileName, which is expected to be under $Subdir.") )
    val linesAsTokens = rawLines.map( _.split(";") )
    linesAsTokens.map( tokens => (tokens(0), tokens(1).toInt, tokens(2).toInt, tokens(3).toDouble, tokens(4).split(",")) )
  }
  
      
  def editDistance(text1: String, text2: String) = o1.util.editDistance(text1, text2, 12)
      

  def animate(pictures: Buffer[Pic], picsPerSecond: Double): Unit = {
    Animation.show(frames = pictures, frameRate = picsPerSecond)
  }

  def animateWithFunction(picGeneratingFunction: Int => Pic, numberOfPics: Int, picsPerSecond: Double): Unit = {  
    Animation.generate(picGeneratingFunction, numberOfPics, picsPerSecond)
  }

  
  def canon(song: String, instruments: Iterable[Int], delay: Int) = {
    import o1.sound.midi._
    import o1.util._
    val (melody, tempo) = song match { 
      case r"(.*?)$melody(?:/([\d]+)$tempoOrNull)?" => (melody, Option(tempoOrNull)) 
    }
    def initialWait(instrumentNr: Int) = " " * (instrumentNr * delay.max(0).min(melody.length))
    val counterpointed = for ((instrument, instrumentNr) <- instruments.take(MaxVoices).zipWithIndex) 
                         yield s"${initialWait(instrumentNr)}[$instrument]$melody"
    counterpointed.mkString("&") + tempo.map( "/" + _ ).getOrElse("")
  }

  
  def censor(text: String, naughtyWords: Traversable[String]) = 
    naughtyWords.foldLeft(text)( (version, naughtyWord) => version.replaceAll(naughtyWord, "[B" + "E" * max(0, naughtyWord.length - 2) + "P]") )

  
  def playDoodads(player: String) = {
    o1.gui.O1SwingDefaults()    
    import o1.gui.Dialog._
    display("Welcome to the game of DOODADS, " + player + "!\nIn the beginning, there are twelve Doodads.\n" +
            "We will take turns choosing to pick up either 1 or 2 Doodads.\nThe one who picks up the last Doodad wins.", Centered) 
    Stream.iterate(12)(playOneRound).takeWhile(gameContinues).force
    display("I'm afraid you lost, " + player + ". Sorry about that.\n", Centered)

    def playOneRound(doodadsLeft: Int) = {
      requestPick(doodadsLeft).map( humanPick => computerPlays(doodadsLeft - humanPick) ).getOrElse(0)
    }
      
    def requestPick(doodadsLeft: Int) = {
      requestInt("There are " + doodadsLeft + " Doodads remaining. How many will you pick up?", number => (number == 1 || number == 2), "Please take either 1 or 2.", Centered)
    }
    
    def computerPlays(doodadsLeft: Int) = { 
      val computerPick = bestChoice(doodadsLeft)
      display("I'll take " + computerPick + " " + (if (computerPick == 1) "Doodad" else "Doodads") + ".", Centered)
      doodadsLeft - computerPick
    }
    
    def gameContinues(doodadsLeft: Int) = doodadsLeft > 0

    def bestChoice(doodadsLeft: Int) = doodadsLeft % 3
  }

  
  def showInfo(word: String) = {
    println("The word: " + word + " was received as a parameter.")
    word.length
  }
  
  
}
