package o1.adventure
import scala.util.Random


/** The class `Adventure` represents text adventure games. An adventure consists of a player and 
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very 
  * specific adventure game that involves a small trip through a twisted forest. All newly created 
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure 
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "Sovereign: Rise of the Viking"
    
  private val Shang = new Area("Shang", "Shang\nYou are somewhere in Chinese territory.\nThere are so many people. Their trade is really thriving.", randomStats, randomRelationship, 252)       
  private val Macedonian = new Area("Macedonian", "Macedonian\nYou are at a kingdom in the north of Greece reigned by Alexander XIV.\nScience are so developing that magic is considered illegitimate.", randomStats, randomRelationship, 630)
  private val Phoenician = new Area("Phoenician", "Phoenician\nYou are in Mediterranean Sea.\nMany people from different culture in the world come, marine trades is crazy.", randomStats, randomRelationship, 403)
  private val Assyrian = new Area("Assyrian", "Assyrian\nYou are somewhere in the north of Iraq.\nThe Tigris river is beautiful and the locals here are beefy.", randomStats, randomRelationship, 312)
  private val Palmyran = new Area("Palmyran", "Palmyran\nYou are in a wealthy landscape of Syria.\nModern technology make this low-population kingdom becomes productive. ", randomStats, randomRelationship, 601)
  private val Sumerian = new Area("Sumerian", "Sumerian\nYou are somewhere in the south of Iraq, beside the Euphrates river.\nFarming is exceptional. The crops smell so good.", randomStats, randomRelationship, 490)
  private val Headquarter = new Area("Headquarter", "Headquarter\nWelcome home. Make our dynasty become great forever!", randomStats, new Ally, 0)
  private val Roman = new Area("Roman", "Roman\nKing Augutus wants a talk. You don't want to mess with this nation.", randomStats, randomRelationship, 980)
  private val Babylonian = new Area("Babylonian", "Babylonian\nYou are somewhere in the middle of Asia and Mediterranean Sea\nCome to see the hanging garden, it's damn magnificient.", randomStats, randomRelationship, 481)
  private val Minoan = new Area("Minoan", "Minoan\nWelcome to Crete island. Take a breath before looking at the navy here.", randomStats, randomRelationship, 315)
  private val Persian = new Area("Persian", "Persian\nYou are in the middle of India and Babylinonian. People only use elephants as transportations, that's sick.\nA hunting competition is happening", randomStats, randomRelationship, 504)
  private val Greek = new Area("Greek", "Greek\nYou are in a beach of Black Sea\nDon't mess up with Greek, a number of hoplites will come immediately.", randomStats, randomRelationship, 387)
  private val Hittite = new Area("Hittite", "Hittite\nYou are somewhere in Turkey.\nA dynasty pioneers developing iron tools and iron weapons.", randomStats, randomRelationship, 560)
  private val Carthagian = new Area("Carthagian", "Carthagian\nYou are in the west of Tunisia. Elephants, Camels and Hoplites here are extraordinarily big.", randomStats, randomRelationship, 747)
  private val Yamato = new Area("Yamato", "Yamato\nYou are in Japan. Come and get some sushi.", randomStats, randomRelationship, 657)
  private val Egyptian = new Area("Egyptianian", "Egyptianian\nYou are in Egyptian.\nA lot of secrets about magic in those pyramids. Somebody says the priests here use black magic to strength the horses and gold miners.", randomStats, randomRelationship, 436)
  private val Choson = new Area("Choson", "Choson\nYou are in North Korea.\nThey train everyone to be a skillful swordsman.", randomStats, randomRelationship, 273)
  
        Shang.setNeighbors(Vector("east" -> Macedonian, "south" -> Assyrian, "west" -> Phoenician))
   Macedonian.setNeighbors(Vector("east" -> Palmyran, "west" -> Shang))
   Phoenician.setNeighbors(Vector("north" -> Shang, "east" -> Assyrian, "south" -> Sumerian))
     Assyrian.setNeighbors(Vector("north" -> Shang, "east" -> Palmyran, "south" -> Headquarter, "west" -> Phoenician))
     Palmyran.setNeighbors(Vector("north" -> Macedonian, "south" -> Roman, "west" -> Assyrian))
     Sumerian.setNeighbors(Vector("east" -> Phoenician, "south" -> Persian))
  Headquarter.setNeighbors(Vector("north" -> Assyrian, "east" -> Minoan, "south" -> Greek, "west" -> Babylonian))
        Roman.setNeighbors(Vector("east" -> Carthagian, "south" -> Minoan, "west" -> Palmyran))
   Babylonian.setNeighbors(Vector("north" -> Headquarter, "south" -> Egyptian))
       Minoan.setNeighbors(Vector("east" -> Roman, "south" -> Hittite, "west" -> Headquarter))
      Persian.setNeighbors(Vector("north" -> Sumerian, "east" -> Greek, "south" -> Yamato))
        Greek.setNeighbors(Vector("north" -> Headquarter, "south" -> Hittite, "west" -> Persian))
      Hittite.setNeighbors(Vector("north" -> Minoan, "east" -> Carthagian, "south" -> Greek))
   Carthagian.setNeighbors(Vector("north" -> Roman, "south" -> Choson, "west" -> Hittite))
       Yamato.setNeighbors(Vector("north" -> Persian, "east" -> Egyptian))
     Egyptian.setNeighbors(Vector("north" -> Babylonian, "west" -> Yamato))
       Choson.setNeighbors(Vector("east" -> Carthagian))
       
    private val Map = Vector[Area](Shang, Macedonian, Phoenician, Assyrian, Palmyran, Sumerian, Headquarter, Roman, Babylonian, Minoan, Persian, Greek, Hittite, Carthagian, Yamato, Egyptian, Choson)
    
    /** The character that the player controls in the game. */
  val player = new Player(Headquarter)

 
 /**************************************************************************************************************************************************************************************************************************************************************************************/
 private val WoodenHelmet     = new Item("wooden helmet", "A helmet made of wood, hopefully it can withstand a hit from a sword.", 5, 5*30)
 private val GoldenHelmet     = new Item("golden helmet", "A helmet made of gold, slightly better than a wooden one, and definitely shiny", 15, 15*30)
 private val SteelHelmet      = new Item("steel helmet", "A helmet made of steel, heavier but more stronger.", 25, 25*30)
 private val ChainHelmet      = new Item("chain helmet", "A helmet made of chainmail, lighter and provide much more reliability in combat.", 35, 35*30)
 private val DiamondHelmet    = new Item("diamond helmet", "A helmet made of diamond, strong and pretty, what more could you ask from them?", 55, 55*30)
 private val WoodenChestplate = new Item("wooden chestplate", "A chestplate made of wood, pray to god that enemies' weapons won't penetrate it.", 25, 25*30)
 private val GoldenChestplate = new Item("golden chestplate", "A chestplate made of gold, better than a wooden one and shiny, won't be that helpful in combat though.", 35, 35*30)
 private val SteelChestplate  = new Item("steel chestplate", "A chestplate made of steel, heavy and strong, can maybe take a sword slash.", 45, 45*30)
 private val ChainChestplate  = new Item("chain helmet", "A chestplate made of chainmail, lighter, more durable. Definitely useful.", 55, 55*30)
 private val DiamondChestplate = new Item("diamond chestplate", "A chestplate made of diamond, very strong and durable, may terrify enemies because of your richness.", 75, 75*30)
 private val ChainBoots       = new Item("chain boots", "A pair of boots made of chainmail, good for long conquest, but still, heavy.", 10, 10*48)
 private val LeatherBoots     = new Item("leather boots", "A pair of boots made of leather, more comfortable and provide more spped.", 30, 30*48)
 private val FeatherBoots     = new Item("feather boots", "A pair of boots made of fine feather, you won't be as fast as Hermes himself, but you definitely gain more benefit in combat.", 50, 50*48)
 private val WoodenSword      = new Item("wooden sword", "A sword made of wood, well, the enemy can still get hurt by it... a little.", 30, 30*35)
 private val GoldenSword      = new Item("golden sword", "A sword made of gold, you can blind your enemy by reflecting the lights by this sword.", 60, 60*35)
 private val IronSword        = new Item("iron sword", "A sword made of iron. Now this is a real weapon that can kill.", 150, 150*35)
 private val DiamondSword     = new Item("diamond sword", "A sword made of diamond. It is pretty, and indeed, it is deadly.", 250, 250*35)
 private val DiamondSpear     = new Item("diamond spear", "THIS IS SPARTA!!!!!!!!!!!", 400, 400*35)
 private val PotentPotion     = new Item("potent potion", "A potion that heals 20% of your health, how nice.", this.player.maxHP * 20 / 100, 50)
 private val NormalPotion     = new Item("normal potion", "A potion that heals you a little bit.", 150, 150 * 8 /30)
 private val HyperPotion      = new Item("hyper potion", "A potion that heals you more than a little bit.", 300, 300 * 8/30)
 private val MaxPotion        = new Item("max potion", "A potion that heals you a lot.", 900, 900 * 8/30)
 private val GoldenEmblem     = new Item("gold emblem", "A king's emblem that helps you recruit more soliders than you normally can.", 1500, 1500 * 3)
 private val DiamondEmblem    = new Item("diamond emblem", "A king's emblem that proves your are the best king. Helps you to recruit even more soliders than you normally can.", 3000, 3000*3)
  
 this.Headquarter.addItem(WoodenHelmet, 1) ; this.Headquarter.addItem(WoodenChestplate, 1) ; this.Headquarter.addItem(WoodenSword, 1) ; this.Headquarter.addItem(NormalPotion, 10)
 this.Assyrian.addItem(GoldenHelmet, 1) ; this.Assyrian.addItem(ChainBoots,1) ; this.Assyrian.addItem(SteelHelmet,1); this.Assyrian.addItem(NormalPotion , 10)
 this.Minoan.addItem(GoldenChestplate, 1) ; this.Minoan.addItem(GoldenSword,1) ; this.Minoan.addItem( PotentPotion, 10) ; this.Minoan.addItem(GoldenEmblem, 1)
 this.Babylonian.addItem(GoldenSword, 1) ; this.Babylonian.addItem(SteelChestplate,1) ; this.Babylonian.addItem(PotentPotion, 10)
 this.Greek.addItem(ChainBoots, 1) ; this.Greek.addItem(SteelChestplate,1) ; this.Greek.addItem(NormalPotion, 10) ; this.Greek.addItem(HyperPotion , 2)
 this.Shang.addItem(SteelHelmet, 1) ; this.Shang.addItem(ChainChestplate,1) ; this.Shang.addItem(IronSword,1) ; this.Shang.addItem(LeatherBoots, 1)
 this.Phoenician.addItem(SteelChestplate, 1) ; this.Phoenician.addItem(IronSword, 1) ; this.Phoenician.addItem(ChainHelmet,1) ; this.Phoenician.addItem(LeatherBoots,1)
 this.Macedonian.addItem(SteelChestplate, 1) ; this.Macedonian.addItem(ChainHelmet, 1) ; this.Macedonian.addItem(HyperPotion , 5)
 this.Palmyran.addItem(ChainHelmet, 1) ; this.Palmyran.addItem(ChainBoots, 1) ; this.Palmyran.addItem(LeatherBoots,1) ; this.Palmyran.addItem(HyperPotion ,10)
 this.Roman.addItem(LeatherBoots, 1) ; this.Roman.addItem(IronSword, 1); this.Roman.addItem(GoldenEmblem, 1)
 this.Carthagian.addItem(ChainChestplate,1) ; this.Carthagian.addItem(SteelHelmet, 1) ; this.Carthagian.addItem(DiamondSword,1) ; this.Carthagian.addItem(LeatherBoots,1)
 this.Hittite.addItem(IronSword,1) ; this.Hittite.addItem(ChainHelmet, 1); this.Hittite.addItem(GoldenEmblem , 1)
 this.Babylonian.addItem(ChainChestplate,1) ; this.Babylonian.addItem(ChainHelmet, 1) ;  this.Babylonian.addItem(GoldenSword, 1)
 this.Choson.addItem(DiamondChestplate, 1) ; this.Choson.addItem(ChainHelmet,1) ; this.Choson.addItem(DiamondSpear, 1) ; this.Choson.addItem(FeatherBoots, 1) ; this.Choson.addItem(MaxPotion , 5) ; this.Choson.addItem(DiamondEmblem, 1)
 this.Egyptian.addItem(DiamondHelmet, 1) ; this.Egyptian.addItem(DiamondSword, 1) ; this.Egyptian.addItem(FeatherBoots, 1) ; this.Egyptian.addItem( DiamondEmblem ,1 )
 this.Sumerian.addItem(SteelChestplate, 1) ; this.Sumerian.addItem(ChainHelmet, 1) ; this.Sumerian.addItem(IronSword, 1) ; this. Sumerian.addItem(HyperPotion, 5)
 this.Persian.addItem(DiamondSpear, 1) ; this.Persian.addItem(DiamondHelmet, 1) ; this.Persian.addItem(HyperPotion, 5)
 this.Yamato.addItem(DiamondChestplate, 1) ; this.Yamato.addItem(DiamondSword, 1) ; this.Yamato.addItem(DiamondHelmet, 1) ; this.Yamato.addItem(MaxPotion, 5)
 
 
    
    
    
     
 /*************************************************************************************************************************************************************************************************************************************************************************************/ 
     

  
  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 40 


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.Map.forall( _.relationship.isAlly)

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */ 
  def isOver = this.isComplete || this.player.hasQuit || this.player.isDead

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "You are a Knight on your journey helping people in need. One day, you reached a Kingdom"

    
  /** Returns a message that is to be displayed to the player at the end of the game. The message 
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "Your conquest has now been suceeded. You became the King of all world."
    else if (this.player.isDead)
      "The Kingdom mourns because the lost of a wise King"
    else  // game over due to player quitting
      "A wise King never quits!!! Shame on you!!!" 
  }

  
  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual 
    * report of what happened, or an error message if the command was unknown. In the latter 
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) { 
      this.turnCount += 1 
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }
  
  def randomStats: Statistic = new Statistic(Random.nextInt(2501) + 1500, Random.nextInt(151) + 50, Random.nextInt(151) + 50, Random.nextInt(101), Random.nextInt(31))
  def randomRelationship = {
    val tmp = Random.nextInt(2)
    if (tmp == 0) new Ally else new Enemy
  }
}

