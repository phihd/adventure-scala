package o1.adventure

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.util.Random

/** The class `Area` represents locations in a text adventure game world. A game world 
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building, 
  * an acre of forest, or something completely different. What different areas have in 
  * common is that players can be located in them and that they can have exits leading to 
  * other, neighboring areas. An area also has a name and a description. 
  * @param name         the name of the area 
  * @param description  a basic description of the area (typically not including information about items) */
class Area(var name: String, var description: String, var Stat : Statistic, var relationship : Relationship, var tradeCost : Int) {
  
  private val neighbors = Map[String, Area]()
  private var Contain = Map[Item, Int]()
  var tradeLeft = 3
  
  /** Returns the area that can be reached from this area by moving in the given direction. The result 
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)

  
  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in 
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }

  
  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling 
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }
  
  
  /** Returns a multi-line description of the area as a player sees it. This includes a basic 
    * description of the area as well as information about exits and items. */
  def fullDescription = {
    val relationship = if (this.relationship.isAlly) this.relationship.toString else {
      this.relationship.toString + ". \nEnemy stats: " + this.Stat.toString 
    }
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    val itemList = if(Contain.size != 0) {
      var index = 0
      var list = ""
      while (index < Contain.unzip._1.toVector.groupBy(_.name).unzip._1.toVector.size) {
        list = list + "\n" + Contain.groupBy(_._1.name).unzip._1.toVector.drop(index).head + " x" + Contain.groupBy(_._1.name).values.flatten.drop(index).head._2 + ",   " + Contain.groupBy(_._1.name).unzip._2.flatten.unzip._1.toVector.drop(index).head.price + " gold/item."
        index += 1
      }
      list
    } else ""
    val items = if(Contain.size != 0) "\n\nYou see here: " + itemList  else ""
    val trade = benefit + "\nTrade left: " + tradeLeft + "/3"
    this.description + items + 
    (if (name == "Headquarter") "" else "\n\n" + relationship) +
    (if (this.relationship.isAlly && name != "Headquarter") "\n\n" +  trade else "") +
    (if (hasBase) "\n\nYou have level " + baseLevel + " base here" else "") +
    exitList  
  }
  
  def benefit = {
         if (name == "Shang") "A huge number of low-priced Chinese soldiers will improve your army comprehensively." + 
    "\nTrade 252 gold for 2 Attack, 2% Critical Chance, 2 Defense, 20 Maximum Health, 2 Speed"
    else if (name == "Assyrian") "'Learn to move and shoot like the wind', Assyrian chariot archers will show you how." +
    "\nTrade 312 gold for 2 Speed and 10 Attack"
    else if (name == "Palmyran") "Technology of Palmyran supply anything under the sun, you just need to pay." +
    "\nTrade 601 gold for 2 Attack, 2% Critical Chance, 2 Defense, 20 Maximum Health, 2 Speed, 10 Potent Potions"
    else if (name == "Babylonian") "So many invaluable and rare plants as medication here" +
    "\nTrade 481 to heal 70% of current missing health and 10 Potent Potions"
    else if (name == "Greek") "Finest phalanx will boost strongly the strength of army" +
    "\nTrade 387 gold for 10 Attack, 2 Defense, 3 Speed"
    else if (name == "Minoan") "Amazing discipline and skill of composite bowmen from Minoan creates bursts of damage" +
    "\nTrade 315 gold for 5% Critical Chance"
    else if (name == "Hittite") "Hittite iron just make their toughness of catapults almost doubled" +
    "\nTrade 560 gold for 300 Maximum Health Point"
    else if (name == "Phoenician") "The Phoenician is acclaimed for their wonderful elephant units, great completion for the frontline" +
    "\nTrade 403 gold for 150 Maximum Health Point and 5 Attack"
    else if (name == "Sumerian") "No words for Sumerian heavy catapults, with consistent high rapid fire, buildings just like clay" +
    "\nTrade 490 gold for 20 Attack"
    else if (name == "Persian") "Persian assassins are sick, don't blink for a second or you may die." +
    "\nTrade 504 gold for 15 Speed"
    else if (name == "Yamato") "You have Yamato heavy calvary, you have a unit that absolutely control battles in different situations" +
    "\nTrade 657 gold for 5 Attack, 5 Defense, 50 Maximum Health Point, 10 Speed"
    else if (name == "Choson") "A thousand of Choson swordsmen will level out everything on their paths" +
    "\nTrade 273 gold for healing 40% of current missing health, 50 Maximum Health Point, 3 Potent Potion"
    else if (name == "Roman") "Roman army is utter perfect with dreadful attack ability, especially from their legions and scythe chariots" +
    "\nTrade 980 gold for 40 Attack"
    else if (name == "Carthagian") "Want a good bargain for the tankiness of the frontline? King of Carthagia give you a damn good offer" +
    "\nTrade 747 gold to increase your Maximum Health Point by 20%"
    else if (name == "Macedonia") "Defense is also the art, sometimes best way to attack is defending" +
    "\nTrade 630 gold to increase your Defense by 30%"
    else if (name == "Egyptian") "The ferociousness of Egyptian chariots is just unstoppable" +
    "\nTrade 436 gold for healing 40% of current missing health, 100 Maximum Health Point, and 5 Potent Potion"
    else ""
  }
  
  def addItem(item: Item, quantity : Int) = if (this.Contain.contains(item)) this.Contain(item) += quantity else this.Contain += item -> quantity
  
  def contains(itemName : String) = this.Contain.unzip._1.exists(_.name == itemName )
    
  def removeItem(itemName : String, quantity : Int) =
    {
      if (this.contains(itemName) )
     {
    if (ItemWithVolume(itemName).values.toVector.head == 0) this.Contain.remove(findItem(itemName).head) 
    else this.Contain(findItem(itemName).head) -= quantity
     }
    }
  
  def findItem(itemName : String) = this.Contain.unzip._1.find(_.name == itemName)
  
  def ItemWithVolume(itemName : String) = this.Contain.filterKeys(_.name == itemName)
  
  var hasBase = false
  def baseCost = {
    if (typeUpgrade == 0) 160/3
    else if (typeUpgrade == 1) 53
    else if (typeUpgrade == 2) 70
    else if (typeUpgrade == 3) 60
    else if (typeUpgrade == 4) 48
    else if (typeUpgrade == 5) 80
    else 0
  }
  
  var baseLevel = 0
  def BaseUpgrade = if (this.hasBase) this.baseLevel += 1 else this.baseLevel
  
  var typeUpgrade = -1
  
  def isCrit = {
    var critChance = Buffer[Boolean]()
    for (i <- 0 to Stat.Critical) critChance += true
    for (i <- Stat.Critical + 1 to 100) critChance += false
    critChance = Random.shuffle(critChance)
    critChance(Random.nextInt(101))
  }
  
  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)
  
}