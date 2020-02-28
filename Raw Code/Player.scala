package o1.adventure

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.util.Random
  
/** A `Player` object represents a player character controlled by the real-life user of the program. 
  *
  * A player object's State is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  var playerName= "Anonymous"
  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var Inventory = Map[Item, Int]()
  var money = 10000
  private var Chestplate : Option[Item] = None
  private var Helmet : Option[Item] = None
  private var Weapon : Option[Item] = None
  private var Emblem : Option[Item] = None
  private var Boots : Option[Item] = None
  var maxHP = 2000
  var Stat = new Statistic(2000, 120, 100, 30, 25)
  def name(newName: String) = 
    {
    playerName = newName  
    "The king: " + newName
    }
  
  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  
  /** Returns the current location of the player. */
  def location = this.currentLocation
  
  def isAlive = this.Stat.HP > 0
  def isDead = !isAlive
  /** Attempts to move the player in the given direction. This is successful if there 
    * is an exit from the player's current location towards the direction name. 
    * Returns a description of the results of the attempt. */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation) 
    if (destination.isDefined) {
      StatUpgrade
      "You go " + direction + "."
    }
    else "You can't go " + direction + "."
  }

  
  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    if (this.money >= 5) 
    {
    this.Stat.HP += 10
    this.money -= 5
    "You rest for a while. Better get a move on, though." 
    }
    else "Everything comes with a cost, even resting!!!"
  }
  
  
  /** Signals that the player wants to quit the game. Returns a description of what happened within 
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  private def isAlly = this.location.relationship.isAlly
  
  /** Returns a brief description of the player's State, for debugging purposes. */
  override def toString = "Now at: " + this.location.name   
  
  /** examine a certain item, returning the item's description */
  def examine(itemName: String): String = 
  {
    if ( this.has(itemName) )
        {
      "You look closely at the " + itemName + "." + "\n" + this.Inventory.filter(_._1.name == itemName).head._1.description
        } else "If you want to examine something, you need to have it first."
  }
  
  def has(itemName: String): Boolean = this.Inventory.exists(_._1.name == itemName)
  
  /** Go through the inventory */
  def inventory = 
  {
    var itemList = if (Inventory.size != 0) 
    {
      var index = 0
      var list = ""
      while (index < Inventory.unzip._1.toVector.groupBy(_.name).unzip._1.toVector.size) {
        list = list + "\n" + Inventory.groupBy(_._1.name).unzip._1.toVector.drop(index).head + " x" + Inventory.groupBy(_._1.name).values.flatten.drop(index).head._2
        index += 1
      }
      list
    } else ""
    if (Inventory.size == 0) "You are empty-handed."
    else 
    {  
      "You are carrying: " + itemList
    }  
  }
  
  /**To see whether the item is a sword or an armor that can be equipped */
  private def isEquipable(itemName : String) = 
  {
    var Tier = itemName.trim.takeWhile( _ != ' ')
    var Type = itemName.trim.drop(Tier.length).trim.toLowerCase
    Type == "chestplate" || Type == "helmet" || Type == "emblem" || Type == "sword" || Type == "spear" || Type == "boots"
  }
  
  /**To see whether the item is 'usable' */
  private def isUsable(itemName : String) =
  {
    var Tier = itemName.trim.takeWhile( _ != ' ')
    var Type = itemName.trim.drop(Tier.length).trim.toLowerCase
    Type == "potion"
  }
  
  /** Used to equip gears for our hero */
  def equip(itemName: String): String =
  {
    if (this.Inventory.exists(_._1.name == itemName) && isEquipable(itemName) )
        {
    var ItemEquip = this.Inventory.find( _._1.name == itemName).head._1
    var Tier = itemName.trim.takeWhile( _ != ' ')
    var Type = itemName.trim.drop(Tier.length).trim.toLowerCase
      if ( Type == "chestplate") 
      {
        this.Chestplate match {
          case None => this.Chestplate = Some(ItemEquip)
          case Some(chestplate) => {
          if (this.Inventory.contains(chestplate)) this.Inventory(chestplate) += 1 else this.Inventory += chestplate -> 1
          Some(ItemEquip)  
          }
        }
        if (this.Inventory(ItemEquip) == 1) this.Inventory.remove(ItemEquip) 
           else this.Inventory(ItemEquip) -= 1
        this.Stat.Defense += this.Chestplate.get.Stat
      }
      else if ( Type == "helmet") 
      {
        this.Helmet match {
          case None => this.Helmet = Some(ItemEquip)
          case Some(helmet) => {
          if (this.Inventory.contains(helmet)) this.Inventory(helmet) += 1 else this.Inventory += helmet -> 1
          Some(ItemEquip)  
          }
        }
        if (this.Inventory(ItemEquip) == 1) this.Inventory.remove(ItemEquip) 
           else this.Inventory(ItemEquip) -= 1
        this.Stat.Defense += this.Helmet.get.Stat
      } 
      else if ( Type == "sword" || Type == "spear") 
      {
        this.Weapon match {
          case None => this.Weapon = Some(ItemEquip)
          case Some(weapon) => {
          if (this.Inventory.contains(weapon)) this.Inventory(weapon) += 1 else this.Inventory += weapon -> 1
          Some(ItemEquip)  
          }
        }
        if (this.Inventory(ItemEquip) == 1) this.Inventory.remove(ItemEquip) 
           else this.Inventory(ItemEquip) -= 1
        this.Stat.Attack += this.Weapon.get.Stat
      } 
      else if ( Type == "boots") 
      {
        this.Boots match {
          case None => this.Boots = Some(ItemEquip)
          case Some(boots) => {
          if (this.Inventory.contains(boots)) this.Inventory(boots) += 1 else this.Inventory += boots -> 1
          Some(ItemEquip)  
          }
        }
        if (this.Inventory(ItemEquip) == 1) this.Inventory.remove(ItemEquip) 
           else this.Inventory(ItemEquip) -= 1
        this.Stat.Defense += this.Helmet.get.Stat
      } 
      else
      {
        this.Emblem match 
        {
          case None => this.Emblem = Some(ItemEquip)
          case Some(emblem) => {
          if (this.Inventory.contains(emblem)) this.Inventory(emblem) += 1 else this.Inventory += emblem -> 1
          Some(ItemEquip)
          }
        }
        if (this.Inventory(ItemEquip) == 1) this.Inventory.remove(ItemEquip) 
           else this.Inventory(ItemEquip) -= 1
        this.maxHP += this.Emblem.get.Stat
      }
      "Equiped " + itemName
        }
    else if (this.Inventory.exists(_._1.name == itemName) && !isEquipable(itemName) )
      itemName + " cannot be equiped!!!"
    else "There is no " + itemName + "to equip"
  }
  
  /** Used to drink potion */
  def use(itemName: String) : String =
  {
    var fullDescription = ""
    var Outcome = ""
    if (this.Inventory.exists(_._1.name == itemName) && isUsable(itemName) )
        {
      if ( itemName.trim.drop( itemName.trim.takeWhile( _ != ' ').length ).trim.toLowerCase == "potion") this.Stat.HP = math.min(this.Stat.HP + this.Inventory.find( _._1.name == itemName).head._1.Stat,maxHP)
      fullDescription = "Used " + itemName + ". \n Your Stat is now: " + this.Stat.toString 
        }
    else if (this.Inventory.exists(_._1.name == itemName) && !isUsable(itemName) )
     fullDescription = itemName + " cannot be used!!!"
    else fullDescription = "There is no " + itemName + "to use."
    if (this.inCombat == true && this.location.relationship.isEnemy) 
    {
      var Damage = 0
      var Description = ""
      
      if (location.isCrit) {
          Stat.HP -= math.max( location.Stat.Attack * 2 - Stat.Defense, 0)
          Damage = math.max( location.Stat.Attack * 2 - Stat.Defense, 0)
          Stat.HP = math.max(Stat.HP, 0)
          Description = "It's a critical hit. "
        } 
        else {
          Stat.HP -= math.max( location.Stat.Attack - Stat.Defense, 0)
          Damage = math.max( location.Stat.Attack - Stat.Defense, 0)
          Stat.HP = math.max(Stat.HP, 0)
          Description = ""
        }
      if (this.Stat.HP == 0) Outcome = "You lost" else Outcome = ""
       fullDescription += "\n " + Description + location.name + " troops dealt " + Damage + " damage(s) to the your troops." + "\n" + showHP + "\n" + Outcome
    }
    fullDescription
  }
  
  /** Buying stuff from the shop */
  def buy(itemName: String, quantity : Int) : String =
  {
    if (this.isAlly && this.location.contains(itemName) )
        {
        var BoughtItem = this.location.findItem(itemName)
        var BoughtItemPrice = BoughtItem.head.price
       if (quantity > 0) {
        if (this.location.ItemWithVolume(itemName).values.head >= quantity) 
          {    
            if (this.money >= BoughtItemPrice * quantity)
              {
                this.money += - BoughtItemPrice * quantity
                this.Inventory += BoughtItem.head -> quantity
                this.location.removeItem( itemName, quantity)
                "Bought " + itemName + " x" + quantity
              } 
            else "Cannot afford " + itemName + " x" + quantity
          } 
        else "The shop only has " + this.location.ItemWithVolume(itemName).values.head + " " + itemName+ "(s)" + " left"
        } 
       else "Please give a correct quantity. "
        } 
    else if (this.isAlly && !this.location.contains(itemName) ) "No " + itemName + " available"
    else "You are now in Enemy Territory, they are not nice enough to sell you anything!!!"
  }
  
  /** Selling stuff for money */
  def sell(itemName: String, quantity: Int) : String =
  {
    if (this.isAlly && this.has(itemName) )
        {
        var SoldItem = this.Inventory.find(_._1.name == itemName).get._1
        var SoldItemPrice = SoldItem.price
        if (this.Inventory.filterKeys(_.name == itemName).unzip._2.head >= quantity)
          {
           this.money += SoldItemPrice * quantity
           if (this.Inventory(SoldItem) == quantity) this.Inventory.remove(SoldItem) 
           else this.Inventory(SoldItem) -= quantity
           this.location.addItem(SoldItem,quantity)
           "Sold " + itemName + " x" + quantity
          } 
        else "You don't have that many " + itemName + " to sell"
        }
    else if (this.location.name == "Shop" && !this.has(itemName) ) "There is no " + itemName + " to sell"
    else "You are now in Enemy Territory, they are not nice enough to buy anything from you!!!"
  }
  
  def addItem(item: Item, quantity : Int) = if (Inventory.contains(item)) Inventory(item) += quantity else Inventory(item) = quantity
  
  def build =
  {
    if (!this.location.hasBase && location.relationship.isAlly) {
      location.typeUpgrade = Random.nextInt(6)
      if (this.money >= this.location.baseCost) {
        location.hasBase = true
        location.BaseUpgrade
        money -= location.baseCost
        "Built a base at " + this.location.name
      }  
      else "You don't have enough money"
    } 
    else {
      if (location.relationship.isEnemy) "Build a base on enemy territory? Are you joking me?"
      else "You have already built a base here"      
    }
  }
  
  def upgrade =
  {
    if (this.location.hasBase)
    {
      if (this.money >= (this.location.baseCost * math.pow(1.2, location.baseLevel)).toInt)
        this.location.BaseUpgrade
        money -= (location.baseCost * math.pow(1.2, location.baseLevel)).toInt
        "Upgrade successful"
    }
    else "You should build a base first"
  }
  
  def trade : String = {
    if (location.name == "Headquarter") return "You get everything free here, it's your country"
    if (location.relationship.isAlly) {
      //tradeCost = realCost * 70%
      if (location.tradeLeft > 0) {
        if (money >= location.tradeCost) {
          if (location.name == "Assyrian") {//312
            Stat.Speed += 2
            Stat.Attack += 10
          } else if (location.name == "Shang") {//252
            Stat.Attack += 2
            Stat.Critical += 2
            Stat.Defense += 2
            maxHP += 20
            Stat.Speed += 2
          } else if (location.name == "Palmyran") {//601
            Stat.Attack += 2
            Stat.Critical += 2
            Stat.Defense += 2
            maxHP += 20
            Stat.Speed += 2
            addItem(new Item("potent potion", "Heal 20% of maximum health", 150, 0), 10)
          } else if (location.name == "Babylonian") {//481
            Stat.HP = math.min(((maxHP - Stat.HP) * 1.7).toInt, maxHP)
            addItem(new Item("potent potion", "Heal 20% of maximum health", 150, 0), 10)
          } else if (location.name == "Greek") {//387
            Stat.Attack += 10
            Stat.Defense += 2
            Stat.Speed += 3
          } else if (location.name == "Minoan") {//315
            Stat.Critical += 5
            addItem(new Item("potent potion", "Heal 20% of maximum health", 150, 0), 5)
          } else if (location.name == "Hittite") {//560
            maxHP += 300
          } else if (location.name == "Phoenician") {//403
            maxHP += 150
            Stat.Attack += 5
          } else if (location.name == "Sumerian") {//490
            Stat.Attack += 20  
          } else if (location.name == "Persian") {//504
            Stat.Speed += 15
          } else if (location.name == "Yamato") {//657
            Stat.Attack += 5
            Stat.Defense += 5
            maxHP += 50
            Stat.Speed += 10
          } else if (location.name == "Choson") {//273
            Stat.HP = math.min(((maxHP - Stat.HP) * 1.4).toInt, maxHP)
            maxHP += 50
            addItem(new Item("potent potion", "Heal 20% of maximum health", 150, 0), 3)
          } else if (location.name == "Roman") {//980
            Stat.Attack += 40
          } else if (location.name == "Carthagian") {//747
            maxHP = (maxHP * 1.2).toInt  
          } else if (location.name == "Macedonia") {//630
            Stat.Defense = (Stat.Defense * 1.3).toInt
          } else if (location.name == "Egyptian") {//436
            Stat.HP = math.min(((maxHP - Stat.HP) * 1.4).toInt, maxHP)
            maxHP += 100
            addItem(new Item("potent potion", "Heal 20% of maximum health", 150, 0), 5)
          }
          money -= location.tradeCost
          location.tradeLeft -= 1
          "A damn good trade, isn't it?"
        }
        else "No money, no funny"
      }
      else "No more trades left"
    }
    else "Trade? Give us the head of your king"
  }
  
  def StatUpgrade = {
    if (location.typeUpgrade == 0) maxHP += 20 * location.baseLevel
    else if (location.typeUpgrade == 1) Stat.HP +=  ((maxHP - Stat.HP) * (20 + location.baseLevel * 2) / 100).toInt
    else if (location.typeUpgrade == 2) Stat.Attack += 2* location.baseLevel
    else if (location.typeUpgrade == 3) Stat.Defense += 2* location.baseLevel
    else if (location.typeUpgrade == 4) Stat.Speed += location.baseLevel
    else if (location.typeUpgrade == 5) Stat.Critical += 2* location.baseLevel
    else {}
  }

  var inCombat = false
  def isCrit = {
    var critChance = Buffer[Boolean]()
    for (i <- 0 to Stat.Critical) critChance += true
    for (i <- Stat.Critical + 1 to 100) critChance += false
    critChance = Random.shuffle(critChance)
    critChance(Random.nextInt(101))
  }
  
  def showHP = "Your HP: " + Stat.HP + "\tEnemy HP: " + location.Stat.HP
  
  def fight = {
    if (location.relationship.isEnemy) {
      this.inCombat = true
      var Damage = 0
      var Outcome = ""
      var Description = ""
      var fullDescription = ""
      if (Stat.Speed < location.Stat.Speed) {
        if (location.isCrit) {
          Stat.HP -= math.max( location.Stat.Attack * 2 - Stat.Defense, 0)
          Damage = math.max( location.Stat.Attack * 2 - Stat.Defense, 0)
          Stat.HP = math.max(Stat.HP, 0)
          Description = "It's a critical hit. "
        } 
        else {
          Stat.HP -= math.max( location.Stat.Attack - Stat.Defense, 0)
          Damage = math.max( location.Stat.Attack - Stat.Defense, 0)
          Stat.HP = math.max(Stat.HP, 0)
          Description = ""
        }
        if (Stat.HP == 0) {
          inCombat = false
          Outcome = "You lost"
        } else Outcome = ""
        
        fullDescription += Description + location.name + " troops dealt " + Damage + " damage(s) to the your troops." + "\n" + showHP + "\n" + Outcome
      }
      else fullDescription = "You go first.\n" + showHP
      "You started a battle with " + this.location.name + "\n" + fullDescription
    }
    else "Please don't fight your Ally, we have enough meaningless wars already"
  }
      
  def attack =
  {
    var Outcome = ""
    var fullDescription = ""
    if (this.inCombat)
    {
      var Damage = 0
      var Description = ""
      if (this.isCrit) {
        this.location.Stat.HP = this.location.Stat.HP - math.max( Stat.Attack * 2 - this.location.Stat.Defense, 0)
        Damage = math.max( Stat.Attack * 2 - this.location.Stat.Defense, 0)
        this.location.Stat.HP = math.max(this.location.Stat.HP, 0)
        Description = "It's a critical hit. "
      } 
      else {
        this.location.Stat.HP = this.location.Stat.HP - math.max( Stat.Attack * 1 - this.location.Stat.Defense, 0)
        Damage = math.max( Stat.Attack - this.location.Stat.Defense, 0)
        this.location.Stat.HP = math.max(this.location.Stat.HP, 0)
        Description = ""  
      }
      fullDescription += Description + "Your troops dealt " + Damage + " damage(s) to " + this.location.name + " troops." + "\n" + showHP
      if (location.Stat.HP > 0) {
        if (location.isCrit) {
          Stat.HP -= math.max( this.location.Stat.Attack * 2 - Stat.Defense, 0)
          Damage = math.max( this.location.Stat.Attack * 2 - Stat.Defense, 0)
          Stat.HP = math.max(Stat.HP, 0)
          Description = "It's a critical hit. "
        } 
        else {
          Stat.HP -= math.max( this.location.Stat.Attack - Stat.Defense, 0)
          Damage = math.max( this.location.Stat.Attack - Stat.Defense, 0)
          Stat.HP = math.max(Stat.HP, 0)
          Description = ""
         }
         fullDescription += "\n" + Description + this.location.name + " troops dealt " + Damage + " damage(s) to the your troops." + "\n" + showHP
      }
        if (this.location.Stat.HP == 0) {
           this.location.relationship = new Ally
           this.inCombat = false
          Outcome ="Enemy lost and became your ally \nWar booty: 2000 gold"
          money += 2000
            } 
        else if (this.Stat.HP == 0) {
          Outcome = "You lost"
          this.inCombat = false
          } else Outcome = ""
       fullDescription + "\n" + Outcome
      }
      else "Are you trying to attack a ghost or something? If so, I am worried about your credibility to be a King :/"
  }
  
  def retreat =
  {
    if (this.inCombat && this.money >= 1000)
    {
      this.inCombat = false
      this.money = math.max(this.money - 1000,0)
      "Being a wise Leader, knowing your fate, you decided to RETREAT!!!"
    } else if (this.inCombat && this.money < 1000) "You don't have enough money to retreat. FIGHT TILL DEATH!!!" 
      else "Before you retreat from a battle, be in a battle first"
  }
  
  def command = "name (A name you like) \ngo (direction); rest; quit \nexamine itemname; inventory \nequip itemname; use itemname \nbuy itemname quantity; sell itemname quantity \nmoney; statistic \ntrade; build; upgrade \nfight; attack; retreat \nREFER TO THE INSTRUCTION.PDF FOR DETAILED INSTRUCTION"
}


