package o1.adventure


/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect, 
  * parsers for such commands. An action object is immutable after creation.
  * @param input  a textual in-game command such as "go east" or "rest" */
class Action(input: String) {

  private val commandText = input.trim.toLowerCase
  private val verb        = commandText.takeWhile( _ != ' ' )
  private val modifiers   = commandText.drop(verb.length).trim

  
  /** Causes the given player to take the action represented by this object, assuming 
    * that the command was understood. Returns a description of what happened as a result 
    * of the action (such as "You go west."). The description is returned in an `Option` 
    * wrapper; if the command was not recognized, `None` is returned. */
  def execute(actor: Player) : Option[String] = {  
     
   if (!actor.inCombat) {
    if (verb == "name") {
      Some(actor.name(input.drop(verb.length).trim))
    } else if (this.verb == "go") {
      Some(actor.go(this.modifiers))
    } else if (this.verb == "rest") {
      Some(actor.rest())
    } else if (this.verb == "quit") { 
      Some(actor.quit())
    } else if (this.verb == "examine") {
      Some(actor.examine(modifiers))
    } else if (this.verb == "inventory") {
      Some(actor.inventory)
    } else if (this.verb == "equip") {
      Some(actor.equip(modifiers))
    } else if (this.verb == "use") {
      Some(actor.use(modifiers))
    } else if (this.verb == "buy") {
      var itemTier = modifiers.takeWhile( _ != ' ' )
      var itemName = modifiers.drop(itemTier.length).trim.takeWhile( _ != ' ' ).trim
      var follower = modifiers.drop(itemName.length + itemTier.length + 1).trim
      var quantity = 0
      if (follower == "") quantity = 0 else if (follower forall Character.isDigit) quantity = follower.toInt else quantity = 0
      Some(actor.buy(itemTier + " " + itemName, quantity))
    } else if (this.verb == "sell") {
      var itemTier = modifiers.takeWhile( _ != ' ' )
      var itemName = modifiers.drop(itemTier.length).trim.takeWhile( _ != ' ' ).trim
      var follower = modifiers.drop(itemName.length + itemTier.length + 1).trim
      var quantity = 0
      if (follower == "") quantity = 0 else if (follower forall Character.isDigit) quantity = follower.toInt else quantity = 0
      Some(actor.sell(itemTier + " " + itemName, quantity))
    } else if (this.verb == "money") {
      Some(actor.money.toString)
    } else if (this.verb == "fight") {
      Some(actor.fight)
    } else if (this.verb == "build") {
      Some(actor.build)
    } else if (this.verb == "upgrade") {
      Some(actor.upgrade)
    } else if (this.verb == "trade") {
      Some(actor.trade)
    } else if (this.verb == "statistic") {
      Some(actor.Stat.toString)
    } else if (this.verb == "command") {
      Some(actor.command)
    } else None
   } 
   else 
   {
     if (this.verb == "use") {
      Some(actor.use(modifiers))
    } else if (this.verb == "attack") {
      Some(actor.attack)
    } else if (this.verb == "retreat") {
      Some(actor.retreat)
    } else if (this.verb == "statistic") {
      Some(actor.Stat.toString)
    } else if (this.verb == "money") {
      Some(actor.money.toString)
    } else if (this.verb == "inventory") {
      Some(actor.inventory)
    } else if (this.verb == "command") {
      Some(actor.command)
    } else None }
    
  }


  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = this.verb + " (modifiers: " + this.modifiers + ")"  

  
}
