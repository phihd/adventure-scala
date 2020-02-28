package o1.adventure

sealed trait Relationship 
{
  def isAlly: Boolean
  def isEnemy: Boolean
}

class Ally extends Relationship 
{
  def isAlly = true
  def isEnemy = false
  override def toString = "An Ally of yours, here, you can trade money for army boost and sell your belongings"
}

class Enemy extends Relationship
{
  def isAlly = false
  def isEnemy = true
  override def toString = "Your enemy, conquer them to turn them into your ally"
}