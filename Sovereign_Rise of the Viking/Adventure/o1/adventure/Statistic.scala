package o1.adventure

class Statistic (var HP : Int, var Attack: Int, var Defense: Int, var Speed : Int, var Critical : Int) {
  override def toString = HP + " health " + ", Attack: " + this.Attack + ", Defense: " + this.Defense + ", Speed: " + Speed + ", Critical rate: " + Critical + "%"
}