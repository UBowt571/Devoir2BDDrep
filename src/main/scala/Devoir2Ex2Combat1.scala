import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer
import scala.util.Random


object Devoir2Ex2Combat1  extends  App {

  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[*]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")
  var d = new Random()

  case class attack_damage(val dice_num:Int,val dice_size:Int,val attack_damage_value:Int)
  case class attack_roll(val dice_num:Int,val dice_size:Int)

  abstract class monster(val AC:Int,var hp:Int,val speed:Int,var altitude:Int){      // Very main class for interaction between every entity
    val ID:Int=monster.lastID
    monster.lastID+=1
    var melee_range:Int
    var ranged_attack_range:Int
    var melee_damage:attack_damage
    var ranged_attack_damage:attack_damage

    def melee_attack(): Unit ={}
    def ranged_attack():Unit={}

    def getAttackRoll()={
      d.nextInt(monster.ATTACK_ROLL.dice_size) * monster.ATTACK_ROLL.dice_num
    }

    override def toString: String = {s"ID : $ID  =  (AC : $AC, HP : $hp, altitude : $altitude, speed : $speed)"}
    var adjList:ListBuffer[edge] = new ListBuffer[edge]
    def findTarget(range:Int): monster ={
      var idealTarget:monster=null
      if(adjList.nonEmpty){
        val closeEnoughEnemies = adjList.filter(x => getEuclidianDist(x) <= range)                       // Check if there are enemies close enough
        if(closeEnoughEnemies.nonEmpty){
          val closestEnemy = closeEnoughEnemies.minBy(_.distance)                                 // Get closest enemy
          val closestEnemies = closeEnoughEnemies.filter(x => (x.distance <= closestEnemy.distance))

          if(closestEnemies.size>1){                                                          // If multiple enemies closest : check for lowest AC
            val lowestACEnemy = findLowestAC(closestEnemies)                                 // Get lowest AC enemy
            val lowestACEnemies = closestEnemies.filter(x =>                                 // Check if there are multiple lowestAC enemies (at same AC)
              getEnemyFromEdge(x).AC<=lowestACEnemy.AC
            )

            if(lowestACEnemies.size>1){                                                          // If multiple enemies closest : check for lowest HP
              idealTarget = findLowestHP(lowestACEnemies)                                        // Get lowest HP enemy
            }else{idealTarget = lowestACEnemy}                                                   // Check if there are multiple lowestHP enemies (at same HP)
          }else{idealTarget = getEnemyFromEdge(closestEnemy)}
        }
      }
      idealTarget
    }
    def findLowestAC(closestEnemies:ListBuffer[edge]):monster ={
      var minAC:Int=monster.MAX_AC_EVER
      var lowestAC_Ennemy:monster=null
      if(closestEnemies.size==1){return getEnemyFromEdge(closestEnemies.head)}    // Should not get in there if size has normally been checked but double check
      if(closestEnemies.size>1){
        closestEnemies.foreach(currentEdge =>{
          val currentEnnemy = getEnemyFromEdge(currentEdge)
          if(currentEnnemy.AC < minAC){
            minAC = currentEnnemy.AC            //  if currentEnemy has a AC lower than the lowest
            lowestAC_Ennemy = currentEnnemy     //  update lowestAC to current and set lowest enemy to current
          }
        })
      }
      lowestAC_Ennemy
    }
    def findLowestHP(lowestAC_Enemies:ListBuffer[edge]): monster ={
      var minHP:Int=monster.MAX_AC_EVER
      var lowestHP_Enemy:monster=null
      if(lowestAC_Enemies.size==1){getEnemyFromEdge(lowestAC_Enemies.head)}    // Should not get in there if size has normally been checked but double check
      if(lowestAC_Enemies.size>1){
        lowestAC_Enemies.foreach(currentEdge =>{
          val currentEnemy = getEnemyFromEdge(currentEdge)
          if(currentEnemy.hp < minHP){
            minHP = currentEnemy.hp            //  if currentEnemy has a HP lower than the lowest
            lowestHP_Enemy = currentEnemy     //  update lowestHP to current and set lowest enemy to current
          }
        })
      }
      lowestHP_Enemy
    }

    def getEnemyFromEdge(currentEdge:edge): monster ={
      var ennemy:monster=null
      if(currentEdge.endA.ID==this.ID){ennemy=currentEdge.endB}else{ennemy=currentEdge.endA}
      ennemy
    }

    def pow(param:Int,power:Int)={scala.math.pow(param,power)}
    def sqrt(param:Double)={scala.math.sqrt(param)}
    def getEuclidianDist(edge:edge) ={
      val distPow2 = pow(edge.distance,2)
      val altPow2 = pow(getEnemyFromEdge(edge).altitude,2)
      sqrt(distPow2 + altPow2)
    }
  }

  object monster{var lastID = 0;val MAX_AC_EVER:Int = 100000000;val DEFAULT_MELEE_RANGE = 10;val ATTACK_ROLL = attack_roll(1,20)}

  object angel_solar extends monster(44,363,50,0){   // angel solar, as stated : object bc only one angel solar
    var melee_range:Int=monster.DEFAULT_MELEE_RANGE
    var ranged_attack_range:Int=110
    var melee_damage:attack_damage = attack_damage(3,6,18)                //3d6+18
    var ranged_attack_damage:attack_damage = attack_damage(2,6,14)        //2d6+14
    def greatSword_melee()={

    }

  object angel_solar extends monster("angel_solar",44,363,0){   // angel solar, as stated : object bc only one angel solar
    def greatSword_melee()={}
    def bow_ranged()={}
    def fly()={}
  }
  class worgRider(pname:String,pAC:Int=18,pHP:Int=13,pAltitude:Int=0) extends monster(pname,pAC,pHP,pAltitude){   // worgRider class, 9 of them are instantiated later
    def battleaxe_melee()={}
    def shortbow_ranged()={}
  }
  class doubleAxeFury(pname:String,pAC:Int=17,pHP:Int=142,pAltitude:Int=0) extends monster(pname,pAC,pHP,pAltitude){  // doubleAxeFury class, 4 of them are instantiated later
    def orcDoubleAxe_melee()={}
    def compositeLongbow_ranged()={}
  }
  object warlord extends monster("warlord",27,141,0){     // warlord, as stated : object bc only one angel solar
    def throwingAxe_ranged()={}
    def flail_melee()={}
  }

  class edge(val endA:monster,val endB:monster,var distance:Int)

  def init()={
    var enemies = new ListBuffer[monster]
    for(i <- 0 until 9){
      enemies += new worgRider
    }
    for(i <- 0 until 4){
      enemies += new doubleAxeFury
    }
    enemies += warlord
    enemies
  }

    var edges = new ListBuffer[edge]
    for(i <- enemies.indices){
      edges += new edge(enemies(i),angel_solar,100)
    }
    enemies
  }

  var enemies = init()      // enemies : ListBuffer containing all "bad" monsters, enemies of the angel solar

  enemies(10).altitude = 100

  val test = 50

}