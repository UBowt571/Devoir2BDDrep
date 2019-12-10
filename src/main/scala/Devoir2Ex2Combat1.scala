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

  case class attack_damage(dice_num:Int, dice_size:Int, attack_damage_value:Int)
  case class attack_roll(dice_num:Int, dice_size:Int)
  case class message(var attackerID:Int,var attackerName:String,var targetID:Int,var messageType:Int,var attackRoll:Int,var value:Int){
    override def toString:String={attackerName+" attacks enemy with ID "+targetID+" | attack_roll : "+this.attackRoll+" | value : "+value}
  }
  case class coordinates(val enemyID:Int,var distance:Int)

  abstract class monster(val AC:Int,var hp:Int,val speed:Int,var altitude:Int,var distance:Int) extends Serializable {      // Very main class for interaction between every entity
    val ID:Int=monster.lastID
    monster.lastID+=1
    var adjList:ListBuffer[edge] = new ListBuffer[edge]
    var adjListInt:ListBuffer[coordinates]= new ListBuffer[coordinates]
    val num_attacks_available:Int
    var num_attacks_realized:Int=0
    var num_melee_attack_realised:Int=0
    var num_ranged_attack_realised:Int=0
    val melee_range:Int
    val ranged_attack_range:Int
    val melee_damage:attack_damage
    val ranged_attack_damage:attack_damage
    var melee_attack_rolls:Array[Int]=_
    var ranged_attack_rolls:Array[Int]=_


    // BASIC FUNCTIONS : MAXIMUM IMPORTANCE : root of program's logic
    def play():ListBuffer[message]={
      var receivedMessages:Array[message]=null
      val messages = new ListBuffer[message]
      if(emittedMessages!=null){
        var test = 50
        receivedMessages = emittedMessages.filter(current => current.targetID==this.ID||current.targetID==monster.ID_BROADCAST)
        var test2=50
        emittedMessages = emittedMessages.filter(current=> current.targetID!=this.ID)
        var test3 = 50
        for(currentReceivedMessage <- receivedMessages){
          if(currentReceivedMessage.messageType==monster.ATTACK){
            if(currentReceivedMessage.attackRoll>this.AC){
              this.hp -= currentReceivedMessage.value
              if(this.hp <= 0){
                messages+=die()
              }
            }
          }else if(currentReceivedMessage.messageType==monster.MOVE){
            if((this.distance-currentReceivedMessage.value) >= 0) {
              this.distance -= currentReceivedMessage.value
            }
          } else if(currentReceivedMessage.messageType == monster.DIE){
            this.adjListInt = adjListInt.filter(currentADJ => currentADJ.enemyID != currentReceivedMessage.attackerID)
          }
        }
      }

        for(i <- 0 until this.num_attacks_available){
          val target_melee = findTarget(melee_range)
          val rangedAttack_target = findTarget(ranged_attack_range)
          if( target_melee != null){
            messages += attack(melee_attack_rolls(num_melee_attack_realised),target_melee.enemyID,melee_damage)
            num_attacks_realized += 1
            num_melee_attack_realised += 1
          }
          else if( rangedAttack_target != null){
            messages += attack(ranged_attack_rolls(num_ranged_attack_realised),target_melee.enemyID,melee_damage)
            num_attacks_realized += 1
            num_ranged_attack_realised += 1
          }else{
            val closestEnemy = adjListInt.minBy(current => current.distance)
            messages += message(this.ID,this.getShortClassName,closestEnemy.enemyID,monster.MOVE,0,this.speed)
          }
        }
      num_attacks_realized = 0;num_ranged_attack_realised = 0;num_melee_attack_realised = 0
      messages
    }

    /**
     *  Code mort au cas où :
     * target.hp -= damage_value
     *
     * if(target.hp <= 0){target.die(this);println(Console.CYAN +target.getShortClassName+Console.RESET + " died !")}else{println(Console.RED +target.getShortClassName+Console.RESET + " is now at "+target.hp+" hp")}
     * }else{
     * println("\n\t| "+Console.BLUE+Console.BOLD+target.getShortClassName+Console.RESET+" has "+target.AC+" AC and attack roll was :"+attack_roll_result+" : "+target.getShortClassName+" failed its attack !" )
     * }
     *
     * @param attack_roll_flat_val
     * @param target
     * @param damage_params
     * @return
     */
    def attack(attack_roll_flat_val:Int,target:Int,damage_params:attack_damage):message = synchronized {
      val attack_roll_result = getAttackRoll(attack_roll_flat_val)
      var current_message:message = null
        var damage_value=0
        for(i <- 0 until damage_params.dice_num){
          damage_value += d.nextInt(damage_params.dice_size)
        }
        damage_value += damage_params.attack_damage_value
        println(Console.RED+Console.BOLD+this.getShortClassName+Console.RESET + " attacks (melee) "+target+" damage : "+damage_value)
        current_message = message(this.ID,this.getShortClassName,target,monster.ATTACK,attack_roll_result,damage_value)
      current_message
    }

    def die()={
      message(this.ID,this.getShortClassName,monster.ID_BROADCAST,monster.DIE,0,0)
    }
    def move(target:coordinates)={
      message(this.ID,this.getShortClassName,target.enemyID,monster.MOVE,0,this.speed)
    }
    def getAttackRoll(param:Int): Int ={
      val rand_result = d.nextInt(monster.ATTACK_ROLL.dice_size)
      print("\tattack roll : "+(rand_result+param))
      if(rand_result == 20){
        println("coup critique !")
      }
      rand_result+param
    }

    // TOOL FUNCTIONS : MODERATE IMPORTANCE : toolbox for program
    def findTarget(range: Int)={
      val targets = adjListInt.filter(_.distance<=range)
      var target:coordinates=null
      if(targets.nonEmpty){target = targets.minBy(_.distance)}
      target
    }
    def findLowestAC(closestEnemies:ListBuffer[edge]):monster ={
      var minAC:Int=monster.MAX_AC_EVER
      var lowestAC_Enemy:monster=null
      if(closestEnemies.length==1){return getEnemyFromEdge(closestEnemies.head)}    // Should not get in there if size has normally been checked but double check
      if(closestEnemies.length>1){
        closestEnemies.foreach(currentEdge =>{
          val currentEnemy = getEnemyFromEdge(currentEdge)
          if(currentEnemy.AC < minAC){
            minAC = currentEnemy.AC            //  if currentEnemy has a AC lower than the lowest
            lowestAC_Enemy = currentEnemy     //  update lowestAC to current and set lowest enemy to current
          }
        })
      }
      lowestAC_Enemy
    }
    def findLowestHP(lowestAC_Enemies:ListBuffer[edge]): monster ={
      var minHP:Int=monster.MAX_AC_EVER
      var lowestHP_Enemy:monster=null
      if(lowestAC_Enemies.length==1){getEnemyFromEdge(lowestAC_Enemies.head)}    // Should not get in there if size has normally been checked but double check
      if(lowestAC_Enemies.length>1){
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

    // TOOL FUNCTIONS : LOW IMPORTANCE : toolbox functions not necessarily for this program (basic math and display functions)
    override def toString: String = {getShortClassName + "  =  (AC : "+AC+", HP : "+hp+", altitude : "+altitude+", speed : "+speed+")"}
    def getShortClassName:String={
      var result = this.getClass.getSimpleName
      val dollar_index = this.getClass.getSimpleName.lastIndexOf("$")
      if(dollar_index!= -1){
        result = result.substring(0,dollar_index)
      }
      result += " (ID : "+this.ID+")"
      result
    }
    def pow(param:Int,power:Int): Double ={scala.math.pow(param,power)}
    def sqrt(param:Double): Double ={scala.math.sqrt(param)}
    def getEuclideanDist(edge:edge): Double ={
      val distPow2 = pow(edge.distance,2)
      val altPow2 = pow(getEnemyFromEdge(edge).altitude,2)
      sqrt(distPow2 + altPow2)
    }
  }

  object monster extends Serializable {var lastID = 0;val MAX_AC_EVER:Int = 100000000;val DEFAULT_SMALL_MELEE_RANGE = 10;val DEFAULT_BIG_MELEE_RANGE = 10;val ATTACK_ROLL = attack_roll(1,20);val ATTACK = 0;val MOVE = 1;val DIE=2;val ID_BROADCAST = -1}

  class angel_solar extends monster(44,363,0,0,0){   // angel solar, as stated : object only bc only one instance
    val max_health: Int = hp
    val melee_range:Int=monster.DEFAULT_BIG_MELEE_RANGE
    val ranged_attack_range:Int=110
    val melee_damage:attack_damage = attack_damage(3,6,18)                //3d6+18
    melee_attack_rolls = Array(35,30,25,20)
    val ranged_attack_damage:attack_damage = attack_damage(2,6,14)        //2d6+14
    ranged_attack_rolls = Array(31,26,21,16)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }
  class worgRider(pDistance:Int=0,pAC:Int=18,pHP:Int=13,pSpeed:Int = 20,pAltitude:Int=0) extends monster(pAC,pHP,pSpeed,pAltitude,pDistance){   // worgRider class, 9 of them are instantiated later
    val melee_range:Int=monster.DEFAULT_SMALL_MELEE_RANGE
    val ranged_attack_range:Int=60
    val melee_damage:attack_damage = attack_damage(1,8,2)                //1d8+2
    melee_attack_rolls = Array(6)
    val ranged_attack_damage:attack_damage = attack_damage(1,6,0)        //1d6
    ranged_attack_rolls = Array(4)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }
  class doubleAxeFury(pDistance:Int=0,pAC:Int=17,pHP:Int=142,pSpeed:Int = 40,pAltitude:Int=0) extends monster(pAC,pHP,pSpeed,pAltitude,pDistance){  // doubleAxeFury class, 4 of them are instantiated later
    val melee_range:Int=monster.DEFAULT_SMALL_MELEE_RANGE
    val ranged_attack_range:Int=110
    val melee_damage:attack_damage = attack_damage(1,8,10)                //1d8+10
    melee_attack_rolls = Array(19,14,9)
    val ranged_attack_damage:attack_damage = attack_damage(1,8,6)        //1d8+6
    ranged_attack_rolls = Array(16,11,6)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }
  class warlord extends monster(27,141,30,0,0){     // warlord, as stated : object only bc only one instance
    val melee_range:Int=monster.DEFAULT_BIG_MELEE_RANGE
    val ranged_attack_range:Int=10
    val melee_damage:attack_damage = attack_damage(1,8,10)                //1d8+10
    melee_attack_rolls = Array(20,15,10)
    val ranged_attack_damage:attack_damage = attack_damage(1,6,5)        //1d6+5
    ranged_attack_rolls = Array(19)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }

  class edge (val endA:monster,val endB:monster,var distance:Int) extends Serializable

  def init()={
//    for(i <- 0 until 9){
//      monsters += new worgRider
//    }
//    for(i <- 0 until 4){
//      monsters += new doubleAxeFury
//    }
    monsters += new warlord
    monsters += new angel_solar
    val angelID = monsters.last.ID
    for(current_monster <- monsters){
      if(!current_monster.isInstanceOf[angel_solar]){
        current_monster.distance = getDist()
        current_monster.adjListInt+= coordinates(angelID,current_monster.distance)
        enemiesID+=current_monster.ID
      }else{
        for(current_sub_monster <- monsters){
          if(!current_sub_monster.isInstanceOf[angel_solar]){
            current_monster.adjListInt+=coordinates(current_sub_monster.ID,current_sub_monster.distance)
          }

        }
      }
    }
    monsters
  }

  def getDist()={
    d.nextInt(10)
  }

  var monsters = new ListBuffer[monster]                         // enemies : ListBuffer containing all "bad" monsters, enemies of the angel solar
  var enemiesID = new ListBuffer[Int]
  init()
  var enemies = new ListBuffer[monster]
  var edges = new ListBuffer[edge]
  var monstersRDD = sc.makeRDD(monsters).map(currentMonster => {
    val tuple = Tuple2(currentMonster.ID,currentMonster)
    tuple
  })

  // END OF INIT
  // START OF THE FIGHT

  println("******* start of the fight *******")
  var round_number = 0
  var emittedMessages:Array[message]=null;
 while(monstersRDD.count() > 1){
   emittedMessages = monstersRDD.flatMap(currentMonster => {
     currentMonster._2.play()
   }).collect()

  for(currentMessage<-emittedMessages){
    if(currentMessage.messageType==monster.DIE){
      monstersRDD = monstersRDD.filter(currentMonster=> currentMonster._1!=currentMessage.attackerID)
    }
  }

   println("******* end of round "+round_number+"*******")
   round_number+=1
 }
}