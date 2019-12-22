import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks._


object Devoir2Ex2Combat1  extends  App {

  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[1]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")
  var d = new Random()

  case class attack_damage(dice_num:Int, dice_size:Int, attack_damage_value:Int)
  case class attack_roll(dice_num:Int, dice_size:Int)
  case class message(var attackerID:Int,var attackerName:String,var targetID:Int,var messageType:Int,var attackRoll:Int,var value:Int,var melee:Boolean){
    override def toString:String={attackerName+" attacks enemy with ID "+targetID+" | attack_roll : "+this.attackRoll+" | value : "+value}
  }
  case class coordinates(enemyID:Int, var distance:Int)

  abstract class monster(val AC:Int,var hp:Int,val speed:Int,var altitude:Int,var distance:Int) extends Serializable {      // Very main class for interaction between every entity
    val ID:Int=monster.lastID
    monster.lastID+=1
    var adjList:ListBuffer[coordinates]= new ListBuffer[coordinates]
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
      var messages = new ListBuffer[message]
      if(emittedMessages!=null){
        receivedMessages = emittedMessages.filter(current => current.targetID==this.ID||current.targetID==monster.ID_BROADCAST)
        emittedMessages = emittedMessages.filter(current=> current.targetID!=this.ID)
        for(currentReceivedMessage <- receivedMessages){
          if(currentReceivedMessage.messageType==monster.ATTACK){
            var display = Console.RED+Console.BOLD+currentReceivedMessage.attackerName+Console.RESET + " attacks "
            if(currentReceivedMessage.melee){display+="(melee) "}else{display+="(ranged) "}
            display+=Console.GREEN+Console.BOLD+this.getShortClassName+Console.RESET +" damage : "+currentReceivedMessage.value+" | ("
            if(currentReceivedMessage.attackRoll == monster.CRITICAL_HIT){display+=" critical hit ! "}else{display += "attack roll :"+currentReceivedMessage.attackRoll+" ; target AC :"+this.AC+" ; target hp : "+this.hp+")"}
            println(display)
            if(currentReceivedMessage.attackRoll>this.AC){
              this.hp -= currentReceivedMessage.value
              if(this.hp <= 0){
                messages+=die()
              }
            }
          }else if(currentReceivedMessage.messageType==monster.MOVE){
            println(Console.BLUE+Console.BOLD+currentReceivedMessage.attackerName+Console.RESET+" moves toward "+Console.BLUE+Console.BOLD+this.getShortClassName+Console.RESET+" at speed "+currentReceivedMessage.value)
            if((this.distance-currentReceivedMessage.value) >= 0) {
              this.distance -= currentReceivedMessage.value
            }
          } else if(currentReceivedMessage.messageType == monster.DIE){
            this.adjList = adjList.filter(currentADJ => currentADJ.enemyID != currentReceivedMessage.attackerID)
          }
        }
      }
        breakable{
          for(i <- 0 until this.num_attacks_available){
            if(this.hp>0){
              val target_melee = findTarget(melee_range)
              val rangedAttack_target = findTarget(ranged_attack_range)
              if( target_melee != null){
                messages += attack(melee_attack_rolls(num_melee_attack_realised),target_melee.enemyID,melee_damage,melee = true)
                num_attacks_realized += 1
                num_melee_attack_realised += 1
              }
              else if( rangedAttack_target != null){
                messages += attack(ranged_attack_rolls(num_ranged_attack_realised),rangedAttack_target.enemyID,melee_damage,melee = false)
                num_attacks_realized += 1
                num_ranged_attack_realised += 1
              }else{
                val closestEnemy = adjList.minBy(current => current.distance)
                if(closestEnemy!=null){
                  val previousDist = closestEnemy.distance
                  if(closestEnemy.distance<=this.speed){closestEnemy.distance = 0}else{closestEnemy.distance -= this.speed}
                  val distanceCrossed = previousDist - closestEnemy.distance
                  if(distanceCrossed >0){
                    messages += message(this.ID,this.getShortClassName,closestEnemy.enemyID,monster.MOVE,0,distanceCrossed,melee = false)
                    println(this.getShortClassName+" distanceCrossed : "+distanceCrossed+" enemy dist:"+closestEnemy.distance)
                  }
                }
                break
              }
            }
          }
        }
      num_attacks_realized = 0;num_ranged_attack_realised = 0;num_melee_attack_realised = 0
      messages
    }

    def attack(attack_roll_flat_val:Int,target:Int,damage_params:attack_damage,melee: Boolean):message = synchronized {
      val attack_roll_result = getAttackRoll(attack_roll_flat_val)
      var current_message:message = null
        var damage_value=0
        for(i <- 0 until damage_params.dice_num){
          damage_value += d.nextInt(damage_params.dice_size)
        }
        damage_value += damage_params.attack_damage_value
        current_message = message(this.ID,this.getShortClassName,target,monster.ATTACK,attack_roll_result,damage_value,melee)
      current_message
    }

    def die(): message ={
      message(this.ID,this.getShortClassName,monster.ID_BROADCAST,monster.DIE,0,0,melee = false)
    }
    def move(target:coordinates): message ={
      message(this.ID,this.getShortClassName,target.enemyID,monster.MOVE,0,this.speed,melee = false)
    }
    def getAttackRoll(param:Int): Int ={
      var rand_result = d.nextInt(monster.ATTACK_ROLL.dice_size)
      if(rand_result == 20){
        rand_result = monster.CRITICAL_HIT    // Critical hit !
      }
      rand_result+param
    }

    // TOOL FUNCTIONS : MODERATE IMPORTANCE : toolbox for program
    def findTarget(range: Int): coordinates ={
      val targets = adjList.filter(_.distance<=range)
      var target:coordinates=null
      if(targets.nonEmpty){
        target = targets.minBy(_.distance)
      }
      target
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
  }

  object monster extends Serializable {var lastID = 0;val MAX_AC_EVER:Int = 100000000;val DEFAULT_SMALL_MELEE_RANGE = 10;val DEFAULT_BIG_MELEE_RANGE = 10;val ATTACK_ROLL = attack_roll(1,20);val ATTACK = 0;val MOVE = 1;val DIE=2;val ID_BROADCAST: Int = -1;val CRITICAL_HIT:Int=1000}

  class angel_solar extends monster(44,363,0,0,0){
    val max_health: Int = hp
    val melee_range:Int=monster.DEFAULT_BIG_MELEE_RANGE
    val ranged_attack_range:Int=110
    val melee_damage:attack_damage = attack_damage(3,6,18)                //3d6+18
    melee_attack_rolls = Array(35,30,25,20)
    val ranged_attack_damage:attack_damage = attack_damage(2,6,14)        //2d6+14
    ranged_attack_rolls = Array(31,26,21,16)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }
  class worgRider(pDistance:Int=0,pAC:Int=18,pHP:Int=13,pSpeed:Int = 20,pAltitude:Int=0) extends monster(pAC,pHP,pSpeed,pAltitude,pDistance){
    val melee_range:Int=monster.DEFAULT_SMALL_MELEE_RANGE
    val ranged_attack_range:Int=60
    val melee_damage:attack_damage = attack_damage(1,8,2)                //1d8+2
    melee_attack_rolls = Array(6)
    val ranged_attack_damage:attack_damage = attack_damage(1,6,0)        //1d6
    ranged_attack_rolls = Array(4)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }
  class doubleAxeFury(pDistance:Int=0,pAC:Int=17,pHP:Int=142,pSpeed:Int = 40,pAltitude:Int=0) extends monster(pAC,pHP,pSpeed,pAltitude,pDistance){
    val melee_range:Int=monster.DEFAULT_SMALL_MELEE_RANGE
    val ranged_attack_range:Int=110
    val melee_damage:attack_damage = attack_damage(1,8,10)                //1d8+10
    melee_attack_rolls = Array(19,14,9)
    val ranged_attack_damage:attack_damage = attack_damage(1,8,6)        //1d8+6
    ranged_attack_rolls = Array(16,11,6)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }
  class warlord extends monster(27,141,30,0,0){
    val melee_range:Int=monster.DEFAULT_BIG_MELEE_RANGE
    val ranged_attack_range:Int=10
    val melee_damage:attack_damage = attack_damage(1,8,10)                //1d8+10
    melee_attack_rolls = Array(20,15,10)
    val ranged_attack_damage:attack_damage = attack_damage(1,6,5)        //1d6+5
    ranged_attack_rolls = Array(19)
    val num_attacks_available:Int = if(melee_attack_rolls.length>ranged_attack_rolls.length)melee_attack_rolls.length else ranged_attack_rolls.length
  }
  /**
   * Initializing the graph : creating beasts and filling "adjList" (neighbour nodes array)
   * @return
   */
  def init()={
    for(i <- 0 until 9){
      monsters += new worgRider
    }
    for(i <- 0 until 4){
      monsters += new doubleAxeFury
    }
    monsters += new warlord
    monsters += new angel_solar
    val angelID = monsters.last.ID
    for(current_monster <- monsters){
      if(!current_monster.isInstanceOf[angel_solar]){
        current_monster.distance = getDist
        current_monster.adjList+= coordinates(angelID,current_monster.distance)
      }else{
        for(current_sub_monster <- monsters){
          if(!current_sub_monster.isInstanceOf[angel_solar]){
            current_monster.adjList+=coordinates(current_sub_monster.ID,current_sub_monster.distance)
          }

        }
      }
    }
    monsters
  }

  def getDist ={
    d.nextInt(10)+400
  }

  var monsters = new ListBuffer[monster]                         // enemies : ListBuffer containing all "bad" monsters, enemies of the angel solar
  init()
  var monstersRDD = sc.makeRDD(monsters)

  // END OF INIT
  // START OF THE FIGHT

  println("******* start of the fight *******")
  var round_number = 0
  var emittedMessages:Array[message]=_
 while(monstersRDD.count() > 1){
   emittedMessages = monstersRDD.flatMap(currentMonster => {
     currentMonster.play()
   }).cache().collect()     // kinda ugly but it seems to work so...

   monstersRDD.cache()      // force apply transformations

  for(currentMessage<-emittedMessages){
    if(currentMessage.messageType==monster.DIE){
      monstersRDD = monstersRDD.filter(currentMonster=> currentMonster.ID!=currentMessage.attackerID)
    }
  }

   println("******* end of round "+round_number+"*******")
   round_number+=1
 }
}