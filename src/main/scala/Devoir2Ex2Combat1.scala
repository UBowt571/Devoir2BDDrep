import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer
import scala.util.Random      // Not yet used


object Devoir2Ex2Combat1  extends  App {

  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[*]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")
  class monster(name:String,var AC:Int,var hp:Int,var altitude:Int){      // Very main class for interaction between every entity
    override def toString: String = {s"name : $name  =  (AC : $AC, HP : $hp, altitude : $altitude)"}
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

  class edge(val src:monster,val dst:monster,var distance:Int)

  def init()={
    var enemies = new ListBuffer[monster]
    for(i <- 0 until 9){
      enemies += new worgRider("worgRider"+(i+1))
    }
    for(i <- 0 until 4){
      enemies += new doubleAxeFury("doubleAxeFury"+(i+1))
    }
    enemies += warlord

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