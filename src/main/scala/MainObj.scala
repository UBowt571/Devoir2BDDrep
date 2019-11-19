import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}


object MainObj extends App {
  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[*]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")

  var first_id = 1
  var num_monsters = 2955

  val listMonsters = get_n_monsters(num_monsters,first_id)
  var listSpellsWithMonsters = matchSpellsWithMonsters(listMonsters)
  printSpellsWithMonsters(listSpellsWithMonsters)

  // FONCTIONS
  def get_n_monsters(n:Integer,first_id:Integer): ListBuffer[Monster] ={
    var listMonsters = new ListBuffer[Monster]
    val url_base = "file:///C:/Users/Gab/IdeaProjects/Devoir2BDDrep_data/monsters/MDB_MonsterBlock.asp%23003FMDBID="
    val url_end = ".html"
    var html:BufferedSource = null
    var s = ""
    for(i <- 0 until n ){
      try {
        html = Source.fromURL(url_base+(first_id+i)+url_end)
        s = html.mkString
      } catch {
        case e: Exception => println("La page html est introuvable. " + e) }
      var monster = new Monster(s, first_id+i)
      listMonsters += monster
    }
    listMonsters
  }

  def matchSpellsWithMonsters(listMonsters : ListBuffer[Monster]): Array[(String, String)] ={
    val listMonstersRDD = sc.makeRDD(listMonsters)
    val monstersFlatMap = listMonstersRDD.flatMap(current_monster => {
      var result = new ListBuffer[(String, String)]()
      for(i <- current_monster.monsterSpells){
        result.append((i, current_monster.name))
      }
      result
    })
    var spellsReduced = monstersFlatMap.reduceByKey((spell, monsters) => spell+monsters)
    var spellsArray = spellsReduced.collect
    spellsArray
  }

  def printSpellsWithMonsters(arraySpells : Array[(String, String)]): Unit ={
    //new PrintWriter("res.txt"){
      for(i <- arraySpells) {
        println(i._1 + " : " + i._2.substring(0,i._2.lastIndexOf(" - ")))
        println("\n")
      }
      //close()
    //}

  }
}
