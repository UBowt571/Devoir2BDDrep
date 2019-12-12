import java.io.PrintWriter

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
  println("Récupération des monstres sur dxcontent terminée. Début de la création de l'index inversé.")
  var listSpellsWithMonsters = matchSpellsWithMonsters(listMonsters)
  printSpellsWithMonsters(listSpellsWithMonsters)

  // FONCTIONS
  def get_n_monsters(n:Integer,first_id:Integer): ListBuffer[Monster] ={
    var listMonsters = new ListBuffer[Monster]
    val url_base = "http://www.dxcontent.com/MDB_MonsterBlock.asp?MDBID="
    // *** Si on utilise le crawler avec les pages web en local (beaucoup plus rapide) ***
    // *** MODIFIER CHEMIN - Source dans Devoir2BDDrep_data.zip ***
    //val url_base = "file:///C:/Users/Gab/IdeaProjects/Devoir2BDDrep/Devoir2BDDrep_data/monsters/MDB_MonsterBlock.asp%23003FMDBID="
    //val url_end = ".html"
    var html:BufferedSource = null
    var s = ""
    for(i <- 0 until n){
      try {
        html = Source.fromURL(url_base+(first_id+i))
        // *** Si on utilise le crawler avec les pages web en local (beaucoup plus rapide) ***
        // html = Source.fromURL(url_base+(first_id+i)+url_end)
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
      val result = new ListBuffer[(String, String)]()
      for(i <- current_monster.monsterSpells){
        result.append((i, current_monster.name))
      }
      result
    })
    val spellsReduced = monstersFlatMap.reduceByKey((spell, monsters) => spell+monsters)
    val spellsArray = spellsReduced.collect
    spellsArray
  }

  def printSpellsWithMonsters(arraySpells : Array[(String, String)]): Unit ={
      for(i <- arraySpells) {
        println(i._1 + " : " + i._2.substring(0,i._2.lastIndexOf(" ~ ")))
        println("\n")
      }
  }
}
