import java.io.{File, PrintWriter}

import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex


object MainObj extends  App {
  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[*]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")

  var first_id = 1
  var num_spells = 1975
  var num_monsters = 2955

  //val listSpells = get_n_spells(num_spells,first_id)
  val listMonsters = get_n_monsters(num_monsters,first_id)

  // AFFICHAGE ET DEBUGS
  val test = 0

  /*for (i <- 1 until num_spells) {
    println(listSpells(i).name)
  }*/


  // FONCTIONS

  def get_n_spells(n:Integer,first_id:Integer): ListBuffer[Spell] ={
    var listSpells = new ListBuffer[Spell]
    val url_base = "file:///C:/Users/Gab/IdeaProjects/Devoir2BDDrep_data/spells/spell_"
    var url_end = ".html"
    for(i <- 0 until n ){
      var html = Source.fromURL(url_base+(first_id+i)+url_end)
      var s = html.mkString
      var spell = new Spell(s, first_id+i)
      listSpells += spell
    }
    listSpells
  }

  def get_n_monsters(n:Integer,first_id:Integer): ListBuffer[Monster] ={
    var listMonsters = new ListBuffer[Monster]
    val url_base = "file:///C:/Users/Gab/IdeaProjects/Devoir2BDDrep_data/monsters/MDB_MonsterBlock.asp%23003FMDBID="
    var url_end = ".html"
    var html:BufferedSource = null
    var s = ""
    for(i <- 0 until n ){
      var link = url_base+(first_id+i)+url_end
      var test2 = 1
      try {
        html = Source.fromURL(url_base+(first_id+i)+url_end)
        s = html.mkString
      } catch {
        case e: Exception => println("La page html est introuvable. " + e) }
      var monster = new Monster(s, first_id+i)
      listMonsters += monster
      println(i+1)
    }
    listMonsters
  }
}
