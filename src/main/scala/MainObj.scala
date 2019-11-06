import java.io.{File, FileNotFoundException}

import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

case class monster(var name: String, var spells: ListBuffer[spell])
case class spell(var id:Int,var name:String)

object MainObj  extends  App {

  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[*]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")

  var file = getFileFromRelativePath(1,"Devoir2BDDrep_data\\indices\\bestiary.html");
  val openingPageTag = "<div id=\"monster-index-wrapper\" class=\"index\">"
  val closingPageTag = "<div class = \"footer\">"
  var regexMatchesStart = new Regex(openingPageTag).findAllMatchIn(file)   //openingPageTag+"|"+
  if(regexMatchesStart.hasNext){
    var currentMatch:Regex.Match = regexMatchesStart.next()
    file = file.substring(currentMatch.end);
    var regexMatchEnd = new Regex(closingPageTag).findAllMatchIn(file)
    if(regexMatchEnd.hasNext){
      var nextMatch = regexMatchEnd.next()
      file = file.substring(0,nextMatch.start)
    }else{System.exit(1)}
  }else{System.exit(1)}

  var monstersLinks = getMonstersLinksFromIndex(file)
  var monstersList = new ListBuffer[monster]

  // Peut-être à mettre dans une fonction ?
  for (i <- monstersLinks.indices){
    var monsterPage = getFileFromRelativePath(1,"Devoir2BDDrep_data\\"+ monstersLinks(i))
    monstersList += getMonsters(monsterPage,monstersLinks(i))
  }

  var test = 50


  def getFileFromRelativePath(upfolder : Integer,filename : String) ={
    var path = new File("").getAbsolutePath
    for(i <- 1 to upfolder ){
      var index_val = path.lastIndexOf("\\")
      path = path.substring(0,index_val);
    }
    path +=("\\"+filename)
    var html:BufferedSource = null
    var s = ""
    try { // Dans le cas ou la page hmtl est introuvable
      html = Source.fromURL("file:///"+path)
      s = html.mkString
    } catch { case e: Exception => println("La page html est introuvable. " + e) }
    s
  }

  def getMonstersLinksFromIndex(source:String)= {
    var letter_blocks = getLetterBlock_Monster(source)
    var monster_links = new ListBuffer[String]
    for (i <- letter_blocks.indices) {
      val currentBlock = letter_blocks(i)
      val openingLineTag = "<li class[ ]*=[ ]*\"[A-z 0-9-]*\">"
      val endLineTag = "[ </a>]*</li>"
      var regexStartEndLine = new Regex(openingLineTag + "|" + endLineTag).findAllMatchIn(currentBlock)
      while(regexStartEndLine.hasNext){
        val currentMonster = currentBlock.substring(regexStartEndLine.next().end, regexStartEndLine.next().start)
        var current_monster_link = getMonsterLink(currentMonster)
        current_monster_link = current_monster_link.substring(current_monster_link.indexOf("/")+1).replace("/","\\")
        if(monster_links.isEmpty){
          monster_links+=current_monster_link
        }else if(current_monster_link!=monster_links.last){
          monster_links+=current_monster_link
        }
      }
    }
    monster_links
  }

  def getMonstersSpells(page:String) ={
    val LineOpeningTag = "<p class[ ]*=[ ]*\"stat-block-[0-9]\">"
    val LineClosingTag = "</p>"
    val linkTag = "<a href[ ]*=[ ]*\"../coreRulebook/spells/"
    var spellsList = new ListBuffer[spell]
    val LineOpeningMatches = new Regex(LineOpeningTag).findAllMatchIn(page)
    while (LineOpeningMatches.hasNext){
      val currentMatch = LineOpeningMatches.next()
      val endlineIndex = (new Regex(LineClosingTag).findAllMatchIn(page.substring(currentMatch.end)).next().start) + currentMatch.end     // Get the index of the "</p>" closing the current line. Index is from the beginning of "page"
      val lineContent = page.substring(currentMatch.end,endlineIndex)
      val spell_checker = new Regex(linkTag).findAllMatchIn(lineContent)
      while(spell_checker.hasNext){
        var spell_name = lineContent.substring(spell_checker.next().end)
        spell_name = spell_name.substring(0,spell_name.indexOf("."));
        if(spellsList.nonEmpty){
          spellsList+=spell((spellsList.last.id)+1,spell_name)
        }else{spellsList+=spell(0,spell_name)}
      }
    }
    spellsList
  }

  def handleWeirdCases(page:String,link:String): Unit ={
    var monsterID = link.substring(link.indexOf("#")+1,link.length)
    val openingTag = "<p id[ ]*=[ ]*\""+monsterID   // opening tag for a monster
  }

  def getLetterBlock_Monster(source: String) ={
    var openingTag = "<ul id=\"index-monsters-[A-z]\" title=\"[A-z] Monsters\">\n*[ \t]*<li class"
    val closingTag = "</a></li>\n*</ul>"
    var regex = new Regex(openingTag+"|"+closingTag).findAllMatchIn(source)
    var blocks = new ListBuffer[String]
    while(regex.hasNext){
      var currentMatch = regex.next()
      var nextMatch = regex.next()
      blocks+= source.substring(currentMatch.start,nextMatch.end)
    }
    blocks
  }

  def getMonsterLink(source:String):String ={
    val linkTagStart = "<a href[ ]*=[ ]*\""
    val linkTagEnd = "\">"
    val regex_link = new Regex(linkTagStart+"|"+linkTagEnd).findAllMatchIn(source)
    if(regex_link.hasNext){
      return source.substring(regex_link.next().end,regex_link.next().start)
    }
    null;
  }

  // Retourne un objet monstre, appelle getMonsterName et getMonsterSpells
  def getMonsters(source: String,link:String)={
    val monsterTag = link.substring(link.indexOf("#")+1) // On coupe au # pour avoir l'ID sur la page
    var monsterName = getMonsterName(source,monsterTag)
    var monsterSpells = new ListBuffer[spell]
    var thismonster = new monster(monsterName,monsterSpells)
    thismonster
  }

  def getMonsterName(source: String, id: String): String ={
    val regexIdandClass = "<(h[1-3]|p) id[ ]*=[ ]*\"[a-zA-Z0-9, -]*\" class[ ]*=[ ]*\"(monster-header|stat-block-title)\">"
    val regexMonsterName = "([a-zA-Z0-9-\"=,.: /]*|<b>[a-zA-Z0-9-\"=,.: /]*</b>)<"
    val regexTag = regexIdandClass + regexMonsterName
    val regex_name = new Regex(regexTag).findAllMatchIn(source)
    while(regex_name.hasNext){ // On cherche sur tous les titres de la page
      val rawName = regex_name.next().toString()
      val titleID = rawName.replaceAll(",", "") // Pour les ID à ,
      if(titleID.contains(id)) {
        return rawName.substring(rawName.lastIndexOf("\">")+2,rawName.lastIndexOf("<"))
      }

    }
    return null;
  }

}