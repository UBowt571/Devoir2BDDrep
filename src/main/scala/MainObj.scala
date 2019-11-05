import java.io.File

import org.apache.spark.{SparkConf, SparkContext}
import scala.collection.mutable.ListBuffer
import scala.io.Source
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
  val closingPageTag = "</a></li>\n*</ul>\n*[ \t]*</div>"
  val testString = "<li class = \"link-book-b5 link-cr-18 link-type-plant link-climate-temperate link-size-Colossal\"><a href = \"../bestiary5/zygomind.html#zygomind\">Zygomind</a></li>\n\n</ul>\n            </div>\n        \t<div class = \"footer\">\n\t\t<p>"
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

  var monstersLinks = getMonsterFromIndex(file)
  var monstersList = new ListBuffer[monster]

  for (i <- monstersLinks.indices){
    var monsterPage = getFileFromRelativePath(1,"Devoir2BDDrep_data\\"+monstersLinks(i))
    var currentMonsters = getMonsters(monsterPage,monstersLinks(i))
//    var result = getMonsterInfoFromMonsterFile(monster,monstersLinks(i))
//    if(result != null){
//      monstersList.appendAll(result)
//    }

//    var test2=15
  }

  var test = 50


  def getFileFromRelativePath(upfolder : Integer,filename : String) ={
    var path = new File("").getAbsolutePath
    for(i <- 1 to upfolder ){
      var index_val = path.lastIndexOf("\\")
      path = path.substring(0,index_val);
    }
    path +=("\\"+filename)
    var html = Source.fromURL("file:///"+path)
    var s = html.mkString
    s
  }

  def getMonsterFromIndex(source:String)= {

    var letter_blocks = getLetterBlock_Monster(source)
    var monster_links = new ListBuffer[String]
    for (i <- letter_blocks.indices) {
      System.out.println("i : "+i)
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

//    var currentSrcTexte = source.substring(regex.next().start,regex.next().end)     // Select one monster from the whole list
//    var regexMonstre = new Regex("href *= *\"[a-zA-Z0-9./#-]*\"").findAllMatchIn(currentSrcTexte)   // find links in the monster's line
//    var listMonsters = new ListBuffer[String];

//    while (regexMonstre.hasNext) {                  // For every link of this line
//      val currentMatch = regexMonstre.next()        // Current link
//      val currentLinkStart = currentMatch.start     // Current link start index
//      val currentLinkEnd = currentMatch.end         // Current link end index
//      var currentLink = currentSrcTexte.substring(currentLinkStart,currentLinkEnd)
//      val endOfHrefIndex = currentLink.indexOf("/",1)
//      currentLink = currentLink.substring(endOfHrefIndex).replace("/","\\");
//      if(listMonsters.nonEmpty){
//        if ( ! listMonsters.last.equals(currentLink)){      // If link is identical to the last one, don't add (should be if link is already in list)
//          listMonsters+=currentLink
//        }
//      }else{listMonsters+=currentLink}
//    }
//    listMonsters
//  }

//  def getMonsterInfoFromMonsterFile(page : String,monsterLink : String):ListBuffer[monster]={
//    var monsters = new ListBuffer[monster]
//    val openingTag = "<h1 [a-zA-Z-\"= ]*>[a-zA-Z-, ]*</h1>\n\t*\n*\t*<p class[ ]*=[ ]*\"flavor-text\">"   // opening tag for a monster
//
//    var monsterStartLine = new Regex(openingTag).findAllMatchIn(page)
//    if(!monsterStartLine.hasNext){
//      return null
//      handleWeirdCases(page,monsterLink)
//    }
//
//    var nextMatch:Regex.Match = null
//    var currentMatch:Regex.Match = null
//    if(monsterStartLine.hasNext){
//      currentMatch = monsterStartLine.next()
//    }
//
//    while(monsterStartLine.hasNext){
//      var regex_nameExtracting = new Regex("<h1 [a-zA-Z-\"= ]*>|</h1>").findAllMatchIn(currentMatch.toString())
//      if(regex_nameExtracting==null){
//        println("failed to extract monster name")
//        System.exit(1)
//      }
//      var monster_name = currentMatch.toString().substring(regex_nameExtracting.next().end,regex_nameExtracting.next().start)
//      nextMatch = monsterStartLine.next()
//      var spells:ListBuffer[spell] = null
//      spells = getMonstersSpells(page.substring(currentMatch.end,nextMatch.start))
//      monsters+=monster( monster_name,spells )       // page is from current monster to next
//      currentMatch = nextMatch
//      monsters+=extractOneMonster(page,currentMatch,nextMatch,monsterStartLine)
//    }
//    var regex_nameExtracting = new Regex("<h1 [a-zA-Z-\"= ]*>|</h1>").findAllMatchIn(currentMatch.toString())
//    var monster_name:String = null
//    if(regex_nameExtracting.hasNext){
//      monster_name = currentMatch.toString().substring(regex_nameExtracting.next().end,regex_nameExtracting.next().start)
//    }else{
//      monster_name = monsterLink.substring(monsterLink.indexOf("#")+1,monsterLink.length-1)
//    }
//    monsters+=monster(monster_name,getMonstersSpells(page.substring(currentMatch.end)) )
//    monsters
//  }
//
//  def extractOneMonster(source:String,pnextMatch:Regex.Match,pcurrentMatch:Regex.Match,pmonsterStartLine:Iterator[Regex.Match],monsterLink:String) ={
//    var nextMatch = pnextMatch
//    var currentMatch = pcurrentMatch
//    var monsterStartLine = pmonsterStartLine
//
//    var monster_name:String=null
//
//    var regex_nameExtracting = new Regex("<h1 [a-zA-Z-\"= ]*>|</h1>").findAllMatchIn(currentMatch.toString())
//    if(!regex_nameExtracting.hasNext){
//      monster_name = currentMatch.toString().substring(regex_nameExtracting.next().end,regex_nameExtracting.next().start)
//    }else{
//      monster_name = monsterLink.substring(monsterLink.indexOf("#")+1,monsterLink.length-1)
//    }
//    monster_name = currentMatch.toString().substring(regex_nameExtracting.next().end,regex_nameExtracting.next().start)
//
//    nextMatch = monsterStartLine.next()
//    var spells = getMonstersSpells(source.substring(currentMatch.end,nextMatch.start))
//    var monster=monster( monster_name,spells )       // page is from current monster to next
//    currentMatch = nextMatch
//    monster:monster
//  }

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
    var test = 50;
  }

  def getLetterBlock_Monster(source: String) ={
    var openingTag = "<ul id=\"index-monsters-[A-z]\" title=\"[A-z] Monsters\">\n*[ \t]*<li class"
    val closingTag = "</a></li>\\n*</ul>"
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
    return null;
  }

  def getMonsterName(source: String): String ={
    val nameTagStart = "\">"
    val regex_name = new Regex(nameTagStart).findAllMatchIn(source)
    if(regex_name.hasNext){
      return source.substring(regex_name.next().end)
    }
    return null;
  }
  def getMonsters(source: String,link:String)={
    val monsterStatTag = link.substring(link.indexOf("#"))
    val test = 50
  }
}