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
  var monstersLinks = getMonsterFromIndex(file)
  var monstersList = new ListBuffer[monster]

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

  def getMonsterFromIndex(source:String)={
    var openingTag = "<ul id=\"index-monsters-[A-z]\" title=\"[A-z] Monsters\">\n*[ \t]*<li class"
    var closingTag = "</a></li>\\n*</ul>"
    var regex = new Regex(openingTag+"|"+closingTag).findAllMatchIn(source)
    var currentSrcTexte = source.substring(regex.next().start,regex.next().end)     // Select one monster from the whole list
    var regexMonstre = new Regex("href *= *\"[a-zA-Z0-9./#-]*\"").findAllMatchIn(currentSrcTexte)   // find links in the monster's line
    var listMonsters = new ListBuffer[String];

    while (regexMonstre.hasNext) {                  // For every link of this line
      val currentMatch = regexMonstre.next()        // Current link
      val currentLinkStart = currentMatch.start     // Current link start index
      val currentLinkEnd = currentMatch.end         // Current link end index
      var currentLink = currentSrcTexte.substring(currentLinkStart,currentLinkEnd)
      val endOfHrefIndex = currentLink.indexOf("/",1)
      currentLink = currentLink.substring(endOfHrefIndex).replace("/","\\");
      if(listMonsters.nonEmpty){
        if ( ! listMonsters.last.equals(currentLink)){      // If link is identical to the last one, don't add (should be if link is already in list)
          listMonsters+=currentLink
        }
      }else{listMonsters+=currentLink}
    }
    listMonsters
  }
}