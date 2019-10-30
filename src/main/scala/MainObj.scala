import java.io.File

import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching
import scala.util.matching.Regex

case class monster(var name: String, var spells: ListBuffer[Int])

object MainObj  extends  App {

  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[*]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")
  var shortened_file = getFileFromRelativePath(1,"Devoir2BDDrep_data\\bestiary\\angel.html");
  getMonsters(shortened_file);

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
    s = s.substring(s.indexOf("<div class = \"body\">"))
    s = s.substring(0,s.indexOf("</div>")+6)
    s
  }

  def getMonsters(page : String):ListBuffer[String]={
    var monsters = new ListBuffer[String]
    var regex = new Regex("<h1 id=\"[A-Za-z-]*\">[A-Za-z-, ]*</h1>\n\t*<p class=\"flavor-text\">|<h1 id=\"[A-Za-z-]*\">[A-Za-z-, ]*</h1>\n\t*\n\t*<p class=\"flavor-text\">").findAllMatchIn(page)

    while (regex.hasNext){
      var currentMatch = regex.next()
      if(page.substring(currentMatch.start+1).indexOf("<h1 ") == -1){
        monsters+= page.substring(currentMatch.start,page.indexOf("</div>"))
      }else{
        var nextH1Title = page.substring(currentMatch.start + 1 )
        var endofsubstr = nextH1Title.indexOf("<h1")
        monsters+= page.substring(currentMatch.start,currentMatch.start + endofsubstr - 1)
      }
    }
    monsters
  }
}