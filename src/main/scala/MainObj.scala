import java.io.File

import org.apache.spark.{SparkConf, SparkContext}

import scala.io.Source


object MainObj  extends  App {

  val conf = new SparkConf()
    .setAppName("SparkTester")
    .setMaster("local[*]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")
  getFileFromRelativePath(1,"Aasimar.html");

  def getFileFromRelativePath(upfolder : Integer,filename : String) ={
    val basePath = new File("").getAbsolutePath
    var newPath = basePath.substring(0,basePath.lastIndexOf("\\"));
    newPath +=("\\"+filename)
    var html = Source.fromURL("file:///"+newPath)
    var s = html.mkString
  }
  var test = 50
}