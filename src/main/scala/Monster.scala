import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class Monster(monsterString:String, monsterID_arg:Integer, var monster_ID:Integer = 0, var content:String = "none",
               var name: String = "none", var monsterSpells : ListBuffer[String] = new ListBuffer[String]) {

  def get_content():Boolean={
    val indexOf1 =monsterString.indexOf("<!-- START Monster -->")
    if(indexOf1 == -1) { content=""; return false }
    content=monsterString.substring(monsterString.indexOf("<!-- START Monster -->"),monsterString.indexOf("<!-- END Monster -->"))
    true
  }

  def get_name(): String ={
    name = content.substring(content.indexOf("align=\"left\"")+28,content.indexOf("</b></td>"))
    name
  }

  def get_spells(): ListBuffer[String] ={
    var monsterSpells = new ListBuffer[String]
    if(content.contains("<div class=\"Details\">\n<p class='HangIndent'><b>Spell-Like Abilities") | content.contains("<div class=\"Details\">\n<p class='HangIndent'><b>Spells Prepared")){
      var textSpell =""
      if(content.contains("<div class=\"Details\">\n<p class='HangIndent'><b>Spell-Like Abilities")){
        textSpell = content.substring(content.indexOf("<div class=\"Details\">\n<p class='HangIndent'><b>Spell-Like Abilities"),content.indexOf("<div class=\"Sep1\"><b>STATISTICS"))
      } else if(content.contains("<div class=\"Details\">\n<p class='HangIndent'><b>Spells Prepared")){
        textSpell = content.substring(content.indexOf("<div class=\"Details\">\n<p class='HangIndent'><b>Spells Prepared"),content.indexOf("<div class=\"Sep1\"><b>STATISTICS"))
      }
      val regex = new Regex("HangIndent_2\'>[a-zA-Z0-9-\"=+,.: /]*</p>").findAllMatchIn(textSpell)
      while(regex.hasNext){ // On cherche sur tous les titres de la page
        val rawName = regex.next().toString()
        monsterSpells.append(rawName)
      }


    }
  monsterSpells
  }

  get_content()
  if(content!=""){ // Si la page du monstre n'est pas vide
    monster_ID = monsterID_arg
    name = get_name()
    monsterSpells = get_spells()
  }

}
