import scala.collection.mutable.ListBuffer

class Monster(monsterString:String, monsterID_arg:Integer, var monster_ID:Integer = 0, var content:String = "none",
               var name: String = "none", var monsterSpells : ListBuffer[Spell] = new ListBuffer[Spell]) {

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

  get_content()
  if(content!=""){ // Si la page du monstre n'est pas vide
    monster_ID = monsterID_arg
    name = get_name()
  }

}
