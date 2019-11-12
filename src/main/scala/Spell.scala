import scala.collection.mutable.ListBuffer
import scala.io.Source

class Spell(spellString:String, spellID_arg:Integer, var spell_ID:Integer = 0, var content:String = "none",
            var name: String = "none") {

  def get_content():Boolean={
    val indexOf1 =spellString.indexOf("<!-- START Spell -->")
    if(indexOf1 == -1) { content=""; return false }
    content=spellString.substring(spellString.indexOf("<!-- START Spell -->"),spellString.indexOf("<!-- END Spell -->"))
    true
  }

  def get_name(): String ={
    name = content.substring(content.indexOf("<P>")+3,content.indexOf("</p>"))
    name
  }

  get_content()
  if(content!=""){ // Si la page du sort n'est pas vide
    spell_ID = spellID_arg
    name = get_name()
  }

}