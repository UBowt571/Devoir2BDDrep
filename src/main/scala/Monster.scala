import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class Monster(monsterString:String, monsterID_arg:Integer, var monster_ID:Integer = 0, var content:String = "none",
               var name: String = "none", var monsterSpells : ListBuffer[String] = new ListBuffer[String])
  extends java.io.Serializable{

  def get_content():Boolean={
    val indexOf1 =monsterString.indexOf("<!-- START Monster -->")
    if(indexOf1 == -1) { content=""; return false }
    content=monsterString.substring(monsterString.indexOf("<!-- START Monster -->"),monsterString.indexOf("<!-- END Monster -->"))
    true
  }

  def get_name(): String ={
    name = content.substring(content.indexOf("align=\"left\"")+28,content.indexOf("</b></td>"))
    name+=" - "
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
      val regex = new Regex("HangIndent_2\'>[a-zA-Z0-9-\"=+,().: /]*</p>").findAllMatchIn(textSpell)
      while(regex.hasNext){ // On récupère chaque ligne de sorts pour un monstre
        var rawName = regex.next().toString()
        rawName = rawName.substring(rawName.indexOf("2'>")+3,rawName.lastIndexOf("</p"))
        monsterSpells.append(rawName)
      }
      monsterSpells = spellMonstersSeparator(monsterSpells)
    }
  monsterSpells
  }

  get_content()
  if(content!=""){ // Si la page du monstre n'est pas vide
    monster_ID = monsterID_arg
    name = get_name()
    monsterSpells = get_spells()
  }

  // Fonction qui découpe les lignes de sorts afin de retourner les noms de sorts conformément au sorts récoltés
  def spellMonstersSeparator(monsterSpells : ListBuffer[String]): ListBuffer[String] ={
    val monsterSpells2:ListBuffer[String] = new ListBuffer[String]
    for(i <- monsterSpells.indices){
      // On commence par enlever les infos sur les cooldowns des spells : "At will - 1st -"
      // ainsi que les informations entre paranthèses : spell1 (DC 12), spell2 (3)
      monsterSpells(i) = monsterSpells(i).replaceAll("Constant[ -]*|([Aa])t ([Ww])ill[ -]*|[0-9](st|nd|rd|th)[ -]*|0 [(]at will[)][ -]*|[(]*[0-9]/(day|week|month|year)[ )-]*|[ ]*[(][A-Za-z0-9.,% -|]*[)]","")
      // On gère et retire les quelques exceptions
      monsterSpells(i) = monsterSpells(i).replaceAll("any one of the following, with a maximum duration of 1 week: |APG","")
      // On détecte spell par spell qui sont séparés par ", "
      val arrayMonsterSpells = monsterSpells(i).split(", ")
      for(i <- arrayMonsterSpells.indices){
        // Un spell indiqué comme "greater spell" s'appelle en réalité "Spell, Greater"
        if(arrayMonsterSpells(i).contains("greater")){
          arrayMonsterSpells(i) = arrayMonsterSpells(i).replaceAll("greater", "")
          arrayMonsterSpells(i) = arrayMonsterSpells(i).trim // On enlève les espaces pour que ce soit correctement formalisé

          arrayMonsterSpells(i) = arrayMonsterSpells(i) + ", greater"
        }
        // On enlève les espaces au début et à la fin
        arrayMonsterSpells(i) = arrayMonsterSpells(i).trim

        monsterSpells2.append(arrayMonsterSpells(i))
      }
    }
    monsterSpells2
  }
}
