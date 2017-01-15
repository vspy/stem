package com.syndicate42.stem
import scala.collection.immutable.HashSet
import scala.annotation._

object RussianStemmer {
  private final val vowels = 
    HashSet('а','е','и','о','у','ы','э','ю','я')
  
  // must follow а или я
  private final val perfectiveGerund1 = 
    for (s <- List("вшись","вши","в"); p<-List("а","я")) yield p+s 
  private final val perfectiveGerund2 = 
    List("ившись","ывшись","ивши","ывши","ив","ыв")
  private final val perfectiveGerund =  
    List.concat(perfectiveGerund1,perfectiveGerund2)

  private final val adjective = 
    List("ее","ие","ые","ое","ими","ыми","ей","ий","ый","ой","ем"  
        ,"им","ым","ом","его","ого","ему","ому","их","ых","ую","юю"
        ,"ая","яя","ою","ею" )

  // must follow а или я
  private final val participle1 = 
    for (s <- List("щ","ющ","ем","нн","вш"); p<-List("а","я")) yield p+s
  private final val participle2 = 
    List("ивш","ывш","ующ")
  
  private final val reflexive = 
    List("ся","сь")
    
  // must follow а или я
  private final val verb1 = 
    (for (s <- List("ешь","нно","ете","йте","ла","на","ли","ем"
                    ,"ло","но","ет","ют","ны","ть","й","л","н")
            ; p <- List("а","я")
          ) yield p+s)
  
  private final val verb2 = 
    List("ила","ыла","ена","ейте","уйте","ите","или","ыли","ей","уй"
          ,"ил","ыл","им","ым","ен","ило","ыло","ено","ят","ует","уют"
          ,"ит","ыт","ены","ить","ыть","ишь","ую","ю")

  private final val noun = 
    List( "а","ев","ов","ие","ье","е","иями","ями","ами","еи","ии","и"
          ,"ией","ей","ой","ий","й","иям","ям","ием","ем","ам","ом"
          ,"о","у","ах","иях","ях", "ы" ,"ь" ,"ию" ,"ью" ,"ю", "ия"
          ,"ья" ,"я")

  private final val verb2orNoun = (verb2 ::: noun)

  private final val superlative = List("ейше","ейш")
  
  private final val derivational = List("ость","ост")
  
  private def afterVowel(word:String):String = findAfterVowel(word.toList)

  @tailrec
  private def findAfter(l:List[Char])(fn:Char=>Boolean):String =
    l.headOption match {
      case Some(c) =>
        if (fn(c)) ( l.tail.foldLeft("")(_+_) )
        else findAfter(l.tail)(fn)
      case None => ""
    }

  private def findAfterVowel(l:List[Char]):String = findAfter(l)(x => vowels.contains(x))

  private def afterNotVowel(str:String):String = findAfter(str.toList)(x => !vowels.contains(x))

  private case class Forms(word:String,rv:String,r1:String,r2:String) {
    def dropLast(n:Int):Forms = {
      def _dropLast(str:String,n:Int):String = if (str.length>n) str.substring(0,str.length-n) else ""
      Forms(_dropLast(word,n),_dropLast(rv,n),_dropLast(r1,n),_dropLast(r2,n))
    }
  }
  
  def stem(word:String):String = 
    if (word.length <= 1) word
    else
  {
    /**
      * In any word, RV is the region after the first vowel, 
      * or the end of the word if it contains no vowel. 
      *
      * R1 is the region after the first non-vowel following a vowel,
      * or the end of the word if there is no such non-vowel. 
      *
      * R2 is the region after the first non-vowel following a vowel
      * in R1, or the end of the word if there is no such non-vowel. 
      * 
      *    p r o t i v o e s t e s t v e n n o m
      *         |<------       RV        ------>|
      *           |<-----       R1       ------>|
      *               |<-----     R2     ------>|
      */    
    val rv = afterVowel(word)
    val r1 = afterNotVowel(rv)
    val r2 = afterNotVowel(afterVowel(r1))
    val forms = Forms(word, rv, r1, r2)
    val s1 = 
      removeRVSuffix(forms,perfectiveGerund) match {
        case(f,true) => { f }
        case(f,false) => {
	  // Otherwise try and remove a REFLEXIVE ending
	  val rx = (removeRVSuffix(f,reflexive)._1)

	  // , and then search in turn for (1) an ADJECTIVAL, (2) a VERB or (3) a NOUN ending
          removeRVSuffix(rx,adjective) match {
            case (f,true) =>
              removeRVSuffix(f,participle1,1) match {
                case (f,true) => f
                case (f,false) => removeRVSuffix(f,participle2)._1
              }

            case (f,false) => removeRVSuffix(f,verb1,1) match {
              case (f,true) => f
              case (f,false) => removeRVSuffix(f,verb2orNoun)._1
            }
          }
        }
      }

    // If the word ends with и (i), remove it. 
    val s2 = removeWordSuffix(s1,List("и")) match { case (fs, _) => fs }
    
    // Search for a DERIVATIONAL ending in R2 (i.e. the entire ending must lie in R2),
    // and if one is found, remove it. 
    val s3 = removeR2Suffix(s2, derivational) match { case (fs, _) => fs }
    
    //(1) Undouble н (n), or, 
    //(2) if the word ends with a SUPERLATIVE ending, remove it and undouble н (n)
    //(3) if the word ends ь (') (soft sign) remove it.
    ( removeWordSuffix(s3,List("нн")) match {
        case (f,true) => f
        case (f,false) => 
          removeWordSuffix(f,superlative) match {
            case (f2,true) => removeWordSuffix(f2, List("нн"))._1
            case (f2,false) => removeWordSuffix(f2, List("ь"))._1
          }
      } ).word
  }
  
  private def removeWordSuffix(f:Forms,list:List[String], correction:Int=0): (Forms,Boolean) = 
    removeSuffix((f=>f.word),f,list,correction)
  private def removeRVSuffix(f:Forms,list:List[String], correction:Int=0): (Forms,Boolean) = 
    removeSuffix((f=>f.rv),f,list,correction)
  private def removeR1Suffix(f:Forms,list:List[String], correction:Int=0): (Forms,Boolean) = 
    removeSuffix((f=>f.r1),f,list,correction) 
  private def removeR2Suffix(f:Forms,list:List[String], correction:Int=0): (Forms,Boolean) = 
    removeSuffix((f=>f.r2),f,list,correction)

  private def removeSuffix(fn:Forms=>String,f:Forms,list:List[String],correction:Int): (Forms,Boolean) = {
    val str = fn(f)
    list.find(str.endsWith(_)) match {
      case Some(suffix) => if (suffix.length>correction) (f.dropLast(suffix.length-correction),true)
                            else (f,true)
      case None => (f,false)
    }
  }
                  
}

