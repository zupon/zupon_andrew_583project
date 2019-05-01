package edu.arizona.cs
import org.scalatest.FunSuite

import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import scala.collection.immutable._
import java.io.File
import util.control.Breaks._

import org.clulab.processors.corenlp.CoreNLPProcessor

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}



// GOOGLE DRIVE LINKS TO INDEXES, JUST TO BE SAFE
// unzip these and put them in "src/main/resources"
//
// STEMS:  https://drive.google.com/open?id=1VSgYDaGKwO8ajUY1uANaUoGubNhMKEz_
// LEMMAS:  https://drive.google.com/open?id=1opI_Nt6FvS68FO7vd5d6tWF3VuhZjX6-
// PLAIN:  https://drive.google.com/open?id=1-gkRA4fZERAt8zR6DqLO8VgbzlrPN7hA

class QueryEngineTest extends FunSuite{

  // preprocess the questions
  val questionsPath = "./src/test/resources/questions.txt"
  val source: BufferedSource = Source.fromFile(questionsPath)
  val lines: List[String] = source.getLines.toList
  val lines2: List[String] = lines.mkString("\n").split("\n\n").toList
  var questions: List[List[String]] = List[List[String]]()
  for(line <- lines2) {
    val question = line.split("\n").toList
    questions = questions :+ question
  }

  def PrintIfHits(ans: ListBuffer[ResultClass]): Unit = {
    // prints results if they exist, prints "no hits" if there are no results
    if (ans.nonEmpty) {
      var i = 1
      for (x <- ans.slice(0,10)) {
        println("Hit " + i + ": " + "\tDocName: " + x.DocName.get("docid") + "\tDocScore: " + x.doc_score)
        i += 1
      }
    }
    else println("No hits!")
  }

  def compareWithAnswer(query: List[String], answer: List[String], result: ListBuffer[ResultClass], model: String): Int = {
    var correct = 0
    if (result.nonEmpty) {
      val selection = result(0).DocName.get("docid")
      if (answer.contains(selection.toLowerCase())) {
        correct += 1
      }
    }
    correct
    }


  def compareWithAnswerImproved(query: List[String], answer: List[String], result: ListBuffer[ResultClass], model: String): Int = {
    var correct = 0
    val proc: CoreNLPProcessor = new CoreNLPProcessor()
    val queryDoc = proc.mkDocument(query.mkString(" "))
    var queryWords = List[String]()
    for (sent <- queryDoc.sentences) {
      val docTokens = sent.words.toList
      queryWords = queryWords ++ docTokens
    }
    var solution = List[String]()
    if (result.nonEmpty) {
      var i = 0
      while(solution.isEmpty) {
        val selection = result(i).DocName.get("docid")
        // process doc to get POS tags
        val doc = proc.mkDocument(selection)
        proc.tagPartsOfSpeech(doc)
        var selectionWords = List[String]()
        var selectionTags = List[String]()
        for (sent <- doc.sentences) {
          val docTokens = sent.words.toList
          selectionWords = selectionWords ++ docTokens
          val docTags = sent.tags.head.toList
          selectionTags = selectionTags ++ docTags
        }
        // check overlap b/w query and selected result, reject if too much overlap
        var overlap = 0
        for (word <- selectionWords) {
          val wordTag = selectionTags(selectionWords.indexOf(word))
          if ((queryWords.mkString(" ").toLowerCase().split(" ") contains word.toLowerCase()) && (wordTag.slice(0,2) == "NN" || wordTag.slice(0,2) == "VB" || wordTag.slice(0,2) == "CD")) {
            overlap += 1
          }
        }
        if (overlap < 1) {
          solution = solution :+ selection.toLowerCase()
        }
        else {
          i += 1
        }
      }
    }
    if (answer.contains(solution.mkString(""))) {
      correct += 1
    }
    correct
  }


  test("QueryEngine.Q1") {

    // for storing naive results
    var correctLemmas = 0
    var correctStems = 0
    var correctPlain = 0

    var correctLemmasClassic = 0
    var correctStemsClassic = 0
    var correctPlainClassic = 0

    // for storing improved results
    var correctLemmasImproved = 0
    var correctStemsImproved = 0
    var correctPlainImproved = 0

    var correctLemmasClassicImproved = 0
    var correctStemsClassicImproved = 0
    var correctPlainClassicImproved = 0

    // initialize query engines for different index searches
//    val objQueryEngineLemmas: QueryEngine = new QueryEngine(lemmatized="lemmas")
    val objQueryEngineStems: QueryEngine = new QueryEngine(lemmatized="stems")
//    val objQueryEnginePlain: QueryEngine = new QueryEngine(lemmatized="plain")

    val proc: CoreNLPProcessor = new CoreNLPProcessor()

    for (query <- questions) {
      val text = query(1).split(" ").toList
      val category = query(0).replaceAll("\\(Alex\\:","").split(" ").toList
      val textAndCategory = text++category

      val doc = proc.mkDocument(textAndCategory.mkString(" "))
      proc.tagPartsOfSpeech(doc)
      var queryWords = List[String]()
      var queryTags = List[String]()
      for (sent <- doc.sentences) {
        val docTokens = sent.words.toList
        queryWords = queryWords ++ docTokens
        val docTags = sent.tags.head.toList
        queryTags = queryTags ++ docTags
      }
      // for improved retrieval, only search subset of query words with "useful" POS tags
      var searchWords = List[String]()
      for (word <- queryWords) {
        val wordTag = queryTags(queryWords.indexOf(word))
        if (wordTag.slice(0,2) == "NN" || wordTag.slice(0,2) == "VB" || wordTag.slice(0,2) == "CD"|| wordTag.slice(0,2) == "JJ" || wordTag.slice(0,2) == "RB" || wordTag.slice(0,2) == "FW") {
          searchWords = searchWords:+ word
        }
      }
      val answer = query(2).toLowerCase().split("\\|").toList // correct answer

      //
      // here is where different searches will be carried out; uncomment to run them
      //

//      println("\nQuery:\t"+textAndCategory.mkString(" "))
//      println("Correct answer:\t"+answer.mkString(" OR ")+"\n")

      //
      // USING NAIVE RANKING
      //
      // with default Lucene similarity (BM25)
      //
//      println("Using BM25 similarity:")
      val ansStems = objQueryEngineStems.runQ(textAndCategory)
      correctStems += compareWithAnswer(textAndCategory,answer,ansStems,"stems")
//      val ansLemmas = objQueryEngineLemmas.runQ(searchWords)
//      correctLemmas += compareWithAnswer(textAndCategory,answer,ansLemmas,"lemmas")
//      val ansPlain = objQueryEnginePlain.runQ(textAndCategory)
//      correctPlain += compareWithAnswer(textAndCategory,answer,ansPlain,"plain")
      //
      // with modified BM25 Similarity
      //
//      println("Using modified BM25 Similarity:")
      val ansStemsClassic = objQueryEngineStems.runQclassic(textAndCategory)
      correctStemsClassic += compareWithAnswer(textAndCategory,answer,ansStemsClassic,"stems")
//      val ansLemmasClassic = objQueryEngineLemmas.runQclassic(textAndCategory)
//      correctLemmasClassic += compareWithAnswer(textAndCategory,answer,ansLemmasClassic,"lemmas")
//      val ansPlainClassic = objQueryEnginePlain.runQclassic(textAndCategory)
//      correctPlainClassic += compareWithAnswer(textAndCategory,answer,ansPlainClassic,"plain")

      //
      // USING IMPROVED RANKING
      //
      // with default Lucene similarity (BM25)
      //
//      println("Using BM25 similarity:")
      val ansStemsImproved = objQueryEngineStems.runQ(searchWords)
      correctStemsImproved += compareWithAnswerImproved(searchWords,answer,ansStemsImproved,"stems")
//      val ansLemmasImproved = objQueryEngineLemmas.runQ(searchWords)
//      correctLemmasImproved += compareWithAnswerImproved(searchWords,answer,ansLemmasImproved,"lemmas")
//      val ansPlainImproved = objQueryEnginePlain.runQ(searchWords)
//      correctPlainImproved += compareWithAnswerImproved(searchWords,answer,ansPlainImproved,"plain")
//      //
//      // with modified BM25 Similarity
//      //
//      println("Using modified BM25 similarity:")
      val ansStemsClassicImproved = objQueryEngineStems.runQclassic(searchWords)
      correctStemsClassicImproved += compareWithAnswerImproved(searchWords,answer,ansStemsClassicImproved,"stems")
//      val ansLemmasClassicImproved = objQueryEngineLemmas.runQclassic(searchWords)
//      correctLemmasClassicImproved += compareWithAnswerImproved(searchWords,answer,ansLemmasClassicImproved,"lemmas")
//      val ansPlainClassicImproved = objQueryEnginePlain.runQclassic(searchWords)
//      correctPlainClassicImproved += compareWithAnswerImproved(searchWords,answer,ansPlainClassicImproved,"plain")
    }

    //
    println("\n--NAIVE RANKING RESULTS--")
    //
    println("BM25 SIMILARITY")
    println("Accuracy with stems:\t"+correctStems.toFloat/100)
//    println("Accuracy with lemmas:\t"+correctLemmas.toFloat/100)
//    println("Accuracy with plain:\t"+correctPlain.toFloat/100)
//
    println("MODIFIED BM25 SIMILARITY")
    println("Accuracy with stems:\t"+correctStemsClassic.toFloat/100)
//    println("Accuracy with lemmas:\t"+correctLemmasClassic.toFloat/100)
//    println("Accuracy with plain:\t"+correctPlainClassic.toFloat/100)

    //
    println("\n--IMPROVED RANKING RESULTS--")
    //
    println("BM25 SIMILARITY")
    println("Accuracy with stems:\t"+correctStemsImproved.toFloat/100)
//    println("Accuracy with lemmas:\t"+correctLemmasImproved.toFloat/100)
//    println("Accuracy with plain:\t"+correctPlainImproved.toFloat/100)
//
    println("MODIFIED BM25 SIMILARITY")
    println("Accuracy with stems:\t"+correctStemsClassicImproved.toFloat/100)
//    println("Accuracy with lemmas:\t"+correctLemmasClassicImproved.toFloat/100)
//    println("Accuracy with plain:\t"+correctPlainClassicImproved.toFloat/100)
  }
}


// k = 1.2, b = 0.1
//NAIVE RANKING RESULTS
//
//BM25 SIMILARITY
//Accuracy with lemmas:   0.16
//Accuracy with stems:    0.18
//Accuracy with plain:    0.15
//
//MODIFIED BM25 SIMILARITY
//Accuracy with lemmas:   0.21
//Accuracy with stems:    0.39
//Accuracy with plain:    0.21
//
//IMPROVED RANKING RESULTS
//
//BM25 SIMILARITY
//Accuracy with lemmas:   0.19
//Accuracy with stems:    0.25
//Accuracy with plain:    0.21
//
//MODIFIED BM25 SIMILARITY
//Accuracy with lemmas:   0.32
//Accuracy with stems:    0.43
//Accuracy with plain:    0.34

