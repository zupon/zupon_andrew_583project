package edu.arizona.cs
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.search.similarities.{BM25Similarity, BooleanSimilarity, ClassicSimilarity, MultiSimilarity, Similarity, TFIDFSimilarity}
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StringField
import org.apache.lucene.document.TextField
import org.apache.lucene.index._
import org.apache.lucene.search._
import org.apache.lucene.queryparser.classic.{QueryParser, QueryParserBase}
import org.apache.lucene.store.{Directory, FSDirectory, IOContext, IndexInput, RAMDirectory}
import java.nio.file.{Path, Paths}

import org.clulab.processors.Processor
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct.DirectedGraphEdgeIterator

import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.IOException
import java.util.Scanner

import QueryEngine._
import org.apache.lucene.analysis.core.WhitespaceAnalyzer

import scala.collection.JavaConversions._
import scala.io.{BufferedSource, Source}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.immutable





object QueryEngine {

  def main(args: Array[String]): Unit = {
    try {

      val query: List[String] = List("submarine", "author")


      // use this for indexing AND to decide which index to use!
      // "stems" for default Lucene stemming w/ StandardAnalyzer
      // "lemmas" for lemmatization with CoreNLPProcessor and Lucene WhitespaceAnalyzer
      // "plain" for no stemming or lemmatization, with CoreNLPProcessor and Lucene WhitespaceAnalyzer
      val lemmatized = "stems"

      // initialize a QueryEngine object
      val objQueryEngine: QueryEngine = new QueryEngine(lemmatized)

      // check if index exists before building index
//      if(!DirectoryReader.indexExists(objQueryEngine.index)) objQueryEngine.buildIndex()

      println("Default Similarity Answer ('submarine author'):")
      val ans: ListBuffer[ResultClass] = objQueryEngine.runQ(query)
      PrintIfHits(ans)

      println("Classic Similarity Answer ('submarine author'):")
      val ans2: ListBuffer[ResultClass] = objQueryEngine.runQclassic(query)
      PrintIfHits(ans2)

    } catch {
      case ex: Exception => println(ex.getMessage)
    }
  }

  def PrintIfHits(ans: ListBuffer[ResultClass]): Unit = {
    // prints results if they exist, prints "no hits" if there are no results
    if (ans.nonEmpty) {
      var i = 1
      for (x <- ans) {
        println("Hit " + i + ": " + "\tDocName: " + x.DocName.get("docid") + "\tDocScore: " + x.doc_score)
        i += 1
      }
    }
    else println("No hits!")
  }
}


class QueryEngine(lemmatized: String) {

  def addDoc(writer:IndexWriter, text:String, docid:String): Unit = {
    val doc = new Document
    doc.add(new TextField("text", text, Field.Store.YES))
    doc.add(new StringField("docid", docid, Field.Store.YES))
    writer.addDocument(doc)
  }

  val allFiles: List[File] = getListOfFiles("./src/main/resources/wiki-subset-20140602")
  val testFile: List[File] = getListOfFiles("./src/main/resources/wiki-example")

  // get a list of files in a directory, taken from Alvin Alexander's Scala Cookbook site
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }


  def processFiles(fileList:List[File]): Array[String] = {
    // process the list of files into an array of strings, where each entry is a wikipedia page

    var allFilesForProcessing: Array[String] = Array[String]()

    // regex patterns to delete from wiki dump
    val refRep = "\\[ref.*?ref\\]"
    val tplRep = "\\[\\/?tpl.*?tpl\\]"
    val fileRep = "\\[\\[File.*?\\]\\]"
    val imageRep = "\\[\\[Image.*?\\]\\]"

    for (file <- fileList) {
      val fileSource = Source.fromFile(file)
      // make an array of each file, where each entry is a document, make some regex replacements
      val fileAsArray = ("\n\n"+fileSource.getLines.mkString("\n")).replaceAll(fileRep,"").split("\n\n\\[\\[(?!(File|Image))")

      for (doc <- fileAsArray) {
        // make other regex replacements, add SEPARATORLOL between title and text of each page
        val docAsString = doc.replaceAll(refRep,"").replaceAll(tplRep, "").replaceAll(imageRep,"").replaceFirst("\\]\\]","SEPARATORLOL").replaceAll("\n"," ")
        allFilesForProcessing = allFilesForProcessing :+ docAsString
      }
    }
    println("Total number of documents in collection:\t"+allFilesForProcessing.length)
    allFilesForProcessing
  }

  // where to store the index, dependent on what type of index
  val IndexStoreDir: Path = if (lemmatized == "lemmas") {
    Paths.get("./src/main/resources/IndexLemmas")
  }
  else if (lemmatized == "stems") {
    Paths.get("./src/main/resources/IndexStems")
  }
  else {
    Paths.get("./src/main/resources/IndexPlain")
  }

  // use WhitespaceAnalyzer if "lemmas" or "plain", use StandardAnalyzer if "stems"
  val analyzer = if (lemmatized == "lemmas" || lemmatized == "plain") new WhitespaceAnalyzer() else new StandardAnalyzer()
  val index: FSDirectory = FSDirectory.open(IndexStoreDir)
  val config = new IndexWriterConfig(analyzer)
  val writer = new IndexWriter(index, config)

  // process all files, only do this if the index doesn't already exist
  val allFilesForProcessing = if(!DirectoryReader.indexExists(index)) processFiles(allFiles) else null


  def buildIndex(): Unit = {
    // build one doc in your Lucene index from each line in the input file

    val t1 = System.nanoTime    // for keeping track of how long indexing takes

    println("BUILDING INDEX...")

    // initialize a processor if we want "lemmas" or "plain", otherwise do nothing
    val proc:Processor =  if (lemmatized == "lemmas" || lemmatized == "plain") new CoreNLPProcessor() else null

    for(line <- allFilesForProcessing) {

      val pageTitle = line.split("SEPARATORLOL").head      // the title of the wikipedia page

      val pageText = line.split("SEPARATORLOL").tail.mkString(" ")      // the text of the wikipedia page

      // get lemmas if using lemmatization
      var pageTextLemmas = Array[String]()
      if (lemmatized == "lemmas") {
        val doc = proc.mkDocument(pageText)
        proc.tagPartsOfSpeech(doc)
        proc.lemmatize(doc)
        for (sentence <- doc.sentences) {
          val docLemmas = sentence.lemmas.head.mkString(" ")
          pageTextLemmas = pageTextLemmas :+ docLemmas
        }
      }

      // get just tokens, joined with spaces, if using "plain" (e.g. no lemmatization or stemming)
      var pageTextTokens = Array[String]()
      if (lemmatized == "plain") {
        val doc = proc.mkDocument(pageText)
        for (sentence <- doc.sentences) {
          val docTokens = sentence.words.mkString(" ")
          pageTextTokens = pageTextTokens :+ docTokens
        }
      }

      //
      // add docs to the index
      //
      if (lemmatized == "lemmas") {
        addDoc(writer, pageTextLemmas.mkString(" "), pageTitle)
      }
      else if (lemmatized == "plain") {
        addDoc(writer, pageTextTokens.mkString(" "), pageTitle)
      }
      else {
        addDoc(writer, pageText, pageTitle)
      }

      val duration = (System.nanoTime - t1) / 1e9d
      println("Running for "+duration)
      println(writer.numDocs+" docs indexed so far!")
    }

    println("Finished indexing with "+analyzer)
    println(writer.numDocs + " document(s) indexed overall.")
    writer.close()
  }


  def runQuery(query: String): ListBuffer[ResultClass] = {
    var doc_score_list = new ListBuffer[ResultClass]()
    // query parser
    val escapedQuery = QueryParserBase.escape(query)
    val q = new QueryParser("text", analyzer).parse(escapedQuery)
    // searcher
    val hitsPerPage = 100
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    val docs = searcher.search(q, hitsPerPage)
    val hits = docs.scoreDocs
    // add hits to ResultClass
    for (hit <- hits) {
      val docid = hit.doc
      val objResultClass: ResultClass = new ResultClass()
      objResultClass.DocName = searcher.doc(docid)
      objResultClass.doc_score = hit.score
      doc_score_list += objResultClass
    }
    doc_score_list
  }

  def runQueryClassic(query: String): ListBuffer[ResultClass] = {
    var doc_score_list = new ListBuffer[ResultClass]()
    // query parser
    val escapedQuery = QueryParserBase.escape(query)
    val q = new QueryParser("text", analyzer).parse(escapedQuery)
    // searcher
    val hitsPerPage = 100
    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    val k = 1.2
    val b = 0.1
    val similarity = new BM25Similarity(k.toFloat, b.toFloat)
    searcher.setSimilarity(similarity)
    val docs = searcher.search(q, hitsPerPage)
    val hits = docs.scoreDocs
    // add hits to ResultClass
    for (hit <- hits) {
      val docid = hit.doc
      val objResultClass: ResultClass = new ResultClass()
      objResultClass.DocName = searcher.doc(docid)
      objResultClass.doc_score = hit.score
      doc_score_list += objResultClass
    }
    doc_score_list
  }


  def runQ(query: List[String]): ListBuffer[ResultClass] = {
    val queryInput = query.mkString(" ")
//    val queryInput = """(docid:""""+query.mkString(" ")+") (text:"+query.mkString(" ")+"""")^2"""
    //    println("Query Input:\t"+queryInput)
    runQuery(queryInput)
  }

  def runQclassic(query: List[String]): ListBuffer[ResultClass] = {
    val queryInput = query.mkString(" ")
//    val queryInput = """(docid:""""+query.mkString(" ")+") (text:"+query.mkString(" ")+"""")^2"""
    //    val queryInput = """docid:""""+query.mkString(" ")+"""" OR text:""""+query.mkString(" ")+"""""""
    runQueryClassic(queryInput)
  }
}
