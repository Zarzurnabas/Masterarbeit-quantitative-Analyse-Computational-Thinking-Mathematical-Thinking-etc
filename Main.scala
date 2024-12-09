import scala.io.Source
import java.io.{File, PrintWriter}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D, Font}
import java.awt.geom.{Ellipse2D, Line2D}
import scala.math.{log, sqrt, pow}
import scala.collection.mutable
import breeze.linalg.max

object UnifiedVectorSpaceSimilarity {

  def readFileAsWords(filePath: String): List[String] = {
    val fileContent = Source.fromFile(filePath).getLines().mkString(" ")
    
    val words = fileContent.toLowerCase.split("[\\s\\p{Punct}&&[^-]]+").toList

    words.filter(_.nonEmpty)
  }

  def buildVocabulary(termFiles: Map[String, String]): Set[String] = {
    termFiles.values.flatMap { path =>
      readFileAsWords(path)
    }.toSet
  }

  def computeTermFrequency(words: List[String], vocabulary: Set[String]): Map[String, Double] = {
    val wordCount = words.groupBy(identity).view.mapValues(_.size.toDouble).toMap
    val totalWords = words.size.toDouble
    println(totalWords)
    val maxTF = (wordCount.maxBy(_._2)._2 / totalWords)
    println(maxTF)
    vocabulary.map { word =>
      word -> ((wordCount.getOrElse(word, 0.0) / totalWords) / maxTF)
    }.toMap
  }

  def computeIDF(termTFs: List[Map[String, Double]], vocabulary: Set[String]): Map[String, Double] = {
    val totalDocs = termTFs.size.toDouble
    val wordDocCount = mutable.Map[String, Int]().withDefaultValue(0)

    termTFs.foreach { tf =>
      tf.keys.foreach { word =>
        if (tf(word) > 0) wordDocCount(word) += 1
      }
    }

    vocabulary.map { word =>
      word -> log(totalDocs / (1 + wordDocCount(word))) 
    }.toMap
  }

  def computeTfIdf(tf: Map[String, Double], idf: Map[String, Double]): Map[String, Double] = {
    tf.map { case (word, tfValue) =>
      word -> (tfValue * idf.getOrElse(word, 0.0))
    }
  }

  def cosineSimilarity(vec1: Map[String, Double], vec2: Map[String, Double]): Double = {
    val a1 = vec1.values.toArray
    val a2 = vec2.values.toArray
    (for((a, b) <- a1 zip a2) yield a * b).sum / ( math.sqrt(a1.map(i => i*i).sum) * math.sqrt(a2.map(i => i*i).sum) )
  }

  def saveVectorToFile(term: String, vector: Map[String, Double], vectorType: String): Unit = {
    val writer = new PrintWriter(new File(s"filepath/${term}_$vectorType.txt"))
    writer.println(s"$vectorType Vector for '$term':")
    vector.toSeq.sortBy(_._1).foreach { case (word, weight) =>
      writer.println(f"$word%-20s : $weight%.5f")
    }
    writer.close()  
  }

  def drawOptimizedCompleteGraph(similarities: Map[(String, String), Double], fileName: String): Unit = {
    val imgSize = 500
    val img = new BufferedImage(imgSize, imgSize, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, imgSize, imgSize)

    val squareCoords = Map(
      "computational thinking" -> (imgSize * 0.25, imgSize * 0.25),
      "mathematical thinking" -> (imgSize * 0.75, imgSize * 0.25),
      "algorithmic thinking" -> (imgSize * 0.25, imgSize * 0.75),
      "algebraic thinking" -> (imgSize * 0.75, imgSize * 0.75)
    )

    similarities.foreach { case ((term1, term2), similarity) =>
      val (x1, y1) = squareCoords(term1)
      val (x2, y2) = squareCoords(term2)

      val thickness = (similarity * 30).toFloat
      g.setStroke(new BasicStroke(thickness))
      g.setColor(new Color(0, 0, (255 * similarity).toInt))
      g.draw(new Line2D.Double(x1, y1, x2, y2))
    }

    g.setColor(Color.BLACK)
    squareCoords.foreach { case (term, (x, y)) =>
      g.fill(new Ellipse2D.Double(x - 4, y - 4, 8, 8))
      g.drawString(term, x.toInt + 5, y.toInt)
    }

    g.dispose()
    javax.imageio.ImageIO.write(img, "png", new File(fileName))
  }


  def main(args: Array[String]): Unit = {
    val termFiles = Map(
      "computational thinking" -> "filepath/CT positive text.txt",
      "mathematical thinking" -> "filepath/MT positive text.txt",
      "algorithmic thinking" -> "filepath/AlgoT positive text.txt",
      "algebraic thinking" -> "filepath/AT positive text.txt"
    )

    val vocabulary = buildVocabulary(termFiles)

    val termTFs = termFiles.map { case (term, path) =>
      val words = readFileAsWords(path)
      term -> computeTermFrequency(words, vocabulary)
    }

    val idf = computeIDF(termTFs.values.toList, vocabulary)

    val termTfIdfs = termTFs.map { case (term, tf) =>
      val tfidf = computeTfIdf(tf, idf)
      
      saveVectorToFile(term, tf, "TF")
      saveVectorToFile(term, tfidf, "TF-IDF")
      
      term -> tfidf
    }

    val writer = new PrintWriter(new File("filepath/cosine similarities.txt"))
    writer.println("Cosine Similarities using Term Frequency (TF):")
    for ((term1, tf1) <- termTFs; (term2, tf2) <- termTFs if term1 < term2) {
      val similarity = cosineSimilarity(tf1, tf2)
      writer.println(s"Similarity between '$term1' and '$term2': $similarity")
    }

    writer.println("\nCosine Similarities using TF-IDF:")
    for ((term1, tfidf1) <- termTfIdfs; (term2, tfidf2) <- termTfIdfs if term1 < term2) {
      val similarity = cosineSimilarity(tfidf1, tfidf2)
      writer.println(s"Similarity between '$term1' and '$term2': $similarity")
    }

    writer.close()

    val similaritiesTF = Map(
      ("computational thinking", "mathematical thinking") -> 0.8912865754752203,
      ("algorithmic thinking", "computational thinking") -> 0.8453238877367175,
      ("algorithmic thinking", "mathematical thinking") -> 0.8823120954900217,
      ("algebraic thinking", "computational thinking") -> 0.8417088967479578,
      ("algebraic thinking", "mathematical thinking") -> 0.893125826481188,
      ("algebraic thinking", "algorithmic thinking") -> 0.8079063382622377
    )

    val similaritiesTFIDF = Map(
      ("computational thinking", "mathematical thinking") -> 0.7505433780376045,
      ("algorithmic thinking", "computational thinking") -> 0.7533423556681036,
      ("algorithmic thinking", "mathematical thinking") -> 0.7751066182505262,
      ("algebraic thinking", "computational thinking") -> 0.6585039114231004,
      ("algebraic thinking", "mathematical thinking") -> 0.6945509769648202,
      ("algebraic thinking", "algorithmic thinking") -> 0.6595547101424761
    )
    drawOptimizedCompleteGraph(similaritiesTF, "filepath/Vollständiger Graph TF.png")
    drawOptimizedCompleteGraph(similaritiesTFIDF, "filepath/Vollständiger Graph TF-IDF.png")

    println("Graphs have been generated")
  }
}





import scala.io.Source
import scala.util.matching.Regex
import java.io.PrintWriter

object Main extends App{
  ResultBuilder.repeatWordsByCount("filepath/MT positive filtered.txt",
                                   "filepath/MT positive text.txt")
}

object WordCounter {
  def count(inputFilePath: String, outputFilePath: String): Unit = {
    val fileContent = Source.fromFile(inputFilePath).getLines().mkString(" ").toLowerCase
    
    val wordPattern: Regex = "\\b[a-zA-Z]+(?:-[a-zA-Z]+)*\\b".r
    
    val words = wordPattern.findAllIn(fileContent).toList
    
    val wordCount = words.groupBy(identity).view.mapValues(_.size).toMap
    
    val resultLines = wordCount
      .toSeq
      .sortBy { case (_, count) => -count } 
      .map { case (word, count) => s"[$word][$count]" } 
    
    val writer = new PrintWriter(outputFilePath)
    try {
      resultLines.foreach(writer.println)
    } finally {
      writer.close()
    }

    println(resultLines)
    println("Extracted words: " + words.size)
  }
}

object Sorter {
  def sortFileAlphabetically(inputFilePath: String, outputFilePath: String): Unit = {
    val wordCounts = Source.fromFile(inputFilePath).getLines().flatMap { line =>
      val pattern = "\\[([^\\]]+)\\]\\[(\\d+)\\]".r
      line match {
        case pattern(word, count) => Some((word, count.toInt))
        case _ => None
      }
    }.toList

    val sortedWordCounts = wordCounts.sortBy { case (word, _) => word }

    val writer = new PrintWriter(outputFilePath)
    try {
      sortedWordCounts.foreach { case (word, count) =>
        writer.println(s"[$word][$count]")
      }
    } finally {
      writer.close()
    }

    println(s"Alphabetically sorted results written to $outputFilePath")
  }
}

object FrequencyFilter {
  def filterSingleOccurrences(inputFilePath: String, outputFilePath: String): Unit = {
    val wordCounts = Source.fromFile(inputFilePath).getLines().flatMap { line =>
      val pattern = "\\[([^\\]]+)\\]\\[(\\d+)\\]".r
      line match {
        case pattern(word, count) => Some((word, count.toInt))
        case _ => None
      }
    }.toList

    val filteredWordCounts = wordCounts.filter { case (_, count) => count > 1 }//.sortBy { case (_, count) => -count }

    val writer = new PrintWriter(outputFilePath)
    try {
      filteredWordCounts.foreach { case (word, count) =>
        writer.println(s"[$word][$count]")
      }
    } finally {
      writer.close()
    }

    println(s"Filtered results written to $outputFilePath")
  }
}

object ResultBuilder {
  def repeatWordsByCount(inputFilePath: String, outputFilePath: String): Unit = {
    val wordCounts = Source.fromFile(inputFilePath).getLines().flatMap { line =>
      val pattern = "\\[([^\\]]+)\\]\\[(\\d+)\\]".r 
      line match {
        case pattern(word, count) => Some((word, count.toInt))
        case _ => None 
      }
    }.toList

    val writer = new PrintWriter(outputFilePath)
    try {
      wordCounts.foreach { case (word, count) =>
        val repeatedWords = List.fill(count)(word).mkString(" ")
        writer.println(repeatedWords)
      }
    } finally {
      writer.close()
    }

    println(s"Words repeated by count written to $outputFilePath")
  }
}
