package u05lab.ex2
  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL
  trait ConferenceReviewing:
    def loadReview(article: Int, scores: Map[Question, Int]): Unit
    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
    def orderedScores(article: Int, question: Question): List[Int]
    def averageFinalScore(article: Int): Double
    def acceptedArticles(): Set[Int]
    def sortedAcceptedArticles(): List[(Int, Double)]
    def averageWeightedFinalScoreMap(): Map[Int, Double]

  object ConferenceReviewing:
    def apply(): ConferenceReviewing = new ConferenceReviewingImpl

  class ConferenceReviewingImpl private[ex2] extends ConferenceReviewing:

    import Question.*
    import ImplUtils.*

    private var data: Map[Int, LazyList[Map[Question, Int]]] = Map()
    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      data = data + data.get(article).map(v => v appended scores).map(article -> _)
        .getOrElse(article -> LazyList(scores))
    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      loadReview(article, toDataMap(relevance, significance, confidence, fin))

    override def orderedScores(article: Int, question: Question): List[Int] =
      data(article).map(m => m(question)).sorted.toList

    override def averageFinalScore(article: Int): Double =
      orderedScores(article, FINAL).sum / orderedScores(article, FINAL).length.toDouble

    override def acceptedArticles(): Set[Int] =
      data.collect{ case (i, _) if averageFinalScore(i) >= 5.0 && orderedScores(i, RELEVANCE).exists(_ >= 8) => i}.toSet

    override def sortedAcceptedArticles(): List[(Int, Double)] =
      acceptedArticles().map(a => (a, averageFinalScore(a))).toList.sortBy((_, s) => s)

    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      data.map((k, ll) => (k, ll.map(m => ws(m(CONFIDENCE),m(FINAL))).sum / ll.length.toDouble))

    override def toString: String = data.toString()

    private object ImplUtils:
      def toDataMap(rel: Int, sig: Int, con: Int, fin: Int): Map[Question, Int] =
        Map(RELEVANCE -> rel, SIGNIFICANCE -> sig, CONFIDENCE -> con, FINAL -> fin)

      def ws(con: Int, fin:Int): Double =
        con * fin / 10.0

    end ImplUtils

