package u05lab.ex2

import org.junit.Assert.assertEquals
import org.junit.Test

class ConferenceReviewingTest :

  val conferenceReviewing: ConferenceReviewing = ConferenceReviewing()

  conferenceReviewing.loadReview(1, 8, 8, 6, 8)
  conferenceReviewing.loadReview(1, 9, 9, 6, 9)
  conferenceReviewing.loadReview(2, 9, 9, 10, 9)
  conferenceReviewing.loadReview(2, 4, 6, 10, 6)
  conferenceReviewing.loadReview(3, 3, 3, 3, 3)
  conferenceReviewing.loadReview(3, 4, 4, 4, 4)
  conferenceReviewing.loadReview(4, 6, 6, 6, 6)
  conferenceReviewing.loadReview(4, 7, 7, 8, 7)

  import Question.*
  val scoreMap: Map[Question, Int] = Map(RELEVANCE -> 8, SIGNIFICANCE -> 8, CONFIDENCE -> 7, FINAL -> 8)
  conferenceReviewing.loadReview(4, scoreMap)
  conferenceReviewing.loadReview(5, 6, 6, 6, 10)
  conferenceReviewing.loadReview(5, 7, 7, 7, 10)


  @Test def testOrderedScores() : Unit =
    assertEquals(List(4,9), conferenceReviewing.orderedScores(2, Question.RELEVANCE))
    assertEquals(List(6,7,8), conferenceReviewing.orderedScores(4, Question.CONFIDENCE))
    assertEquals(List(10,10), conferenceReviewing.orderedScores(5, Question.FINAL))

  @Test def testAverageFinalScore(): Unit =
    assertEquals(8.5, conferenceReviewing.averageFinalScore(1),0.01)
    assertEquals(7.5, conferenceReviewing.averageFinalScore(2),0.01)
    assertEquals(3.5, conferenceReviewing.averageFinalScore(3),0.01)
    assertEquals(7.0, conferenceReviewing.averageFinalScore(4),0.01)
    assertEquals(10.0, conferenceReviewing.averageFinalScore(5),0.01)

  @Test def testAcceptedArticles(): Unit =
    assertEquals(Set(1,2,4), conferenceReviewing.acceptedArticles())

  @Test def testSortedAcceptedArticles(): Unit =
    assertEquals(List((4,7.0), (2,7.5), (1,8.5)), conferenceReviewing.sortedAcceptedArticles())

  @Test def optionalTestAverageWeightedFinalScore(): Unit =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals((4.8 + 5.4) / 2, conferenceReviewing.averageWeightedFinalScoreMap()(1),0.01)
    // e simile per gli altri
    assertEquals((9.0 + 6.0) / 2, conferenceReviewing.averageWeightedFinalScoreMap()(2),0.01)
    assertEquals((0.9 + 1.6) / 2, conferenceReviewing.averageWeightedFinalScoreMap()(3),0.01)
    assertEquals((3.6 + 5.6 + 5.6) / 3, conferenceReviewing.averageWeightedFinalScoreMap()(4),0.01)
    assertEquals((6.0 + 7.0) / 2, conferenceReviewing.averageWeightedFinalScoreMap()(5),0.01)
    assertEquals(5, conferenceReviewing.averageWeightedFinalScoreMap().size)
