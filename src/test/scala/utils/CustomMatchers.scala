package sisgrana
package utils

import org.scalatest.matchers.{BeMatcher, MatchResult}

trait CustomMatchers {
  class SubsetOfMatcher[A](superset: Set[A]) extends BeMatcher[Set[A]] {
    override def apply(testedSet: Set[A]): MatchResult =
      MatchResult(
        testedSet `subsetOf` superset,
        s"$testedSet is not a subset of $superset",
        s"$testedSet is a subset of $superset",
      )
  }

  def subsetOf[A](superset: Set[A]) = new SubsetOfMatcher[A](superset)
}

object CustomMatchers extends CustomMatchers
