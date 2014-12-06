package com.typesafe.abide.core

import scala.tools.abide.traversal._
import com.typesafe.abide.core._
import scala.collection.mutable

class MutateInsideIterationTest extends TraversalTest {

  val rule = new MutateInsideIteration(context)

  "Mutation" should "be discovered in for loops" in {
    val tree = fromString("""
      import scala.collection.mutable.ArrayBuffer
      class Toto {
        def bar(): Unit = {
          val xs = ArrayBuffer(1, 2, 3)
          for (x <- xs)
            xs -= x
        }
      }
    """)

    val issues = global.ask { () => apply(rule)(tree).map(_.tree.toString) }
    issues should be(List("xs"))
  }

  "Mutation" should "be discovered in foreach" in {
    val tree = fromString("""
      import scala.collection.mutable.ArrayBuffer
      class Toto {
        def bar(): Unit = {
          val xs = ArrayBuffer(1, 2, 3)
          xs foreach { x =>
            xs -= x
          }
        }
      }
    """)

    val issues = global.ask { () => apply(rule)(tree).map(_.tree.toString) }
    issues should be(List("xs"))
  }

  "Mutation" should "be discovered in maps" in {
    val tree = fromString("""
      import scala.collection.mutable
      class Toto {
        def bar(): Unit = {
          val ys = mutable.Map(1 -> "a")

          ys foreach { case (k, v) =>
            ys -= k
            ys remove 1
          }
        }
      }
    """)

    val issues = global.ask { () => apply(rule)(tree).map(_.tree.toString) }
    issues should be(List("ys", "ys"))
  }

  "Mutation" should "be discovered when using multiple generators" in {
    val tree = fromString("""
      import scala.collection.mutable
      class Toto {
      		val icodes = mutable.Map(1 -> "a")

        def bar(): Unit = {
          val xs = mutable.ListBuffer(1)
          for ( p <- icodes; x <- xs) {
            icodes -= p._1
            xs remove x
          }
        }
      }
    """)

    val issues = global.ask { () => apply(rule)(tree).map(_.tree.toString) }
    issues should be(List("Toto.this.icodes"))
  }

}

