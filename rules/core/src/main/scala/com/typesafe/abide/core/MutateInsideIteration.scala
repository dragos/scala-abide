package com.typesafe.abide.core

import scala.tools.abide.traversal.ScopingRule
import scala.tools.abide.Context

/**
 * Flag calls to methods that mutate the underlying collection while iterating over
 * the same collection.
 *
 * {{{
 *   val xs = ArrayBuffer(...)
 *
 *   for (x <- xs) xs -= x
 *   xs foreach { x => xs -= x }
 * }}}
 *
 * @note It doesn't do any points-to analysis, so it will only catch cases where
 *       the collection is referred by the same name in both the call to the iteration
 *       method and the call to the mutation method.
 * @see  https://github.com/scala/scala/pull/3911#issuecomment-57312576 for an example in
 *       the wild (scala compiler codebase)
 */
class MutateInsideIteration(val context: Context) extends ScopingRule {
  import context.universe._

  val name = "MutateInsideIteration"
  type Owner = Tree
  case class Warning(pos: Position, message: String, tree: Tree) extends RuleWarning

  private lazy val MutableTrait: ClassSymbol = rootMirror.getClassByName(TypeName("scala.Mutable"))

  private def isMutable(tpe: Type) = {
    tpe.baseType(MutableTrait) ne NoType
  }

  val iterationMethods: Set[Name] =
    Set("foreach", "filter", "withFilter", "filterNot", "exists", "forall", "map", "flatMap").map(TermName.apply)

  val mutationMethods: Set[Name] =
    Set("-=", "--=", "remove", "clear", "+=", "+=:", "++=",
        "append", "appendAll", "drop", "dropRight", "insert", "prepend", "prependAll").map(TermName.apply)

  val step: PartialFunction[Tree, Unit] = {
    case t @ q"$qual.$name[..$_](..$_)" if isMutable(qual.tpe) =>
      if (iterationMethods(name.decodedName))
        enter(qual)
      else if (mutationMethods(name.decodedName) && state.in(_.equalsStructure(qual)))
        nok(Warning(t.pos, s"Mutation inside iteration over $qual", qual))
  }
}
