// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package transform

import scala.collection.immutable.Seq
import lang.Trees._
import lang.Identifiers._
import lang.Types.{FinitePrecisionType, MatrixType, RealType, TypeTree, VectorType}
import lang.Extractors._

/**
  Transforms the code into SSA-like form.

  Note:
    - does the transformation for all functions
    - only puts arithmetic in SSA form
    - the order of computation is kept

  Prerequisites:
    None
 */
object TACTransformerPhase extends DaisyPhase {
  override val name = "TAC transformer"
  override val description = "Three-address form transformers transforms the function bodies into SSA form."
  override implicit val debugSection = DebugSectionTransform

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    // need to replace function bodies, create a copy of the whole program
    val fncsToConsider = if (ctx.hasFlag("approx")) functionsToConsider(ctx, prg).filter(_.returnType == RealType)
    else functionsToConsider(ctx, prg)
    val newDefs = fncsToConsider.map(fnc => fnc.copy(body = Some(toSSA(fnc.body.get))))
    (ctx, Program(prg.id, newDefs ++ functionsToConsider(ctx, prg).diff(fncsToConsider)))
  }

  def freshLiteral(tp: TypeTree = RealType): Variable = {
    val t1 = FreshIdentifier("_tmp", tp, true)
    tp match {
      case RealType => Variable(t1)
      case FinitePrecisionType(_) => Variable(t1)
      case VectorType(_) => VectorLiteral(t1)
      case MatrixType(_) => MatrixLiteral(t1)
    }
  }

  def toSSA(e: Expr): Expr = e match {
    case t: Terminal => t
    // todo debug TAC on DS

    case ArithOperator(Seq(t), recons) =>
      val tree = toSSA(t)
      merge(tree, NoTree(RealType), recons)

    case n @ ArithOperator(Seq(l, r), recons) =>
      val lhs = toSSA(l)
      val rhs = toSSA(r)

      merge(lhs, rhs, recons)


    case Let(id, value, body) =>
      val ssaValue = toSSA(value)
      val ssaBody = toSSA(body)

      val tmp = ssaValue match {
        case l: Let => replaceBody(ssaValue, id, ssaBody)
        case _ =>
          Let(id, ssaValue, ssaBody)
      }

      tmp

    case IfExpr(cond, thenn, elze) =>
      // TODO: do something with the condition
      val ssaThen = toSSA(thenn)
      val ssaElse = toSSA(elze)

      IfExpr(cond, ssaThen, ssaElse)

    case x @ GreaterThan(_, _) => x
    case x @ GreaterEquals(_, _) => x
    case x @ LessThan(_, _) => x
    case x @ LessEquals(_, _) => x

    case x @ ApproxPoly(_, _, _, _) => x
  }

  private def replaceBody(expr: Expr, nextId: Identifier, nextBody: Expr): Expr = (expr: @unchecked) match {
    case Let(id, value, x @ Let(id2, value2, body)) =>
      Let(id, value, replaceBody(x, nextId, nextBody))

    case Let(id, value, body) =>
      Let(id, value,
        Let(nextId, body, nextBody))
  }

  def merge(lhs: Expr, rhs: Expr, recons: (Seq[Expr]) => Expr): Expr = (lhs, rhs) match {
    case (Let(id, v, b), _) if isSimpleExpr(b)=>
      val lit = freshLiteral(b.getType)
      Let(id, v,
        Let(lit.id, b, merge(lit, rhs, recons)))

    case (Let(id, v, b), _) =>
      Let(id, v, merge(b, rhs, recons))

    case (p1, Let(id, v, b)) if (isSimpleExpr(p1)) =>
      val lit = freshLiteral(p1.getType)
      Let(lit.id, p1,
        Let(id, v,
          merge(lit, b, recons)))

    case (t: Terminal, Let(id, v, b)) if isSimpleExpr(b) =>
      val lit = freshLiteral(b.getType)
      Let(id, v,
        Let(lit.id, b, merge(t, lit, recons)))


    case (t: Terminal, Let(id, v, b)) =>
      Let(id, v, merge(t, b, recons))

    case (t1: Terminal, NoTree(_)) => recons(Seq(t1))

    case (t1: Terminal, t2: Terminal) => recons(Seq(t1, t2))

    case (v: Terminal, p) if isSimpleExpr(p) =>
      val lit = freshLiteral(p.getType)
      Let(lit.id, p, recons(Seq(v, lit)))

    case (p, v: Terminal) if isSimpleExpr(p) =>
      val lit = freshLiteral(p.getType)
      Let(lit.id, p, recons(Seq(lit, v)))

    case (p1, p2) if (isSimpleExpr(p1) && isSimpleExpr(p2))=>
      val t1 = freshLiteral(p1.getType)
      val t2 = freshLiteral(p2.getType)
      Let(t1.id, p1,
        Let(t2.id, p2, recons(Seq(t1, t2))))
  }


  def isSimpleExpr(e: Expr): Boolean = e match {
    case ArithOperator(_, _) => true
    case _ => false
  }
}
