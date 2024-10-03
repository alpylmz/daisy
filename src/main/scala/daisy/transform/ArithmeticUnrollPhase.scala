// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package transform

import daisy.lang.Extractors._
import daisy.lang.Identifiers._
import daisy.lang.TreeOps
import daisy.lang.TreeOps.{getLastExpression, isMatrix, isVector, replace}
import daisy.lang.Trees._
import daisy.lang.Types.RealType
import daisy.tools.FinitePrecision.Precision
import daisy.tools.{DSAbstraction, Interval, Rational}

import scala.annotation.tailrec
import scala.collection.immutable.Seq

/**
 * Transforms the code by unrolling all loops over DS.
 *
 * Note:
 * - ...
 *
 * Prerequisites:
 * SpecProcessingPhase
 */
object ArithmeticUnrollPhase extends DaisyPhase {
  override val name = "Loop unrolling"
  override val description = "Unrolls all loops over data structures."
  override implicit val debugSection = DebugSectionTransform

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val uniformPrecision = ctx.option[Precision]("precision")
    // need to replace function bodies, create a copy of the whole program
    val fncsToConsider = functionsToConsider(ctx, prg)
    var specInputRanges: Map[Identifier, Map[Identifier, Interval]] = ctx.specInputRanges
    var specInputErrors: Map[Identifier, Map[Identifier, Rational]] = ctx.specInputErrors

    val newDefs = fncsToConsider.map(fnc => {
      val transformed = unrollAll(fnc.body.get, ctx.dsAbstractions(fnc.id), uniformPrecision)
      specInputRanges += (fnc.id -> (specInputRanges(fnc.id) ++ transformed._2))
      specInputErrors += (fnc.id -> (specInputErrors(fnc.id) ++ transformed._3))
      val args = transformed._2.keySet.map(ValDef).toSeq
      val realArgs = fnc.params.filter(vd => vd.getType == RealType)
      fnc.copy(body = Some(transformed._1), params = realArgs ++ args)
    })

    println("Unrolled functions:")
    newDefs.foreach(println)

    (ctx.copy(specInputRanges = specInputRanges,
      specInputErrors = specInputErrors), Program(prg.id, newDefs ++ functionsToConsider(ctx, prg).diff(fncsToConsider)))
  }


  /**
   * Unroll all loops over data structures to operations on Reals
   *
   * @param fnc FunDef of the program to be unrolled
   * @return unrolled expression [[Expr]]
   */
  def unrollAll(fnc: Expr, dsaRangeMap: Map[Expr, DSAbstraction], prec: Precision):
  (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) = {
    var addedIds: Map[(Expr, Int), Identifier] = Map()

    def rec(e: Expr): (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) = {
    val () = println(s"rec: $e")
    e match {
        // one example: xx = x * 2. i = xx, v = x * 2, b is the rest
        case Let(i, v, b) => {
            val () = println("Let")
            val () = println(s"i: $i")
            // get the type of i
            val () = println(s"i.getType: ${i.getType}")
            val () = println(s"v.getType: ${v.getType}")
            val () = println(s"v: $v")
            val () = println(s"b: $b")
            val () = println("next")
            // if my assumptions are correct, we should see this branch whenever v is a vector
            if(isVector(v)){
                // but not with an accumulator
                // I am assuming that v is relatively simple
                // It will simplify the code a lot
                rec(createUnrolledLet(i, v, b, dsaRangeMap))
            }
            else{
                val _ = rec(b)
                (e, Map(), Map())
            } 
        }
        case x => {
            val () = println(s"rec: $x")
            (x, Map(), Map())
        }
    }}

    val rec_fnc = rec(fnc)
    println("rec_fnc: " + rec_fnc._1)
    rec_fnc
  }
  // TODO don't forget to add to the context modified intermediate results/input ranges for all vector/matrix elements

  def createUnrolledLet(i: Identifier, v: Expr, b: Expr, dsaRangeMap: Map[Expr, DSAbstraction]): Expr = {
    // i size is equal to v size
    // I'll have vSize number of new identifiers, all for i_0, i_1, ... i_vSize
    v match {
        case Times(t1, t2) => {
            if(isVector(t1) == isVector(t2)){
                val () = println("This shouldn't happen!")
                val () = println("ERROR: Both are vectors or both are not vectors, not supported")
                val () = println("v: " + v)
                v
            }
            val ret = 
            if(isVector(t1)){
                val vSize = dsaRangeMap(t1).dsSize
                // vector times scalar
                // In this case, resulting let should be in this format:
                // Let(i_0, v_0 * t2, Let(i_1, v_1 * t2, ... Let(i_vSize, v_vSize * t2, b)))
                val zero = 0
                var currLet = Let(FreshIdentifier(s"$i$zero", RealType), Times(Variable(FreshIdentifier(s"q$zero")), t2), b)
                for(j <- 1 to vSize){
                    val newId = FreshIdentifier(s"$i$j", RealType)
                    val newLet = Let(newId, Times(Variable(FreshIdentifier(s"q$j")), t2), currLet)
                    currLet = newLet
                }
                currLet
            }
            else{
                v
            }
            ret

        }
    }  
  }

    ////////////////////////////def vectorScalarMult(v: Vector, s: RealLiteral): Expr = {
    ////////////////////////////    // Implement vector scalar multiplication with fold
    ////////////////////////////    val vSize = dsaRangeMap(v).dsSize
    ////////////////////////////    val accId = FreshIdentifier(s"acc$v", RealType)
    ////////////////////////////    val xId = FreshIdentifier(s"x$v", RealType)
    ////////////////////////////    val args = Seq(ValDef(accId), ValDef(xId))
    ////////////////////////////    val body = Times(Variable(accId), Variable(xId))
    ////////////////////////////    val res = unrollFoldwLetsOnVector(v, 0, vSize, s, args, body, dsaRangeMap(v), prec, addedIds)
    ////////////////////////////    addedIds ++= res._4
    ////////////////////////////    res._1
    ////////////////////////////}
////////////////////////////
    ////////////////////////////def arithOperatorHandler(e: Expr): Unit = {
    ////////////////////////////    e match{
    ////////////////////////////        case Times(t1, t2) =>
    ////////////////////////////            // need to check the types of t1 and t2
    ////////////////////////////            val isvector1 = isVector(t1)
    ////////////////////////////            val isvector2 = isVector(t2)
    ////////////////////////////            if(isvector1 && !(isvector1 && isvector2)){
    ////////////////////////////                vectorScalarMult(t1, t2)
    ////////////////////////////            }
    ////////////////////////////            else if(isvector2 && !(isvector1 && isvector2)){
    ////////////////////////////                vectorScalarMult(t2, t1)
    ////////////////////////////            }
    ////////////////////////////    }
    ////////////////////////////}


  /**
   * Unrolls map over vectors/{matrix elements} with temporary Real variables:
   * replace by nested let statements.
   *
   * @param v        vector to fold
   * @param fromInd  start index of v
   * @param toInd    end index of the unrolling on v
   * @param init     initial value for fold
   * @param args     arguments of the lambda function
   * @param body     lambda function performing the fold
   * @param dsaRange vector abstraction for ranges
   * @param prec     uniform precision for inputs
   * @return let-expression equivalent to the unrolled fold over v, intermediate ranges and errors updated with fresh variables ranges
   */
  def unrollMapwLetsOnVector(v: Expr,
                              fromInd: Int,
                              toInd: Int,
                              init: Expr,
                              args: Seq[ValDef],
                              body: Expr,
                              dsaRange: DSAbstraction,
                              prec: Precision): (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) = {
    //val accId = args.head.id
    val xId = args.head.id
    var updSpecRanges: Map[Identifier, Interval] = Map()
    var updSpecErrs: Map[Identifier, Rational] = Map()

    def getIdAndBody(ind: Int): (Identifier, Expr) = {
      val newId = FreshIdentifier(s"acc$v$ind", RealType, alwaysShowUniqueID = true)
      val eltId = FreshIdentifier(s"${v}_$ind", RealType, alwaysShowUniqueID = true)
      val newValue = replace {
        case Variable(id) if id == xId =>
          updSpecRanges += eltId -> dsaRange.at(ind).toInterval
          updSpecErrs += eltId -> prec.absRoundoff(dsaRange.at(ind).toInterval)
          Variable(eltId)
      }(body)
      (newId, newValue)
    }

    @tailrec
    def buildRecursiveLet(cInd: Int, lastID: Identifier, accBody: Expr): Expr = {
      val nextI = cInd - 1
      if (nextI < fromInd)
        accBody // acc is replaced with init, no new IDs are used
      else {
        val (newId, newBody) = getIdAndBody(nextI)
        val newLet = Let(lastID, newBody, accBody)
        buildRecursiveLet(nextI, newId, newLet)
      }
    }

    val (newId, newBody) = getIdAndBody(toInd-1)
    val unrolled = buildRecursiveLet(toInd-1, newId, newBody)
    (unrolled, updSpecRanges, updSpecErrs)
  }

  /**
   * Unrolls folds over vectors with temporary Real varibales: replace by nested let statements.
   *
   * @param v        vector to fold
   * @param fromInd  start index of v
   * @param toInd    end index of the unrolling on v
   * @param init     initial value for fold
   * @param args     arguments of the lambda function
   * @param body     lambda function performing the fold
   * @param dsaRange vector abstraction for ranges
   * @param prec     uniform precision for inputs
   * @return let-expression equivalent to the unrolled fold over v, intermediate ranges and errors updated with fresh variables ranges
   */
  def unrollFoldwLetsOnVector(v: Expr,
                              fromInd: Int,
                              toInd: Int,
                              init: Expr,
                              args: Seq[ValDef],
                              body: Expr,
                              dsaRange: DSAbstraction,
                              prec: Precision,
                              addedIds: Map[(Expr, Int), Identifier]):
  (Expr, Map[Identifier, Interval], Map[Identifier, Rational], Map[(Expr, Int), Identifier]) = {
    val accId = args.head.id
    val xId = args(1).id
    var updSpecRanges: Map[Identifier, Interval] = Map()
    var updSpecErrs: Map[Identifier, Rational] = Map()
    var moreAddedIds: Map[(Expr, Int), Identifier] = Map()

    def getIdAndBody(ind: Int): (Identifier, Expr) = {
      val newId = FreshIdentifier(s"acc$v$ind", RealType)
      val eltId = addedIds.getOrElse((v, ind), FreshIdentifier(s"${v}_$ind", RealType, alwaysShowUniqueID = true))
      if (!addedIds.contains(v, ind)) {
        moreAddedIds += (v,ind) -> eltId
      }
      val newValue = replace {
        case Variable(id) if id == accId => if (ind == fromInd) init else Variable(newId)
        case Variable(id) if id == xId =>
          updSpecRanges += eltId -> dsaRange.at(ind).toInterval
          updSpecErrs += eltId -> prec.absRoundoff(dsaRange.at(ind).toInterval)
          Variable(eltId)
      }(body)
      (newId, newValue)
    }

    @tailrec
    def buildRecursiveLet(cInd: Int, lastID: Identifier, accBody: Expr): Expr = {
      val nextI = cInd - 1
      if (nextI < fromInd)
        accBody // acc is replaced with init, no new IDs are used
      else {
        val (newId, newBody) = getIdAndBody(nextI)
        val newLet = Let(lastID, newBody, accBody)
        buildRecursiveLet(nextI, newId, newLet)
      }
    }

    val (newId, newBody) = getIdAndBody(toInd-1)
    val unrolled = buildRecursiveLet(toInd-1, newId, newBody)
    (unrolled, updSpecRanges, updSpecErrs, moreAddedIds)
  }
}
