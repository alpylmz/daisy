// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package transform

import daisy.lang.Extractors._
import daisy.lang.Identifiers._
import daisy.lang.TreeOps
import daisy.lang.TreeOps.{getLastExpression, isMatrix, isVector, replace}
import daisy.lang.Trees._
import daisy.lang.Types.{FinitePrecisionType, MatrixType, RealType, VectorType}
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



  def getInputDSSize(ctx: Context, fnc: FunDef, x: ValDef): Int = {
    // This function gets the size of x in the spec
    //val dsSize = ctx.dsAbstractions(fnc.id)(x).dsSize
    // iterate over dsAbstractions(fnc.id) to match expressions with x valdef
    var size = 0
    val () = println("x: " + x)
    for((k, v) <- ctx.dsAbstractions(fnc.id)){
        val () = println("k: " + k)
        val () = k match{
            case RealLiteral(r) => {
                val () = println("ERROR: Not a ValDef, but a RealLiteral")
            }
            case FinitePrecisionLiteral(r, prec, _) => {
                val () = println("ERROR: Not a ValDef, but a FinitePrecisionLiteral")
            }
            case Variable(id) => {
                if(id == x.id){
                    size = v.dsSize
                }
            }
            case _ => {
                val () = println("ERROR: Not a ValDef")
            }
        }
    }
    size
  }

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val uniformPrecision = ctx.option[Precision]("precision")
    // need to replace function bodies, create a copy of the whole program
    val fncsToConsider = functionsToConsider(ctx, prg)
    var specInputRanges: Map[Identifier, Map[Identifier, Interval]] = ctx.specInputRanges
    var specInputErrors: Map[Identifier, Map[Identifier, Rational]] = ctx.specInputErrors

    val newDefs = fncsToConsider.map(fnc => {
      val () = println("fnc: " + fnc)
      val () = println("fnc.params: " + fnc.params)
      // fnc params is just a list
      // iterate over it
      // if the element is a real, leave it alone
      // if the element is a vector, unroll it
      // if the element is a matrix, unroll it
      var unrolledParamsFlattened: List[ValDef] = List() // Terrible Solution
      val unrolledParams = fnc.params.map(vd => vd.getType match {
        case RealType => {
          unrolledParamsFlattened = unrolledParamsFlattened :+ vd
          vd
        }
        case VectorType(_) =>
          //val vSize = ctx.dsAbstractions(fnc.id)(vd.id).dsSize
          //val () = println("vSize: " + vSize)
          val () = println("ctx.dsAbstractions(fnc.id): " + ctx.dsAbstractions(fnc.id))
          val () = println("sizeofvd: " + getInputDSSize(ctx, fnc, vd))
          val vdSize = getInputDSSize(ctx, fnc, vd)
          //val () = println("ctx.dsAbstractions(fnc.id)(vd.id): " + ctx.dsAbstractions(fnc.id)(vd))
          val () = println("specInputRanges: " + specInputRanges)
          val () = println("specInputRanges(fnc.id): " + specInputRanges(fnc.id))
            val zero = 0
            var currList = List(FreshIdentifier(s"${vd.id}$zero", RealType))
            for(j <- 1 to vdSize){
                val newId = FreshIdentifier(s"${vd.id}$j", RealType)
                val newLet = FreshIdentifier(s"${vd.id}_$j")
                currList = currList :+ newId
                unrolledParamsFlattened = unrolledParamsFlattened :+ ValDef(newId)
            }
            currList
        case MatrixType(_) =>
          val mSize = specInputRanges(fnc.id)(vd.id)
            val zero = 0
            var currList = List(FreshIdentifier(s"${vd.id}$zero", RealType))
            val () = println("ERROR: Matrix unrolling not supported yet")
            for(j <- 1 to 5){
                val newId = FreshIdentifier(s"${vd.id}$j", RealType)
                val newLet = FreshIdentifier(s"${vd.id}_$j")
                currList = currList :+ newId
                unrolledParamsFlattened = unrolledParamsFlattened :+ ValDef(newId)
            }
            currList
      })
      // for some reason cannot flatten the list with flatten function

      val () = println("unrolledParams: " + unrolledParamsFlattened)
      
      val () = println("fnc.precondition: " + fnc.precondition)
      // we also need to overwrite the preconditions, again by unrolling
      val newPrecondition = 



      val transformed = unrollAll(fnc.body.get, ctx.dsAbstractions(fnc.id), uniformPrecision)
      specInputRanges += (fnc.id -> (specInputRanges(fnc.id) ++ transformed._2))
      specInputErrors += (fnc.id -> (specInputErrors(fnc.id) ++ transformed._3))
      val args = transformed._2.keySet.map(ValDef).toSeq
      val realArgs = fnc.params.filter(vd => vd.getType == RealType)
      fnc.copy(body = Some(transformed._1), params = unrolledParamsFlattened)
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
                // t1 is a Variable
                val t1Id = t1 match{
                    case Variable(id) => id
                    case _ => {
                        val () = println("ERROR: t1 is not a Variable")
                        ""
                    }
                }
                val zero = 0
                var currLet = Let(FreshIdentifier(s"$i$zero", RealType), Times(Variable(FreshIdentifier(s"$t1Id$zero")), t2), b)
                for(j <- 1 to vSize){
                    val newId = FreshIdentifier(s"$i$j", RealType)
                    val newLet = Let(newId, Times(Variable(FreshIdentifier(s"$t1Id$j")), t2), currLet)
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




}
