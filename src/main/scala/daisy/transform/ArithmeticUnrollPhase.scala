// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package transform

import daisy.lang.Extractors._
import daisy.lang.Identifiers._
import daisy.lang.TreeOps
import daisy.lang.TreeOps.{getLastExpression, isMatrix, isVector, replace}
import daisy.lang.Trees._
import daisy.lang.Types.{FinitePrecisionType, MatrixType, RealType, VectorType, TypeTree}
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

  var freshIdentifierList: List[Identifier] = List()

  def getOrCreateFreshIdentifier(name: String, tpe: TypeTree): Identifier = {
    // if the name is already in the list, return it
    // otherwise, create a new one and return it
    val id = freshIdentifierList.find(_.name == name)
    id match {
      case Some(x) => {
        val () = println("Found in the list: " + x)
        x
      }
      case None => {
        val newId = FreshIdentifier(name, tpe)
        val () = println("Not found in the list, creating a new one: " + newId)
        freshIdentifierList = freshIdentifierList :+ newId
        newId
      }
    }
  }

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

    var newspecInputRanges: Map[Identifier, Map[Identifier, Interval]] = Map()
    var newspecInputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
    val () = println("specInputRanges: " + specInputRanges)
    val newDefs = fncsToConsider.map(fnc => {
      var specInputRangeForFunc: Map[Identifier, Interval] = Map()
      var specInputErrorForFunc: Map[Identifier, Rational] = Map()
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
          //var currList = List(FreshIdentifier(s"${vd.id}_0", RealType))
          var currList = List(getOrCreateFreshIdentifier(s"${vd.id}_0", RealType))
          unrolledParamsFlattened = unrolledParamsFlattened :+ ValDef(currList(0))
          for(j <- 1 to vdSize - 1){
              val newId = getOrCreateFreshIdentifier(s"${vd.id}_$j", RealType)
              val newLet = getOrCreateFreshIdentifier(s"${vd.id}_$j", RealType)
              currList = currList :+ newId
              unrolledParamsFlattened = unrolledParamsFlattened :+ ValDef(newId)
          }
          currList
        case MatrixType(_) =>
          val mSize = specInputRanges(fnc.id)(vd.id)
            var currList = List(getOrCreateFreshIdentifier(s"${vd.id}_0", RealType))
            val () = println("ERROR: Matrix unrolling not supported yet")
            for(j <- 1 to 5){
                val newId = getOrCreateFreshIdentifier(s"${vd.id}_$j", RealType)
                val newLet = getOrCreateFreshIdentifier(s"${vd.id}_$j", RealType)
                currList = currList :+ newId
                unrolledParamsFlattened = unrolledParamsFlattened :+ ValDef(newId)
            }
            currList
      })
      // for some reason cannot flatten the list with flatten function

      val () = println("unrolledParams: " + unrolledParamsFlattened)
      
      val () = println("fnc.precondition: " + fnc.precondition)
      // we also need to overwrite the preconditions, again by unrolling
      // A typical precondition:
      // fnc.precondition: Some(((x ? -62.54) ? (x ? 15.02) ? x.(4) ? Tree? (class daisy.lang.Trees$VectorRange) ? Tree? (class daisy.lang.Trees$VectorRange) ? Tree? (class daisy.lang.Trees$VectorRange) ? Tree? (class daisy.lang.Trees$VectorRange)))
      // I'll only use VectorRange, others seems useless
      var newPreconditionFlattened: List[Expr] = List()
      val newPrecondition = fnc.precondition.map(precond => precond match{
        case And(exprs) => {
          val () = println("exprs: " + exprs)
          exprs.map(expr => expr match{
            case VectorRange(v, fromInd, toInd, lb, ub) =>
              val () = println("expr: " + expr)
              if(fromInd != toInd){
                val () = println("ERROR: fromInd and toInd should be the same, the other case is not supported")
              }
              val vId = v match{
                case VectorLiteral(id) => id
                case _ => {
                    val () = println("ERROR: Not a VectorLiteral")
                    ""
                }
              }
              val unrollId = getOrCreateFreshIdentifier(s"${vId}_$fromInd", RealType)
              val () = println("unrollId: " + unrollId)
              newPreconditionFlattened = newPreconditionFlattened :+ GreaterThan(Variable(unrollId), lb)
              newPreconditionFlattened = newPreconditionFlattened :+ LessThan(Variable(unrollId), ub)
              specInputRangeForFunc = specInputRangeForFunc + (unrollId -> Interval(lb.value, ub.value))
              specInputErrorForFunc = specInputErrorForFunc + (unrollId -> uniformPrecision.absRoundoff(Interval(lb.value, ub.value)))
              val () = println("specInputRangeForFunc: " + specInputRangeForFunc)
            case _ => {
                val () = println("ERROR: Not a VectorRange")
                val () = println("the type is: " + precond.getClass)
                val () = println("expr: " + expr)
            }
          })  
        }

      })
      //val () = println("newPrecondition: " + newPreconditionFlattened)
      fnc.precondition = Some(And(newPreconditionFlattened))
      val () = println("fnc.precondition: " + fnc.precondition)
      val () = println("newspecInputRanges: " + newspecInputRanges)
      val () = println("specInputRangeForFunc: " + specInputRangeForFunc)
      val () = println("specInputErrors: " + specInputErrors)
      val () = println("specInputErrorForFunc: " + specInputErrorForFunc)
      val () = println("newspecInputErrors: " + newspecInputErrors)

      newspecInputRanges = newspecInputRanges + (fnc.id -> specInputRangeForFunc)
      newspecInputErrors = newspecInputErrors + (fnc.id -> specInputErrorForFunc)

      val transformed = unrollAll(fnc.body.get, ctx.dsAbstractions(fnc.id), uniformPrecision)
      fnc.copy(body = Some(transformed), params = unrolledParamsFlattened)
    })

    // what are the specInputRanges? we need to update them, too
    val () = println("specInputRanges: " + specInputRanges)
    

    println("Unrolled functions:")
    newDefs.foreach(println)

    (ctx.copy(specInputRanges = newspecInputRanges,
      specInputErrors = newspecInputErrors), Program(prg.id, newDefs ++ functionsToConsider(ctx, prg).diff(fncsToConsider)))
  }


  /**
   * Unroll all loops over data structures to operations on Reals
   *
   * @param fnc FunDef of the program to be unrolled
   * @return unrolled expression [[Expr]]
   */
  def unrollAll(fnc: Expr, dsaRangeMap: Map[Expr, DSAbstraction], prec: Precision):
  Expr = {
    var addedIds: Map[(Expr, Int), Identifier] = Map()

    def rec(e: Expr): Expr = {
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
                createUnrolledLet(i, v, b, dsaRangeMap)
            }
            else{
                val () = println("b: " + b)
                val _ = rec(b)
                e
            } 
        }
        case VectorElement(expr, index) => {
            // first get the name of vector
            val vName = expr match{
                case Variable(id) => id
                case _ => {
                    val () = println("ERROR: Not a Variable")
                    ""
                }
            }
            val () = println("VectorElement")
            // then get the index
            val indexInt = index match{
                case IntegerLiteral(i) => i
                case Int32Literal(i) => i
                case _ => {
                    val () = println("ERROR: Not an IntegerLiteral")
                    val () = println("type: " + index.getClass)
                    0
                }
            }
            val () = println("changing: " + vName + " to: " + s"${vName}_$indexInt")
            (Variable(getOrCreateFreshIdentifier(s"${vName}_$indexInt", RealType)))
        }
        //case x => {
        //    val () = println(s"rec: $x")
        //    val () = println(s"x.getType: ${x.getType}")
        //    (x, Map(), Map())
        //}
    }}

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
                val () = println("identifier is: " + s"${i}_0")
                val () = println("body is: " + b)
                var currLet = Let(getOrCreateFreshIdentifier(s"${i}_0", RealType), Times(Variable(getOrCreateFreshIdentifier(s"${t1Id}_0", RealType)), t2), rec(b))
                for(j <- 1 to vSize - 1){
                    val newId = getOrCreateFreshIdentifier(s"${i}_$j", RealType)
                    val newLet = Let(newId, Times(Variable(getOrCreateFreshIdentifier(s"${t1Id}_$j", RealType)), t2), currLet)
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

    val rec_fnc = rec(fnc)
    println("rec_fnc: " + rec_fnc)
    rec_fnc
  }
  // TODO don't forget to add to the context modified intermediate results/input ranges for all vector/matrix elements

  

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
