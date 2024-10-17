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
import daisy.lang.Extractors.ArithOperator

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
  var sizeMap: Map[Identifier, Seq[Int]] = Map()
  var indexCounter: Int = 0

  // sometimes some Vector elements may get caught here
  // I don't need to generate new temporary identifiers for them. 
  // Vector elements can directly be generated to {vector_name}_{index}
  // matrix element can be generated to {matrix_name}_{row}_{col}
  def findOrCreateIndex(expr: Expr): String = {
    expr match{
        case Variable(id) => id.name
        case VectorElement(v, index) => {
            val vName = v match{
                case Variable(id) => id.name
                case _ => {
                    val () = println("ERROR: Not a Variable")
                    ""
                }
            }
            val indexInt = index match{
                case IntegerLiteral(i) => i
                case Int32Literal(i) => i
                case _ => {
                    val () = println("ERROR: Not an IntegerLiteral")
                    0
                }
            }
            s"${vName}_$indexInt"
        }
        case MatrixElement(m, irow, icol) => {
            val mName = m match{
                case Variable(id) => id.name
                case _ => {
                    val () = println("ERROR: Not a Variable")
                    ""
                }
            }
            val irowInt = irow match{
                case IntegerLiteral(i) => i
                case Int32Literal(i) => i
                case _ => {
                    val () = println("ERROR: Not an IntegerLiteral")
                    0
                }
            }
            val icolInt = icol match{
                case IntegerLiteral(i) => i
                case Int32Literal(i) => i
                case _ => {
                    val () = println("ERROR: Not an IntegerLiteral")
                    0
                }
            }
            s"${mName}_${irowInt}_$icolInt"
        }
        case _ => {
            val () = println("WARNING: Not a Variable")
            // print the expression
            val () = println("expr: " + expr)
            val () = println("type: " + expr.getClass)
            indexCounter = indexCounter + 1
            throw new Exception("ERROR: Not a Variable")
            s"temp${indexCounter}"
        }
    }
  }

  def getOrCreateFreshIdentifier(name: String, tpe: TypeTree): Identifier = {
    // if the name is already in the list, return it
    // otherwise, create a new one and return it
    val id = freshIdentifierList.find(_.name == name)
    id match {
      case Some(x) => {
        x
      }
      case None => {
        val newId = FreshIdentifier(name, tpe)
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
    for((k, v) <- ctx.dsAbstractions(fnc.id)){
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

  // This function should also hold the logic to decide for new sizes, e.g. what happens if you multiply two matrices? What is the new size?
  def getDSSize(expr: Expr): Seq[Int] = {
    expr match{
      case Variable(id) => {
        sizeMap(id)
      }
      case Times(t1, t2) => {
        val t1Size: Seq[Int] = 
          if(isVector(t1) || isMatrix(t1)) 
            getDSSize(t1)
          else 
            Seq(1)
          
        val t2Size: Seq[Int] =
          if(isVector(t2) || isMatrix(t2)) 
            getDSSize(t2)
          else 
            Seq(1)
          

        if(isMatrix(t1) == true && isMatrix(t2) == true){
          val t1RowSize = t1Size.head
          val t1ColSize = t1Size(1)
          val t2RowSize = t2Size.head
          val t2ColSize = t2Size(1)
          if(t1ColSize != t2RowSize){
            val () = println("ERROR: t1 and t2 sizes are not the same")
            val () = println("t1RowSize: " + t1RowSize)
            val () = println("t1ColSize: " + t1ColSize)
            val () = println("t2RowSize: " + t2RowSize)
            val () = println("t2ColSize: " + t2ColSize)
            val () = println("t1: " + t1)
            val () = println("t2: " + t2)
          }
          Seq(t1RowSize, t2ColSize)
        }
        else if(isVector(t1) == true && isMatrix(t2) == true){
          val () = println("ERROR: You cannot multiply a vector by a matrix")
          val () = println("expr: " + expr)
          throw new Exception("ERROR: You cannot multiply a vector by a matrix")
        }
          
        else if((isVector(t1) == true) && (isVector(t2) == true)){
          val () = println("ERROR: You cannot multiply two vectors")
          val () = println("expr: " + expr)
          Seq()
        }
        else if((isVector(t1) == true) && (isVector(t2) == false)){
          Seq(t1Size.head)
        }
        else if((isVector(t1) == false) && (isVector(t2) == true)){
          Seq(t2Size.head)
        }
        else{
          Seq(1)
        }
      }
      
      case Plus(t1, t2) => {
        val t1Size: Seq[Int] = 
          if(isVector(t1) || isMatrix(t1)){
            getDSSize(t1)
          }
          else{
            Seq(1)
          }
        val t2Size: Seq[Int] =
          if(isVector(t2) || isMatrix(t2)){
            getDSSize(t2)
          }
          else{
            Seq(1)
          }
        if((isMatrix(t1) == true) && (isMatrix(t2) == true)) {
          if(t1Size != t2Size){
            val () = println("ERROR: t1 and t2 sizes are not the same")
            val () = println("t1Size: " + t1Size)
            val () = println("t2Size: " + t2Size)
            val () = println("t1: " + t1)
            val () = println("t2: " + t2)

            // raise an exception
            throw new Exception("ERROR: t1 and t2 sizes are not the same")
          }
          val () = println("Returning t1Size: " + t1Size)
          t1Size
        }
        else if((isVector(t1) == true) && (isVector(t2) == true)){
          if(t1Size != t2Size){
            val () = println("ERROR: t1 and t2 sizes are not the same")
            val () = println("t1Size: " + t1Size)
            val () = println("t2Size: " + t2Size)
            val () = println("t1: " + t1)
            val () = println("t2: " + t2)

            // raise an exception
            throw new Exception("ERROR: t1 and t2 sizes are not the same")
          }
          t1Size
        }
        else if((isVector(t1) == true) && (isVector(t2) == false)){
          t1Size
        }
        else if((isVector(t1) == false) && (isVector(t2) == true)){
          t2Size
        }
        else{
          val () = println("ERROR: Returning 1 for Plus")
          Seq(1)
        }
      }

      case Minus(t1, t2) => {
        val t1Size: Seq[Int] = 
          if(isVector(t1) || isMatrix(t1)){
            getDSSize(t1)
          }
          else{
            Seq(1)
          }
        val t2Size: Seq[Int] =
          if(isVector(t2) || isMatrix(t2)){
            getDSSize(t2)
          }
          else{
            Seq(1)
          }
        if((isMatrix(t1) == true) && (isMatrix(t2) == true)) {
          if(t1Size != t2Size){
            val () = println("ERROR: t1 and t2 sizes are not the same")
            val () = println("t1Size: " + t1Size)
            val () = println("t2Size: " + t2Size)
            val () = println("t1: " + t1)
            val () = println("t2: " + t2)
            
            // raise an exception
            throw new Exception("ERROR: t1 and t2 sizes are not the same")
          }
          t1Size
        }
        else if((isVector(t1) == true) && (isVector(t2) == true)){
          if(t1Size != t2Size){
            val () = println("ERROR: t1 and t2 sizes are not the same")
            val () = println("t1Size: " + t1Size)
            val () = println("t2Size: " + t2Size)
            val () = println("t1: " + t1)
            val () = println("t2: " + t2)
          }
          t1Size
        }
        else if((isVector(t1) == true) && (isVector(t2) == false)){
          t1Size
        }
        else if((isVector(t1) == false) && (isVector(t2) == true)){
          t2Size
        }
        else{
          val () = println("ERROR: Returning 1 for Minus")
          Seq(1)
        }
      }

      case UMinus(t) => {
        getDSSize(t)
      }

      case Division(t1, t2) => {
        val t1Size: Seq[Int] = 
          if(isVector(t1)){
            getDSSize(t1)
          }
          else{
            Seq(1)
          }
        if(isMatrix(t1) && isMatrix(t2)){
          // check if the sizes are the same
          val t1Size: Seq[Int] = getDSSize(t1)
          val t2Size: Seq[Int] = getDSSize(t2)
          if(t1Size != t2Size){
            val () = println("ERROR: t1 and t2 sizes are not the same")
            val () = println("t1Size: " + t1Size)
            val () = println("t2Size: " + t2Size)
            val () = println("t1: " + t1)
            val () = println("t2: " + t2)
            
            throw new Exception("ERROR: t1 and t2 sizes are not the same")
          }
          t1Size
        }
        // If t1 is a vector and t2 is a scalar, then it is okay
        else if((isVector(t1) == true) && (isVector(t2) == false)){
          t1Size
        }
        else if((isVector(t1) == true) && (isVector(t2) == true)){
          val () = println("ERROR: You cannot divide two vectors")
          val () = println("expr: " + expr)
          Seq()
        }
        else if((isVector(t1) == false) && (isVector(t2) == true)){
          val () = println("ERROR: You cannot divide a scalar by a vector")
          val () = println("expr: " + expr)
          Seq()
        }
        else{
          Seq(1)
        }
      }

      case Sin(e) => {
        getDSSize(e)
      }

      case Cos(e) => {
        getDSSize(e)
      }

      case Tan(e) => {
        getDSSize(e)
      }

      case _ => {
        val () = println("ERROR: DSSize match failed")
        val () = println("expr: " + expr)
        // print the type of the expression
        val () = println("type: " + expr.getClass)
        Seq()
      }
    }
  }

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val uniformPrecision = ctx.option[Precision]("precision")
    // need to replace function bodies, create a copy of the whole program
    val fncsToConsider = functionsToConsider(ctx, prg)
    var specInputRanges: Map[Identifier, Map[Identifier, Interval]] = ctx.specInputRanges
    var specInputErrors: Map[Identifier, Map[Identifier, Rational]] = ctx.specInputErrors

    var newspecInputRanges: Map[Identifier, Map[Identifier, Interval]] = Map()
    var newspecInputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
    val newDefs = fncsToConsider.map(fnc => {
      val () = println("Unrolling function: " + fnc.id)
      val () = println("fnc: " + fnc)
      var specInputRangeForFunc: Map[Identifier, Interval] = Map()
      var specInputErrorForFunc: Map[Identifier, Rational] = Map()
      // I'll set an empty list for the ds sizes just in case, but it might be wrong
      sizeMap = Map()
      // The next stage is to put input sizes into sizeMap
      // I'll iterate over dsAbstractions(fnc.id) and put the sizes into sizeMap
      for((k, v) <- ctx.dsAbstractions(fnc.id)){
          val () = k match{
              case RealLiteral(r) => {
                  val () = println("ERROR: Not a ValDef, but a RealLiteral")
              }
              case FinitePrecisionLiteral(r, prec, _) => {
                  val () = println("ERROR: Not a ValDef, but a FinitePrecisionLiteral")
              }
              case Variable(id) => {
                  if(isMatrix(k)){
                    sizeMap = sizeMap + (id -> Seq(v.numRows, v.numCols))
                  }
                  else{
                    sizeMap = sizeMap + (id -> Seq(v.dsSize))
                  }
              }
              case _ => {
                  val () = println("ERROR: Not a ValDef")
              }
          }
      }
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
        case VectorType(_) => {
          // getInputDSSize returns us an integer
          // in MatrixType, we are getting the total size, there is no difference between rows and columns
          val vdSize = getInputDSSize(ctx, fnc, vd)        //var currList = List(FreshIdentifier(s"${vd.id}_0", RealType))
          var currList: List[Identifier] = List()
          for(j <- 0 to vdSize - 1){
            val newId = getOrCreateFreshIdentifier(s"${vd.id}_$j", RealType)
            currList = currList :+ newId
            unrolledParamsFlattened = unrolledParamsFlattened :+ ValDef(newId)
          }
          currList
        }
        case MatrixType(_) => {
          val rowSize = ctx.dsAbstractions(fnc.id)(Variable(vd.id)).numRows
          val colSize = ctx.dsAbstractions(fnc.id)(Variable(vd.id)).numCols
          var currList: List[Identifier] = List()
          for(i <- 0 to rowSize - 1){
            for(j <- 0 to colSize - 1){
              val newId = getOrCreateFreshIdentifier(s"${vd.id}_${i}_$j", RealType)
              currList = currList :+ newId
              unrolledParamsFlattened = unrolledParamsFlattened :+ ValDef(newId)
            }
          }
        }

      })
      // for some reason cannot flatten the list with flatten function

      // we also need to overwrite the preconditions, again by unrolling
      // A typical precondition:
      // fnc.precondition: Some(((x ? -62.54) ? (x ? 15.02) ? x.(4) ? Tree? (class daisy.lang.Trees$VectorRange) ? Tree? (class daisy.lang.Trees$VectorRange) ? Tree? (class daisy.lang.Trees$VectorRange) ? Tree? (class daisy.lang.Trees$VectorRange)))
      // I'll only use VectorRange, others seems useless
      var newPreconditionFlattened: List[Expr] = List()
      fnc.precondition.map(precond => precond match{
        case And(exprs) => {
          exprs.map(expr => expr match{
            case VectorRange(v, fromInd, toInd, lb, ub) => {
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
              newPreconditionFlattened = newPreconditionFlattened :+ GreaterThan(Variable(unrollId), lb)
              newPreconditionFlattened = newPreconditionFlattened :+ LessThan(Variable(unrollId), ub)
              specInputRangeForFunc = specInputRangeForFunc + (unrollId -> Interval(lb.value, ub.value))
              specInputErrorForFunc = specInputErrorForFunc + (unrollId -> uniformPrecision.absRoundoff(Interval(lb.value, ub.value)))
            }
            case MatrixRange(v, indices, lb, ub) => {
              // MatrixRange(v: MatrixLiteral, indices: Seq[(Int, Int)], lb: RealLiteral, ub: RealLiteral)
              // all indices in the matrix range has only one element, which is another set that includes 2 elements, row and column position
              val rowInd = indices.head._1
              val colInd = indices.head._2
              val vId = v match{
                case MatrixLiteral(id) => id
                case _ => {
                    val () = println("ERROR: Not a MatrixLiteral")
                    ""
                }
              }
              val unrollId = getOrCreateFreshIdentifier(s"${vId}_${rowInd}_$colInd", RealType)
              newPreconditionFlattened = newPreconditionFlattened :+ GreaterThan(Variable(unrollId), lb)
              newPreconditionFlattened = newPreconditionFlattened :+ LessThan(Variable(unrollId), ub)
              specInputRangeForFunc = specInputRangeForFunc + (unrollId -> Interval(lb.value, ub.value))
              specInputErrorForFunc = specInputErrorForFunc + (unrollId -> uniformPrecision.absRoundoff(Interval(lb.value, ub.value)))
            }
            case _ => {
            }
          })  
        }

      })

      //val () = println("newPrecondition: " + newPreconditionFlattened)
      //val () = println("unrolledParamsFlattened: " + unrolledParamsFlattened)
      fnc.precondition = Some(And(newPreconditionFlattened))

      newspecInputRanges = newspecInputRanges + (fnc.id -> specInputRangeForFunc)
      newspecInputErrors = newspecInputErrors + (fnc.id -> specInputErrorForFunc)

      val transformed = unrollAll(fnc.body.get, ctx.dsAbstractions(fnc.id), uniformPrecision)
      fnc.copy(body = Some(transformed), params = unrolledParamsFlattened)
    })
    

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
    // TODO: get rid of dsaRangeMap dependency here
    val _ = dsaRangeMap.map(x =>
      x._1 match {
        case Variable(id) => {
          freshIdentifierList = freshIdentifierList :+ id
        }
      }  
    )
  
  def createLetExpr(id: String, opr: Any, t1: Expr, t2: Expr, body: Expr): Expr = {
    // if the opr is Plus, Minus, Times, Division, then we'll call createLetExprTwo
    // if the opr is Sin, Cos, Tan, then we'll call createLetExprOne

    opr match {
      case Plus => {
        createLetExprTwo(id, Plus, t1, t2, body)
      }
      case Minus => {
        createLetExprTwo(id, Minus, t1, t2, body)
      }
      case Times => {
        createLetExprTwo(id, Times, t1, t2, body)
      }
      case Division => {
        createLetExprTwo(id, Division, t1, t2, body)
      }
      case Sin => {
        createLetExprOne(id, Sin, t1, body)
      }
      case Cos => {
        createLetExprOne(id, Cos, t1, body)
      }
      case Tan => {
        createLetExprOne(id, Tan, t1, body)
      }
      case _ => {
        val () = println("ERROR: opr is not supported")
        val () = println("opr: " + opr)
        t1
      }
    }
  }

  def createLetExprTwo(id: String, opr: (Expr, Expr) => Expr, t1: Expr, t2: Expr, body: Expr): Expr = {
    val t1Id = findOrCreateIndex(t1)
    val t2Id = findOrCreateIndex(t2)
    if(isMatrix(t1) && isMatrix(t2)){
      // These identifiers *may* be different then the ones in findOrCreateIndex, not sure
      val t1Identifier: Identifier = t1 match { 
        case Variable(id) => 
          id
      }
      val t2Identifier: Identifier = t2 match { 
        case Variable(id) => 
          id
      }
      val t1Size: Seq[Int] = sizeMap(t1Identifier)
      val t2Size: Seq[Int] = sizeMap(t2Identifier)
      val t1RowSize = t1Size.head
      val t1ColSize = t1Size(1)
      val t2RowSize = t2Size.head
      val t2ColSize = t2Size(1)
      if((t1RowSize != t2RowSize) || (t1ColSize != t2ColSize)){
        val () = println("ERROR: t1 and t2 sizes are not the same")
        val () = println("t1RowSize: " + t1RowSize)
        val () = println("t1ColSize: " + t1ColSize)
        val () = println("t2RowSize: " + t2RowSize)
        val () = println("t2ColSize: " + t2ColSize)
        val () = println("t1: " + t1)
        val () = println("t2: " + t2)
        val () = println("opr: " + opr)
      }
      // id_row_col
      var currLet = Let(getOrCreateFreshIdentifier(s"${id}_0_0", RealType), opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_0_0", RealType)), Variable(getOrCreateFreshIdentifier(s"${t2Id}_0_0", RealType))), rec(body))
      for(i <- 0 to t1RowSize - 1){
        for(j <- 0 to t1ColSize - 1){
          if(i == 0 && j == 0){
            // we already did this
            ()
          }
          else{
            val newId = getOrCreateFreshIdentifier(s"${id}_${i}_$j", RealType)
            val newLet = Let(newId, opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_${i}_$j", RealType)), Variable(getOrCreateFreshIdentifier(s"${t2Id}_${i}_$j", RealType))), currLet)
            currLet = newLet
          }
        }
      }
      currLet
    }
    else if(isVector(t1) && isVector(t2)){
      val t1Size = getDSSize(t1).head
      val t2Size = getDSSize(t2).head
      if(t1Size != t2Size){
        val () = println("ERROR: t1 and t2 sizes are not the same")
        val () = println("t1Size: " + t1Size)
        val () = println("t2Size: " + t2Size)
        val () = println("t1: " + t1)
        val () = println("t2: " + t2)
        val () = println("opr: " + opr)
      }
      var currLet = Let(getOrCreateFreshIdentifier(s"${id}_0", RealType), opr(
        Variable(getOrCreateFreshIdentifier(s"${t1Id}_0", RealType)), 
        Variable(getOrCreateFreshIdentifier(s"${t2Id}_0", RealType))), rec(body))
      for(j <- 1 to t1Size - 1){
          val newId = getOrCreateFreshIdentifier(s"${id}_$j", RealType)
          val newLet = Let(newId, opr(
            Variable(getOrCreateFreshIdentifier(s"${t1Id}_$j", RealType)), 
            Variable(getOrCreateFreshIdentifier(s"${t2Id}_$j", RealType))), currLet)
          currLet = newLet
      }
      currLet
    }
    else if(isVector(t1)) { // TODO: CHECK
      val size = getDSSize(t1).head
      var currLet = Let(getOrCreateFreshIdentifier(s"${t1Id}_0", RealType), 
        opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_0", RealType)), 
            Variable(getOrCreateFreshIdentifier(t2Id, RealType))), rec(body))
      for(j <- 1 to size - 1){
          val newId = getOrCreateFreshIdentifier(s"${t1Id}_$j", RealType)
          val newLet = Let(newId, opr(
            Variable(getOrCreateFreshIdentifier(s"${t1Id}_$j", RealType)), 
            Variable(getOrCreateFreshIdentifier(t2Id, RealType))), currLet)
          currLet = newLet
      }
      currLet
    }
    else if(isVector(t2)) { // TODO: CHECK
      val size = getDSSize(t2).head
      var currLet = Let(getOrCreateFreshIdentifier(s"${id}_0", RealType), opr(
        Variable(getOrCreateFreshIdentifier(t1Id, RealType)), // the other element can be a vector or matrix element that needs to be processed
        Variable(getOrCreateFreshIdentifier(s"${t2Id}_0", RealType))), rec(body))
      for(j <- 1 to size - 1){
          val newId = getOrCreateFreshIdentifier(s"${id}_$j", RealType)
          val newLet = Let(newId, opr(
            Variable(getOrCreateFreshIdentifier(t1Id, RealType)), // the other element can be a vector or matrix element that needs to be processed
            Variable(getOrCreateFreshIdentifier(s"${t2Id}_$j", RealType))), currLet)
          currLet = newLet
      }
      currLet
    }
    else{ // TODO: CHECK
      // there may be some Vector or Matrix elements in t1 and t2. And we still need to process them to convert the names to {vector_name}_{index}
      Let(getOrCreateFreshIdentifier(s"${id}", RealType), opr(
        Variable(getOrCreateFreshIdentifier(s"${t1Id}", RealType)), 
        Variable(getOrCreateFreshIdentifier(s"${t2Id}", RealType))), rec(body))
    }
  }

  def createLetExprOne(id: String, opr: (Expr) => Expr, t1: Expr, body: Expr): Expr = {
    val t1Id = findOrCreateIndex(t1)
    if(isVector(t1)){
      val t1Size = getDSSize(t1).head
      var currLet = Let(getOrCreateFreshIdentifier(s"${id}_0", RealType), opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_0", RealType))), rec(body))
      for(j <- 1 to t1Size - 1){
          val newId = getOrCreateFreshIdentifier(s"${id}_$j", RealType)
          val newLet = Let(newId, opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_$j", RealType))), currLet)
          currLet = newLet
      }
      currLet
    }
    else if(isMatrix(t1)){
      val t1Size: Seq[Int] = getDSSize(t1)
      val t1RowSize = t1Size.head
      val t1ColSize = t1Size(1)
      // id_row_col
      var currLet = Let(getOrCreateFreshIdentifier(s"${id}_0_0", RealType), opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_0_0", RealType))), rec(body))
      for(i <- 0 to t1RowSize - 1){
        for(j <- 0 to t1ColSize - 1){
          if(i == 0 && j == 0){
            // we already did this
            ()
          }
          else{
            val newId = getOrCreateFreshIdentifier(s"${id}_${i}_$j", RealType)
            val newLet = Let(newId, opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_${i}_$j", RealType))), currLet)
            currLet = newLet
          }
        }
      }
      currLet
    }
    else{ // TODO: CHECK
      Let(getOrCreateFreshIdentifier(s"${id}", RealType), opr(t1), rec(body))
    }
  }

  def rec(e: Expr): Expr = {
    e match {
        case Let(i, v, b) => {
            if(isVector(v) | isMatrix(v)){
                // but not with an accumulator
                // I am assuming that v is relatively simple
                // It will simplify the code a lot
                val vSize = getDSSize(v)
                sizeMap = sizeMap + (i -> vSize)
                createUnrolledLet(i, v, b)
            }
            else{
                sizeMap = sizeMap + (i -> Seq(1))
                val res = createUnrolledLet(i, v, b)
                res
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
            (Variable(getOrCreateFreshIdentifier(s"${vName}_$indexInt", RealType)))
        }
        case MatrixElement(expr, row_index, col_index) => {
            // first get the name of matrix
            val mName = expr match{
                case Variable(id) => id
                case _ => {
                    val () = println("ERROR: Not a Variable")
                    ""
                }
            }
            // then get the row and column indices
            val rowInd = row_index match{
                case IntegerLiteral(i) => i
                case Int32Literal(i) => i
                case _ => {
                    val () = println("ERROR: Not an IntegerLiteral")
                    val () = println("type: " + row_index.getClass)
                    0
                }
            }
            val colInd = col_index match{
                case IntegerLiteral(i) => i
                case Int32Literal(i) => i
                case _ => {
                    val () = println("ERROR: Not an IntegerLiteral")
                    val () = println("type: " + col_index.getClass)
                    0
                }
            }
            (Variable(getOrCreateFreshIdentifier(s"${mName}_${rowInd}_$colInd", RealType)))
        }
        case Variable(id) => {
            val id_str = id.name
            (Variable(getOrCreateFreshIdentifier(id_str, RealType)))
        }
        case _ => {
          val () = println("ERROR: rec match failed")
          val () = println("expr: " + e)
          // p[r]int the type of the expression
          val () = println("type: " + e.getClass)
          throw new Exception("ERROR: rec match failed")
          e
        }
    }}

    def createUnrolledLet(i: Identifier, v: Expr, b: Expr): Expr = {
    // i size is equal to v size
    v match {
        case Times(t1, t2) => {
          if(isVector(t1) == isVector(t2)){
              val () = println("This shouldn't happen!")
              val () = println("ERROR: Both are vectors or both are not vectors, not supported")
              val () = println("v: " + v)
              v
          }
          createLetExpr(i.name, Times, t1, t2, b)
        }
        case Plus(t1, t2) => {
          createLetExpr(i.name, Plus, t1, t2, b)
        }
        case Division(t1, t2) => {
          createLetExpr(i.name, Division, t1, t2, b)
        }
        case Minus(t1, t2) => {
          createLetExpr(i.name, Minus, t1, t2, b)
        }
        case Sin(e) => {
          createLetExpr(i.name, Sin, e, e, b)
        }
        case Cos(e) => {
          createLetExpr(i.name, Cos, e, e, b)
        }


    }  
  }

    val rec_fnc = rec(fnc)
    rec_fnc
  }

}
