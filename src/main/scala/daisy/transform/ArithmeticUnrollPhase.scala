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

      case VectorFromList(list, size) => {
        Seq(size)
      }

      case MatrixFromLists(lists, numRows, numCols) => {
        Seq(numRows, numCols)
      }

      case CrossProduct(lhs, rhs) => {
        // both must be matrices for now, vectors are not supported
        if(isVector(lhs) && isVector(rhs)){
          val () = println("ERROR: CrossProduct is not supported for vectors")
          Seq()
        }
        else if(isMatrix(lhs) && isMatrix(rhs)){
          val lhsSize = getDSSize(lhs)
          val rhsSize = getDSSize(rhs)

          // matrix multiplication
          val lhsRowSize = lhsSize.head
          val lhsColSize = lhsSize(1)
          val rhsRowSize = rhsSize.head
          val rhsColSize = rhsSize(1)
          // check size
          if(lhsColSize != rhsRowSize){
            val () = println("ERROR: lhsColSize and rhsRowSize should be the same")
            throw new Exception("ERROR: lhsColSize and rhsRowSize should be the same")
            Seq()
          }
          Seq(lhsRowSize, rhsColSize)
        }
        // matrix times vector is also supported
        else if(isMatrix(lhs) && isVector(rhs)){
          val lhsSize = getDSSize(lhs)
          val rhsSize = getDSSize(rhs).head

          // matrix multiplication
          val lhsRowSize = lhsSize.head
          val lhsColSize = lhsSize(1)
          // check size
          if(lhsColSize != rhsSize){
            val () = println("ERROR: lhsColSize and rhsSize should be the same")
            val () = println("lhs: " + lhs)
            val () = println("rhs: " + rhs)
            val () = println("lhsSize: " + lhsSize)
            val () = println("rhsSize: " + rhsSize)
            throw new Exception("ERROR: lhsColSize and rhsSize should be the same")
            Seq()
          }
          Seq(lhsRowSize)
        }
        else{
          val () = println("ERROR: CrossProduct is only supported for matrices")
          Seq()
        }
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
    // First add the parameters to the freshIdentifierList
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
      case UMinus => {
        createLetExprOne(id, UMinus, t1, body)
      }
      case _ => {
        val () = println("ERROR: opr is not supported")
        val () = println("opr: " + opr)
        t1
      }
    }
  }

  def createLetExprTwo(id: String, opr: (Expr, Expr) => Expr, t1: Expr, t2: Expr, body: Expr): Expr = {
    var processed_body = body
    var t1Id = ""
    var t2Id = ""
    try{
      t1Id = findOrCreateIndex(t1)
    }
    catch{
      case e: Exception => {
        // check if we further need t2
        try{
          t2Id = findOrCreateIndex(t2)
          // only t1 is processed
          val newLet1 = rec(t1)
          val () = println("t1: " + t1)
          val () = println("t2: " + t2)
          val () = println("rec(t1): " + newLet1)
          throw new Exception("ERROR: t1 is not supported")
        }
        catch{
          case e: Exception => {
            // need to process both
            // just use rec
            val newLet1 = rec(t1)
            val newLet2 = rec(t2)
            val () = println("t1: " + t1)
            val () = println("t2: " + t2)
            val () = println("rec(t1): " + newLet1)
            val () = println("rec(t2): " + newLet2)
            throw new Exception("ERROR: Both t1 and t2 are not supported")
            // we will add these definitions to the body
            // new body is newLet1(newLet2(body))
          }
        }
      }
    }
    try{
      t2Id = findOrCreateIndex(t2)
    }
    catch{
      case e: Exception => {
        // only t1 is processed, the new let is 
        // v is t2
        // b is body
        // we need a random identifier again for t2
        indexCounter = indexCounter + 1
        val newId = getOrCreateFreshIdentifier(s"temp${indexCounter}", t1.getType)
        // processed_body = createUnrolledLet(newId, t2, body)
        processed_body = Let(newId, t2, body)
        t2Id = newId.name
        val () = println("t1: " + t1)
        val () = println("t2: " + t2)
        val () = println("rec(t2): " + processed_body)

      }
    }
    
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
      var currLet = Let(getOrCreateFreshIdentifier(s"${id}_0_0", RealType), opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_0_0", RealType)), Variable(getOrCreateFreshIdentifier(s"${t2Id}_0_0", RealType))), rec(processed_body))
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
    else if(isMatrix(t1) && isVector(t2)){
      val t1Size: Seq[Int] = getDSSize(t1)
      val t1RowSize = t1Size.head
      val t1ColSize = t1Size(1)
      val t2Size = getDSSize(t2).head
      if(t1ColSize != t2Size){
        val () = println("ERROR: t1 and t2 sizes are not the same")
        val () = println("t1RowSize: " + t1RowSize)
        val () = println("t1ColSize: " + t1ColSize)
        val () = println("t2Size: " + t2Size)
        val () = println("t1: " + t1)
        val () = println("t2: " + t2)
        val () = println("opr: " + opr)
      }
      // id_row_col
      var currLet = Let(getOrCreateFreshIdentifier(s"${id}_0_0", RealType), opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_0_0", RealType)), Variable(getOrCreateFreshIdentifier(s"${t2Id}_0", RealType))), rec(processed_body))
      for(i <- 0 to t1RowSize - 1){
        for(j <- 0 to t1ColSize - 1){
          if(i == 0 && j == 0){
            // we already did this
            ()
          }
          else{
            val newId = getOrCreateFreshIdentifier(s"${id}_${i}_$j", RealType)
            val newLet = Let(newId, opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}_${i}_$j", RealType)), Variable(getOrCreateFreshIdentifier(s"${t2Id}_$j", RealType))), currLet)
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
        Variable(getOrCreateFreshIdentifier(s"${t2Id}_0", RealType))), rec(processed_body))
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
            Variable(getOrCreateFreshIdentifier(t2Id, RealType))), rec(processed_body))
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
        Variable(getOrCreateFreshIdentifier(s"${t2Id}_0", RealType))), rec(processed_body))
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
        Variable(getOrCreateFreshIdentifier(s"${t2Id}", RealType))), rec(processed_body))
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
      // there may be some Vector or Matrix elements in t1 and t2. And we still need to process them to convert the names to {vector_name}_{index}
      Let(getOrCreateFreshIdentifier(s"${id}", RealType), opr(Variable(getOrCreateFreshIdentifier(s"${t1Id}", RealType))), rec(body))
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
          val () = println("WE GOT A VARIABLE: " + id)
            val id_str = id.name
            (Variable(getOrCreateFreshIdentifier(id_str, RealType)))
        }

        case RealLiteral(r) => {
            RealLiteral(r)
        }

        // If it is something else, then we first need to process that and put the value in a temporary variable
        // and then we can use that temporary variable in the expression
        case _ => {
          val () = println("ERROR: rec match failed")
          val () = println("expr: " + e)
          // p[r]int the type of the expression
          val () = println("type: " + e.getClass)
          throw new Exception("ERROR: rec match failed")
          e
        }
    }}

    def createUnrolledLet(id: Identifier, v: Expr, b: Expr): Expr = {
    // i size is equal to v size
    v match {
        case Times(t1, t2) => {
          if(isVector(t1) == true && isVector(t2) == true){
              val () = println("This shouldn't happen!")
              val () = println("ERROR: Both are vectors, not supported")
              val () = println("v: " + v)
              v
          }
          createLetExpr(id.name, Times, t1, t2, b)
        }
        case Plus(t1, t2) => {
          createLetExpr(id.name, Plus, t1, t2, b)
        }
        case Division(t1, t2) => {
          createLetExpr(id.name, Division, t1, t2, b)
        }
        case Minus(t1, t2) => {
          createLetExpr(id.name, Minus, t1, t2, b)
        }
        case Sin(e) => {
          createLetExpr(id.name, Sin, e, e, b)
        }
        case Cos(e) => {
          createLetExpr(id.name, Cos, e, e, b)
        }
        case UMinus(e) => {
          val () = println("Uminus b: " + b)
          val () = println("Uminus e: " + e)
          val () = println("Uminus id: " + id)
          createLetExpr(id.name, UMinus, e, e, b)
        }

        // this just means that it is already unrolled
        // I think we certainly use rec(v) here for name unrolling just in case
        // it may not be unrolled
        // v may be matrix, the variable may be a matrix, or both could be vectors, which means it is just an assignment
        // we need to handle all of these cases
        case Variable(var_name) => {
          val id_str = id.name

          if(isMatrix(v)){
            // need to unroll all assignments
            val vSize: Seq[Int] = getDSSize(v)
            val vRowSize = vSize.head
            val vColSize = vSize(1)
            var currLet = Let(getOrCreateFreshIdentifier(s"${id_str}_0_0", RealType), Variable(getOrCreateFreshIdentifier(s"${var_name.name}_0_0", RealType)), rec(b))
            for(i <- 0 to vRowSize - 1){
              for(j <- 0 to vColSize - 1){
                if(i == 0 && j == 0){
                  // we already did this
                  ()
                }
                else{
                  val newId = getOrCreateFreshIdentifier(s"${id_str}_${i}_$j", RealType)
                  val newLet = Let(newId, Variable(getOrCreateFreshIdentifier(s"${var_name.name}_${i}_$j", RealType)), currLet)
                  currLet = newLet
                }
              }
            }
            currLet
          }
          else if(isVector(v)){
            val vSize = getDSSize(v).head
            var currLet = Let(getOrCreateFreshIdentifier(s"${id_str}_0", RealType), Variable(getOrCreateFreshIdentifier(s"${var_name.name}_0", RealType)), rec(b))
            for(j <- 1 to vSize - 1){
              val newId = getOrCreateFreshIdentifier(s"${id_str}_$j", RealType)
              val newLet = Let(newId, Variable(getOrCreateFreshIdentifier(s"${var_name.name}_$j", RealType)), currLet)
              currLet = newLet
            }
            currLet
          }
          else{
            Let(getOrCreateFreshIdentifier(id_str, RealType), v, b)
          }
        }

        case MatrixElement(expr, row_index, col_index) => {
          val mName = expr match{
            case Variable(id) => id
            case _ => {
                val () = println("ERROR: Not a Variable")
                ""
            }
          }
          val rowInd = row_index match{
            case IntegerLiteral(i) => i
            case Int32Literal(i) => i
            case _ => {
                val () = println("ERROR: Not an IntegerLiteral")
                0
            }
          }
          val colInd = col_index match{
            case IntegerLiteral(i) => i
            case Int32Literal(i) => i
            case _ => {
                val () = println("ERROR: Not an IntegerLiteral")
                0
            }
          }
          Let(getOrCreateFreshIdentifier(s"${mName}_${rowInd}_$colInd", RealType), v, b)
        }

        case VectorElement(expr, index) => {
          val vName = expr match{
            case Variable(id) => id
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
          Let(getOrCreateFreshIdentifier(s"${vName}_$indexInt", RealType), v, b)
        }

        case VectorFromList(list, size) => {
          val id_str = id.name
          val vSize = size
          var currLet = Let(getOrCreateFreshIdentifier(s"${id_str}_0", RealType), rec(list.head), rec(b))
          for(j <- 1 to vSize - 1){
            val newId = getOrCreateFreshIdentifier(s"${id_str}_$j", RealType)
            val newLet = Let(newId, rec(list(j)), currLet)
            currLet = newLet
          }
          currLet
        }

        // write every element as another variable 
        case MatrixFromLists(lists, numRows, numCols) => {
          var currLet = Let(getOrCreateFreshIdentifier(s"${id.name}_0_0", RealType), rec(lists.head.head), rec(b))
          for(i <- 0 to numRows - 1){
            for(j <- 0 to numCols - 1){
              if(i == 0 && j == 0){
                // we already did this
                ()
              }
              else{
                val newId = getOrCreateFreshIdentifier(s"${id.name}_${i}_$j", RealType)
                try{
                  currLet = Let(newId, rec(lists(i)(j)), currLet)
                }
                catch {
                  case e: Exception => {
                    // For now I'll assume that the error is because lists(i)(j) is not a variable, but an operation
                    // In this case, we first need to process that and put the value in a temporary variable,
                    // and then we can use that temporary variable in the expression
                    indexCounter = indexCounter + 1
                    val tempId = getOrCreateFreshIdentifier(s"temp${indexCounter}", RealType)
                    // First use createUnrolledLet to process the expression
                    val tempLet = createUnrolledLet(tempId, lists(i)(j), Let(newId, Variable(tempId), currLet))
                    tempLet match{
                      case Let(i, v, b) => {
                        currLet = Let(i, v, b)
                      }
                      case _ => {
                        val () = println("ERROR: tempLet is not a Let")
                        throw new Exception("ERROR: tempLet is not a Let")
                      }
                    }
                  }
                }
              }
            }
          }
          currLet
        }

        case CrossProduct(lhs, rhs) => {
          // vector cross product is not supported yet
          if(isVector(lhs) && isVector(rhs)){
            val () = println("ERROR: CrossProduct is not supported for vectors")
            v
          }
          else if(isMatrix(lhs) && isMatrix(rhs)){
            val lhsSize = getDSSize(lhs)
            val rhsSize = getDSSize(rhs)
            val lhsRowSize = lhsSize.head
            val lhsColSize = lhsSize(1)
            val rhsRowSize = rhsSize.head
            val rhsColSize = rhsSize(1)
            if(lhsColSize != rhsRowSize){
              val () = println("ERROR: lhsColSize and rhsRowSize should be the same")
              throw new Exception("ERROR: lhsColSize and rhsRowSize should be the same")
              v
            }
            // since we are in a functional language, I'll represent everything as clear as possible
            // every temporary variable will be named as {lhs}_{rhs}_{index+1} and index will be increased by 1
            // please remind yourself that this is a matrix multiplication
            // we need to multiply every row of lhs with every column of rhs

            var newLet = Let(
              getOrCreateFreshIdentifier(s"${id.name}_0_0", RealType),
              Plus(
                Times(
                  Variable(getOrCreateFreshIdentifier(s"${lhs}_0_0", RealType)),
                  Variable(getOrCreateFreshIdentifier(s"${rhs}_0_0", RealType))
                ),
                Plus(
                  Times(
                    Variable(getOrCreateFreshIdentifier(s"${lhs}_0_1", RealType)),
                    Variable(getOrCreateFreshIdentifier(s"${rhs}_1_0", RealType))
                  ),
                  Times(
                    Variable(getOrCreateFreshIdentifier(s"${lhs}_0_2", RealType)),
                    Variable(getOrCreateFreshIdentifier(s"${rhs}_2_0", RealType))
                  )
                )
              ),
              rec(b)
            )
            // continue this in every element
            for(i <- 0 to lhsRowSize - 1){
              for(j <- 0 to rhsColSize - 1){
                if(i == 0 && j == 0){
                  // we already did this
                  ()
                }
                else{
                  val newId = getOrCreateFreshIdentifier(s"${id.name}_${i}_$j", RealType)
                  val tempLet = Let(
                    newId,
                    Plus(
                      Times(
                        Variable(getOrCreateFreshIdentifier(s"${lhs}_${i}_0", RealType)),
                        Variable(getOrCreateFreshIdentifier(s"${rhs}_0_$j", RealType))
                      ),
                      Plus(
                        Times(
                          Variable(getOrCreateFreshIdentifier(s"${lhs}_${i}_1", RealType)),
                          Variable(getOrCreateFreshIdentifier(s"${rhs}_1_$j", RealType))
                        ),
                        Times(
                          Variable(getOrCreateFreshIdentifier(s"${lhs}_${i}_2", RealType)),
                          Variable(getOrCreateFreshIdentifier(s"${rhs}_2_$j", RealType))
                        )
                      )
                    ),
                    newLet
                  )
                  newLet = tempLet
                }
              }
            }
            val () = println("newLet: " + newLet)
            newLet

          }
          // matrix times vector is also supported
          else if(isMatrix(lhs) && isVector(rhs)){
            val lhsSize = getDSSize(lhs)
            val rhsSize = getDSSize(rhs).head
            val lhsRowSize = lhsSize.head
            val lhsColSize = lhsSize(1)
            if(lhsColSize != rhsSize){
              val () = println("ERROR: lhsColSize and rhsSize should be the same")
              throw new Exception("ERROR: lhsColSize and rhsSize should be the same")
              v
            }
            // since we are in a functional language, I'll represent everything as clear as possible
            // every temporary variable will be named as {lhs}_{rhs}_{index+1} and index will be increased by 1
            // please remind yourself that this is a matrix multiplication
            // we need to multiply every row of lhs with every column of rhs

            var newLet = Let(
              getOrCreateFreshIdentifier(s"${id.name}_0", RealType),
              Plus(
                Times(
                  Variable(getOrCreateFreshIdentifier(s"${lhs}_0_0", RealType)),
                  Variable(getOrCreateFreshIdentifier(s"${rhs}_0", RealType))
                ),
                Plus(
                  Times(
                    Variable(getOrCreateFreshIdentifier(s"${lhs}_0_1", RealType)),
                    Variable(getOrCreateFreshIdentifier(s"${rhs}_1", RealType))
                  ),
                  Times(
                    Variable(getOrCreateFreshIdentifier(s"${lhs}_0_2", RealType)),
                    Variable(getOrCreateFreshIdentifier(s"${rhs}_2", RealType))
                  )
                )
              ),
              rec(b)
            )
            // continue this in every element
            for(i <- 0 to lhsRowSize - 1){
              if(i == 0){
                ()
              }
              else{
                val newId = getOrCreateFreshIdentifier(s"${id.name}_$i", RealType)
                val tempLet = Let(
                  newId,
                  Plus(
                    Times(
                      Variable(getOrCreateFreshIdentifier(s"${lhs}_${i}_0", RealType)),
                      Variable(getOrCreateFreshIdentifier(s"${rhs}_0", RealType))
                    ),
                    Plus(
                      Times(
                        Variable(getOrCreateFreshIdentifier(s"${lhs}_${i}_1", RealType)),
                        Variable(getOrCreateFreshIdentifier(s"${rhs}_1", RealType))
                      ),
                      Times(
                        Variable(getOrCreateFreshIdentifier(s"${lhs}_${i}_2", RealType)),
                        Variable(getOrCreateFreshIdentifier(s"${rhs}_2", RealType))
                      )
                    )
                  ),
                  newLet
                )
                newLet = tempLet
              }
            }
            newLet
          }
          else{
            val () = println("ERROR: CrossProduct is only supported for matrices")
            throw new Exception("ERROR: CrossProduct is only supported for matrices")
            v
          }
        }


    }  
  }

    val rec_fnc = rec(fnc)
    rec_fnc
  }

}
