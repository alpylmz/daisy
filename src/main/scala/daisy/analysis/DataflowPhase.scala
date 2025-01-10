// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees
import lang.Trees._
import lang.Identifiers._
import lang.Types.RealType
import tools._
import FinitePrecision._
import lang.TreeOps.allIDsOf
import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees._
import daisy.opt.MixedPrecisionOptimizationPhase.computeAbsErrorInDiffIntervalsReturnTuple


import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import java.util.concurrent.Semaphore
import scala.concurrent.ExecutionContext.Implicits.global


/**
  Computes and stores intermediate ranges.

  Prerequisites:
    - SpecsProcessingPhase
 */
object DataflowPhase extends DaisyPhase with RoundoffEvaluators with IntervalSubdivision with opt.CostFunctions {
  override val name = "Dataflow error"
  override val description = "Computes ranges and absolute errors via dataflow analysis"

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "errorMethod",
      Set("affine", "interval", "intervalMPFR", "affineMPFR"),
      "affine",
      "Method for error analysis"),
    StringChoiceOption(
      "choosePrecision",
      Set("no", "fixed", "float"),
      "no",
      "choose the fixed/floating-point precision which satisfies error bound")
  )
  override implicit val debugSection = DebugSectionAnalysis

  var rangeMethod = ""
  var errorMethod = ""
  var trackRoundoffErrs = true

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    rangeMethod = ctx.option[String]("rangeMethod")
    errorMethod = ctx.option[String]("errorMethod")
    trackRoundoffErrs = !ctx.hasFlag("noRoundoff")

    val choosePrecision = ctx.option[String]("choosePrecision")

    val mixedPrecision = ctx.option[Option[String]]("mixed-precision").isDefined
    val uniformPrecision = ctx.option[Precision]("precision")
    val reporter = ctx.reporter

    ctx.reporter.info(s"using $rangeMethod for ranges, $errorMethod for errors")

    var uniformPrecisions = Map[Identifier, Precision]()

    val fncsToConsider = if (ctx.hasFlag("approx")) functionsToConsider(ctx, prg).filter(_.returnType == RealType)
      else functionsToConsider(ctx, prg)
    ctx.reporter.info("initialized some variables")
    // returns (abs error, result range, interm. errors, interm. ranges)
    val res: Map[Identifier, (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval], Map[Identifier, Precision])] =
      fncsToConsider.map({ fnc =>

      var inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)
      // sort inputValMap by the variable name
      inputValMap = inputValMap.toList.sortBy(_._1.name).toMap

      val fncBody = fnc.body.get

      if (choosePrecision != "no" && !ctx.specResultErrorBounds.contains(fnc.id)) {
        reporter.warning(s"Function ${fnc.id} does not have target error bound, cannot choose precision.")
      }

      if (choosePrecision != "no" && ctx.specResultErrorBounds.contains(fnc.id)) {
        reporter.info("analyzing fnc: " + fnc.id)

        // the max tolerated error
        val targetError = ctx.specResultErrorBounds(fnc.id)

        val availablePrecisions = choosePrecision match {
          case "fixed" =>
            // the max available amount of bits
            val maxBits = uniformPrecision match {
              case FixedPrecision(b) => b
              case _ => 32 // TODO put default elsewhere
            }
            (1 to maxBits).map(x => FixedPrecision(x))
          case "float" => List(Float16, Float32, Float64, DoubleDouble, QuadDouble)
          case s => throw new Exception(s"Unknown choice value for choosePrecision: $s. Stopping now")
        }

        reporter.info(s"choosing among $choosePrecision precisions")

        // save the intermediate result
        var res: (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval]) = null

        // find precision which is sufficient
        availablePrecisions.find( prec => {
          try {
            reporter.debug(s"trying precision $prec")
            val allIDs = fnc.params.map(_.id)
            val inputErrorMap = allIDs.map(id => (id -> prec.absRoundoff(inputValMap(id)))).toMap
            val precisionMap: Map[Identifier, Precision] = allIDsOf(fnc.body.get).map(id => (id -> prec)).toMap

            res = computeRoundoff(inputValMap, inputErrorMap, precisionMap, fncBody,
              prec, fnc.precondition.get, ctx) // replaced ctx.specAdditionalConstraints(fnc.id)

            res._1 <= targetError
          } catch {
            case OverflowException(_) | DivisionByZeroException(_) => false // div by zero can disappear with higher precisions
          }
        }) match {

          case None =>
            val lastPrec = availablePrecisions.last
            reporter.warning(s"Highest available precision ${lastPrec} " +
              "is not sufficient. Using it anyway.")
            uniformPrecisions = uniformPrecisions + (fnc.id -> lastPrec)

          case Some(prec) =>
            uniformPrecisions = uniformPrecisions + (fnc.id -> prec)
        }
        val result: (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval], Map[Identifier, Precision]) =
          (res._1, res._2, res._3, res._4, allIDsOf(fnc.body.get).map(id => (id -> uniformPrecisions(fnc.id))).toMap)
        (fnc.id -> result)

      } else {
        ctx.reporter.info("analyzing fnc: " + fnc.id)

        if (!mixedPrecision) {
          ctx.reporter.info(s"error analysis for uniform $uniformPrecision precision")
        }
        val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

        // add variables from let statements that do not have any explicit type assignment
        val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id) ++
          allIDsOf(fnc.body.get).diff(ctx.specInputPrecisions(fnc.id).keySet).map(id => (id -> uniformPrecision)).toMap
        uniformPrecisions = uniformPrecisions + (fnc.id -> uniformPrecision) // so that this info is available in codegen

        val precond = fnc.precondition.get // replaced ctx.specAdditionalConstraints(fnc.id)
        
        reporter.info("starting roundoff at time: " + System.currentTimeMillis())
        val res = computeRoundoffDivisionByZero(inputValMap, inputErrorMap, precisionMap, fncBody,
          uniformPrecision, precond, ctx)
        reporter.info("finished roundoff at time: " + System.currentTimeMillis())
        // close the execution context
        //executionContext.shutdown()
        reporter.info("res: " + res._1)
        reporter.info("res: " + res._2)
        val result: (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval], Map[Identifier, Precision]) = (res._1, res._2, res._3, res._4, precisionMap)
        (fnc.id -> result)
      
      }
    }).toMap

    /*
    (
      daisy.lang.Identifiers.Identifier, 
      (
        daisy.tools.Rational, 
        daisy.tools.Interval, 
        scala.collection.immutable.Map[_ <: (daisy.lang.Trees.Expr, daisy.PathCond), daisy.tools.Rational], 
        Map[(daisy.lang.Trees.Expr, daisy.PathCond),daisy.tools.Interval], 
        Map[daisy.lang.Identifiers.Identifier,daisy.tools.FinitePrecision.Precision])) <:< 
    (
      daisy.lang.Identifiers.Identifier, 
      (
        daisy.tools.Rational, 
        daisy.tools.Interval, 
        Map[(daisy.lang.Trees.Expr, daisy.PathCond),daisy.tools.Rational], 
        Map[(daisy.lang.Trees.Expr, daisy.PathCond),daisy.tools.Interval], 
        Map[daisy.lang.Identifiers.Identifier,daisy.tools.FinitePrecision.Precision]
      )
    )
    */
    ///println("inputValMap: ")
    ///println(inputValMap)
    ///// precond is not used in interval anyways
    ///val (total_range, newRangeMap) = computeRange(inputValMap, fncBody, precond)

    (ctx.copy(specInputPrecisions = ctx.specInputPrecisions ++ res.mapValues(_._5).toMap,
      uniformPrecisions = ctx.uniformPrecisions ++ uniformPrecisions,
      resultAbsoluteErrors = ctx.resultAbsoluteErrors ++ res.mapValues(_._1).toMap,
      resultRealRanges = ctx.resultRealRanges ++ res.mapValues(_._2).toMap,
      intermediateAbsErrors = ctx.intermediateAbsErrors ++ res.mapValues(_._3).toMap,
      intermediateRanges = ctx.intermediateRanges ++ res.mapValues(_._4).toMap,
      assignedPrecisions = ctx.assignedPrecisions ++ uniformPrecisions.map({case (fncid, prec) => fncid -> Map[Identifier, Precision]().withDefaultValue(prec)})
    ), prg)

  }

  def computeRange(inputValMap: Map[Identifier, Interval], expr: Expr, precond: Expr):
  (Interval, Map[(Expr, PathCond), Interval]) = {

    (rangeMethod: @unchecked) match {
      case "interval" =>
        evalRange[Interval](expr, inputValMap, Interval.apply)

      case "affine" =>
        val (rng, intrmdRange) = evalRange[AffineForm](expr,
          inputValMap.mapValues(AffineForm(_)).toMap, AffineForm.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)

      case "smt" =>
        // SMT can take into account additional constraints
        val (rng, intrmdRange) = evalRange[SMTRange](expr,
          inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, precond)) }),
          SMTRange.apply(_, precond))
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)

      case "intervalMPFR" =>
        val (rng, intrmdRange) = evalRange[MPFRInterval](expr,
          inputValMap.mapValues(MPFRInterval(_)).toMap, MPFRInterval.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)

      case "affineMPFR" =>
        val (rng, intrmdRange) = evalRange[MPFRAffineForm](expr,
          inputValMap.mapValues(MPFRAffineForm(_)).toMap, MPFRAffineForm.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)
    }
  }

  def computeErrors(intermediateRanges: Map[(Expr, PathCond), Interval], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], expr: Expr, constPrecision: Precision):
  (Rational, Map[(Expr, PathCond), Rational]) = {

    (errorMethod: @unchecked) match {
      case "interval" =>
        /*
        val (resRoundoff, allErrors) = evalRoundoff[Interval](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(Interval.+/-).toMap,
          zeroError = Interval.zero,
          fromError = Interval.+/-,
          interval2T = Interval.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(Interval.maxAbs).toMap)
        */
        
        val (resRoundoff, allErrors) = evalRoundoffInterval(expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(Interval.+/-).toMap,
          zeroError = Interval.zero,
          fromError = Interval.+/-,
          interval2T = Interval.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(Interval.maxAbs).toMap)
      
      case "affine" =>
        println("computing roundoff errors with affine forms")
        val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(AffineForm.+/-).toMap,
          zeroError = AffineForm.zero,
          fromError = AffineForm.+/-,
          interval2T = AffineForm.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)).toMap)

      case "intervalMPFR" =>

        val (resRoundoff, allErrors) = evalRoundoff[MPFRInterval](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(MPFRInterval.+/-).toMap,
          zeroError = MPFRInterval.zero,
          fromError = MPFRInterval.+/-,
          interval2T = MPFRInterval.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)).toMap)

      case "affineMPFR" =>

        val (resRoundoff, allErrors) = evalRoundoff[MPFRAffineForm](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(MPFRAffineForm.+/-).toMap,
          zeroError = MPFRAffineForm.zero,
          fromError = MPFRAffineForm.+/-,
          interval2T = MPFRAffineForm.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)).toMap)
    }
  }

  def expressionGetFinalLet(expr: Expr): Expr = {
    expr match {
      case Let(id, value, body) => expressionGetFinalLet(body)
      case _ => expr
    }
  }

  def takeCombinations(
    lst: List[List[Expr]]
  ): List[Map[Identifier, Interval]] = {

    // Step 1: convert each sub-list of Expr into a List of Map(id -> Interval(...))
    val listOfMapLists: List[List[Map[Identifier, Interval]]] =
      lst.map { subList =>
        subList.map {
          case And(conds) =>
            // Expecting conds of length 2:  a(0) is GreaterThan(...), a(1) is LessThan(...)
            val (id, lb) = conds(0) match {
              case GreaterThan(Variable(id), RealLiteral(value)) => (id, value)
            }
            val ub = conds(1) match {
              case LessThan(_, RealLiteral(value)) => value
            }
            Map(id -> Interval(lb, ub))
        }
      }

    // If there's nothing, return empty
    if (listOfMapLists.isEmpty) return Nil

    // Step 2: combine them step by step
    var result: List[Map[Identifier, Interval]] = List(Map.empty)

    for (maps <- listOfMapLists) {
      // Build up new partial results in a buffer
      val newResult = scala.collection.mutable.ListBuffer[Map[Identifier, Interval]]()
      // For each existing combination, combine with every map in `maps`
      for (oldCombo <- result; m <- maps) {
        // Merge them (++ on Map is cheap for small maps, but can still allocate)
        newResult += (oldCombo ++ m)
      }
      // Convert to a list at the end of this layer
      result = newResult.toList
    }

    result
  }



  def inputValMaptoPrecondition(inputValMap: Map[Identifier, Interval]): Expr = {
    // Just read all intervals and add to the Expr as And()s
    // I will try to do that now
    var preconditions = List[Expr]()
    inputValMap.foreach(x => {
      val id = x._1
      val interval = x._2
      //preconditions = preconditions :+ And(GreaterThan(Variable(id), RealLiteral(interval.xlo)), LessThan(Variable(id), RealLiteral(interval.xhi)))
      preconditions = preconditions :+ GreaterThan(Variable(id), RealLiteral(interval.xlo))
      preconditions = preconditions :+ LessThan(Variable(id), RealLiteral(interval.xhi))
    })
    //reporter.info("preconditions_inside: ")
    //reporter.info(preconditions)
    // the next step is somehow to combine all of these into one expression
    // I will try to do that now, it will be like flatten
    // Use Seq to flatten the list
    var result = And(preconditions)

    result
  }

  def calculateInputValMaps(precond: Expr, inputValMap: Map[Identifier, Interval]): List[Map[Identifier, Interval]] = {
    val preconditions_list = precond match{
      case Trees.And(a) => a
    }

    val inputValMapSorted = inputValMap.toList.sortBy(_._1.name)
    val () = println("Dividing the inputValMap: " + inputValMapSorted)

    val minimumIntervalSize = 0.1
    var i = 0
    var preconditions = List[List[Expr]]()
    var inputValMaps = List[Map[Identifier, Interval]]()
    var less_than_min_count = 0
    while(i < preconditions_list.length){
      val lower_bound = preconditions_list(i)
      val upper_bound = preconditions_list(i+1)
      val lower_bound_value = lower_bound match {
        case LessThan(_, value) => value match {
          case RealLiteral(r) => r
        }
        case GreaterThan(_, value) => value match {
          case RealLiteral(r) => r
        }
      }
      val upper_bound_value = upper_bound match {
        case LessThan(_, value) => value match {
          case RealLiteral(r) => r
        }
        case GreaterThan(_, value) => value match {
          case RealLiteral(r) => r
        }
      }
      val var_id = lower_bound match {
        case LessThan(id, _) => id
        case GreaterThan(id, _) => id
      }
      //reporter.info("Lower bound value:")
      //reporter.info(lower_bound_value)
      //reporter.info("Upper bound value:")
      //reporter.info(upper_bound_value)
      if(upper_bound_value - lower_bound_value < minimumIntervalSize){
        // if the difference is too small, we can skip this one, but other variables still may need to be subdivided
        preconditions = preconditions :+ List(
          And(
            GreaterThan(var_id, RealLiteral(lower_bound_value)),
            LessThan(var_id, RealLiteral(upper_bound_value)),
          ),
        )
        less_than_min_count = less_than_min_count + 1
      }
      else{
        
        preconditions = preconditions :+ List(
          And(
            GreaterThan(var_id, RealLiteral(lower_bound_value)),
            LessThan(var_id, RealLiteral((upper_bound_value + lower_bound_value) / 2)),
          ),
          And(
            GreaterThan(var_id, RealLiteral((upper_bound_value + lower_bound_value) / 2)),
            LessThan(var_id, RealLiteral(upper_bound_value)),
          ),
        )
      }
      i = i + 2
    }

    println("Taking combinations")

    inputValMaps = takeCombinations(preconditions)

    println("Finished taking combinations")

    // I am too lazy to solve the bug, if there is an element identical to inputValMap, remove it from inputValMaps
    inputValMaps = inputValMaps.filter(x => x != inputValMap)
    
    // if the length of inputValMaps is 1, then we need to raise an exception
    if(inputValMaps.length == 0){
      // return dummy results with a big error
      throw new Exception("Cannot solve DivisionByZeroException with inputValMap: " + inputValMap)
    }

    inputValMaps

  }

  def computeRoundoffDivisionByZero(inputValMap: Map[Identifier, Interval], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], expr: Expr, constPrecision: Precision, precond: Expr, ctx: Context):
    (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval]) = {

    var inputValMaps = List[Map[Identifier, Interval]]()
    // divide no matter what
    try{
      inputValMaps = calculateInputValMaps(precond, inputValMap)
      println("Done with calculateInputValMaps")
    } catch {
      // if you can't divide have only one inputValMap
      case e: Exception => 
        val (resRange, intermediateRanges) = computeRange(inputValMap, expr, precond)

        val (resError, intermediateErrors) = computeErrors(intermediateRanges, inputErrorMap, precisionMap, expr,
          constPrecision)

        val () = println("INFO: Successfull on minimal " + inputValMap)
    
        return (resError, resRange, intermediateErrors, intermediateRanges)
    }
    //var results = List[(Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval])]()

    val () = println("Starting threads for inputValMap: " + inputValMap)
    // run the computations in parallel
    val futureResults: List[Future[(Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval])]] = inputValMaps.map(inputValMapNew => {
      Future {
        try{
          val temp_precond = inputValMaptoPrecondition(inputValMapNew)
          val () = println("MaptoPrecond thread")
          val (resRange, intermediateRanges) = computeRange(inputValMapNew, expr, temp_precond)
          val () = println("Range thread")
          val (resError, intermediateErrors) = computeErrors(intermediateRanges, inputErrorMap, precisionMap, expr,
            constPrecision)
    
            val () = println("INFO: Successfull " + inputValMapNew)
    
          (resError, resRange, intermediateErrors, intermediateRanges)
        } catch {
          case e: DivisionByZeroException => 
            // if there is a division by zero exception, return a big error
            val () = println("DivisionByZeroException with inputValMap: " + inputValMapNew)
            // return dummy results with a big error
            (Rational(424242424), Interval(-424242424, 424242424), Map(), Map())
        }
      }
    })
    
    // wait for all the futures to complete
    var results = Await.result(Future.sequence(futureResults), Duration.Inf)
    val () = println("Finished threads for inputValMap: " + inputValMap)
    
    // Then, go over all the results. Find the exceptions, and recursively call the function
    // with the new inputValMap
    // if there is a division by zero exception, then we need to subdivide the inputValMap
    // into smaller intervals
    // then we need to call the function recursively with the new inputValMap
    var j = 0
    inputValMaps.map(inputValMapNew => {
      val temp_precond = inputValMaptoPrecondition(inputValMapNew)
      // is there a division by zero exception?
      if(results(j)._1 == Rational(424242424)){
        val () = println("Exception found in " + j)
        // if there is a division by zero exception, then we need to subdivide the inputValMap
        // into smaller intervals
        // then we need to call the function recursively with the new inputValMap
        val new_results = computeRoundoffDivisionByZero(inputValMapNew, inputErrorMap, precisionMap, expr, constPrecision, temp_precond, ctx)
        val () = println("INFO: Successfully solved exception " + inputValMapNew)
        results = results.updated(j, new_results)
      }
      j = j + 1
    })

    // result is resError, ResRange, intermediateErrors, intermediateRanges

    //println("finding max error")
    // find the maximum error
    var max_error = results(0)._1
    var max_error_index = 0
    for(i <- 1 until results.length){
      //println("curr error: " + results(i)._1)
      if(results(i)._1 > max_error){
        max_error = results(i)._1
        max_error_index = i
      }
    }
    //println("max error: " + max_error)

    //println("finding resRange")
    // merge all resRanges
    var resRange = results(0)._2
    for(i <- 1 until results.length){
      //println("curr resRange: " + results(i)._2)
      resRange = resRange.union(results(i)._2)
    }
    //println("resRange: " + resRange)

    // merge all intermediateErrors
    var intermediateErrors = results(0)._3
    for(i <- 1 until results.length){
      // find the same keys, and store the maximum value
      results(i)._3.foreach({ case (expr, error) =>
        if(intermediateErrors.contains(expr)){
          if(intermediateErrors(expr) < error){
            intermediateErrors = intermediateErrors.updated(expr, error)
          }
        }
        else{
          intermediateErrors = intermediateErrors + (expr -> error)
        }
      })
    }

    // merge all intermediateRanges
    //println("finding intermediateRanges")
    var intermediateRanges = results(0)._4
    for(i <- 1 until results.length){
      // find the same keys, and take the union of the intervals
      //println("curr intermediateRanges: " + results(i)._4)
      results(i)._4.foreach({ case (expr, range) =>
        if(intermediateRanges.contains(expr)){
          intermediateRanges = intermediateRanges.updated(expr, intermediateRanges(expr).union(range))
        }
        else{
          intermediateRanges = intermediateRanges + (expr -> range)
        }
      }) 
    }
    //println("intermediateRanges: ")
    //println(intermediateRanges)
    (max_error, resRange, intermediateErrors, intermediateRanges)
    }

  def computeRoundoff(inputValMap: Map[Identifier, Interval], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], expr: Expr, constPrecision: Precision, precond: Expr, ctx: Context):
    (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval]) = {

    val startTime = System.currentTimeMillis()
    val (resRange, intermediateRanges) = computeRange(inputValMap, expr, precond)
    ctx.reporter.info(s"Range computation took ${System.currentTimeMillis() - startTime} ms")

    // print ranges
    ctx.reporter.info("resRange: " + resRange)
    intermediateRanges.foreach({ case (expr, range) =>
      // if the expr is a let expression, print the last expression
      // case match
      expr match {
        case (a, b) =>
          val filtered_expr = expressionGetFinalLet(a)
          filtered_expr match{
            case Plus(_, _) => 
            case Minus(_, _) => 
            case Times(_, _) => 
            case Division(_, _) => 
            case FloatLiteral(_) => 
            case _ => //ctx.reporter.info(s"$filtered_expr: $range")
          }
        case _ =>
          //ctx.reporter.info(s"$expr: $range")
      }
      //ctx.reporter.info(s"$expr: $range")
    })

    // print curr time
    ctx.reporter.info("Computing errors")

    val errorStartTime = System.currentTimeMillis()
    val (resError, intermediateErrors) = computeErrors(intermediateRanges, inputErrorMap, precisionMap, expr,
      constPrecision)
    ctx.reporter.info(s"Error computation took ${System.currentTimeMillis() - errorStartTime} ms")

    // print errors
    ctx.reporter.info("resError: " + resError)

    (resError, resRange, intermediateErrors, intermediateRanges)
  }
}