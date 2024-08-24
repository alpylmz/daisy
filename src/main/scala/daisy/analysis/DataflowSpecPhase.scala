// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.Identifiers._
import lang.Types.RealType
import tools._
import FinitePrecision._
import lang.TreeOps.allIDsOf
import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees._
import lang.Types._
import lang.Extractors.{ArithOperator, ElemFnc}
import lang.Trees._
import lang.TreeOps._

/**
  Computes and stores intermediate ranges.

  Prerequisites:
    - SpecsProcessingPhase
 */
object DataflowSpecPhase extends DaisyPhase with RoundoffEvaluators with IntervalSubdivision with opt.CostFunctions {
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

    type RangeMap = Map[(Expr, Seq[Expr]), Interval]

  var rangeMethod = ""
  var errorMethod = ""
  var trackRoundoffErrs = true

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    rangeMethod = ctx.option[String]("rangeMethod")
    errorMethod = ctx.option[String]("errorMethod")
    trackRoundoffErrs = !ctx.hasFlag("noRoundoff")

    // let's for now divide every variable's bound by 2, so that
    // if we have 3 variables, we'll have 2^3 = 8 subdivisions

    val choosePrecision = ctx.option[String]("choosePrecision")

    val uniformPrecision = ctx.option[Precision]("precision")
    val reporter = ctx.reporter

    val targetError = ctx.target_error_dataflowspecphase
    reporter.info("target_error: " + targetError)

    val fncsToConsider = if (ctx.hasFlag("approx")) functionsToConsider(ctx, prg).filter(_.returnType == RealType)
      else functionsToConsider(ctx, prg)

    val fnc = fncsToConsider.head

    // Assuming there is only one function
    val fncBody = fnc.body.get

    val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

    // add variables from let statements that do not have any explicit type assignment
    val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id) ++
    allIDsOf(fnc.body.get).diff(ctx.specInputPrecisions(fnc.id).keySet).map(id => (id -> uniformPrecision)).toMap

    // read the inputValMap, and create a sequence where each element is a subdivided version
    // of the whole inputValMap
    // for example, if inputValMap = {x -> [0, 1], y -> [0, 1]}
    // then the sequence will be
    // Seq(
    //   {x -> [0, 0.5], y -> [0, 0.5]},
    //   {x -> [0, 0.5], y -> [0.5, 1]},
    //   {x -> [0.5, 1], y -> [0, 0.5]},
    //   {x -> [0.5, 1], y -> [0.5, 1]}
    // )

    // precond only used when the range calculation uses smt, but still, I'll change its values
    // just to be safe and future-proof
    val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)
    //ctx.reporter.info("inputValMap: " + inputValMap)
    val precond = fnc.precondition.get // replaced ctx.specAdditionalConstraints(fnc.id)
    //ctx.reporter.info("precond: " + precond)
    
    // I guess the best way is just writing a recursive function
    val err = smartSubdivide(inputValMap, inputErrorMap, ctx.typeConfig, fncBody, uniformPrecision, precond, ctx, targetError)
    ctx.reporter.info("err: " + err)

    //val res = computeRoundoff(inputValMap, inputErrorMap, precisionMap, fncBody,
    //uniformPrecision, precond, ctx)


    (ctx, prg) // unnecessary, here for the sake of the type

  }

  def smartSubdivide(
    inputValMap: Map[Identifier, Interval], 
    inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], 
    expr: Expr, constPrecision: Precision, 
    precond: Expr, 
    ctx: Context,
    targetError: Rational
    ): Rational = {
    ctx.reporter.info("current inputValMap: " + inputValMap)
    ctx.reporter.info("current precisionMap: " + precisionMap)
    val (resRange, intermediateRanges) = computeRange(inputValMap, expr, precond)
    val resError = computeAbsError(expr, precisionMap, constPrecision, intermediateRanges, ctx.path)
        
        
    ctx.reporter.info("resError: " + resError)
    ctx.reporter.info("resRange: " + resRange)
    // check if error is within targetError
    if (resError <= targetError) {
      return resError
    }
    // if the size of any interval is 0.1, then we can't subdivide further
    // return resError
    val intervalSizes = inputValMap.map({ case (id, interval) => interval.xhi - interval.xlo })
    if (intervalSizes.exists(_ <= 0.1)) {
      return resError
    }


    // if not, subdivide the inputValMap
    // create the sequence of subintervals
    // for example, if inputValMap = {x -> [0, 1], y -> [0, 1]}
    // then the sequence will be
    // Seq(
    //   {x -> [0, 0.5], y -> [0, 0.5]},
    //   {x -> [0, 0.5], y -> [0.5, 1]},
    //   {x -> [0.5, 1], y -> [0, 0.5]},
    //   {x -> [0.5, 1], y -> [0.5, 1]}
    // )
    def subdivide(inputValMap: Map[Identifier, Interval]): Seq[Map[Identifier, Interval]] = {
        if (inputValMap.isEmpty) {
            return Seq(Map())
        }
        // take head
        val (id, interval) = inputValMap.head
        // remove head
        val tail = inputValMap.tail
        val interval_start = interval.xlo
        val interval_end = interval.xhi
        val interval_mid = (interval_start + interval_end) / 2.0
        val left = Map(id -> Interval(interval_start, interval_mid))
        val right = Map(id -> Interval(interval_mid, interval_end))
        val left_subdivided = subdivide(tail) map (m => left ++ m)
        val right_subdivided = subdivide(tail) map (m => right ++ m)
        left_subdivided ++ right_subdivided
    }
    val subintervals = subdivide(inputValMap)
    ctx.reporter.info("subintervals: " + subintervals)
    // for each subinterval, call smartSubdivide
    val errors = subintervals map (subinterval => smartSubdivide(subinterval, inputErrorMap, precisionMap, expr, constPrecision, precond, ctx, targetError))
    // return the maximum error
    ctx.reporter.info("returning max error: " + errors.max)
    errors.max
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

  /*
    Changes a real-valued program into a finite-precision one according to a given
    type configuration and by inserting necessary (down) casts.

    In addition, this function also computes the updated range map, including
    expressions with casts.
  */
  def applyFinitePrecision(expr: Expr, typeConfig: Map[Identifier, Precision],
    currRanges: RangeMap): (Expr, RangeMap) = {

    def recurse(e: Expr, path: PathCond): (Expr, RangeMap) = (e: @unchecked) match {

      case x @ Variable(id) =>
        val newExpr = Variable(id.changeType(FinitePrecisionType(typeConfig(id))))
        (newExpr, Map((newExpr, emptyPath) -> currRanges(x, emptyPath)))

      case Let(id, x @ Variable(idV), body) =>
        val idPrec = typeConfig(id)
        val newValue = Variable(idV.changeType(FinitePrecisionType(typeConfig(idV))))
        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, bodyMap + ((newValue, path) -> currRanges(x, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))

      case Let(id, x @ RealLiteral(r), body) =>
        val idPrec = typeConfig(id)
        val newValue = FinitePrecisionLiteral(r, idPrec, x.stringValue)
        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, bodyMap + ((newValue, path) -> currRanges(x, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))

      case Let(id, x @ ElemFnc(t @ Variable(tId), recons), body) =>
        println("ElemFnccorrect")
        println(id)
        val idPrec = typeConfig (id)

        val (valueExpr, valueMap) = recurse(t, path)
        var newValue: Expr = recons(valueExpr)
        if (idPrec < typeConfig(tId) ) { // need to downcast
        newValue = Cast(newValue, FinitePrecisionType(idPrec))
        }

        val (bodyExpr, bodyMap) = recurse (body, path)

        val newExpr = Let (id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, (bodyMap ++ valueMap) + ((newValue, path) -> currRanges (x, path) ) +
        ((Variable (id), emptyPath) -> currRanges ((Variable (id), emptyPath) ) ) +
          ((x, path) -> currRanges (x, path) ) ) // fixes the issue of java.util.NoSuchElementException: key not found: (sin(_tmp),List())


      case Let(id, x @ ArithOperator(Seq(t @ Variable(tId)), recons), body) =>
        val idPrec = typeConfig(id)

        val (valueExpr, valueMap) = recurse(t, path)
        var newValue: Expr = recons(Seq(valueExpr))
        if (idPrec < typeConfig(tId)) { // need to downcast
          newValue = Cast(newValue, FinitePrecisionType(idPrec))
        }

        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, (bodyMap ++ valueMap) + ((newValue, path) -> currRanges(x, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))

      case Let(id, x @ ArithOperator(Seq(y @ Variable(lhs), z @ Variable(rhs)), recons), body) =>
        val idPrec = typeConfig(id)
        val lPrec = typeConfig(lhs)
        val rPrec = typeConfig(rhs)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        val leftExpr = Variable(lhs.changeType(FinitePrecisionType(opPrec)))
        val rightExpr = Variable(rhs.changeType(FinitePrecisionType(opPrec)))

        val leftMap: RangeMap = Map((leftExpr, emptyPath) -> currRanges(y, emptyPath))
        val rightMap = Map((rightExpr, emptyPath) -> currRanges(z, emptyPath))

        var newValue = recons(Seq(leftExpr, rightExpr))

        val rangeMap = (leftMap ++ rightMap) + ((newValue, path) -> currRanges(x, path))
        if (idPrec < opPrec) { //need to downcast
          newValue = Cast(newValue, FinitePrecisionType(idPrec))
        }

        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, (rangeMap ++ bodyMap) +
          ((newValue, path) -> currRanges(x, path)) +
          ((leftExpr, path) -> currRanges(y, path)) +
          ((rightExpr, path) -> currRanges(z, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))


      case x @ ArithOperator(Seq(t @ Variable(_)), recons) =>
        val (newExpr, newMap) = recurse(t, path)
        val tmp = recons(Seq(newExpr))
        (tmp, newMap + ((tmp, path) -> currRanges(x, path)))

      case x @ ArithOperator(Seq(y @ Variable(_), z @ Variable(_)), recons) =>
        val (lhsExpr, lhsMap) = recurse(y, path)
        val (rhsExpr, rhsMap) = recurse(z, path)
        val tmp = recons(Seq(lhsExpr, rhsExpr))
        (tmp, (lhsMap ++ rhsMap) + ((tmp, path) -> currRanges(x, path)))

      case x @ IfExpr(cond, thenn, elze) =>
        val (condExpr, condMap) = recurse(cond, path)
        val (thenExpr, thenMap) = recurse(thenn, path :+ cond)
        val (elseExpr, elseMap) = recurse(elze, path :+ lang.TreeOps.negate(cond))
        val tmp = IfExpr(condExpr, thenExpr, elseExpr)

        (tmp, (condMap ++ thenMap ++ elseMap) + ((tmp, path) -> currRanges(x, path)))

      case LessThan(lhs, rhs) =>
        (LessThan(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())

      case GreaterThan(lhs, rhs) =>
        (GreaterThan(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())

      case LessEquals(lhs, rhs) =>
        (LessEquals(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())

      case GreaterEquals(lhs, rhs) =>
        (GreaterEquals(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())


    }
    recurse(expr, emptyPath)
  }

  /*
    Computes the roundoff error by first introducing casts into the expressions,
    based on the typeConfig, and then running the standard roundoff error analysis.
   */
  def computeAbsError(expr: Expr, typeConfig: Map[Identifier, Precision],
    constantsPrecision: Precision, rangeMap: Map[(Expr, PathCond), Interval],
    path: PathCond, approximate: Boolean = false): Rational = {

    // create finite-precision version
    val (finitePrecBody, newRangeMap) = applyFinitePrecision(expr, typeConfig, rangeMap)

    // roundoff errors depend on the typeConfig, need to be recomputed
    val inputErrorMap: Map[Identifier, Rational] = freeVariablesOf(expr).map({
      case id: Identifier =>
        (id -> {
            val idPrec_ = typeConfig.filter(x => x._1.name == id.name)
            val idPrec = idPrec_.head._2
            idPrec.absRoundoff(rangeMap((Variable(id), emptyPath)))
        })
    }).toMap

    // run regular roundoff error analysis
    val (resRoundoff, _) = evalRoundoff[AffineForm](finitePrecBody, rangeMap ++ newRangeMap,
      typeConfig,
      inputErrorMap.mapValues(AffineForm.+/-).toMap,
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = constantsPrecision,
      trackRoundoffErrors = true,
      approxRoundoff = approximate)

    Interval.maxAbs(resRoundoff.toInterval)
  }

  def computeErrors(intermediateRanges: Map[(Expr, PathCond), Interval], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], expr: Expr, constPrecision: Precision):
  (Rational, Map[(Expr, PathCond), Rational]) = {

    (errorMethod: @unchecked) match {
      case "interval" =>
        val (resRoundoff, allErrors) = evalRoundoff[Interval](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(Interval.+/-).toMap,
          zeroError = Interval.zero,
          fromError = Interval.+/-,
          interval2T = Interval.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(Interval.maxAbs).toMap)

      case "affine" =>

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

  def computeRoundoff(
    inputValMap: Map[Identifier, Interval], 
    inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], 
    expr: Expr, constPrecision: Precision, 
    precond: Expr, 
    ctx: Context):
    (Rational, Interval) = {

    ctx.reporter.info("Computing range")

    val (resRange, intermediateRanges) = computeRange(inputValMap, expr, precond)

    // print curr time
    ctx.reporter.info("Computing errors")

    val (resError, intermediateErrors) = computeErrors(intermediateRanges, inputErrorMap, precisionMap, expr,
      constPrecision)

    // print errors
    ctx.reporter.info("resError: " + resError)

    (resError, resRange)
  }
}
