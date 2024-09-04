// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Types.{ FinitePrecisionType}
import lang.Trees._
import lang.Identifiers._
import FinitePrecision._
import Rational._
import daisy.utils.CachingMap

trait RoundoffEvaluators extends RangeEvaluators {

  /**
   * Calculates the roundoff error for a given uniform precision
   * using interval arithmetic for ranges and affine arithmetic for errors.
   *
   * @param expr expression for which to compute roundoff
   * @param inputValMap real-valued ranges of all input variables
   * @param inputErrorMap errors of all input variables (incl. roundoff)
   * @param uniformPrecision precision for the entire computation
   *
   * @return (max. absolute roundoff error bound, real-valued result interval)
   */
  def uniformRoundoff_IA_AA(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision,
    trackRoundoffErrors: Boolean = true,
    approxRoundoff: Boolean = false): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[Interval](expr, inputValMap, Interval.apply)

    val (resRoundoff, _) = evalRoundoff[AffineForm](expr, intermediateRanges,
      Map.empty.withDefaultValue(uniformPrecision),
      inputErrorMap.mapValues(AffineForm.+/-).toMap,
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors,
      approxRoundoff)

    (Interval.maxAbs(resRoundoff.toInterval), resRange)
  }

  /**
   * Calculates the roundoff error for a given uniform precision
   * using affine arithmetic for ranges and affine arithmetic for errors.
   *
   * @param expr expression for which to compute roundoff
   * @param inputValMap real-valued ranges of all input variables
   * @param inputErrorMap errors of all input variables (incl. roundoff)
   * @param uniformPrecision precision for the entire computation
   */
  def uniformRoundoff_AA_AA(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision,
    trackRoundoffErrors: Boolean = true,
    approxRoundoff: Boolean = false): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[AffineForm](expr,
      inputValMap.mapValues(AffineForm(_)).toMap, AffineForm.apply)

    val (resRoundoff, _) = evalRoundoff[AffineForm](expr,
      intermediateRanges.mapValues(_.toInterval).toMap,
      Map.empty.withDefaultValue(uniformPrecision),
      inputErrorMap.mapValues(AffineForm.+/-).toMap,
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors,
      approxRoundoff)

    (Interval.maxAbs(resRoundoff.toInterval), resRange.toInterval)
  }

  /**
   * Calculates the roundoff error for a given uniform precision
   * using SMTRange for ranges and affine arithmetic for errors.
   *
   * @param expr expression for which to compute roundoff
   * @param inputValMap real-valued ranges of all input variables
   * @param inputErrorMap errors of all input variables (incl. roundoff)
   * @param uniformPrecision precision for the entire computation
   */
  def uniformRoundoff_SMT_AA(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    precondition: Expr,
    uniformPrecision: Precision,
    trackRoundoffErrors: Boolean = true,
    approxRoundoff: Boolean = false): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[SMTRange](expr,
      inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, precondition)) }),
      SMTRange.apply(_, precondition))

    val (resRoundoff, _) = evalRoundoff[AffineForm](expr,
      intermediateRanges.mapValues(_.toInterval).toMap,
      Map.empty.withDefaultValue(uniformPrecision),
      inputErrorMap.mapValues(AffineForm.+/-).toMap,
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors,
      approxRoundoff)

    (Interval.maxAbs(resRoundoff.toInterval), resRange.toInterval)
  }

  /**
   * Theorem statement: If y / 2 <= x <= 2 * y
   * then the result of (x - y) does not produce any roundoff error
   * @param x
   * @param y
   * @return true if the theorem applies, false otherwise
   */
  @inline
  private def sterbenzTheoremApplies(x: Interval, y: Interval): Boolean = {
    x.xhi <= 2 * y.xlo && y.xhi <= 2 * x.xlo
  }

  /**
    Computes the absolute roundoff error for the given expression.

    The ranges of all the intermediate expressions have to be given in rangeMap.
    Allows mixed-precision by providing (possibly different) precisions for
    all declared variables (input parameters as well as locally defined variables.)
    Constants are assumed to be all in one precision, given by the user.

   */
  def evalRoundoff[T <: RangeArithmetic[T]](
    expr: Expr,
    range: Map[(Expr, PathCond), Interval],
    precision: Map[Identifier, Precision],
    freeVarsError: Map[Identifier, T],
    zeroError: T,
    fromError: Rational => T,
    interval2T: Interval => T,
    constantsPrecision: Precision,
    trackRoundoffErrors: Boolean, // if false, propagate only initial errors
    approxRoundoff: Boolean = false,
    resultAbsErrors: Map[Identifier, Rational] = Map(),
    resultErrorsMetalibm: Map[Expr, Rational] = Map(),
    precomputedIntermedErrs: CachingMap[(Expr, PathCond), (T, Precision)] = CachingMap.empty[(Expr, PathCond), (T, Precision)]()
    ): (T, Map[(Expr, PathCond), T]) = {

    var _computeNewErrorTime = 0.0
    var _realLiteralTime = 0.0
    var _int32LiteralTime = 0.0
    var _finitePrecisionLiteralTime = 0.0
    var _plusTime = 0.0
    var _minusTime = 0.0
    var _timesTime = 0.0
    var _fmaTime = 0.0
    var _divisionTime = 0.0
    var _intPowTime = 0.0
    var _uminusTime = 0.0
    var _sqrtTime = 0.0
    var _sinTime = 0.0
    var _cosTime = 0.0
    var _tanTime = 0.0
    var _asinTime = 0.0
    var _acosTime = 0.0
    var _atanTime = 0.0
    var _expTime = 0.0
    var _logTime = 0.0
    var _approxTime = 0.0
    var _letTime = 0.0
    var _variableTime = 0.0
    var _ifExprTime = 0.0
    var _castTime = 0.0
    var _approxPolyTime = 0.0

    var _minuseval1Time = 0.0
    var _minuseval2Time = 0.0
    var _minuseval3Time = 0.0

    var _timeseval1Time = 0.0
    var _timeseval2Time = 0.0
    var _timeseval3Time = 0.0
    var _timeseval4Time = 0.0

    var _leteval1Time = 0.0
    var _leteval2Time = 0.0
    var _leteval3Time = 0.0


    val intermediateErrors = if (precomputedIntermedErrs.nonEmpty) precomputedIntermedErrs else new CachingMap[(Expr, PathCond), (T, Precision)]

    for ((id, err) <- freeVarsError){
      intermediateErrors.put((Variable(id), emptyPath), (err, precision(id)))
    }

    def computeNewError(range: Interval, propagatedError: T, prec: Precision): (T, Precision) = _computeNewError(range, propagatedError, prec, prec.absRoundoff)

    def computeNewErrorTranscendental(range: Interval, propagatedError: T, prec: Precision): (T, Precision) = _computeNewError(range, propagatedError, prec, prec.absTranscendentalRoundoff)

    def _computeNewError(range: Interval, propagatedError: T, prec: Precision,
                         roundoffComputationMethod: Interval => Rational): (T, Precision) =
    if (trackRoundoffErrors) {
      val startTime = System.currentTimeMillis()
      val actualRange: Interval = range + propagatedError.toInterval
      var rndoff = roundoffComputationMethod(actualRange)
      if (approxRoundoff) {
        rndoff = Rational.limitSize(rndoff)
      }
      val endTime = System.currentTimeMillis()
      _computeNewErrorTime = _computeNewErrorTime + (endTime - startTime)
      (propagatedError +/- rndoff, prec)
    } else {
      (propagatedError, prec)
    }

    var evalminus2timemeasuring = 0.0
    var evalminus2timemeasuringTotal = 0.0

    def eval(e: Expr, p: PathCond): (T, Precision) = {
      if(evalminus2timemeasuring != 0.0){
        println("evalminus2timemeasuring: " + evalminus2timemeasuring)
        println("currtime: " + System.currentTimeMillis())
        evalminus2timemeasuringTotal = evalminus2timemeasuringTotal + (System.currentTimeMillis() - evalminus2timemeasuring)
        evalminus2timemeasuring = 0.0
      }

      intermediateErrors.getOrAdd((e, p), {
      case x @ (RealLiteral(r), _) =>
        val error = if (constantsPrecision.canRepresent(r) || !trackRoundoffErrors) {
          zeroError
        } else {
          fromError(constantsPrecision.absRoundoff(r))
        }
        (error, constantsPrecision)
      case (Int32Literal(i), _) => (zeroError, constantsPrecision) // todo check something for i?

      // these can appear after mixed-precision tuning
      case x @ (FinitePrecisionLiteral(r, prec, _), _) =>
        val error = if (prec.canRepresent(r)) {
          zeroError
        } else {
          fromError(prec.absRoundoff(r))
        }
        (error, prec)


      case x @ (Plus(lhs, rhs), path) =>
        val (errorLhs, precLhs) = eval(lhs, path)
        val (errorRhs, precRhs) = eval(rhs, path)

        val propagatedError = errorLhs + errorRhs

        computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs)  /* Scala semantics */)

      case x @ (Minus(lhs, rhs), path) =>
        val startTime = System.currentTimeMillis()
        val startTimeEval1 = System.currentTimeMillis()
        val (errorLhs, precLhs) = eval(lhs, path)
        val endTimeEval1 = System.currentTimeMillis()
        _minuseval1Time = _minuseval1Time + (endTimeEval1 - startTimeEval1)
        val startTimeEval2 = System.currentTimeMillis()
        evalminus2timemeasuring = System.currentTimeMillis()
        val (errorRhs, precRhs) = eval(rhs, path)
        val endTimeEval2 = System.currentTimeMillis()
        _minuseval2Time = _minuseval2Time + (endTimeEval2 - startTimeEval2)

        val startTimeEval3 = System.currentTimeMillis()
        val propagatedError = errorLhs - errorRhs
        val precision = getUpperBound(precLhs, precRhs)
        val endTimeEval3 = System.currentTimeMillis()
        _minuseval3Time = _minuseval3Time + (endTimeEval3 - startTimeEval3)

        if (precision.isInstanceOf[FloatPrecision] && sterbenzTheoremApplies(range(lhs, path), range(rhs, path))) {
          _minusTime = _minusTime + (System.currentTimeMillis() - startTime)
          (propagatedError, precision)
        } else {
          _minusTime = _minusTime + (System.currentTimeMillis() - startTime)
          computeNewError(range(x), propagatedError, precision)
        }

      case x @ (Times(lhs, rhs), path) =>
        val startTime = System.currentTimeMillis()
        val startTime1 = System.currentTimeMillis()
        val (errorLhs, precLhs) = eval(lhs, path)
        val endTime1 = System.currentTimeMillis
        _timeseval1Time = _timeseval1Time + (endTime1 - startTime1)
        val startTime2 = System.currentTimeMillis()
        val (errorRhs, precRhs) = eval(rhs, path)
        val endTime2 = System.currentTimeMillis()
        _timeseval2Time = _timeseval2Time + (endTime2 - startTime2)

        val startTime3 = System.currentTimeMillis()
        val rangeLhs = range(lhs, path)
        val rangeRhs = range(rhs, path)
        val endTime3 = System.currentTimeMillis()
        _timeseval3Time = _timeseval3Time + (endTime3 - startTime3)
        val startTime4 = System.currentTimeMillis()
        val abstractRangeLhs = interval2T(rangeLhs)
        val abstractRangeRhs = interval2T(rangeRhs)
        val endTime4 = System.currentTimeMillis()
        _timeseval4Time = _timeseval4Time + (endTime4 - startTime4)

        val propagatedError =
          abstractRangeLhs * errorRhs +
          abstractRangeRhs * errorLhs +
          errorLhs * errorRhs

        val precision = getUpperBound(precLhs, precRhs)
        // No roundoff error if one of the operands is a non-negative power of 2
        if ((rangeLhs.isNonNegative && rangeLhs.isPowerOf2)
          || (rangeRhs.isNonNegative && rangeRhs.isPowerOf2)) {
          _timesTime = _timesTime + (System.currentTimeMillis() - startTime)
          (propagatedError, precision)
        } else {
          _timesTime = _timesTime + (System.currentTimeMillis() - startTime)
          computeNewError(range(x), propagatedError, precision)
        }

      case x @ (FMA(fac1, fac2, sum), path) =>
        val startTime = System.currentTimeMillis()
        val (errorFac1, precFac1) = eval(fac1, path)
        val (errorFac2, precFac2) = eval(fac2, path)
        val (errorSum, precSum) = eval(sum, path)

        val rangeFac1 = interval2T(range(fac1, path))
        val rangeFac2 = interval2T(range(fac2, path))

        val propagatedError =
          rangeFac1 * errorFac2 +
          rangeFac2 * errorFac1 +
          errorFac1 * errorFac2 +
          errorSum

        _fmaTime = _fmaTime + (System.currentTimeMillis() - startTime)
        computeNewError(range(x), propagatedError, getUpperBound(precFac1, precFac2, precSum))

      case x @ (Division(lhs, rhs), path) =>
        val startTime = System.currentTimeMillis()
        val (errorLhs, precLhs) = eval(lhs, path)
        val (errorRhs, precRhs) = eval(rhs, path)

        val rangeLhs = range(lhs, path)
        val rangeRhs = range(rhs, path)

        // inverse, i.e. we are computing x * (1/y)
        val rightInterval = rangeRhs + errorRhs.toInterval // the actual interval, incl errors

        // the actual error interval can now contain 0, check this
        if (rightInterval.includes(Rational.zero)) {
          throw DivisionByZeroException("trying to divide by error interval containing 0")
        }
        val a = Interval.minAbs(rightInterval)
        val errorMultiplier: Rational = -one / (a*a)
        val invErr = errorRhs * errorMultiplier

        // error propagation
        val inverse: Interval = rangeRhs.inverse

        val propagatedError =
          interval2T(rangeLhs) * invErr +
          interval2T(inverse) * errorLhs +
          errorLhs * invErr

        _divisionTime = _divisionTime + (System.currentTimeMillis() - startTime)
        computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs))

      case x @ (IntPow(base, n), path) =>
        val startTime = System.currentTimeMillis()
        val (errorT, prec) = eval(base, path)
        val rangeT = interval2T(range(base, path))

        var r = rangeT
        var e = errorT
        for (_ <- 0 until n) {
          e = r * errorT + rangeT * e + e * errorT
          r *= rangeT
        }

        // The error of pow in java.Math is 1 ulp, thus we rely that the method
        // computeNewErrorTranscendental gives us 1 ulp error
        _intPowTime = _intPowTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(r.toInterval, e, prec)

      case x @ (UMinus(t), path) =>
        val (error, prec) = eval(t, path)
        (- error, prec)

      case x @ (Sqrt(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO: needs to fail for fixed-point precision
        val (errorT, prec) = eval(t, path)
        val rangeT = range(t, path)

        if ((errorT.toInterval.xlo + rangeT.xlo) <= Rational.zero && Sign.ofExpression(t, path, range) != Sign.Positive) {
          throw NegativeSqrtException("trying to take the square root of a negative number or zero")
        }

        val mepsilon = prec match {
          case pr@FloatPrecision(_) => pr.machineEpsilon
          case pr@FixedPrecision(_) => pr.sqrtEpsilonZeroToOne
        }
        val a = try {
          Interval.minAbs(rangeT)
        } catch {
          case _: AssertionError =>  mepsilon // if the lower bound is near or equals zero, can't compute minAbs()
        }
        val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)

        val propagatedError = errorT * errorMultiplier
        // PRECiSa's way of computing the error would be
        //val errorMultiplier = Rational(1L, 2L)* Rational.fromDouble(Math.ulp(sqrtUp(a).toDouble))
        //val propagatedError = errorT + fromError(errorMultiplier)

        // TODO: check that this operation exists for this precision
        //println("range map is " + range)
        _sqrtTime = _sqrtTime + (System.currentTimeMillis() - startTime)
        computeNewError(range(x), propagatedError, prec)

      case x @ (Sin(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // Bound the slope of sin(x) over the range by computing its
        // derivative (i.e. cos(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv =  range(t, path).cosine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _sinTime = _sinTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Cos(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // Bound the slope of cos(x) over the range by computing its
        // derivative (i.e. -sin(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv = -range(t, path).sine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _cosTime = _cosTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Tan(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // compute the derivative as 1/cos^2(x)
        val intCosine = range(t, path).cosine
        val deriv = (intCosine * intCosine).inverse

        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _tanTime = _tanTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Asin(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // compute the max slope (derivative), will be one of the end points
        // instead of trying to figure out which one, compute both
        val Interval(a, b) = range(t, path)
        val errorMultiplier = max(abs(1 / sqrtDown(1 - a * a)), abs(1 / sqrtDown(1 - b * b)))
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _asinTime = _asinTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Acos(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // compute the max slope (derivative), will be one of the end points
        // instead of trying to figure out which one, compute both
        val Interval(a, b) = range(t, path)
        val errorMultiplier = max(abs(1 / sqrtDown(1 - a * a)), abs(1 / sqrtDown(1 - b * b)))
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _acosTime = _acosTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Atan(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // compute the max slope (derivative)
        val Interval(a, b) = range(t, path)
        val errorMultiplier = if (a >= Rational.zero) {
          // The interval is fully above zero, so the maximum derivative is at 1 / (a**2 + 1)
          1 / (a * a + Rational.one)
        } else if (b <= Rational.zero) {
          // The interval is fully below zero, so the maximum derivative is at 1 / (b**2 + 1), since b is the closest to
          // zero.
          1 / (b * b + Rational.one)
        } else {
          // The interval contains zero, so the maximum value is 1 / (0**2 + 1) = 1
          Rational.one
        }
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _atanTime = _atanTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Exp(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // maximal slope is always at the right ending point
        val b = range(t, path).xhi

        // compute the maximal slope over the interval
        // (exp(x) is the derivative of exp(x))
        val errorMultiplier = expUp(b)

        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _expTime = _expTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Log(t), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // maximal slope is always at the left ending point
        val a = range(t, path).xlo

        // compute the maximal slope over the interval (1/x is the derivative of log(x))
        val errorMultiplier = Rational.one / a

        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        _logTime = _logTime + (System.currentTimeMillis() - startTime)
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ (Approx(original, t, metalibmError, errorMultiplier, _, _), path) =>
        val startTime = System.currentTimeMillis()
        // TODO not supported for fixed-points
        // TODO: figure out which approach to use
        //val (errorT, prec) = eval(t, path, false)
        val (errorT, prec) = eval(t, path)
        val maxRangeX = Interval.maxAbs(range(x))
        val absoluteMetalibmError = metalibmError * maxRangeX
        val propagatedError = errorT * errorMultiplier
        val newError = propagatedError +/- absoluteMetalibmError

        _approxTime = _approxTime + (System.currentTimeMillis() - startTime)
        (newError, prec)

      case x @ (Let(id, value, body), path) =>
        val startTime = System.currentTimeMillis()
        val startTime1 = System.currentTimeMillis()
        val (valueError, valuePrec) = eval(value, path)
        val endTime1 = System.currentTimeMillis()
        _leteval1Time = _leteval1Time + (endTime1 - startTime1)

        val startTime2 = System.currentTimeMillis()
        val idPrec = precision(id)
        val error = if (idPrec < valuePrec) { // we need to cast down
          val valueRange = range(value, path)
          computeNewError(valueRange, valueError, idPrec)._1
        } else {
          valueError
        }
        val endTime2 = System.currentTimeMillis()
        _leteval2Time = _leteval2Time + (endTime2 - startTime2)

        val startTime3 = System.currentTimeMillis()
        intermediateErrors.put((Variable(id), path), (error, valuePrec)) // no problem as identifiers are unique
        val endTime3 = System.currentTimeMillis()
        _leteval3Time = _leteval3Time + (endTime3 - startTime3)
        _letTime = _letTime + (System.currentTimeMillis() - startTime)
        eval(body, path)

      case (Variable(id), path) =>
        if (path.nonEmpty)
          intermediateErrors(Variable(id), emptyPath)
        else
          throw new Exception("Unknown variable: " + id)

      case x @ (IfExpr(cond, thenn, elze), path) =>
        val startTime = System.currentTimeMillis()

        // a branch is feasible if the range for it exists
        val thenRes: Option[(T, Precision)] = range.get((thenn, path :+ cond)) match {
          case Some(_) => Some(eval(thenn, path :+ cond))
          case _ => None
        }

        val elseRes: Option[(T, Precision)] = range.get((elze, path :+ lang.TreeOps.negate(cond))) match {
          case Some(_) => Some(eval(elze, path :+ lang.TreeOps.negate(cond)))
          case _ => None
        }

        (thenRes, elseRes) match {
          case (Some((errorThen, precThen)), Some((errorElse, precElse))) => {
            val propagatedError = interval2T(errorThen.toInterval.union(errorElse.toInterval))// take max of the two errors
            _ifExprTime = _ifExprTime + (System.currentTimeMillis() - startTime)
            computeNewError(range(x), propagatedError, getUpperBound(precThen, precElse))
          }

          case (Some((errorThen, precThen)), None) =>{
            _ifExprTime = _ifExprTime + (System.currentTimeMillis() - startTime)
            computeNewError(range(x), errorThen, precThen)
          }

          case (None, Some((errorElse, precElse))) =>{
            _ifExprTime = _ifExprTime + (System.currentTimeMillis() - startTime)
            computeNewError(range(x), errorElse, precElse)
          }

          case (None, None) => // should not happen; should have already failed for ranges
            throw new Exception("Not supported")
        }

      case x @ (Cast(t, FinitePrecisionType(prec)), path) =>
        val startTime = System.currentTimeMillis()
        val (errorT, precT) = eval(t, path)

        if (prec > precT) {
          // upcast does not lead to roundoff error
          _castTime = _castTime + (System.currentTimeMillis() - startTime)
          (errorT, prec)
        } else {
          // add new roundoff error corresponding to the cast precision
          _castTime = _castTime + (System.currentTimeMillis() - startTime)
          computeNewError(range(x), errorT, prec)
        }

      case x @ (ApproxPoly(orig, _, fncId, totalError), path) =>
        val startTime = System.currentTimeMillis()
        val approxPrec = (fncId.getType: @unchecked) match {
          case FinitePrecisionType(a) => Some(a)
          case _ =>
            // approx fnc must already have finite precision assignedinf
            throw new Exception(s"Approximation must have precision assigned ${x._1}")
        }

        // TODO: store resultAbsError directly in ApproxNode?
        if (resultAbsErrors.contains(fncId)){
          _approxPolyTime = _approxPolyTime + (System.currentTimeMillis() - startTime)
          (fromError(resultErrorsMetalibm(x._1) + resultAbsErrors(fncId)), approxPrec.get)
        }
        else{
          _approxPolyTime = _approxPolyTime + (System.currentTimeMillis() - startTime)
          (fromError(totalError), approxPrec.get) // for another Metalibm phase (not ApproxPhase)
        }


      case x => throw new Exception(s"Not supported $x")

    })
  }

    println("starting eval")
    val startEval = System.currentTimeMillis()
    val (resError, _) = eval(expr, emptyPath)
    val endEval = System.currentTimeMillis()
    println(s"eval time: ${endEval - startEval} ms")
    println(s"_computeNewError time: " + _computeNewErrorTime)
    println(s"_realLiteral time: " + _realLiteralTime)
    println(s"_int32Literal time: " + _int32LiteralTime)
    println(s"_finitePrecisionLiteral time: " + _finitePrecisionLiteralTime)
    println(s"_plus time: " + _plusTime)
    println(s"_minus time: " + _minusTime)
    println(s"_times time: " + _timesTime)
    println(s"_fma time: " + _fmaTime)
    println(s"_division time: " + _divisionTime)
    println(s"_intPow time: " + _intPowTime)
    println(s"_uminus time: " + _uminusTime)
    println(s"_sqrt time: " + _sqrtTime)
    println(s"_sin time: " + _sinTime)
    println(s"_cos time: " + _cosTime)
    println(s"_tan time: " + _tanTime)
    println(s"_asin time: " + _asinTime)
    println(s"_acos time: " + _acosTime)
    println(s"_atan time: " + _atanTime)
    println(s"_exp time: " + _expTime)
    println(s"_log time: " + _logTime)
    println(s"_approx time: " + _approxTime)
    println(s"_let time: " + _letTime)
    println(s"_variable time: " + _variableTime)
    println(s"_ifExpr time: " + _ifExprTime)
    println(s"_cast time: " + _castTime)
    println(s"_approxPoly time: " + _approxPolyTime)
    val allOthers = _realLiteralTime + _int32LiteralTime + _finitePrecisionLiteralTime + _plusTime + _minusTime + _timesTime + _fmaTime + _divisionTime + _intPowTime + _uminusTime + _sqrtTime + _sinTime + _cosTime + _tanTime + _asinTime + _acosTime + _atanTime + _expTime + _logTime + _approxTime + _letTime + _variableTime + _ifExprTime + _castTime + _approxPolyTime
    println(s"all others: $allOthers")
    
    println("microbenchmarks")
    println(s"_minuseval1 time: " + _minuseval1Time)
    println(s"_minuseval2 time: " + _minuseval2Time)
    println(s"_minuseval3 time: " + _minuseval3Time)
    println(s"_timeseval1 time: " + _timeseval1Time)
    println(s"_timeseval2 time: " + _timeseval2Time)
    println(s"_timeseval3 time: " + _timeseval3Time)
    println(s"_timeseval4 time: " + _timeseval4Time)
    println(s"_leteval1 time: " + _leteval1Time)
    println(s"_leteval2 time: " + _leteval2Time)
    println(s"_leteval3 time: " + _leteval3Time)

    println(s"evalminus2timemeasuringTotal: $evalminus2timemeasuringTotal")

    (resError, intermediateErrors.mapValues(_._1).toMap)
  }


}
