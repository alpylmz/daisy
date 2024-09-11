// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Types.{ FinitePrecisionType}
import lang.Trees._
import lang.Identifiers._
import FinitePrecision._
import Rational._
import daisy.utils.CachingMap
import tools.{AffineForm, Interval, Rational}

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Structure;

trait roundoff2 extends Library {
  def _Z14wholemultlowerffffffff(xlo: Float, xhi: Float, ylo: Float, yhi: Float, xlo2: Float, xhi2: Float, ylo2: Float, yhi2: Float): Float
  def _Z14wholemultupperv(): Float
}

trait RoundoffEvaluators extends RangeEvaluators {

  val multFunc = Native.loadLibrary("foo", classOf[roundoff2])

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
    interval2T: daisy.tools.Interval => T,
    constantsPrecision: Precision,
    trackRoundoffErrors: Boolean, // if false, propagate only initial errors
    approxRoundoff: Boolean = false,
    resultAbsErrors: Map[Identifier, Rational] = Map(),
    resultErrorsMetalibm: Map[Expr, Rational] = Map(),
    precomputedIntermedErrs: CachingMap[(Expr, PathCond), (T, Precision)] = CachingMap.empty[(Expr, PathCond), (T, Precision)]()
    ): (T, Map[(Expr, PathCond), T]) = {


    val intermediateErrors = if (precomputedIntermedErrs.nonEmpty) precomputedIntermedErrs else new CachingMap[(Expr, PathCond), (T, Precision)]

    for ((id, err) <- freeVarsError){
      intermediateErrors.put((Variable(id), emptyPath), (err, precision(id)))
    }

    def computeNewError(range: Interval, propagatedError: T, prec: Precision): (T, Precision) = _computeNewError(range, propagatedError, prec, prec.absRoundoff)

    def computeNewErrorTranscendental(range: Interval, propagatedError: T, prec: Precision): (T, Precision) = _computeNewError(range, propagatedError, prec, prec.absTranscendentalRoundoff)

    def _computeNewError(range: Interval, propagatedError: T, prec: Precision,
                         roundoffComputationMethod: Interval => Rational): (T, Precision) =
    if (trackRoundoffErrors) {
      val actualRange: Interval = range + propagatedError.toInterval
      var rndoff = roundoffComputationMethod(actualRange)
      if (approxRoundoff) {
        rndoff = Rational.limitSize(rndoff)
      }
      (propagatedError +/- rndoff, prec)
    } else {
      (propagatedError, prec)
    }

    var plusTime = 0.0
    var minusTime = 0.0
    var timesTime = 0.0
    var divTime = 0.0
    var sinTime = 0.0
    var cosTime = 0.0
    var uminusTime = 0.0
    var letTime = 0.0
//
    var rangeLhsTime = 0.0
    var rangeRhsTime = 0.0
    var abstractRangeLhsTime = 0.0
    var abstractRangeRhsTime = 0.0
    var precisionTime = 0.0
    var computenewerrortime = 0.0
    var checkiftime = 0.0
    var multiplytime = 0.0
    var propagatedError1time = 0.0
    var propagatedError2time = 0.0
    var propagatedError3time = 0.0
    var propagatedError4time = 0.0
    var propagatedError5time = 0.0


    def eval(e: Expr, p: PathCond): (T, Precision) = intermediateErrors.getOrAdd((e, p), {

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

        val startTime = System.nanoTime()
        val propagatedError = errorLhs + errorRhs
        val res = computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs)  /* Scala semantics */)
        val endTime = System.nanoTime()
        plusTime += (endTime - startTime) / 1000000.0
        res


      case x @ (Minus(lhs, rhs), path) =>
        val (errorLhs, precLhs) = eval(lhs, path)
        val (errorRhs, precRhs) = eval(rhs, path)

        val startTime = System.nanoTime()
        val propagatedError = errorLhs - errorRhs
        val precision = getUpperBound(precLhs, precRhs)

        if (precision.isInstanceOf[FloatPrecision] && sterbenzTheoremApplies(range(lhs, path), range(rhs, path))) {
          val endTime = System.nanoTime()
          minusTime += (endTime - startTime) / 1000000.0
          (propagatedError, precision)
        } else {
          val res = computeNewError(range(x), propagatedError, precision)
          val endTime = System.nanoTime()
          minusTime += (endTime - startTime) / 1000000.0
          res
        }

      case x @ (Times(lhs, rhs), path) =>
        val (errorLhs, precLhs) = eval(lhs, path)
        val (errorRhs, precRhs) = eval(rhs, path)

        val startTime = System.nanoTime()
        val rangeLhs = range(lhs, path)
        val endtime1 = System.nanoTime()
        rangeLhsTime += (endtime1 - startTime) / 1000000.0
        val startTime2 = System.nanoTime()
        val rangeRhs = range(rhs, path)
        val endtime2 = System.nanoTime()
        rangeRhsTime += (endtime2 - startTime2) / 1000000.0
        val startTime3 = System.nanoTime()
        val abstractRangeLhs = interval2T(rangeLhs)
        val endtime3 = System.nanoTime()
        abstractRangeLhsTime += (endtime3 - startTime3) / 1000000.0
        val startTime4 = System.nanoTime()
        val abstractRangeRhs = interval2T(rangeRhs)
        val endtime4 = System.nanoTime()
        abstractRangeRhsTime += (endtime4 - startTime4) / 1000000.0

        val starttime44 = System.nanoTime()
        val propagatedError1 = abstractRangeLhs * errorRhs
        val propagatedError2 = abstractRangeRhs * errorLhs
        val propagatedError3 = errorLhs * errorRhs
        val propagatedError = propagatedError1 + propagatedError2 + propagatedError3
        val endTime44 = System.nanoTime()
        multiplytime += (endTime44 - starttime44) / 1000000.0

        val startTime5 = System.nanoTime()
        val precision = getUpperBound(precLhs, precRhs)
        val endtime5 = System.nanoTime()
        precisionTime += (endtime5 - startTime5) / 1000000.0
        // No roundoff error if one of the operands is a non-negative power of 2
        val starttime55 = System.nanoTime()
        if ((rangeLhs.isNonNegative && rangeLhs.isPowerOf2)
          || (rangeRhs.isNonNegative && rangeRhs.isPowerOf2)) {
          val endTime = System.nanoTime()
          checkiftime += (endTime - starttime55) / 1000000.0
          timesTime += (endTime - startTime) / 1000000.0
          (propagatedError, precision)
        } else {
          val endTime66 = System.nanoTime()
          checkiftime += (endTime66 - starttime55) / 1000000.0
          val startTime6 = System.nanoTime()
          val res = computeNewError(range(x), propagatedError, precision)
          val endTime = System.nanoTime()
          computenewerrortime += (endTime - startTime6) / 1000000.0
          timesTime += (endTime - startTime) / 1000000.0
          res
        }
      
      case x @ (Cast(t, FinitePrecisionType(prec)), path) =>
        val (errorT, precT) = eval(t, path)

        if (prec > precT) {
          // upcast does not lead to roundoff error
          (errorT, prec)
        } else {
          // add new roundoff error corresponding to the cast precision
          computeNewError(range(x), errorT, prec)
        }

      case x @ (Division(lhs, rhs), path) =>
        val (errorLhs, precLhs) = eval(lhs, path)
        val (errorRhs, precRhs) = eval(rhs, path)

        val startTime = System.nanoTime()
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

        val res = computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs))
        val endTime = System.nanoTime()
        divTime += (endTime - startTime) / 1000000.0
        res

      case x @ (UMinus(t), path) =>
        val (error, prec) = eval(t, path)
        (- error, prec)

      case x @ (Sin(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        val startTime = System.nanoTime()

        // Bound the slope of sin(x) over the range by computing its
        // derivative (i.e. cos(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv =  range(t, path).cosine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        val res = computeNewErrorTranscendental(range(x), propagatedError, prec)
        val endTime = System.nanoTime()
        sinTime += (endTime - startTime) / 1000000.0
        res
        

      case x @ (Cos(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        val startTime = System.nanoTime()
        // Bound the slope of cos(x) over the range by computing its
        // derivative (i.e. -sin(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv = -range(t, path).sine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        val res = computeNewErrorTranscendental(range(x), propagatedError, prec)
        val endTime = System.nanoTime()
        cosTime += (endTime - startTime) / 1000000.0
        res

      case x @ (Let(id, value, body), path) =>
        val (valueError, valuePrec) = eval(value, path)

        val startTime = System.nanoTime()
        val idPrec = precision(id)
        val error = if (idPrec < valuePrec) { // we need to cast down
          val valueRange = range(value, path)
          computeNewError(valueRange, valueError, idPrec)._1
        } else {
          valueError
        }

        intermediateErrors.put((Variable(id), path), (error, valuePrec)) // no problem as identifiers are unique
        val endTime = System.nanoTime()
        letTime += (endTime - startTime) / 1000000.0
        eval(body, path)

      case (Variable(id), path) =>
        if (path.nonEmpty)
          intermediateErrors(Variable(id), emptyPath)
        else
          throw new Exception("Unknown variable: " + id)

      
      case x => throw new Exception(s"Not supported $x")

    })

    val (resError, _) = eval(expr, emptyPath)

    println(s"Plus time: $plusTime")
    println(s"Minus time: $minusTime")
    println(s"Times time: $timesTime")
    println(s"Div time: $divTime")
    println(s"Sin time: $sinTime")
    println(s"Cos time: $cosTime")
    println(s"Uminus time: $uminusTime")
    println(s"Let time: $letTime")

    println(s"Range Lhs time: $rangeLhsTime")
    println(s"Range Rhs time: $rangeRhsTime")
    println(s"Abstract Range Lhs time: $abstractRangeLhsTime")
    println(s"Abstract Range Rhs time: $abstractRangeRhsTime")
    println(s"Precision time: $precisionTime")
    println(s"Compute new error time: $computenewerrortime")
    println(s"Check if time: $checkiftime")
    println(s"Multiply time: $multiplytime")

    println(s"Propagated Error 1 time: $propagatedError1time")
    println(s"Propagated Error 2 time: $propagatedError2time")
    println(s"Propagated Error 3 time: $propagatedError3time")
    println(s"Propagated Error 4 time: $propagatedError4time")
    println(s"Propagated Error 5 time: $propagatedError5time")
    
    (resError, intermediateErrors.mapValues(_._1).toMap)
  }


  /**
    Computes the absolute roundoff error for the given expression.

    The ranges of all the intermediate expressions have to be given in rangeMap.
    Allows mixed-precision by providing (possibly different) precisions for
    all declared variables (input parameters as well as locally defined variables.)
    Constants are assumed to be all in one precision, given by the user.

   */
  def evalRoundoffInterval[Interval <: RangeArithmetic[Interval]](
    expr: Expr,
    range: Map[(Expr, PathCond), daisy.tools.Interval],
    precision: Map[Identifier, Precision],
    freeVarsError: Map[Identifier, daisy.tools.Interval],
    zeroError: daisy.tools.Interval,
    fromError: Rational => daisy.tools.Interval,
    interval2T: daisy.tools.Interval => daisy.tools.Interval,
    constantsPrecision: Precision,
    trackRoundoffErrors: Boolean, // if false, propagate only initial errors
    approxRoundoff: Boolean = false,
    resultAbsErrors: Map[Identifier, Rational] = Map(),
    resultErrorsMetalibm: Map[Expr, Rational] = Map(),
    precomputedIntermedErrs: CachingMap[(Expr, PathCond), (daisy.tools.Interval, Precision)] = CachingMap.empty[(Expr, PathCond), (daisy.tools.Interval, Precision)]()
    ): (daisy.tools.Interval, Map[(Expr, PathCond), daisy.tools.Interval]) = {


    val intermediateErrors = if (precomputedIntermedErrs.nonEmpty) precomputedIntermedErrs else new CachingMap[(Expr, PathCond), (daisy.tools.Interval, Precision)]

    for ((id, err) <- freeVarsError){
      intermediateErrors.put((Variable(id), emptyPath), (err, precision(id)))
    }

    def computeNewError(range: daisy.tools.Interval, propagatedError: daisy.tools.Interval, prec: Precision): (daisy.tools.Interval, Precision) = _computeNewError(range, propagatedError, prec, prec.absRoundoff)

    def computeNewErrorTranscendental(range: daisy.tools.Interval, propagatedError: daisy.tools.Interval, prec: Precision): (daisy.tools.Interval, Precision) = _computeNewError(range, propagatedError, prec, prec.absTranscendentalRoundoff)

    def _computeNewError(range: daisy.tools.Interval, propagatedError: daisy.tools.Interval, prec: Precision,
                         roundoffComputationMethod: daisy.tools.Interval => Rational): (daisy.tools.Interval, Precision) =
    if (trackRoundoffErrors) {
      val actualRange: daisy.tools.Interval = range + propagatedError.toInterval
      var rndoff = roundoffComputationMethod(actualRange)
      if (approxRoundoff) {
        rndoff = Rational.limitSize(rndoff)
      }
      (propagatedError +/- rndoff, prec)
    } else {
      (propagatedError, prec)
    }

    var plusTime = 0.0
    var minusTime = 0.0
    var timesTime = 0.0
    var divTime = 0.0
    var sinTime = 0.0
    var cosTime = 0.0
    var uminusTime = 0.0
    var letTime = 0.0
    var rangeLhsTime = 0.0
    var rangeRhsTime = 0.0
    var abstractRangeLhsTime = 0.0
    var abstractRangeRhsTime = 0.0
    var precisionTime = 0.0
    var computenewerrortime = 0.0
    var checkiftime = 0.0
    var multiplytime = 0.0
    var propagatedError1time = 0.0
    var propagatedError2time = 0.0
    var propagatedError3time = 0.0
    var propagatedError4time = 0.0
    var propagatedError5time = 0.0


    def eval(e: Expr, p: PathCond): (daisy.tools.Interval, Precision) = intermediateErrors.getOrAdd((e, p), {

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
        val res = computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs)  /* Scala semantics */)
        res


      case x @ (Minus(lhs, rhs), path) =>
        val (errorLhs, precLhs) = eval(lhs, path)
        val (errorRhs, precRhs) = eval(rhs, path)

        val propagatedError = errorLhs - errorRhs
        val precision = getUpperBound(precLhs, precRhs)

        if (precision.isInstanceOf[FloatPrecision] && sterbenzTheoremApplies(range(lhs, path), range(rhs, path))) {
          (propagatedError, precision)
        } else {
          val res = computeNewError(range(x), propagatedError, precision)
          res
        }

      case x @ (Times(lhs, rhs), path) =>
        val (errorLhs, precLhs) = eval(lhs, path)
        val (errorRhs, precRhs) = eval(rhs, path)

        val rangeLhs = range(lhs, path)
        val rangeRhs = range(rhs, path)
        val abstractRangeLhs = interval2T(rangeLhs)

        val abstractRangeRhs = interval2T(rangeRhs)

        // NEW METHOD
        val abstractRangeLhsxlo = abstractRangeLhs.xlo
        val abstractRangeLhsxhi = abstractRangeLhs.xhi
        val errorRhsxlo = errorRhs.xlo
        val errorRhsxhi = errorRhs.xhi
        val abstractRangeRhsxlo = abstractRangeRhs.xlo
        val abstractRangeRhsxhi = abstractRangeRhs.xhi
        val errorLhsxlo = errorLhs.xlo
        val errorLhsxhi = errorLhs.xhi
        val propagatedErrorxlo: Float = multFunc._Z14wholemultlowerffffffff(
          abstractRangeLhsxlo.toFloat,
          abstractRangeLhsxhi.toFloat,
          abstractRangeRhsxlo.toFloat,
          abstractRangeRhsxhi.toFloat,
          errorRhsxlo.toFloat,
          errorRhsxhi.toFloat,
          errorLhsxlo.toFloat,
          errorLhsxhi.toFloat
        )
        val propagatedErrorxhi: Float = multFunc._Z14wholemultupperv()
        val propagatedError = Interval(propagatedErrorxlo, propagatedErrorxhi)

        // OLD METHOD:
        //val propagatedError =
        //  abstractRangeLhs * errorRhs +
        //  abstractRangeRhs * errorLhs +
        //  errorLhs * errorRhs

        val precision = getUpperBound(precLhs, precRhs)
        // No roundoff error if one of the operands is a non-negative power of 2
        if ((rangeLhs.isNonNegative && rangeLhs.isPowerOf2)
          || (rangeRhs.isNonNegative && rangeRhs.isPowerOf2)) {
          (propagatedError, precision)
        } else {

          val res = computeNewError(range(x), propagatedError, precision)


          res
        }

      case x @ (Division(lhs, rhs), path) =>
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
        val a = daisy.tools.Interval.minAbs(rightInterval)
        val errorMultiplier: Rational = -one / (a*a)
        val invErr = errorRhs * errorMultiplier

        // error propagation
        val inverse: daisy.tools.Interval = rangeRhs.inverse

        val propagatedError =
          interval2T(rangeLhs) * invErr +
          interval2T(inverse) * errorLhs +
          errorLhs * invErr

        val res = computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs))

        res

      case x @ (UMinus(t), path) =>
        val (error, prec) = eval(t, path)
        (- error, prec)

      case x @ (Sin(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)



        // Bound the slope of sin(x) over the range by computing its
        // derivative (i.e. cos(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv =  range(t, path).cosine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        val res = computeNewErrorTranscendental(range(x), propagatedError, prec)

        res
        

      case x @ (Cos(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t, path)

        // Bound the slope of cos(x) over the range by computing its
        // derivative (i.e. -sin(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv = -range(t, path).sine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        val res = computeNewErrorTranscendental(range(x), propagatedError, prec)
        res
      
      case x @ (Cast(t, FinitePrecisionType(prec)), path) =>
        val (errorT, precT) = eval(t, path)

        if (prec > precT) {
          // upcast does not lead to roundoff error
          (errorT, prec)
        } else {
          // add new roundoff error corresponding to the cast precision
          computeNewError(range(x), errorT, prec)
        }

      case x @ (Let(id, value, body), path) =>
        val (valueError, valuePrec) = eval(value, path)

        val idPrec = precision(id)
        val error = if (idPrec < valuePrec) { // we need to cast down
          val valueRange = range(value, path)
          computeNewError(valueRange, valueError, idPrec)._1
        } else {
          valueError
        }

        intermediateErrors.put((Variable(id), path), (error, valuePrec)) // no problem as identifiers are unique
        eval(body, path)

      case (Variable(id), path) =>
        if (path.nonEmpty)
          intermediateErrors(Variable(id), emptyPath)
        else
          throw new Exception("Unknown variable: " + id)

      
      case x => throw new Exception(s"Not supported $x")

    })

    val (resError, _) = eval(expr, emptyPath)

    (resError, intermediateErrors.mapValues(_._1).toMap)
  }


}