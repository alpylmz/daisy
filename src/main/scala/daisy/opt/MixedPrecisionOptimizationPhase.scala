package daisy
package opt

import java.io.File
import scala.collection.immutable.Seq
import scala.collection.mutable.{Set => MSet}
import util.Random
import lang.Trees._
import lang.Trees
import lang.Types._
import lang.Identifiers._
import tools.FinitePrecision._
import lang.TreeOps._
import tools.{AffineForm, Interval, Rational}
import lang.Extractors.{ArithOperator, ElemFnc}
import tools._
import daisy.ProgramLanguage._
import java.net.IDN

/**
 * This phase optimizes and determines a suitable mixed-precision type assignment.
 *
 * It only considers roundoff errors during the optimization, i.e. no additional
 * roundoff errors (for now). If we want to take into account initial errors too,
 * an additional analysis step is required before this phase (to determine how large
 * the roundoff errors can be), but we will leave this for later.
 *
 * The phase transforms the initial real-valued program into a finite-precision one
 * (by changing the types of the arithmetic nodes).
 * The final error computation (and certification at some point) will be done
 * by the AbsErrorPhase, which should follow this phase.
 *
 * Prerequisites:
 * - SpecsProcessingPhase
 * - RangePhase (assumes that real-valued ranges are attached to the tree)
 */
object MixedPrecisionOptimizationPhase extends DaisyPhase with CostFunctions
  with search.GeneticSearch[Map[Identifier, Precision]] with tools.RoundoffEvaluators {

  val minimumIntervalSize = 0.1
  val epsilonRes = 0.01

  /* 
   * Copied from daisy/src/main/scala/daisy/Main.scala, and stripped down to the essentials.
   */
  val globalOptions: Set[CmdLineOption[Any]] = Set(
    FlagOption(
      "help",
      "Show this message"),
    FlagOption(
      "silent",
      "Don't print anything, except for results."),
    MultiChoiceOption(
      "debug",
      DebugSections.all.map(s => s.name -> s).toMap,
      "For which sections to print debug info"),
    FlagOption(
      "dynamic",
      "Run dynamic analysis"),
    FlagOption(
      "codegen",
      "Generate code (as opposed to just doing analysis)"),
    FlagOption(
      "three-address",
      "Transform code to three-address code prior to analysis."),
    FlagOption(
      "rewrite",
      "Rewrite expression to improve accuracy"),
    MultiStringOption(
      "functions",
      List("f1", "f2"),
      "Which functions to consider"),
    FlagOption(
      "print-tough-smt-calls",
      "If enabled, will print those SMT queries to file which take longer"),
    StringChoiceOption(
      "solver",
      Set("dReal", "z3"),
      "z3",
      "smt solver to use for smt range analysis"),
    ChoiceOption(
      "analysis",
      Map("dataflow" -> analysis.DataflowPhase, "opt" -> analysis.TaylorErrorPhase,
          "relative" -> analysis.RelativeErrorPhase),
      "dataflow",
      "Which analysis method to use"),
    FlagOption(
      "subdiv",
      "Apply subdivision to absolute error computation."
    ),
    ChoiceOption(
      "precision",
      Map("Float16" -> Float16, "Float32" -> Float32, "Float64" -> Float64,
        "Quad" -> DoubleDouble, "QuadDouble" -> QuadDouble) ++
        (1 to 64).map(x => ("Fixed" + x -> FixedPrecision(x))),
      "Float64",
      "(Default, uniform) precision to use"),
    StringChoiceOption(
      "rangeMethod",
      Set("affine", "interval", "smt", "intervalMPFR", "affineMPFR"),
      "interval",
      "Method for range analysis"),
    FlagOption(
      "noRoundoff",
      "Do not track roundoff errors"),
    FlagOption(
      "noInitialErrors",
      "Do not track initial errors specified by user"),
    FlagOption(
      "pow-roll",
      "Roll products, e.g. x*x*x -> pow(x, 3)"
    ),
    FlagOption(
      "pow-unroll",
      "Unroll products, e.g. pow(x, 3) => x*x*x"
    ),
    StringOption(
      "mixed-precision",
      """File with type assignment for all variables.
        The format is the following:
        function_name = {
          variable_name_1: prec_1
          variable_name_2: prec_2
          ... }
        function_name_2 = {
          variable_name_i: prec_i }

        The file can also only give a partial precision map."""),
    FlagOption(
      "denormals",
      "Include parameter for denormals in the FP abstraction (for optimization-based approach only)."),


    FlagOption("mixed-cost-eval", "Mixed-precision cost function evaluation experiment"),
    FlagOption("mixed-exp-gen", "Mixed-precision experiment generation"),
    FlagOption("mixed-tuning", "Perform mixed-precision tuning"),
    FlagOption(
      "approx",
      "Replaces expensive transcendental function calls with its approximations"
    ),
    StringOption(
      "spec",
      "Specification file with intervals for input variables and target error."),
    StringChoiceOption(
      "cost",
      Set("area", "ml", "combined"),
      "area",
      "Cost function for mixed-tuning and approximation phases."),

    FlagOption("metalibm", "approximate an elementary function from Metalibm"),
    FlagOption("benchmarking", "generates the benchmark file"),
    FlagOption("print-ast", "prints the AST of a parsed program"),
    FlagOption("unroll", "unrolls all loops over DS [WARN] only used with --print-ast at the moment"),
    FlagOption("ds", "applies abstraction to data structures and computes ranges, errors"),
    FlagOption("ds-naive", "naive analysis of programs with data structures (ranges, errors)")
  )

  lazy val allPhases: Set[DaisyPhase] = Set(
    analysis.SpecsProcessingPhase,
    transform.CompilerOptimizationPhase,
    analysis.AbsErrorPhase,
    analysis.RangePhase,
    analysis.DataflowPhase,
    analysis.DSAbstractionPhase,
    analysis.DSNaivePhase,
    analysis.RelativeErrorPhase,
    analysis.TaylorErrorPhase,
    analysis.DataflowSubdivisionPhase,
    backend.CodeGenerationPhase,
    transform.TACTransformerPhase,
    transform.PowTransformerPhase,
    analysis.DynamicPhase,
    opt.RewritingOptimizationPhase,
    transform.ConstantTransformerPhase,
    opt.MixedPrecisionOptimizationPhase,
    experiment.MixedPrecisionExperimentGenerationPhase,
    experiment.CostFunctionEvaluationExperiment,
    backend.InfoPhase,
    frontend.ExtractionPhase,
    frontend.CExtractionPhase,
    opt.ApproxPhase,
    opt.MetalibmPhase,
    //transform.ReassignElemFuncPhase,
    experiment.BenchmarkingPhase,
    transform.DecompositionPhase,
    transform.UnrollPhase
  )
  
  var rangeMethod = ""
  var errorMethod = ""
  val trackRoundoffErrs = true


/* 
 * Copied from daisy/src/main/scala/daisy/Main.scala, and stripped down to the essentials.
 */
  def processOptions(args: List[String]): Option[Context] = {
    val initReporter = new DefaultReporter(Set(), false)

    val argsMap: Map[String, String] =
      args.filter(_.startsWith("--")).map(_.drop(2).split("=", 2).toList match {
        case List(name, value) => name -> value
        case List(name) => name -> "yes"
      }).toMap
    
    // all available options from all phases
    val allOptions: Set[CmdLineOption[Any]] = 
      globalOptions ++ allPhases.flatMap(_.definedOptions)

    // go through all options and check if they are defined, else use default
    val opts: Map[String, Any] = allOptions.map({
      case FlagOption(name, _) => name -> argsMap.get(name).isDefined

      case StringOption(name, _) => name -> argsMap.get(name)

      case MultiStringOption(name, _, _) =>
        name -> argsMap.get(name).map(_.stripPrefix("[").stripPrefix("]").split(":").toList).getOrElse(Nil)

      case NumOption(name, default, _) => argsMap.get(name) match {
        case None => name -> default
        case Some(s) => try {
          name -> s.toLong
        } catch {
          case e: NumberFormatException =>
            initReporter.warning(s"Can't parse argument for option $name, using default")
            name -> default
        }
      }

      case ChoiceOption(name, choices, default, _) => argsMap.get(name) match {
        case Some(s) if choices.keySet.contains(s) => name -> choices(s)
        case Some(s) =>
          initReporter.warning(s"Unknown choice value for $name: $s. Options: " +
            s"${choices.keySet.toSeq.sorted.mkString(", ")}. Using default $default")
          name -> choices(default)
        case None => name -> choices(default)
      }

      case MultiChoiceOption(name, choices, _) => argsMap.get(name) match {
        case Some("all") | Some("[all]") =>
          name -> choices.values.toList
        case Some(ss) => name -> ss.stripPrefix("[").stripSuffix("]").split(":").toList.filter {
          case "all" =>
            initReporter.warning(s"'all' in list for $name, ignoring"); false
          case s if !choices.keySet.contains(s) =>
            initReporter.warning(s"Unknown choice value for $name: $s. Options: ${choices.keySet.toSeq.sorted.mkString(", ")}"); false
          case _ => true
        }.map(choices(_))
        case None => name -> Nil
      }
    }).toMap

    def inputInfo: (String, ProgramLanguage.Value) = args.filterNot(_.startsWith("-")) match {
      case Seq() => initReporter.fatalError("No input file")
      case Seq(f) if new File(f).exists && f.endsWith(".c") => (f, ProgramLanguage.CProgram)
      case Seq(f) if new File(f).exists => (f, ProgramLanguage.ScalaProgram)
      case Seq(f) => initReporter.fatalError(s"File $f does not exist")
      case fs => initReporter.fatalError("More than one input file: " + fs.mkString(", "))
    }

    val (inputFile, programLanguage) = inputInfo
    Option(Context(
      initReport = initReporter.report,
      file = inputFile,
      lang = programLanguage,
      options = opts
    ))
  }

  override val name = "mixed-precision optimization"
  override val description = "determines a suitable mixed-precision type assignment"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption("mixed-opt-method", Set("random", "delta", "genetic"), "delta",
      "Algorithm to use for mixed-precision optimization")
  )

  override implicit val debugSection = DebugSectionOptimization

  var reporter: Reporter = null

  type TypeConfig = Map[Identifier, Precision]
  type RangeMap = Map[(Expr, Seq[Expr]), Interval]

  var optimizationMethod: String = ""

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val (newCtx, newPrg) = runPhaseHelper(ctx, prg)
    (newCtx, newPrg)
  }


  def runPhaseHelper(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    val defaultPrecision = ctx.option[Precision]("precision")
    optimizationMethod = ctx.option[String]("mixed-opt-method")

    val availablePrecisions = defaultPrecision match {
      case FixedPrecision(b) => // given precision specifies the upper bound
        //(1 to b).map(FixedPrecision(_))
        // bad implementation
        // the above implementation gives outflow errors for many cases, and it is hard to tackle every one of them
        // also it searches for every precision, but for a CPU it is not necessary
        // in a test with panda's 2nd joint, the above implementation took around 2 minutes
        // and the below implementation took around 1 and a half minute
        // it helps, and I think it will have a better impact on more complicated cases
        // for the panda's 3rd joint, with the below implementation it took 2.5 minutes
        // Fixed8, Fixed16, Fixed32, Fixed64
        if(b < 8){
          reporter.warning("Precision is too low, setting it to 8")
          Seq(FixedPrecision(8))
        }
        else if(b < 16){
          Seq(FixedPrecision(8))
        }
        else if(b < 32){
          Seq(FixedPrecision(8), FixedPrecision(16))
        }
        else if(b < 64){
          Seq(FixedPrecision(8), FixedPrecision(16), FixedPrecision(32))
        }
        else{
          Seq(FixedPrecision(8), FixedPrecision(16), FixedPrecision(32), FixedPrecision(64))
        }
      case _ =>
        Seq(Float32, Float64, DoubleDouble)
    }
    reporter.info(s"Optimisation method: $optimizationMethod")

    // store updated ranges for each function
    var resAbsoluteErrors: Map[Identifier, Rational] = Map()
    var precisionMap: Map[Identifier, Map[Identifier, Precision]] = Map()
    var newIntermediateRanges: Map[Identifier, RangeMap] = Map()
    var newInputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()

    // only consider function which have no assigned types yet
    val fncsToConsider = if (ctx.hasFlag("approx")) functionsToConsider(ctx, prg).filter(_.returnType == RealType)
      else functionsToConsider(ctx, prg)

    val newDefs: Seq[FunDef] = fncsToConsider.map(fnc => {
      // print all elements of fnc
      fnc.params.foreach(x => reporter.info(s"Param: $x"))
      // print preconditions
      reporter.info("Preconditions:")
      reporter.info(fnc.precondition)
      // The input bounds are stored inside preconditions. We need to modify them!
      val fullBody = fnc.body.get
      reporter.info(s"Analyzing function ${fnc.id}")
      val _rangeMap = ctx.intermediateRanges(fnc.id)
      //reporter.info("ranges are here?")
      //reporter.info(_rangeMap)

      // there is an output error to optimize for
      val (typeConfig, constPrec) = if (ctx.specResultErrorBounds.contains(fnc.id)) {
        val targetError = ctx.specResultErrorBounds(fnc.id)

        val paths = extractPaths(fullBody, emptyPath, _rangeMap)

        

        val res: Seq[(TypeConfig, Precision)] = paths.map({ case (pathCond, _body, rangeMap) =>
          reporter.info(s"Analyzing path: $pathCond")
          val body = removeUnusedVars(_body)

          // Step 1: find the smallest available precision which satisfies the error bound
          // TODO: search from the end, the first one which does not satisfy the spec
          //reporter.info("First search for some reason")
          availablePrecisions.find( prec => {
            try {
              // path may contain unused variables, which is why use the allIDsOf here
              // TODO: remove unused variables, as they may produce suboptimal results
              val rndoff = computeAbsError(body, allIDsOf(body).map(id => (id -> prec)).toMap,
                prec, rangeMap, pathCond, approximate = false)
              rndoff <= targetError
            } catch {  // e.g. overflow when precision is not enough
              case _ : Throwable => false
            }
          })
          //reporter.info("Second search for some reason")
          // search from the end:
          availablePrecisions.reverse.find( prec => {
            try {
              // print precision
              reporter.warning(s"Trying precision: $prec")
              // path may contain unused variables, which is why use the allIDsOf here
              val rndoff = computeAbsError(body, allIDsOf(body).map(id => (id -> prec)).toMap,
                prec, rangeMap, pathCond, approximate = false)
              rndoff <= targetError
            } catch {  // e.g. overflow when precision is not enough
              case _ : Throwable => false
            }
          }) match {
            case None =>
              reporter.warning(s"Highest precision is *not enough* for ${fnc.id}; " +
                s"generating the highest-precision (${availablePrecisions.last}) version anyway.")
              (allIDsOf(body).map(v => (v -> availablePrecisions.last)).toMap,
                availablePrecisions.last)

            case Some(prec) if prec == availablePrecisions.head =>
              reporter.info(s"No mixed-precision optimization needed for ${fnc.id} - " +
                s"lowest (${availablePrecisions.head}) is sufficient")
              (allIDsOf(body).map(v => (v -> prec)).toMap, prec)

            case Some(lowestUniformPrec) =>
              val indexLowestUniformPrec = availablePrecisions.indexOf(lowestUniformPrec)

              // Step 2: optimize mixed-precision
              reporter.info(s"Optimizing mixed-precision for ${fnc.id}...")
              val consideredPrecisions = availablePrecisions.take(indexLowestUniformPrec + 1)
              reporter.info(s"Considered precisions: $consideredPrecisions")
              reporter.info(s"Lowest possible uniform precision: ${availablePrecisions(indexLowestUniformPrec)}")

              val costFnc = consideredPrecisions.last match {
                case DoubleDouble | QuadDouble =>
                  simpleMixedPrecisionCost _

                case FixedPrecision(_) =>
                  val original = ctx.originalProgram.defs.find(_.id == fnc.id) match {
                    case Some(x) => x.body.get
                    case None => throw new Exception(s"Original body of the function ${fnc.id} is not available in the context")
                  }
                  ctx.reporter.debug(s"Using ${ctx.option[String]("cost")} cost")
                  ctx.option[String]("cost") match {
                    case "area" => areaBasedCostFunction _
                    case "ml" => mlRegressionCostFunction(original, _:Expr, _:TypeConfig)
                    case "combined" => combinedCost(original, _:Expr, _:TypeConfig)
                  } //  simpleMixedPrecisionCost _

                case _ =>
                  benchmarkedMixedPrecisionCost _
              }

              reporter.info(s"Using cost function: ${costFnc}")

              optimizationMethod match {
                case "delta" =>

                  val (tpeconfig, prec) = (deltaDebuggingSearch(ctx, body, targetError, fnc.params, costFnc,
                    computeAbsErrorInDiffIntervals(ctx, body, _, _, rangeMap, pathCond, approximate = true, targetError),
                    //computeAbsError(body, _, _, rangeMap, pathCond, approximate = true),
                    consideredPrecisions),
                    lowestUniformPrec)

                  (tpeconfig, prec)

                case "random" =>

                  (randomSearch(body, targetError, costFnc,
                    computeAbsError(body, _, _, rangeMap, pathCond, approximate = true),
                    consideredPrecisions, maxTries = 1000),
                    lowestUniformPrec)

                case "genetic" =>

                  (geneticSearch(body, targetError, benchmarkedMixedPrecisionCost,
                    computeAbsError(body, _, _, rangeMap, pathCond, approximate = true),
                    consideredPrecisions),
                  lowestUniformPrec)

              }
          }
        })

        val (tpeConfigs, constPrecs) = res.unzip

        (mergeTypeConfigs(tpeConfigs), constPrecs.max)

      } else {
        // If no error is given in the postcondition, assign default precision
        reporter.warning(s"No target error bound for ${fnc.id}, " +
          s"assigning default uniform precision $defaultPrecision.")

        (allVariablesOf(fullBody).map(v => (v -> defaultPrecision)).toMap,
          defaultPrecision)
      }

      // final step: apply found type config, and update ranges (mainly due to casts)
      // types have only been applied to variables used in computation, but not conditionals
      // assign default precision to those:
      val _typeConfig = (allIDsOf(fullBody) -- typeConfig.keys).map(id => (id -> constPrec)).toMap
      val newTypeConfig = typeConfig ++ _typeConfig
      val (updatedBody, newRangeMap) = applyFinitePrecision(fullBody, newTypeConfig, _rangeMap)

      // this is what computeAbsError does, but we need the intermediate information
      val inputErrorMap: Map[Identifier, Rational] = freeVariablesOf(fullBody).map({
        case id => (id -> newTypeConfig(id).absRoundoff(_rangeMap((Variable(id), emptyPath))))
      }).toMap
      val (resRoundoff, _) = evalRoundoff[Interval](updatedBody, _rangeMap ++ newRangeMap,
        newTypeConfig, inputErrorMap.mapValues(Interval.+/-).toMap, Interval.zero, Interval.+/-,
        Interval.apply, constPrec, true, false)
      val resError = Interval.maxAbs(resRoundoff.toInterval)

      resAbsoluteErrors = resAbsoluteErrors + (fnc.id -> resError)
      precisionMap = precisionMap + (fnc.id -> (newTypeConfig))
      newIntermediateRanges = newIntermediateRanges + (fnc.id -> newRangeMap)
      newInputErrors = newInputErrors + (fnc.id -> inputErrorMap)

      val updatedParams = fnc.params.map(valDef =>
        ValDef(valDef.id.changeType(FinitePrecisionType(typeConfig(valDef.id)))))

      val resPrecision = (updatedBody.getType: @unchecked) match {
        case FinitePrecisionType(tpe) => tpe
      }

      fnc.copy(returnType = FinitePrecisionType(resPrecision), params = updatedParams,
        body = Some(updatedBody))

    }) // end of newDefs

    (ctx.copy(resultAbsoluteErrors = resAbsoluteErrors,
      intermediateRanges = ctx.intermediateRanges ++ newIntermediateRanges,
      specInputPrecisions = ctx.specInputPrecisions ++ precisionMap,
      assignedPrecisions = ctx.specInputPrecisions ++ precisionMap,
      specInputErrors = ctx.specInputErrors ++ newInputErrors),
      Program(prg.id, newDefs ++ (functionsToConsider(ctx, prg).diff(fncsToConsider)))) // todo when fnc to consider is specified add approx ones too
  }

  def computeAbsErrorInDiffIntervals(ctx: Context, expr: Expr, typeConfig: Map[Identifier, Precision],
    constantsPrecision: Precision, rangeMap: Map[(Expr, PathCond), Interval],
    path: PathCond, approximate: Boolean = false, targetError: Rational): Rational = {

      // somehow if we start a new pipeline here, it causes many problems,
      // starting with identifiers no longer matching for some reason,
      // and some other bugs that I could not figure out, yet
      // It seems a recursive call in this function is the only way to go
      // I am saying a recursive call on this function, because I am not sure if we need to apply applyFinitePrecision
      // and some other functions each time. I'll play safe for now
      // let's print every parameter
      //reporter.info("expr:")
      //reporter.info(expr)
      //reporter.info("typeConfig:")
      //reporter.info(typeConfig)
      //reporter.info("constantsPrecision:")
      //reporter.info(constantsPrecision)
      //reporter.info("rangeMap:") // it seems only thing about input bounds is this
      //reporter.info(rangeMap)
      //reporter.info("path:") // this was empty in my trials
      //reporter.info(path)
      //reporter.info("approximate:")
      //reporter.info(approximate)

      // The only thing we need to recalculate each time seems to be rangeMap
      // there is no one function on it, rangePhase calculates it.

      val originalProgram = ctx.originalProgram
      // assuming rangeMethod to be affine for now
      val fncs = functionsToConsider(ctx, originalProgram)
      val fnc = fncs.head // assuming there is only one function
      reporter.info("Function:")
      reporter.info(fnc)
      // fnc has some preconditions, I believe if we modify then, we can get the desired results without any more changes
      // I will try to modify them
      reporter.info("Preconditions:")
      reporter.info(fnc.precondition)
      reporter.info("body:")
      reporter.info(fnc.body.get)
      val preconditions_list = fnc.precondition match {
        case Some(x) => {
          // x here is ((qpos5 > 0.2) ? (qpos5 < 0.21) ? (qpos6 > 0.2) ? (qpos6 < 0.21))
          // first let's try to print them one by one
          // I will try to print the first one
          // the elements are OR operators, so I guess I can again use a match case
          x match {
            case Trees.And(a) => {
              // a must be a list of conditions
              a
            }
          }
        }
      }

      // Now, every element of this list is a condition, (var_name < or > value)
      // I assume the list is sorted, so that the first element is the lower bound and the second element is the upper bound,
      // and so on
      // The next step is from this producing a list of preconditions, which is subdivided versions of the original one
      // I will try to do that now
      var i = 0
      var preconditions = List[List[Expr]]()
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

        //if(upper_bound_value - lower_bound_value < minimumIntervalSize){
        //  // if the difference is too small, we can skip this one
        //  i = i + 2
        //  continue
        //}

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
        i = i + 2
      }
      // now I have the preconditions, I will try to print them
      reporter.info("New preconditions:")
      reporter.info(preconditions)
      // preconditions should have the structure:
      //List(
      //  List(((qpos5 > 0.2) ? (qpos5 < 0.205)), ((qpos5 > 0.205) ? (qpos5 < 0.21))), 
      //  List(((qpos6 > 0.2) ? (qpos6 < 0.205)), ((qpos6 > 0.205) ? (qpos6 < 0.21)))
      //)
      reporter.info(
        takeCombinations(preconditions)
      )
      // now, take each inner list, and flatten it
      val all_preconditions = processInnerLists(
        takeCombinations(preconditions)
      )
      reporter.info("All preconditions:")
      reporter.info(all_preconditions)

      // now, we can also construct inputValMap from all_preconditions
      // inputValMap is: List((qpos5,[0.2,0.21]), (qpos6,[0.2,0.21]))
      var inputValMaps: List[Map[Identifier, Interval]] = List()
      var inputValMap: Map[Identifier, Interval] = Map()
      all_preconditions.foreach({
        case And(a) => {
          // a must be a list of conditions
          a match {
            case List(first, second) => {
              first match {
                case And(b) => {
                  b match {
                    case Seq(GreaterThan(Variable(id), RealLiteral(lower)), LessThan(_, RealLiteral(upper))) => {
                      val a = (id -> Interval(lower, upper))
                      inputValMap = inputValMap ++ Map(id -> Interval(lower, upper))
                      second match {
                        case And(c) => {
                          c match {
                            case Seq(GreaterThan(Variable(id), RealLiteral(lower)), LessThan(_, RealLiteral(upper))) => {
                              val a = (id -> Interval(lower, upper))
                              inputValMap = inputValMap ++ Map(id -> Interval(lower, upper))
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        inputValMaps = inputValMaps :+ inputValMap
      })

      reporter.info("InputValMaps:")
      reporter.info(inputValMaps)

      inputValMaps.foreach(inputValMap => {
        
        val (resRange, intermediateRanges) = evalRange[AffineForm](fnc.body.get,
            inputValMap.map(x => (x._1 -> AffineForm(x._2))), AffineForm.apply)
      
        reporter.info("ResRange:")
        reporter.info(resRange)
      })




      computeAbsError(expr, typeConfig, constantsPrecision, rangeMap, path, approximate)
    }
  
  def takeCombinations(lst: List[List[Expr]]): List[List[Expr]] = {
    lst match {
      case Nil => List(Nil)
      case head :: tail =>
        for {
          h <- head
          t <- takeCombinations(tail)
        } yield h :: t
    }
  }
  
  def processInnerLists(lst: List[List[Expr]]): List[Expr] = {
    lst match {
      case Nil => List()
      case head :: tail => {
        // head here is the form List(((qpos5 > 0.2) ? (qpos5 < 0.205)), ((qpos6 > 0.2) ? (qpos6 < 0.205)))
        // I want to convert it to (qpos5 > 0.2) ? (qpos5 < 0.205), (qpos6 > 0.2) ? (qpos6 < 0.205)
        List(processInnerListsHelper(head)) ++ processInnerLists(tail)
      }
    }
  }

  def processInnerListsHelper(lst: List[Expr]): Expr = {
    lst match {
      case Nil => throw new Exception("Empty list")
      case head :: Nil => head
      case head :: tail => And(head, processInnerListsHelper(tail))
    }
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
        (id -> typeConfig(id).absRoundoff(rangeMap((Variable(id), emptyPath))))
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

  // merges type configs from each path by taking the type upper bound
  // for a variable which has been assigned in different paths
  private def mergeTypeConfigs(configs: Seq[TypeConfig]): TypeConfig = {
    val allIDs: Seq[Identifier] = configs.flatMap(_.keys).toSet.toList

    allIDs.map(id => {
      val allPrec: Seq[Precision] = configs.flatMap(_.get(id))

      if (allPrec.size == 1) {
        (id -> allPrec.head)
      } else {
        (id -> allPrec.tail.fold(allPrec.head)({
          case (prec, newPrec) => getUpperBound(prec, newPrec)
        }))
      }
    }).toMap
  }

  // Extracts paths in the program and for each computes
  // - the path condition
  // - the path expression
  // - the new rangeMap for the new path (without the path condition)
  private def extractPaths(e: Expr, path: PathCond, currRanges: RangeMap): Seq[(PathCond, Expr, RangeMap)] = e match {

    case Let(id, value, body) =>
      // there shouldn't be any if-expr in the value (at least we don't support it)
      val (_, _, _rangeMapValue) = extractPaths(value, path, currRanges).head
      val rangeMapValue = _rangeMapValue + ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath)))

      extractPaths(body, path, currRanges).map({
        case (pCond: PathCond, pathBody: Expr, rangeMapBody) =>
          (pCond, Let(id, value, pathBody), rangeMapValue ++ rangeMapBody)
      })

    case IfExpr(cond, thenn, elze) =>
      val thenPaths = extractPaths(thenn, path :+ cond, currRanges).map({
        case (pCond: PathCond, pathBody: Expr, rangeMap) => (cond +: pCond, pathBody, rangeMap)
      })
      val elsePaths = extractPaths(elze, path :+ negate(cond), currRanges).map({
        case (pCond: PathCond, pathBody: Expr, rangeMap) => (negate(cond) +: pCond, pathBody, rangeMap)
      })
      thenPaths ++ elsePaths

    case x @ ArithOperator(args, recons) =>
      val rangeMap: RangeMap = args.map( arg => {
        (arg, emptyPath) -> currRanges(arg, path)
      }).toMap + ((x, emptyPath) -> currRanges(x, path))
      Seq((Seq(), e, rangeMap))

    case _ => Seq((Seq(), e, Map((e, emptyPath) -> currRanges(e, path))))

  }

  def removeUnusedVars(body: Expr): Expr = {
    val unused = allIDsOf(body) -- allVariablesOf(body)

    def remove(e: Expr): Expr = e match {
      // var is unused, skip
      case Let(id, v, b) if (unused.contains(id)) => remove(b)
      case Let(id, v, b) => Let(id, v, remove(b))
      case _ => e
    }

    remove(body)
  }

  /**
   * Performs Precimonious' delta-debugging search to find a valid type configuration.
   *
   * @param expr expression to be optimized
   * @param errorSpec target error, i.e. maximum abs. error tolerated
   * @param params free variable/function parameters of expr
   * @param errorFnc error function to use for evaluating the error (this is not fixed for testing purposes)
   * @param availablePrecisions which precisions are available for assigning
   * @return (type configuration, return precision)
   */
  def deltaDebuggingSearch(ctx: Context, expr: Expr, errorSpec: Rational, params: Seq[ValDef],
    costFnc: (Expr, Map[Identifier, Precision]) => Rational,
    errorFnc: (Map[Identifier, Precision], Precision) => Rational,
    availablePrecisions: Seq[Precision]): TypeConfig = {

    // TODO:
    // somewhere here, use args, have a clear context, and call SpecsProcessingPhase and RangePhase
    // and then you can call DataflowPhase to calculate errors.
    // I believe getting the results from that should be pretty easy, and the rest should be just a small algorithm
    // that replaces computeAbsError calls here, which what I was thinking computeAbsErrorInDiffIntervals is for
    //  TODO:
    /*
    var process_args = ctx.args
    reporter.info("process_args")
    reporter.info(ctx.args.toList)
    // remove the delta-debugging search from the args, just in case
    val new_args = process_args.filterNot(_ == "--mixed-tuning")
    reporter.info(new_args.toList)

    reporter.info("starting new process, see what happens")

    processOptions(process_args.toList) match{
      case Some(new_ctx) =>
        val new_pipeline = frontend.ExtractionPhase >> analysis.SpecsProcessingPhase >> analysis.RangePhase >> analysis.DataflowPhase
        new_pipeline.run(ctx, Program(null, Nil))
      case None =>
        reporter.fatalError("Error in processing the arguments")
    }
    reporter.info("finished new process, see what happens")

    // use new_ctx to get results
    reporter.info("specInputPrecisions")
    reporter.info(ctx.specInputPrecisions) // I guess we'll use this to assign precisions?
    reporter.info("uniformPrecisions")
    reporter.info(ctx.uniformPrecisions)
    reporter.info("resultAbsoluteErrors")
    reporter.info(ctx.resultAbsoluteErrors) // this
    reporter.info("resultRealRanges")
    reporter.info(ctx.resultRealRanges) // and that are the most important ones
    reporter.info("intermediateAbsErrors")
    reporter.info(ctx.intermediateAbsErrors) //this is too complicated, it returns everything as a tree
    reporter.info("intermediateRanges")
    reporter.info(ctx.intermediateRanges) //this is too complicated, it returns everything as a tree
    reporter.info("assignedPrecisions")
    reporter.info(ctx.assignedPrecisions)
    */

    rangeMethod = ctx.option[String]("rangeMethod")
    errorMethod = ctx.option[String]("errorMethod")


    reporter.info("Starting delta debugging search...")

    val highestPrecision = availablePrecisions.last
    val lowestPrecision = availablePrecisions.head

    reporter.info(s"Available precisions: $availablePrecisions")
    reporter.info(s"Highest precision: $highestPrecision")
    reporter.info(s"Lowest precision: $lowestPrecision")

    // map from each precision to its index in the availablePrecisions list
    val precIndexMap: Map[Precision, Int] = availablePrecisions.zipWithIndex.toMap

    def lowerVariables(vars: Seq[Identifier], typeConfig: Map[Identifier, Precision]): Map[Identifier, Precision] = {
      typeConfig.map({
        case (id, prec) =>
          if (vars.contains(id)) {
            // lower the variable's precision
            (id -> availablePrecisions(precIndexMap(prec) - 1))
          } else {
            (id -> prec)
          }
      })
    }

    // constants that are representable in lowest precision and thus need not be optimised
    // used during experiments
    val constants = Seq[Identifier]()

    // all variables appearing in the expr **in order of appearance**
    val allVars: Seq[Identifier] = {
      val inputParams: Seq[Identifier] = params.map(_.id)

      val letDefs = lang.TreeOps.fold[Seq[Identifier]] {
        case (e, subs) =>
          val subvs = subs.flatten.toList
          e match {
            //case Variable(i) => subvs :+ i
            case Let(i, RealLiteral(r), _) if (lowestPrecision.canRepresent(r)) =>
              //constants :+= i; subvs
              subvs :+ i
            case Let(i, _, _) => subvs :+ i
            case _ => subvs
          }
      }(expr)
      inputParams ++ letDefs.reverse
    }
    //reporter.info(s"allVars: $allVars")
    assert(constants.size == 0)

    //reporter.info("calculated allVars")

    var numValidConfigs = 0
    val candidateTypeConfigs = MSet[TypeConfig]()   // for info purposes only

    // factor out so that it can be changed easily
    val lessThanCost: (Rational, Rational) => Boolean = lessThanByRationalCost

    // @param currentVars are those variables that we want to lower
    // @param typeConfig is the current Type configuration (of which we know that it is a valid one)
    // @returns the 'optimal' type config satisfying the spec and has smallest cost
    def deltaDebug(currentVars: Seq[Identifier], typeConfig: Map[Identifier, Precision], depth: Int): Map[Identifier, Precision] = {

      if (depth >= 1000) {
        reporter.warning("!! delta debugging, max depth level reached !!")
        typeConfig
      } else {

        // 1: lower all variables in varsToOpt
        val loweredTypeConfig = lowerVariables(currentVars, typeConfig)
        candidateTypeConfigs += loweredTypeConfig

        // 2: evaluate current config
        //reporter.info("first errorfnc")
        //reporter.info("one run")
        val currentError = errorFnc(loweredTypeConfig, highestPrecision)
        //reporter.info("first errorfnc finished")

        // 3a: if the error is below threshold, we are done recursing
        if (currentError <= errorSpec) {
          //reporter.info("error below spec")
          numValidConfigs = numValidConfigs + 1
          val fixedVars = allVars.diff(currentVars)

          if (fixedVars.isEmpty) { // nothing to optimize further
            //reporter.info("no more variables to optimize")
            loweredTypeConfig
          } else {
            val (fixedVarsLeft, fixedVarsRight) = fixedVars.splitAt(fixedVars.length / 2)
            //reporter.info("lowervariables")
            val loweredLeft = lowerVariables(fixedVarsLeft, loweredTypeConfig)
            val loweredRight = lowerVariables(fixedVarsRight, loweredTypeConfig)
            //reporter.info("lowervariables")
            candidateTypeConfigs += loweredLeft; candidateTypeConfigs += loweredRight

            //reporter.info("second errorfnc")
            //reporter.info("one run")
            val errorLeft = errorFnc(loweredLeft, highestPrecision)
            //reporter.info("second errorfnc finished")
            //reporter.info("third errorfnc")
            //reporter.info("one run")
            val errorRight = errorFnc(loweredRight, highestPrecision)
            //reporter.info("third errorfnc finished")

            // choose the one with lowest cost
            //reporter.info("costfnc")
            var currentMinCost = costFnc(expr, loweredTypeConfig)
            //reporter.info("costfnc finished")
            var currentBestConfig = loweredTypeConfig
            /* TODO: Robert' edit (but unclear why it is correct)
            val typeConfigCost = costFnc(expr, typeConfig)
            val loweredCost = costFnc(expr, loweredTypeConfig)
            // choose the one with lowest cost
            var (currentMinCost, currentBestConfig) = if (lessThanCost(typeConfigCost, loweredCost)) {
              (typeConfigCost, typeConfig)
            } else {
              (loweredCost, loweredTypeConfig)
            }*/

            if (errorLeft < errorSpec) {
              numValidConfigs = numValidConfigs + 1
              val costLeft = costFnc(expr, loweredLeft)
              if (lessThanCost(costLeft, currentMinCost)) {
                currentBestConfig = loweredLeft
                currentMinCost = costLeft
              }
            }

            if (errorRight < errorSpec) {
              numValidConfigs = numValidConfigs + 1
              val costRight = costFnc(expr, loweredRight)
              if (lessThanCost(costRight, currentMinCost)) {
                currentBestConfig = loweredRight
                currentMinCost = costRight
              }
            }

            currentBestConfig
          }

        } else {
          // 3b: if the error is not below, we need to continue to subdivide
          //reporter.info("error above spec")

          if (currentVars.length <= 1) {
            typeConfig
          } else {
            //reporter.info("subdividing")

            // subdivide variables, divide by 2 should be integer division
            val (currVarsLeft, currVarsRight) = currentVars.splitAt(currentVars.length / 2)

            // sometimes the split causes one side to give DivisionByZero errors, and I guess we can just skip them
            // However, what can we do if both sides give DivisionByZero errors?
            // I guess we can return the typeConfig as it is

            try{
              val configLeft = deltaDebug(currVarsLeft, typeConfig, depth + 1)
              // the left side is valid, so we can try the right side
              try {
                val configRight = deltaDebug(currVarsRight, typeConfig, depth + 1)
                // both sides are valid, so we need to choose the one with the lowest cost
                val costLeft = costFnc(expr, configLeft)
                val costRight = costFnc(expr, configRight)

                if (lessThanCost(costLeft, costRight)) {
                  configLeft
                } else {
                  configRight
                }
              } catch {
                case e: DivisionByZeroException => {
                  reporter.warning("Division by zero error in delta debugging, skipping right side")
                  configLeft
              } 
            }
            } catch {
              case e: DivisionByZeroException => {
                reporter.warning("Division by zero error in delta debugging, skipping left side")
                try {
                  val configRight = deltaDebug(currVarsRight, typeConfig, depth + 1)
                  configRight
                } catch {
                  case e: DivisionByZeroException => { // both of them give DivisionByZero errors
                    reporter.warning("Division by zero error in delta debugging, skipping right side")
                    return typeConfig
                  }
                }
              }
            }
          }
        }
      }
    }


    // experiment, strawmans version of shuffle
    //allVars = allVars.toSet.toList

    // possible optimization: integer constants and contants representable in the lowest precision
    // are fixed to have lowest precision and are not optimized
    val initialTypeConfig = allVars.map(i => (i -> highestPrecision)).toMap ++
      constants.map(i => (i -> lowestPrecision)).toMap

    val originalCost = costFnc(expr, initialTypeConfig)
    reporter.info(s"Initial cost: $originalCost")

    var currentTypeConfig = initialTypeConfig
    var currentVars = allVars  // variables which can be lowered

    // initially the low precision is the second last precision
    var currentLowPrecIndex = availablePrecisions.length - 2
    reporter.info(s"Initial low precision index: $currentLowPrecIndex")

    var continue = true
    while (continue && currentLowPrecIndex >= 0) {
      // do delta debugging, which will lower some variables
      //reporter.info("Starting delta debugging...")
      reporter.info(s"Current low precision index: $currentLowPrecIndex")
      try {
        val newTypeConfig = deltaDebug(currentVars, currentTypeConfig, 0)
        // if nothing changed, stop
        if (newTypeConfig == currentTypeConfig) {
          continue = false
        } else {
          // something did change, so fix the higher-precision variables and only
          // keep the low-precision ones in the running
          val lowPrecision = availablePrecisions(currentLowPrecIndex)
  
          val varsToLowerFurther = allVars.filter((id => newTypeConfig(id) == lowPrecision))
  
          // update everything
          currentVars = varsToLowerFurther
          currentTypeConfig = newTypeConfig
          currentLowPrecIndex = currentLowPrecIndex - 1
        }
      }
      catch { // If the deltaDebugging fails, I guess we can stop the current iteration?
        case e: DivisionByZeroException => {
          reporter.warning("Division by zero error in delta debugging, skipping")
          continue = false
        }
      }
    }
    val finalCost = costFnc(expr, currentTypeConfig)
    reporter.info(s"initial Cost: $originalCost - final cost: $finalCost")
    reporter.info(s"number of valid type configs: ---- $numValidConfigs, out of ${candidateTypeConfigs.size} unique configs seen")
    currentTypeConfig
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

  /**
   * Generates maxTries random type configurations and returns the one which
   * - satisfies the error specification (worst-case absolute error) and
   * - has smallest cost as given by the cost function
   *
   * @param expr AST to be optimized
   * @param maxTries maximum number of type configurations to try
   * @param errorSpec worst-case absolute error to tolerate
   * @param costFnc function to evaluate the cost of a type config
   * @return (type configuration, precision of return value)
   * TODO: the cost function does not need to return Rationals, perhaps Double will do?
   */
  def randomSearch(expr: Expr, errorSpec: Rational, costFnc: (Expr, Map[Identifier, Precision]) => Rational,
    errorFnc: (Map[Identifier, Precision], Precision) => Rational,
    availablePrecisions: Seq[Precision], maxTries: Int = 1000): TypeConfig = {

    val rand = new Random(4789)

    val numPrecisions = availablePrecisions.length
    val highestPrecision = availablePrecisions.last
    val ids = allVariablesOf(expr)

    reporter.info("-----Starting random search----")

    // try a reasonable number of configs
    val maxUniqueTypeConfigs: Long = math.pow(numPrecisions, ids.size).toLong
    val maxCandTypeConfigs: Long = math.min(maxTries, maxUniqueTypeConfigs)
    // the factor is to accommodate random assignment to pick the same
    val maxIterCount = 15 * maxCandTypeConfigs

    var iterCount = 0L

    val candidateTypeConfigs = MSet[TypeConfig]()

    // randomly sample candidate configs
    while (candidateTypeConfigs.size < maxCandTypeConfigs && iterCount < maxIterCount) {
      val tmp = ids.map(id => (id -> availablePrecisions(rand.nextInt(numPrecisions)))).toMap
      candidateTypeConfigs += tmp
      iterCount = iterCount + 1
    }
    //if (iterCount >= maxIterCount) reporter.warning("maxIterCount reached in random search")
    if (candidateTypeConfigs.size == maxUniqueTypeConfigs) reporter.info("exhaustive search")

    reporter.info(s"Generated ${candidateTypeConfigs.size} unique type configs, " +
      s"exhaustive search would need: $maxUniqueTypeConfigs")

    // test all of them, and keep track of best (only)
    //val veryLargeRational = Rational(100000l, 1l)
    var numValidConfigs = 0

    // default is the highest precision, since we know that that must work
    var bestCandidate: TypeConfig = ids.map(v => (v -> availablePrecisions.last)).toMap
    //var bestReturnPrec: Precision = availablePrecisions.last
    var bestCost: Rational = costFnc(expr, bestCandidate)

    candidateTypeConfigs.foreach(tpeConfig => {

      val error = errorFnc(tpeConfig, highestPrecision)

      if (error <= errorSpec) {
        // hard-constraint error spec is satisfied, so the cost function decides
        numValidConfigs += 1

        val cost = costFnc(expr, tpeConfig)
        if(cost < bestCost) {
          bestCandidate = tpeConfig
          //bestReturnPrec = retPrec
          bestCost = cost
        }

      }
    })
    reporter.info(s"number of valid type configs: ---- $numValidConfigs ----")

    bestCandidate
  }


  // ----------- Things for genetic search ------------
  var rand = new Random(123456789)

  /*
    Things to tune:
      - mutation function: should we also increase the precision again (with some prob)
      - crossover a percentage of vars instead of just two
      - for crossover perhaps keep a list of types in order of appearance? somehow, this will need to be a hack
      - figure out if starting from high and going to low is the right choice
        and how to adjust the fitness function accordingly
      - TODO: we may want to do it the other way around, since we found that there
        are few valid configs close the higher errors?
      - is the # of generations and population size sufficient, should it be increases based on size of benchmark?
  */

  def mutate(tpeConfig: TypeConfig): TypeConfig = {

    // randomly pick a node
    val keys = tpeConfig.keys.toList
    val id = keys(rand.nextInt(keys.size))

    // decide what to do with the associated type
    val currentType = tpeConfig(id)
    val newType = (currentType: @unchecked) match {
      case Float32 => Float32     // TODO: perhaps we want to move it up again (with some prob?)
      case Float64 => Float32
      case DoubleDouble => Float64
    }

    // update tpeConfig
    val newConfig = tpeConfig + (id -> newType)

    newConfig
  }

  override def crossover(t1: TypeConfig, t2: TypeConfig): (TypeConfig, TypeConfig) = {

    // lets pick two ids and switch them
    val keys = t1.keys.toList     // these are not in the same order as they appear in the program...
    val index = rand.nextInt(keys.size - 1)
    val (id1, id2) = (keys(index), keys(index + 1))

    val newTypeConfig1 = t1 + (id1 -> t2(id1)) + (id2 -> t2(id2))
    val newTypeConfig2 = t2 + (id1 -> t1(id1)) + (id2 -> t1(id2))

    (newTypeConfig1, newTypeConfig2)
  }

  /**
   * Runs a genetic search to find a valid type configuration
   * @param  expr expression to be optimized
   * @param  errorSpec maximum abs error tolerated
   * @param  costFnc function to evaluate the cost of a type config
   * @return (type configuration, return precision)
   */
  def geneticSearch(expr: Expr, errorSpec: Rational,
    costFnc: (Expr, Map[Identifier, Precision]) => Rational,
    errorFnc: (Map[Identifier, Precision], Precision) => Rational,
    availablePrecisions: Seq[Precision]): TypeConfig = {

    reporter.info("-----Starting genetic search----")

    val highestPrecision = availablePrecisions.last
    val maxCost = Rational(100000)
    var numValidConfigs = 0
    var validConfigs = MSet[TypeConfig]()

    // cache results; sometimes there are duplicates
    val cache = collection.mutable.HashMap[TypeConfig, Rational]()
    var numUsedCache = 0

    // initialize population with all variables at the highest available precision
    val initialTypeConfig: TypeConfig = allVariablesOf(expr).map(id => (id, highestPrecision)).toMap


    val (newTypeConfig, _) = runGenetic(
      initialTypeConfig,
      //(t: TypeConfig) => t,   // why does copy not work? do we need to copy, the map is immutable...?
      (tpeConfig: TypeConfig) => {

        // the fitness is a combination of the error and the cost
        val error = if (cache.contains(tpeConfig)) {
            numUsedCache = numUsedCache + 1
            cache(tpeConfig)
          } else {
            val tmp = errorFnc(tpeConfig, highestPrecision)
            cache += (tpeConfig -> tmp)
            tmp
          }

        val cost = if (error <= errorSpec) {
          validConfigs += tpeConfig
          costFnc(expr, tpeConfig)

        } else {
          // the maxCost is added so that we penalize those configs which do not satisfy the spec
          // if we compare two configs which do not satisfy the errorSpec,
          //then the smaller error is better as it is closer to satisfying the spec

          maxCost + error
        }
        cost
      },
      crossoverProb = 0.3)

    reporter.info(s"number of valid type configs: ---- $numValidConfigs, unique: ${validConfigs.size}----")
    reporter.info(s"# used cache: $numUsedCache")

    newTypeConfig
  }

}