import com.typesafe.config._
import dsl.dsl._
import json._
import pureconfig.ConfigReader.Result
import pureconfig._
import scala.util._

object Main extends App {

  type Rule[F[_], A] = RuleSym[F] => F[A]

  def rule[F[_]]: Rule[F, Boolean] = (ruleSym: RuleSym[F]) => {
    implicit val rs: RuleSym[F] = ruleSym
    (VAR("categoryId") EQUALS NUM(10)) AND
      (VAR("price") EQUALS NUM(3))
  }

  type Doc = Map[String, Int]
  type Pretty[_] = String

  val prettyInterpreter = new RuleSym[Pretty] {
    override def num(value: Int): Pretty[Int] = value.toString

    override def variable(name: String): Pretty[Int] = s"$$$name"

    override def equal(lhs: Pretty[Int], rhs: Pretty[Int]): Pretty[Boolean] =
      s"($lhs EQUALS $rhs)"

    override def and(
        lhs: Pretty[Boolean],
        rhs: Pretty[Boolean]
    ): Pretty[Boolean] = s"$lhs AND $rhs"
  }

  println(rule(prettyInterpreter))

  val interpreter = new RuleSym[Doc => *] {
    override def num(value: Int): Doc => Int = _ => value

    override def variable(name: String): Doc => Int = doc => doc(name)

    override def equal(lhs: Doc => Int, rhs: Doc => Int): Doc => Boolean =
      doc => lhs(doc) == rhs(doc)

    override def and(lhs: Doc => Boolean, rhs: Doc => Boolean): Doc => Boolean =
      doc => lhs(doc) && rhs(doc)
  }

  val compiled = rule[Doc => *](interpreter)

  println(
    compiled(
      Map(
        "categoryId" -> 10,
        "price" -> 3
      )
    )
  )
/*
  type JsonRule[A] = Json

  val jsonInterpreter = new RuleSym[JsonRule] {
    override def num(value: Int): JsonRule[Int] = Json.Num(value)

    override def variable(name: String): JsonRule[Int] = Json.Str(name)

    override def equal(
        lhs: JsonRule[Int],
        rhs: JsonRule[Int]
    ): JsonRule[Boolean] =
      Json.Obj("EQUALS" -> Json.Arr(lhs, rhs))

    override def and(
        lhs: JsonRule[Boolean],
        rhs: JsonRule[Boolean]
    ): JsonRule[Boolean] =
      Json.Obj("AND" -> Json.Arr(lhs, rhs))
  }

  println(rule[JsonRule](jsonInterpreter))

  implicit def encoder[A]: Encoder[Rule[JsonRule, A]] =
    (r: Rule[JsonRule, A]) => r(jsonInterpreter)

  implicit def ruleIntDecoder[F[_]]: Decoder[Rule[F, Int]] = {
    case Json.Num(value) =>
      Right((rs: RuleSym[F]) => rs.num(value))
    case Json.Str(name) =>
      Right((rs: RuleSym[F]) => rs.variable(name))
    case other =>
      Left(s"Invalid number: $other")
  }

  implicit def ruleBoolDecoder[F[_]]: Decoder[Rule[F, Boolean]] = {
    case Json.Obj("EQUALS" -> Json.Arr(lhs, rhs)) =>
      for {
        left <- lhs.as[Rule[F, Int]]
        right <- rhs.as[Rule[F, Int]]
      } yield (rs: RuleSym[F]) => rs.equal(left(rs), right(rs))
    case Json.Obj("AND" -> Json.Arr(lhs, rhs)) =>
      for {
        left <- lhs.as[Rule[F, Boolean]](ruleBoolDecoder[F])
        right <- rhs.as[Rule[F, Boolean]](ruleBoolDecoder[F])
      } yield (rs: RuleSym[F]) => rs.and(left(rs), right(rs))
    case other =>
      Left(s"Invalid boolean: $other")
  }

  val result = rule[JsonRule].asJson
    .as[Rule[Pretty, Boolean]]
    .map(r => r(prettyInterpreter))

  println("*" * 80)
  println(result)
*/
  def readerReadsMissingKeys[A](
      cursor: ConfigObjectCursor,
      key: String,
      reader: ConfigReader[A]
  ): Result[A] = {
    reader match {
      case r: ReadsMissingKeys => r.from(cursor.atKeyOrUndefined(key))
      case r                   => cursor.atKey(key).flatMap(r.from)
    }
  }

  def readerPartial1[B, A](
      cursor: ConfigObjectCursor,
      f: PartialFunction[A, B]
  )(a: A): Result[B] =
    Try(f(a)) match {
      case Failure(exception) => cursor.failed[B](error.ExceptionThrown(exception))
      case Success(value) => Right(value)
    }

  def readerForObj1Partial[B, A](key: String)(
      f: PartialFunction[A, B]
  )(implicit reader: ConfigReader[A]): ConfigReader[B] =
    (cur: ConfigCursor) =>
      cur.asObjectCursor.flatMap { cursor =>
        readerReadsMissingKeys(cursor, key, reader).flatMap(
          readerPartial1(cursor, f)
        )
      }

  implicit def ruleIntConfigReader[F[_]]: ConfigReader[Rule[F, Int]] = {
    val numConfigReader: ConfigReader[Rule[F, Int]] =
      ConfigReader.forProduct1[Rule[F, Int], Int]("num")(value =>
        (rs: RuleSym[F]) => rs.num(value)
      )

    val variableConfigReader: ConfigReader[Rule[F, Int]] =
      ConfigReader.forProduct1[Rule[F, Int], String]("variable")(value =>
        (rs: RuleSym[F]) => rs.variable(value)
      )

    numConfigReader orElse variableConfigReader
  }

  def equalsDecode[F[_]]: ConfigReader[Rule[F, Boolean]] =
    readerForObj1Partial[Rule[F, Boolean], List[Rule[F, Int]]]("equal") {
      case left :: right :: Nil =>
        (rs: RuleSym[F]) => rs.equal(left(rs), right(rs))
    }

  def andDecode[F[_]]: ConfigReader[Rule[F, Boolean]] =
    readerForObj1Partial[Rule[F, Boolean], List[Rule[F, Boolean]]]("and") {
      case left :: tail =>
        (rs: RuleSym[F]) =>
          tail.foldLeft(left(rs)){
            case (acc, right ) => rs.and(acc, right(rs))
          }
    }

  implicit def ruleBoolConfDecoder[F[_]]: ConfigReader[Rule[F, Boolean]] =
    equalsDecode[F] orElse andDecode[F]

  type RuleConf[A] = ConfigValue

  final def writerForObj1[A](
      key: String
  )(value: A)(implicit writer: ConfigWriter[A]): ConfigValue = {
    val emptyConf = ConfigFactory.empty()
    writer match {
      case w: WritesMissingKeys[A @unchecked] =>
        w.toOpt(value) match {
          case Some(v) => emptyConf.withValue(key, v)
          case None    => emptyConf
        }
      case w => emptyConf.withValue(key, w.to(value))
    }
  }.root()

  val confWriterInterpreter = new RuleSym[RuleConf] {
    override def num(value: Int): RuleConf[Int] =
      writerForObj1("num")(value)

    override def variable(name: String): RuleConf[Int] =
      writerForObj1("variable")(name)

    override def equal(
        lhs: RuleConf[Int],
        rhs: RuleConf[Int]
    ): RuleConf[Boolean] =
      writerForObj1("equal")(List(lhs, rhs))

    override def and(
        lhs: RuleConf[Boolean],
        rhs: RuleConf[Boolean]
    ): RuleConf[Boolean] =
      writerForObj1("and")(List(lhs, rhs))
  }

  val confResult: ConfigValue = rule[RuleConf](confWriterInterpreter)
  println(confResult)
  def readConfig[F[_]](
      configValue: ConfigValue
  )(ruleSym: RuleSym[F]): Result[F[Boolean]] =
    ConfigReader[Rule[F, Boolean]]
      .from(configValue)
      .map(_(ruleSym))

  val readResult = readConfig(confResult)(prettyInterpreter)
  println(readResult)

  val configStr =
    """
      |{
      |    "and" = [
      |{
      |            "equal" = [
      |                { "variable" = "categoryId"},
      |                { "num" = 12 }
      |            ]
      |        },
      |        {
      |            "equal" = [
      |                { "variable" = "price" },
      |                { "num" =  3 },
      |            ]
      |        },
      |        {
      |            "equal" = [
      |                { "variable" = "price" },
      |                { "num" =  5 },
      |            ]
      |        }
      |    ]
      |}
      |""".stripMargin

  println(
    ConfigSource
      .string(configStr)
      .load[Rule[Pretty, Boolean]]
      .map(r => r(prettyInterpreter))
  )
  val config: Config = ConfigFactory.load()

  def loadConfig[F[_]](
      ruleSym: RuleSym[F]
  )(config: Config): Result[F[Boolean]] =
    ConfigSource
      .fromConfig(config)
      .load[Rule[F, Boolean]]
      .map(_(ruleSym))

  println(loadConfig(prettyInterpreter)(config))

}
