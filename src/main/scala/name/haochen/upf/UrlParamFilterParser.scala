package name.haochen.upf

import fastparse.WhitespaceApi
import fastparse.core.Parsed.{Failure, Success}
import name.haochen.upf.UrlParamFilter.{AtomFilter, Filter}

object UrlParamFilterParser {
  def parse(str: String): Option[Filter] = Parse.start.parse(str) match {
    case Success(f, _) => Some(UrlParamFilter.simplify(f))
    case failure @ Failure(_, _, _) => None
  }

  import fastparse.noApi._

  def CharNotIn(chars: Seq[Char]*) = CharPred(c => chars.forall(_.forall(_ != c)))

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    val base = CharPred(c => c.isWhitespace)
    NoTrace(base.rep)
  }

  class ParserOps[A](pa: P[A]) {
    import White._

    import Function.const
    def just[R](x: R): P[R] = pa.map(const(x))
  }

  implicit def toParserOps[A, B](a: A)(implicit ev: A => P[B]) = new ParserOps(ev(a))

  object Lex {
    import fastparse.all._

    val numericChar: P[Unit] = P(CharIn('0' to '9'))
    val numericChars: P[Unit] = P(numericChar.rep(1))
    // ref: https://so-zou.jp/software/tech/programming/tech/regular-expression/meta-character/variable-width-encoding.htm
    val japaneseChar: P[Unit] = CharIn('\u3041' to '\u3096', '\u30A1' to '\u30FA', "々〇〻",
      '\u3400' to '\u9FFF', '\uF900' to '\uFAFF', '\uD840' to '\uD87F', '\uDC00' to '\uDFFF', "ー")
    val basicPrefixedIdentChar: P[Unit] = CharIn('0' to '9', 'a' to 'z', 'A' to 'Z', "_-")

    def quotedString(delim: String): P[String] = P(delim ~
      (CharNotIn(delim, "\\").! |
        "\\\\".just("\\") |
        ("\\" + delim).just(delim)).rep.map(_.mkString) ~ delim)
    def quotableIdentLiteral(base: P[String]): P[String] = P(quotedString("|") | base).filter(_.nonEmpty)

    val param: P[String] = quotableIdentLiteral(P((basicPrefixedIdentChar | japaneseChar).rep.!)).opaque("<param>")
    val int: P[Int] = P(("-".? ~ numericChars).!.map(r => r.toInt)).opaque("<int>")
  }

  object Parse {
    import Lex._
    import White._
    import UrlParamFilter._

    val intPredOp: P[Int=>Int=>Boolean] = P(
      "=".just((x:Int)=>(_:Int)==x) |
        "<=".just((x:Int)=>(_:Int)<=x) |
        ">=".just((x:Int)=>(_:Int)>=x) |
        "<".just((x:Int)=>(_:Int)<x) |
        ">".just((x:Int)=>(_:Int)>x)
    )
    val intPred: P[Predicate] = P(param ~ intPredOp ~ int).map { case (p, op, v) => IntPredicate(p, op(v)) }

    val strPredOp: P[String=>String=>Boolean] = P(
      "=".just((x:String)=>(_:String)==x) |
        // %= means 'begins with'
        "%=".just((x:String)=>(_:String).startsWith(x)) |
        // ~= means 'satisfies regex'
        "~=".just((x:String)=>(_:String).matches(x))
    )
    val strPred: P[Predicate] = P(param ~ strPredOp ~ quotedString("\"")).map { case (p, op, v) => StrPredicate(p, op(v)) }

    val typePred: P[Predicate] = P(
      (param ~ "^exists").map(ParamExistsPredicate) |
        (param ~ "^positive").map(PositiveFlagPredicate) |
        (param ~ "^int").map(IntPredicate(_, _=> true)))

    val pred: P[Predicate] = P(typePred | intPred | strPred)

    lazy val filter: P[Filter] = filterDisj
    val filterTrivial: P[Filter] = P("@true".just(TrueFilter) | "@false".just(FalseFilter))
    val filterTerm: P[Filter] = P(("(" ~ filter ~ ")") | filterTrivial | ("!" ~ filter).map(NegFilter) | pred.map(AtomFilter))
    val filterConj: P[Filter] = P(filterTerm ~ ("and" ~ filterTerm).rep).map { case (t0, ts) => ConjFilter(t0 :: ts.toList) }
    val filterDisj: P[Filter] = P(filterConj ~ ("or" ~ filterConj).rep).map { case (t0, ts) => DisjFilter(t0 :: ts.toList) }

    val start: P[Filter] = P(Start ~ filter ~ End)
  }

}
