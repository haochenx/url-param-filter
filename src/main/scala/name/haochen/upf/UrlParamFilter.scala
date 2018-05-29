package name.haochen.upf

object UrlParamFilter {
  def simplify(f: Filter): Filter = f match {
    case ConjFilter(List(f1)) => simplify(f1)
    case DisjFilter(List(f1)) => simplify(f1)
    case _ => f
  }

  sealed trait Predicate {
    val param: String
    def pred(v: String): Boolean
  }

  sealed trait Filter {
    def test(params: Map[String, String]): Boolean
  }
  object TrueFilter extends Filter {
    override def test(params: Map[String, String]): Boolean = true
  }
  object FalseFilter extends Filter {
    override def test(params: Map[String, String]): Boolean = false
  }
  case class AtomFilter(pred: Predicate) extends Filter {
    override def test(params: Map[String, String]): Boolean = params.contains(pred.param) && pred.pred(params(pred.param))
  }
  case class ConjFilter(filters: List[Filter]) extends Filter {
    override def test(params: Map[String, String]): Boolean = filters.forall(f=>f.test(params))
  }
  case class DisjFilter(filters: List[Filter]) extends Filter {
    override def test(params: Map[String, String]): Boolean = filters.exists(f=>f.test(params))
  }
  case class NegFilter(filter: Filter) extends Filter {
    override def test(params: Map[String, String]): Boolean = !filter.test(params)
  }

  case class GeneralPredicate(param: String, pred: String=>Boolean) extends Predicate {
    override def pred(v: String) = pred.apply(v)
  }
  case class StrPredicate(param: String, pred: String=>Boolean) extends Predicate {
    override def pred(v: String): Boolean = pred.apply(v)
  }
  case class IntPredicate(param: String, pred: Int=>Boolean) extends Predicate {
    override def pred(v: String): Boolean = try {
      pred.apply(v.toInt)
    } catch {
      case _: NumberFormatException => false
    }
  }
}
