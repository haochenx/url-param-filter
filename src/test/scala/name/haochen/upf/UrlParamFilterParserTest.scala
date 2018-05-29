package name.haochen.upf

import org.scalatest.FunSuite

class UrlParamFilterParserTest extends FunSuite {
  import UrlParamFilterParser.parse

  test("testParse") {
    {
      val Some(f) = parse(""" param1="hello" """)
      assert(f.test(Map("param1"->"hello")))
      assert(!f.test(Map("param1"->"hell")))
      assert(!f.test(Map()))
    }

    {
      val Some(f) = parse(""" param1="hello" and param2="wow" """)
      assert(f.test(Map("param1"->"hello", "param2"->"wow")))
      assert(!f.test(Map("param1"->"hello", "param2"->"wowo")))
      assert(!f.test(Map("param1"->"hell", "param2"->"wow")))
      assert(!f.test(Map("param1"->"hell", "param2"->"wowo")))
      assert(!f.test(Map("param1"->"hello")))
      assert(!f.test(Map("param2"->"wow")))
    }
    {
      val Some(f) = parse(""" param1="hello" or param2="wow" """)
      assert(f.test(Map("param1"->"hello", "param2"->"wow")))
      assert(f.test(Map("param1"->"hello", "param2"->"wowo")))
      assert(f.test(Map("param1"->"hell", "param2"->"wow")))
      assert(!f.test(Map("param1"->"hell", "param2"->"wowo")))
      assert(f.test(Map("param1"->"hello")))
      assert(f.test(Map("param2"->"wow")))
    }

    {
      val Some(f) = parse(""" param1 %= "hello" """)
      assert(f.test(Map("param1"->"hello")))
      assert(f.test(Map("param1"->"hello213")))
      assert(f.test(Map("param1"->"hellowow")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param1"->"halo123")))
    }
    {
      val Some(f) = parse(""" param1 ~= "h.+o" """)
      assert(f.test(Map("param1"->"hello")))
      assert(f.test(Map("param1"->"h32o")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param1"->"halo123")))
      assert(!f.test(Map("param1"->"ho")))
    }
    {
      val Some(f) = parse(""" param1 = 10 """)
      assert(f.test(Map("param1"->"10")))
      assert(!f.test(Map("param1"->"20")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param1"->"halo123")))
      assert(!f.test(Map("param1"->"ho")))
    }
    {
      val Some(f) = parse(""" param1 > 10 """)
      assert(f.test(Map("param1"->"11")))
      assert(f.test(Map("param1"->"20")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param1"->"halo123")))
      assert(!f.test(Map("param1"->"ho")))
    }
    {
      val Some(_) = parse(""" param1 < 10 """)
      val Some(_) = parse(""" param1 >= 10 """)
      val Some(_) = parse(""" param1 <= 10 """)
    }

    {
      val Some(f) = parse(""" ! param1 = 10 """)
      assert(!f.test(Map("param1"->"10")))
      assert(f.test(Map("param1"->"20")))
      assert(f.test(Map()))
      assert(f.test(Map("param1"->"halo123")))
      assert(f.test(Map("param1"->"ho")))
    }
    {
      val Some(f) = parse(""" ! (param1 = 10) """)
      assert(!f.test(Map("param1"->"10")))
      assert(f.test(Map("param1"->"20")))
      assert(f.test(Map()))
      assert(f.test(Map("param1"->"halo123")))
      assert(f.test(Map("param1"->"ho")))
    }

    {
      val Some(f) = parse(""" @true """)
      assert(f.test(Map("param1"->"10")))
      assert(f.test(Map("param1"->"20")))
      assert(f.test(Map()))
      assert(f.test(Map("param1"->"halo123")))
      assert(f.test(Map("param1"->"ho")))
    }
    {
      val Some(f) = parse(""" @false """)
      assert(!f.test(Map("param1"->"10")))
      assert(!f.test(Map("param1"->"20")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param1"->"halo123")))
      assert(!f.test(Map("param1"->"ho")))
    }

    {
      val Some(f) = parse(""" param1 ^int """)
      assert(f.test(Map("param1"->"10")))
      assert(f.test(Map("param1"->"20")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param1"->"halo123")))
      assert(!f.test(Map("param1"->"ho")))
    }
    {
      val Some(f) = parse(""" param1 ^exists """)
      assert(f.test(Map("param1"->"10")))
      assert(f.test(Map("param1"->"halo123")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param2"->"ho")))
    }
    {
      val Some(f) = parse(""" param1 ^yes """)
      assert(f.test(Map("param1"->"true")))
      assert(f.test(Map("param1"->"yes")))
      assert(f.test(Map("param1"->"1")))
      assert(!f.test(Map()))
      assert(!f.test(Map("param2"->"no")))
      assert(!f.test(Map("param2"->"0")))
      assert(!f.test(Map("param2"->"meow")))
    }

  }

}
