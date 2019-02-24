use "ponytest"
use c="collections"
use "itertools"
use "../range-proposal"

trait RangeTest is UnitTest

  fun assert_range_eq(expected:Array[U8], range:Range[U8], h:TestHelper)?=>
    var i:USize = 0
      
    while(range.has_next())
        do
          h.assert_eq[U8](expected(i)?,range.next())
        
        i=i+1
      end


class RangeInit is RangeTest
  fun name(): String => "Range init"
    fun apply(h: TestHelper) =>
      let lower_bound = Inclusive[U8](1)
      let upper_bound = Inclusive[U8](5)
      let r:Range[U8] = Range[U8](lower_bound, upper_bound , 5)
      h.assert_eq[U8](r.get_increment_step(), 5)
      h.assert_is[Bound[U8]](r.get_upper_bound(), upper_bound)
      h.assert_is[Bound[U8]](r.get_lower_bound(), lower_bound)



class RangeInclSimple is RangeTest
  fun name(): String => "Range inclusive simple"
    fun apply(h: TestHelper)? =>
      let r:Range[U8] = Range[U8](Inclusive[U8](0), Inclusive[U8](5) , 1)
      let expected:Array[U8] = [1;2;3;4;5]
      assert_range_eq(expected,r,h)?

class RangeExclSimple is RangeTest
  fun name(): String => "Range exclusive simple"
    fun apply(h: TestHelper)? =>
      let r:Range[U8] = Range[U8](Exclusive[U8](0), Exclusive[U8](5) , 1)
      let expected:Array[U8] = [2;3;4]
      assert_range_eq(expected,r,h)?

