use "ponytest"
use c="collections"
use "itertools"
use "../range-proposal"
class RangeInit is UnitTest
  fun name(): String => "Range init"
    fun apply(h: TestHelper) =>
      let lower_bound = Inclusive[U8](1)
      let upper_bound = Inclusive[U8](5)
      let r:Range[U8] = Range[U8](lower_bound, upper_bound , 5)
      h.assert_eq[U8](r.get_increment_step(), 5)
      h.assert_is[Bound[U8]](r.get_upper_bound(), upper_bound)
      h.assert_is[Bound[U8]](r.get_lower_bound(), lower_bound)



class RangeInclSimple is UnitTest
  fun name(): String => "Range inclusive simple"
    fun apply(h: TestHelper) =>
      let lower_bound = Inclusive[U8](0)
      let upper_bound = Inclusive[U8](5)
      let r:Range[U8] = Range[U8](lower_bound, upper_bound , 1)
      let pairs = Iter[U8](r).zip[U8]([1;2;3;4;5].values())
      for pair in pairs do 
        (let a, let b) = pair
        h.assert_eq[U8](a,b)
      end