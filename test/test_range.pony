use "ponytest"
use c="collections"
use "itertools"
use "../range-proposal"

trait RangeTest

  fun assert_range_eq[S: (Real[S] val & Number) =U8](expected:Array[S], range:Range[S], h:TestHelper)?=>
    var i:USize = 0
      
    while(range.has_next())
        do
          h.assert_eq[S](expected(i)?,range.next())
        
        i=i+1
      end

class RangeTests is UnitTest

  fun name(): String => "range tests"
  fun apply(h: TestHelper)? =>
    RangeInit[U8](h)
    RangeInclSimple[U8](h)?
    RangeInclStep3[U8](h)?
    RangeExclSimple[U8](h)?
    RangeExclStep3[U8](h)?
    RangeInclSimpleBackward[U8](h)?
    RangeExclSimpleBackward[U8](h)?

    RangeInit[F32](h)
    RangeInclSimple[F32](h)?
    RangeInclStep3[F32](h)?
    RangeExclSimple[F32](h)?
    RangeExclStep3[F32](h)?
    RangeInclSimpleBackward[F32](h)?
    RangeExclSimpleBackward[F32](h)?


    RangeInit[I32](h)
    RangeInclSimple[I32](h)?
    RangeInclStep3[I32](h)?
    RangeExclSimple[I32](h)?
    RangeExclStep3[I32](h)?
    RangeInclSimpleBackward[I32](h)?
    RangeExclSimpleBackward[I32](h)?

primitive RangeInit[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper) =>
      let lower_bound = Inclusive[T](1)
      let upper_bound = Inclusive[T](5)
      let r:Range[T] = Range[T](lower_bound, upper_bound , 5)
      h.assert_eq[T](r.get_increment_step(), 5)
      h.assert_is[Bound[T]](r.get_upper_bound(), upper_bound)
      h.assert_is[Bound[T]](r.get_lower_bound(), lower_bound)



primitive RangeInclSimple[T: (Real[T] val & Number) ] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](Inclusive[T](0), Inclusive[T](5) , 1)
      let expected:Array[T] = [0;1;2;3;4;5]
      assert_range_eq[T](expected,r,h)?



primitive RangeInclStep3[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[U8] = Range[U8](Inclusive[U8](0), Inclusive[U8](9) , 3)
      let expected:Array[U8] = [0;3;6;9]
      assert_range_eq(expected,r,h)?


primitive RangeExclSimple[T: (Real[T] val & Number)] is RangeTest

    fun apply(h: TestHelper)? =>
      let r:Range[U8] = Range[U8](Exclusive[U8](0), Exclusive[U8](5) , 1)
      let expected:Array[U8] = [1;2;3;4]
      assert_range_eq(expected,r,h)?



primitive RangeExclStep3[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[U8] = Range[U8](Exclusive[U8](0), Exclusive[U8](10) , 3)
      let expected:Array[U8] = [1;4;7]
      assert_range_eq(expected,r,h)?


primitive RangeInclSimpleBackward[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[U8] = Range[U8](Inclusive[U8](5), Inclusive[U8](0), 1)
      let expected:Array[U8] = [5;4;3;2;1]
      assert_range_eq(expected,r,h)?

primitive RangeExclSimpleBackward[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[U8] = Range[U8](Exclusive[U8](5), Exclusive[U8](0), 1)
      let expected:Array[U8] = [4;3;2]
      assert_range_eq(expected,r,h)?

