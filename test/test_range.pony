use "ponytest"
use c="collections"
use "itertools"
use "../range-proposal"

trait RangeTest



  fun assert_range_eq[S: (Real[S] val & Number) =U8](expected:Array[S], range:Range[S], h:TestHelper)?=>
    var i:USize = 0
    if expected.size()==0
      then
          h.assert_false(range.has_next())
      else
        repeat 
          let v=range.next()
          h.assert_eq[S](expected(i)?,v)
          i=i+1

        until range.has_next() end
      end

class RangeTests is UnitTest

  fun name(): String => "range tests"
  fun apply(h: TestHelper)? =>
    RangeInit[U8](h)
    RangeIncl[U8](h)
    RangeInclSimple[U8](h)?
    RangeInclStep3[U8](h)?
    RangeExclSimple[U8](h)?
    RangeExclStep3[U8](h)?
    RangeInclSimpleBackward[U8](h)?
    RangeExclSimpleBackward[U8](h)?

    RangeInit[F32](h)
    RangeIncl[F32](h)
    RangeInclSimple[F32](h)?
    RangeInclStep3[F32](h)?
    RangeExclSimple[F32](h)?
    RangeExclStep3[F32](h)?
    RangeInclSimpleBackward[F32](h)?
    RangeExclSimpleBackward[F32](h)?


    RangeInit[I32](h)
    RangeIncl[I32](h)
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
      h.assert_is[Bound[T]](r.get_begin(), lower_bound)
      h.assert_is[Bound[T]](r.get_end(), upper_bound)

primitive RangeIncl[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper) =>
      let r:Range[T] = Range[T].incl(1, 5 , 5)
      h.assert_eq[T](r.get_increment_step(), 5)
      h.assert_eq[Bound[T]](r.get_begin(), Inclusive[T](1))
      h.assert_eq[Bound[T]](r.get_end(), Inclusive[T](5))


primitive RangeInclSimple[T: (Real[T] val & Number) ] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](Inclusive[T](0), Inclusive[T](5) , 1)
      let expected:Array[T] = [0;1;2;3;4;5]
      assert_range_eq[T](expected,r,h)?

      let r2:Range[T] = Range[T].incl(0,5,1)
      assert_range_eq[T](expected,r2,h)?




primitive RangeInclStep3[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](Inclusive[T](0), Inclusive[T](9) , 3)
      let expected:Array[T] = [0;3;6;9]
      assert_range_eq[T](expected,r,h)?

      let r2:Range[T] = Range[T].incl(0,9,3)
      assert_range_eq[T](expected,r2,h)?



primitive RangeExclSimple[T: (Real[T] val & Number)] is RangeTest

    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](Exclusive[T](0), Exclusive[T](5) , 1)
      let expected:Array[T] = [1;2;3;4]
      assert_range_eq[T](expected,r,h)?



primitive RangeExclStep3[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](Exclusive[T](0), Exclusive[T](10) , 3)
      let expected:Array[T] = [1;4;7]
      assert_range_eq[T](expected,r,h)?


primitive RangeInclSimpleBackward[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](Inclusive[T](5), Inclusive[T](0), 1)
      let expected:Array[T] = [5;4;3;2;1]
      h.assert_false(r.is_forward())
      assert_range_eq[T](expected,r,h)?

primitive RangeExclSimpleBackward[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](Exclusive[T](5), Exclusive[T](0), 1)
      let expected:Array[T] = [4;3;2]

      h.assert_false(r.is_forward())
      assert_range_eq[T](expected,r,h)?



class RangeOverflowTests is UnitTest

  fun name(): String => "overflow tests"
  fun apply(h: TestHelper)? =>
    RangeExclBackwardMaxValue[U8](h)?
    RangeExclBackwardMaxValue[U16](h)?
    RangeExclBackwardMaxValue[U32](h)?
    RangeExclBackwardMaxValue[U64](h)?
    RangeExclBackwardMaxValue[I64](h)?
    RangeExclBackwardMaxValue[F64](h)?


    RangeExclForwardMinValue[U8](h)?
    RangeExclForwardMinValue[U16](h)?
    RangeExclForwardMinValue[U32](h)?
    RangeExclForwardMinValue[U64](h)?
    RangeExclForwardMinValue[I64](h)?
    RangeExclForwardMinValue[F64](h)?


primitive RangeExclBackwardMaxValue[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](
        Exclusive[T](T.max_value()),
         Exclusive[T](T.max_value()-4), 
         1)
      let expected:Array[T] = [
        T.max_value()-1
        T.max_value()-2
        T.max_value()-3
        ]
      
      assert_range_eq[T](expected,r,h)?


primitive RangeExclForwardMinValue[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](
        Exclusive[T](T.min_value()),
         Exclusive[T](T.min_value()+4), 
         1)
      let expected:Array[T] = [
        T.min_value()+1
        T.min_value()+2
        T.min_value()+3
        ]
      
      assert_range_eq[T](expected,r,h)?




class RangeErrorTests is UnitTest

  fun name(): String => "error tests, they should always return empty list"
  fun apply(h: TestHelper)? =>
    RangeErrorStepBack[I8](h)?
    RangeErrorStepFor[I8](h)?



primitive RangeErrorStepBack[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](
        Exclusive[T](5),
         Exclusive[T](0), 
         -1)
      let expected:Array[T] = []
      assert_range_eq[T](expected,r,h)?


primitive RangeErrorStepFor[T: (Real[T] val & Number)] is RangeTest
    fun apply(h: TestHelper)? =>
      let r:Range[T] = Range[T](
        Exclusive[T](0),
         Exclusive[T](5), 
         -1)
      let expected:Array[T] = []
      
      assert_range_eq[T](expected,r,h)?