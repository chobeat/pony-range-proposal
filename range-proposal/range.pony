interface val Bound[T: (Real[T] val & Number) = USize]
  fun get_value():T    
  fun get_upper():T
  fun get_lower():T


class val Inclusive[T: (Real[T] val & Number) = USize] is Bound[T]
  let v: T
  new val create(value: T) =>
    v = consume value
  fun get_value():T=>
    v

  fun get_upper():T=>
    v

  fun get_lower():T=>
    v
class val Exclusive[T: (Real[T] val & Number) = USize] is Bound[T]
  let v: T
  new val create(value: T) =>
    v = consume value
  fun get_value():T=>
    v

  fun get_upper():T=>
    v-1

  fun get_lower():T=>
    v+1

class Range[T: (Real[T] val & Number) = USize]
  let _lower_bound: Bound[T]
  let _upper_bound: Bound[T]
  let _step: T
  var _current:T
  
  new create(lower: Bound[T], upper: Bound[T], step: T) =>
    _lower_bound = lower
    _upper_bound = upper
    _step = consume step
    _current = _lower_bound.get_lower()
    
  new inclusive(lower: T, upper: T, step: T) =>
    _lower_bound = Inclusive[T](consume lower)
    _upper_bound = Inclusive[T](consume upper)
    _step = consume step
    _current = _lower_bound.get_lower()

  fun has_next(): Bool => _current < _upper_bound.get_upper()

  fun ref next(): T =>
    if has_next() then
      _current = _current + _step
      _current
    else
      _current
    end

  fun get_increment_step():T=>
    _step


  fun get_upper_bound():Bound[T]=>
    _upper_bound
    

  fun get_lower_bound():Bound[T]=>
    _lower_bound
