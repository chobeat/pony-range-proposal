use "debug"

trait val Bound[T: (Real[T] val & Number) = USize] is (Equatable[Bound[T]] & Stringable)
  fun get_value():T
  fun get_upper(is_forward:Bool):T
  fun get_lower(is_forward:Bool):T
  fun eq(that:Bound[T]):Bool=>
    // TODO: find a better way to compare to another Bound
    (this.get_value()==that.get_value()) and 
    (this.get_upper(true)==that.get_upper(true)) and 
    (this.get_lower(true)==that.get_lower(true)) and 
    (this.get_upper(false)==that.get_upper(false)) and 
    (this.get_lower(false)==that.get_lower(false)) 

class val Inclusive[T: (Real[T] val & Number) = USize] is Bound[T]
  let v: T
  new val create(value: T) =>
    v = consume value

  fun get_value():T=>
    v
  fun get_upper(is_forward:Bool):T=>
    v

  fun get_lower(is_forward:Bool):T=>
    v

  fun string(): String iso^ =>
    ("Inclusive("+v.string()+")").string()

class val Exclusive[T: (Real[T] val & Number) = USize] is Bound[T]
  let v: T
  new val create(value: T) =>
    v = consume value
  fun get_value():T=>
    v

  fun get_upper(is_forward:Bool):T=>
    if is_forward
    then
      v-1
    else
      v+1
    end
  fun get_lower(is_forward:Bool):T=>
    if is_forward
    then
      v+1
    else
      v-1
    end

  fun string(): String iso^ =>
    ("Exclusive("+v.string()+")").string()

class Range[T: (Real[T] val & Number) = USize]
  let _begin: Bound[T]
  let _end: Bound[T]
  let _step: T
  let _is_forward:Bool
  var _current:T
  let _is_invalid:Bool

  new create(b: Bound[T], e: Bound[T], step: T=1) =>
    _begin= b
    _end = e

    _is_forward = _begin.get_value() < _end.get_value()

    _is_invalid = (step < 1)
    _step = if _is_forward then
               step 
            else 
              - step
            end

    _current = _begin.get_lower(_is_forward)

  new incl(b: T, e: T, step: T=1) =>
    // TODO: define this constructor as a function of the main once possible
    _begin= Inclusive[T](b)
    _end = Inclusive[T](e)
    _is_forward = _begin.get_value() < _end.get_value()

    _is_invalid = (step < 1)
    _step = if _is_forward then
               step 
            else 
              - step
            end

    _current = _begin.get_lower(_is_forward)

 fun is_forward():Bool=>
  _is_forward

 fun is_invalid():Bool=>
  _is_invalid

  fun has_next(): Bool => 
    if _is_invalid then
      false

    elseif _is_forward
      then
        _current <= _end.get_upper(_is_forward) 
    else
        _current <= _begin.get_lower(_is_forward)
    end

  fun ref next(): T =>
    if has_next() then
      let result = _current
      _current = _current + _step
      result
    else
      _current
    end

  fun get_increment_step():T=>
    _step

  fun get_end():Bound[T]=>
    _end

  fun get_begin():Bound[T]=>
    _begin
