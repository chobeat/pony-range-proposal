trait val Bound[T: (Real[T] val & Number) = USize] is (Equatable[Bound[T]] & Stringable)
  // value of the bound
  fun get_value():T
  // returns the actual number to use as an upper bound if according to the 
  // direction of the range
  fun get_upper(is_forward:Bool):T
  // returns the actual number to use as a lower bound if according to the 
  // direction of the range
  
  fun get_lower(is_forward:Bool):T


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

  fun eq(that:Bound[T]):Bool =>
    match that
    | let i:Inclusive[T]=> i.get_value()==this.get_value()
    else
      false
    end

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
  fun eq(that:Bound[T]):Bool =>
    match that
    | let i:Exclusive[T]=> i.get_value()==this.get_value()
    else
      false
    end
  fun string(): String iso^ =>
    ("Exclusive("+v.string()+")").string()


class Range[T: (Real[T] val & Number) = USize]
  let _begin: Bound[T]
  let _end: Bound[T]
  // stores the increment or decrement to be performed at every step.
  let _step: T
  // indicates if the range increases or decreases at every step.
  let _is_forward:Bool
  // stores the next value to return.
  var _next:T
  // represents the validity of the range.
  let _is_invalid:Bool
  // indicates if the current step is the last one.
  var _is_last:Bool

  new create(b:T,e:T,step:T)=>
    _begin= Inclusive[T](b)
    _end = Exclusive[T](e)
    _is_forward = _begin.get_value() < _end.get_value()
    // TODO: this condition doesn't always work with float arithmetics
    _is_invalid = (step < 1) or (_begin==_end)
    _is_last=false
    // move this to a dedicated function once possible. This operation is performed here as an optimization
    _step = if _is_forward then
               step 
            else 
              - step
            end

    _next = _begin.get_lower(_is_forward)


  new define(b: Bound[T], e: Bound[T], step: T=1) =>
    _begin= b
    _end = e
    _is_forward = _begin.get_value() < _end.get_value()
    // TODO: this condition doesn't always work with float arithmetics
    _is_invalid = (step < 1) or (_begin==_end)
    _is_last=false
    // move this to a dedicated function once possible. This operation is performed here as an optimization
    _step = if _is_forward then
               step 
            else 
              - step
            end

    _next = _begin.get_lower(_is_forward)


  new incl(b: T, e: T, step: T=1) =>
    // TODO: define this constructor as a function of the main once possible
    _begin= Inclusive[T](b)
    _end = Inclusive[T](e)
    _is_forward = _begin.get_value() < _end.get_value()
    _is_invalid = (step < 1) or (_begin==_end)
    _is_last=false
    _step = if _is_forward then
               step 
            else 
              - step
            end

    _next = _begin.get_lower(_is_forward)

 
 new to(e:T, step:T=1)=>
    // TODO: define this constructor as a function of the main once possible
    
    _begin=  Inclusive[T](0)
    _end = Inclusive[T](e)
    _is_forward = _begin.get_value() < _end.get_value()
    _is_last=false
    _is_invalid = (step < 1) or (_begin==_end)
    _step = if _is_forward then
               step 
            else 
              - step
            end

    _next = _begin.get_lower(_is_forward)
  

 fun is_forward():Bool=>
  _is_forward

 fun is_invalid():Bool=>
  _is_invalid

  fun has_next(): Bool => 
    
    not (_is_invalid or _is_last)
      

  fun _will_overflow(current:T):Bool =>
    if _is_forward
    then
      _next < current
    else
      _next > current
    end

  fun _bound_reached(current:T):Bool=>
    let bound = if _is_forward then _end.get_upper(_is_forward) else _end.get_lower(_is_forward) end

    current == bound

  fun _is_last_value(current:T):Bool=>
    // this function is defined in this way to avoid evaluating _will_overflow
    // unless it's strictly necessary. The falseness of _bound_reached implies
    // the falseness of _will_overflow that is necessary only if the bounds are
    // the max or min values of a data type
   if _bound_reached(current)
   then
    true
  else 
    _will_overflow(current)
  end
  fun ref next(): T =>
      let current = _next
      _is_last=_is_last_value(current)
      _next = _next + _step
      current


  fun get_increment_step():T=>
    _step

  fun get_end():Bound[T]=>
    _end

  fun get_begin():Bound[T]=>
    _begin
