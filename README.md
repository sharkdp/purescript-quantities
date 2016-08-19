# purescript-quantities

--- *WORK IN PROGRESS* ---

A PureScript library to represent and work with physical quantities and units. Note that this library
focuses on a representation at *run time* as opposed to other projects which use the type system to
encode physical units at *compile time*.

## Examples

``` purs
> 2.0 .* minutes ⊕ 30.0 .* seconds
(Right 2.5min)

> (85.0 .* miles ./ hour) `asValueIn` (meters ./ second)
(Right 37.9984)

> (10.0 .* meters ./ second) `asValueIn` (kilo meters ./ hour)
(Right 36.0)

> showError $ (10.0 .* miles) `asValueIn` (grams .^ 2.0)
Cannot unify unit 'mi' with unit 'g²'
```

Calculate the oscillation period *T = 2π sqrt(L/g)* of a pendulum with length *L = 20cm*:
``` purs
> let g = 9.81 .* meters ./ second .^ 2.0
> let length = 20.0 .* centi meter
> let period = scalar (2.0 * pi) ⊗ sqrt (length ⊘ g)

> period `asValueIn` milli seconds
(Right 897.1402930932749)
```
