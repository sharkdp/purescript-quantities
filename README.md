# purescript-quantities

A PureScript library to represent and work with physical quantities and units. Note that this library
focuses on a representation at *run time* as opposed to other projects which use the type system to
encode physical units at *compile time*.

## Examples

``` purs
> showResult $ 2.0 .* minutes ⊕ 30.0 .* seconds
"2.5min"

> showResult $ (85.0 .* miles ./ hour) `convertTo` (meters ./ second)
"37.9984m/s"

> showResult $ (10.0 .* meters ./ second) `convertTo` (kilo meters ./ hour)
"36.0km/h"

> showResult $ (10.0 .* miles) `convertTo` (grams .^ 2.0)
"Cannot unify unit 'mi' with unit 'g²'"

> showResult $ sin (90.0 .* degree)
"1.0"
```

Calculate the time it takes to download a *2.7GB* file on a *6Mbit/s* connection:
``` purs
> let filesize = 2.7 .* giga byte
> let speed = 6.0 .* mega bit ./ second
> showResult $ (filesize ⊘ speed) `convertTo` hours
"1.0h"
```

Calculate the oscillation period *T = 2π sqrt(L/g)* of a pendulum with length *L = 20cm*:
``` purs
> let g = 9.81 .* meters ./ second .^ 2.0
> let length = 20.0 .* centi meter
> let period = scalar (2.0 * pi) ⊗ sqrt (length ⊘ g)

> showResult $ period `convertTo` milli seconds
"897.1402930932747ms"
```

## Documentation

- [Module documentation](https://pursuit.purescript.org/packages/purescript-quantities)
