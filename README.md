# purescript-quantities

A PureScript library to represent and work with physical quantities and units. Note that this library
focuses on a representation at *run time* as opposed to other projects which use the type system to
encode physical units at *compile time*.

## Examples

``` purs
> showResult $ 2.0 .* minute ⊕ 30.0 .* second
"2.5 min"

> showResult $ (85.0 .* mile ./ hour) `convertTo` (meter ./ second)
"37.9984 m/s"

> showResult $ (10.0 .* meter ./ second) `convertTo` (kilo meter ./ hour)
"36 km/h"

> log $ showResult $ (10.0 .* joule) `convertTo` watt
Cannot convert unit 'J' (SI: 'm²·g/s²')
            to unit 'W' (SI: 'm²·g/s³')

> showResult $ sin (90.0 .* degree)
"1"
```

Calculate the time it takes to download a *2.7GB* file on a *6Mbit/s* connection:
``` purs
> let filesize = 2.7 .* giga byte
> let speed = 6.0 .* mega bit ./ second
> showResult $ (filesize ⊘ speed) `convertTo` minute
"60 min"
```

Calculate the oscillation period *T = 2π sqrt(L/g)* of a pendulum with length *L = 20cm*:
``` purs
> let g = 9.81 .* meter ./ second .^ 2.0
> let length = 20.0 .* centi meter
> let period = scalar 2.0 ⊗ pi ⊗ sqrt (length ⊘ g)

> prettyPrint (fullSimplify period)
"0.89714 s"
```

## Development
```
npm install decimal.js
spago build
spago test
```

## Documentation

- [Module documentation](https://pursuit.purescript.org/packages/purescript-quantities)
