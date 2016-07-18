# purescript-quantities

--- *WORK IN PROGRESS* ---

A PureScript library to represent and work with physical quantities and units. Note that this library
focuses on a representation at *run time* as opposed to other projects which use the type system to
encode physical units at *compile time*.

## Example

``` purs
> 2.0 .* minutes ⊕ 30.0 .* seconds
(Right 2.5min)

> (85.0 .* miles ./ hour) `asValueIn` (meters ./ second)
(Right 37.9984)

> showError $ (10.0 .* miles) `asValueIn` (grams .^ 2.0)
Cannot unify unit 'mi' with unit 'g²'
```
