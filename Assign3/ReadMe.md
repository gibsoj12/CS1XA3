# Assignment 3

## References:

* [Used deleeuwj1 function which checks for infinity](https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3/Exprs/ExprEval.hs)
* [Used chenc118 idea to force-fit everything to work for all integral types](https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs)

### Functionality:
-Able to parse expressions of the proceeding form
-Able to partially or completely evaluate expressions

### Parsing:

-To write an addition expression, seperate variables by `+` i.e `x+1`
-To write a multiplication expression, seperate variables by `*`, for division use `/` i.e `x*2` or `x/2`
-To write a power expression, use the `^` symbol i.e `x^2`
-To write a log with base expression, type `log(e1,e2)` it is important to include the brackets,
and the expressions must be comma seperated. i.e `log(x,x+1)`
- To write any trig functions (cos,sin) place the expression within brackets, this is the same for ln,exp
i.e `cos(x)`,`sin(x)`,`ln(x)`,`exp(x)`
-To negate an expression use `-` i.e `-x`
-To write an inverse expression use inv(expression) i.e `inv(x)`