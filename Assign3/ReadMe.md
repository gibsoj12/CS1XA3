# Math Library

## NOTE!

- Must install `generic-random` to use the ExprTest module.

## Documentation can be found here:
[docs](https://gibsoj12.github.io/docs/ExprEval.html)

## References:

* [Used deleeuwj1 function which checks for infinity](https://github.com/deleeuwj1/CS1XA3/blob/master/Assign3/Exprs/ExprEval.hs)
* [Used chenc118 idea to force-fit everything to work for all integral types](https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs)

### Functionality:
- Able to parse expressions of the proceeding form
- Able to partially or completely evaluate expressions
- Able to perform partial differentiation on expressions


### Expression Type:

```Haskell

data Expr a = Add (Expr a) (Expr a)
            | Neg (Expr a)
            | Mult (Expr a) (Expr a)
            | Const a
            | Var String
            | Sine (Expr a)
            | Cosine (Expr a)
            | Ln (Expr a)
            | Lawg (Expr a) (Expr a) 
            | Inv (Expr a)
            | Pow (Expr a) (Expr a)
            | Exp (Expr a

```

### Operations:

- Addition -> `x+1`
- Multiplication    -> `x*2`
- Division   -> `x/2` 
- Power Expression   -> `x^2`
- Log With Base  -> `log(x,x+1)` (Note it is important to seperate the expressions by a comma)
- Trig   -> `cos(x)` -> `sin(x)`
- Natural exponent   -> `exp(x)`
- Negation   -> `-x`
- Inverse    -> `inv(x)`