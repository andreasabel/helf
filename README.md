# helf
A Haskell implementation of the Edinburgh Logical Framework.

helf parses and typechecks `.elf` files written for the Twelf implementation
of the Logical Framework.  helf is mainly a laboratory to experiment with different
representation of lambda-terms for bidirectional typechecking.

## Limitations

helf
+ only understands the fixity pragmas for operators.  It ignores all other pragmas.
+ only implements bidirectional type checking.  It does not have unification or type reconstruction.
+ does not give nice error messages.

## Installation

Requires GHC and cabal, for instance via the Haskell Platform.
In a shell, type
```
  cabal install helf
```

## Examples

File `eval.elf`:
```elf
% Untyped lambda calculus.

tm   : type.
abs  : (tm -> tm) -> tm.
app  : tm -> (tm -> tm).

% cbn weak head evaluation.

eval : tm -> tm -> type.

eval/abs : {M : tm -> tm}
  eval (abs M) (abs M).

eval/app : {M : tm} {M' : tm -> tm} {N : tm} {V : tm}
  eval M (abs M') ->
  eval (M' N) V   ->
  eval (app M N) V.
```
Type check with:
```
  helf eval.elf
```

For more examples, see `test/succeed/`.
