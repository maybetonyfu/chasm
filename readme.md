<!-- ghc -fno-code -fforce-recomp -ddump-types Test2.hs -->
## Usage of stack

```bash
# To build library only
stack build chasm:lib

# To build executables only
stack build chasm:exe:ig
```

## Constraint generation

This program `x :: [Int] -> [Int]` will generate these constraints:
```
x :- Function t1 t2,
t1 :- List t3,
t2 :- List t4,
t3 :- Int,
t4 :- Int
```

This program `import Data.List`
will generate these constraints:
```
(++) :- ...,
head :- ...,
last :- ...,
...
```
