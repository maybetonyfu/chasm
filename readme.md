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


## Performance Optimization

Original
```sh
tony@pop-os:~/Projects/chasm$ time stack runghc chasm/src/Environment.hs 

real	0m9.061s
user	0m8.851s
sys	0m0.207s
tony@pop-os:~/Projects/chasm$ time stack runghc chasm/src/Environment.hs 

real	0m9.038s
user	0m8.839s
sys	0m0.202s
tony@pop-os:~/Projects/chasm$ time stack runghc chasm/src/Environment.hs 

real	0m8.994s
user	0m8.813s
sys	0m0.185s
tony@pop-os:~/Projects/chasm$ time stack runghc chasm/src/Environment.hs 

real	0m9.014s
user	0m8.807s
sys	0m0.207s
```