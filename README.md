## aplparse: An APL Parser in Standard ML

This software implements an APL parser in Standard ML. 

## Example

The APL program

```apl
diff ← {1↓⍵−¯1⌽⍵}
signal ← {¯50⌈50⌊50×(diff 0,⍵)÷0.01+⍵}
```

compiles into the following abstract syntax tree (pretty printed):

    [Assign(diff,Lam(App2(Drop,1,App2(Sub,Omega,App2(Rot,-1,Omega))))),
     Assign(signal,Lam(App2(Max,-50,
                         App2(Min,50,
                           App2(Times,50,
                             App2(Div,
                               App1(diff,App2(Cat,0,Omega)),
                               App2(Add,0.01,Omega)
                             )
                           )
                         )
                       )
                      )
           )
    ]

## Try it!

The software makes use of the [smlunicode
library](https://github.com/melsman/smlunicode) for lexing, which
means that you need to checkout the
[smlunicode](https://github.com/melsman/smlunicode) sources in a
folder besides the aplparse sources.

You also need a Standard ML compiler (e.g., [Mlton](http://www.mlton.org/)).

Then simply write `make test` followed by `./test` in your shell.

## Limitations

Todo: Operators and improved error handling.

## License

This software is published under the [MIT License](MIT_LICENSE.md).