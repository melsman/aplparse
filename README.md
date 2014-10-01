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

The parser compiles with either [MLton](http://mlton.org) or
[MLKit](http://www.elsman.com/mlkit/). 

For compilation and use,
[Smackage](https://github.com/standardml/smackage) is assumed. Add the
following entry to your `~/.smackage/sources.local` file:

    aplparse git git://github.com/melsman/aplparse.git

Now write

    $ smackage refresh
    $ smackage get aplparse

The implementation builds on the unicode library available at
[https://github.com/melsman/unicode](https://github.com/melsman/unicode),
but Smackage will arrange for this library to be fetched and installed
automatically.

Then simply write `smackage make aplparse tests` in your shell.

To use the MLKit as a compiler, write instead:

    $ smackage make aplparse clean
    $ MLCOMP=mlkit smackage make aplparse tests

## Limitations

Todo: improved error handling. Although position information is now
maintained in the parser, not all parser errors are reported with
relevant position information.

## License

This software is published under the [MIT License](MIT_LICENSE.md).