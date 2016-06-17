⍝ Complex values

a ← 1J3
b ← ¯3J2
c ← ¯3.1J2
d ← ¯3J2.2
e ← ¯.3J.2
f ← ¯.3J¯.2
g ← ¯3j2

h ← a + b + c + d + e + f + g

⍝ One with nested arrays:
⍝ ⎕←' #'[9>|⊃{⍺+⍵*2}/9⍴⊂¯3×.7j.5-⍉a∘.+0j1×a←(⍳n+1)÷n←98]

⍝ Here is one without nested arrays:
⍝ ⎕←' #'[9>|m∘{⍺+⍵*2}⍣9⊢m←¯3×.7j.5-⍉a∘.+0j1×a←(⍳n+1)÷n←98]

⍝ One without nested arrays, but with certain parentheses in place
⎕←' #'[9>|m∘({⍺+⍵*2}⍣9)⊢(m←¯3×.7j.5-⍉a∘.+0j1×(a←(⍳n+1)÷(n←98)))]

⍝ And one that uses trains:

⍝ ⎕←' #'[9>|m∘(2*⍨+)⍣9⊢m←¯3×.7j.5-⍉a∘.+0j1×a←(⍳n+1)÷n←98]

⍝ Credit to @arcfide for these. See also
⍝ https://github.com/ngn/apl/blob/master/examples/7-mandelbrot.apl
