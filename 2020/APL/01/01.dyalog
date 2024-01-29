⍝ Parsing
n←⍎¨⊃⎕nget'input.txt'1

⍝ Part 1
twosum←{×/⍵[⊃⍸2020=⍵∘.+⍵]}
⎕←twosum n

⍝ Part 2
threesum←{×/⍵[⊃⍸2020=⍵∘.+⍵∘.+⍵]}
⎕←threesum n
