⍝ Parsing
Parse←{(∧⌿'-: '∘.≠⍵)⊆⍵}
ParseRange←{(⍎(1⊃⍵),' ',2⊃⍵) (3⊃⍵) (4⊃⍵)}
l←(ParseRange∘Parse)¨⊃⎕nget'input.txt'1

⍝ Part 1
IsValid ← {b←0 1+1⊃⍵ ⋄ 1=+/b<⍨+/(2⊃⍵)=3⊃⍵}
⎕←+/IsValid¨l

⍝ Part 2
IsValid2 ← {b←1⊃⍵ ⋄ ≠/(2⊃⍵)=(3⊃⍵)[b]}
⎕←+/IsValid2¨l
