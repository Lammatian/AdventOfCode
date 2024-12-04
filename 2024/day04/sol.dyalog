⍝ Diagonal SE: id
⍝ Diagonal SW: ⌽
⍝ Diagonal NE: ⊖
⍝ Diagonal NW: ⌽⊖
⍝ E: id
⍝ W: ⌽
⍝ S: ⍉
⍝ N: ⍉⊖

⍝ Get input
filename←⊃2⌷⊢2⎕NQ#'GetCommandLineArgs'
i←↑⊃⎕nget filename 1

⍝ Part 1
s←+/{+/,'XMAS'⍷⍵}¨(⍉i) (⌽i) i (⍉⊖i)
id←∘.=⍨⍳
d←+/{+/,⍵}¨({'XMAS'≡(,id 4)/,⍵}⌺4 4)¨i (⌽i) (⊖i) (⌽⊖i)
⎕←s+d

⍝ Part 2
m←(id 3)∨⌽id 3
c←↑'MMASS' 'MSAMS' 'SSAMM' 'SMASM'
⎕←+/,({∨/c(≡⍤1)⍨(,m)/,⍵}⌺3 3)i
