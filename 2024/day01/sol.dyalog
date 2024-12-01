⍝ Get input
filename←⊃2⌷⊢2⎕NQ#'GetCommandLineArgs'
s←⊃⎕nget filename 1

⍝ Part 1
a←{⍵[⍋⍵]}(1⌷⊢∘⍎)¨s
b←{⍵[⍋⍵]}(2⌷⊢∘⍎)¨s
⎕←+/|a-b

⍝ Part 2
⎕←+/{⍵×+/⍵=b}¨a
