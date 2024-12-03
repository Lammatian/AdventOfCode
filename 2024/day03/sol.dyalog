⍝ Get input
filename←⊃2⌷⊢2⎕NQ#'GetCommandLineArgs'
inp←∊⊃⎕nget filename 1

⍝ Part 1
⍝ TODO: I don't understand why I need ⊢⍵ there
m←{+/(×/⍎)¨'mul\((\d+),(\d+)\)'⎕S'\1 \2'⊢⍵}
⎕←m inp

⍝ Part 2
inp←'do()',inp,'don''t()'
⎕←+/m¨'do\(\)(.+?)don''t'⎕S'\1'⊢inp
