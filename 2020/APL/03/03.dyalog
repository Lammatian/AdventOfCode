⍝ Parsing
⍝ There must be a better way to convert a 'nested' array to array with same shape
g←('#'∘=)¨⊃⎕nget'input.txt'1
h←≢g
w←≢⊃g
m←h w⍴∊g

⍝ Part 1
⍝ e for 'extended'
e←(,⍣(¯1+⌈1+3×h÷w))⍨m
⍝ f for 'filtered'
f←e/⍨1 0 0⍴⍨⌈/⍴e
⎕←+/h⍴(¯1+⍳2⊃⍴f)⊖f

⍝ Part 2
Solve←{
    ⍝ ⍺ - the speed to the right and down
    ⍝ ⍵ - the grid with rocks
    h w←⍴⍵
    dx dy←⍺
    copies←¯1+⌈(1+dx×h-1)÷w×dy                ⍝ Number of grid copies needed
    ext←(,⍣copies)⍨⍵                          ⍝ Extended grid
    mask←(2⊃⍴ext)⍴1,(dx-1)⍴0
    landrows←mask/ext                         ⍝ Only rows where we land
    rotated←((-dy)+dy×⍳2⊃⍴landrows)⊖landrows  ⍝ Shift cells we land on to top row
    +/rotated⍴⍨⌈h÷dy
}
⎕←×/(Solve∘m)¨((1 1) (3 1) (5 1) (7 1) (1 2))
