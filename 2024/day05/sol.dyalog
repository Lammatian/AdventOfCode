⍝ Get input and parse
filename←⊃2⌷⊢2⎕NQ#'GetCommandLineArgs'
s←⊃⎕nget filename 1
⍝ Updates
u←⍎¨¨','(≠⊆⊢)¨(','∊¨s)/s
⍝ Rules
r←{⍎¨'|'(≠⊆⊢)⍵}¨('|'∊¨s)/s

⍝ Part 1
⍝ Upper triangular matrix without diagonal
m←∘.<⍨⍳
⍝ Binary array indicating which updates are correctly ordered
o←~(∨/∨/)¨{r∘.≡⌽¨(,m ≢⍵)/,⍵}¨(∘.{⍺ ⍵}⍨⊢)¨u
⍝ Correctly ordered updates
c←o/u
⎕←+/{⍵[⌈2÷⍨≢⍵]}¨c

⍝ Part 2
⍝ Incorrectly ordered updates
n←u/⍨~o

⍝ The idea is that for each pair of numbers in an update, there must be a
⍝ rule. Otherwise there would possibly be multiple orderings
⍝ E.g. with rules 1|2, 3|2 and update 1,2,3, there are two possible orderings:
⍝   * 1,3,2
⍝   * 3,1,2
⍝ Now, let's take all the rules for each pair of numbers from the update
⍝ Then the ordering of the numbers will be based on the frequency of the
⍝ left number of each rule.
⍝ E.g. with rules 1|2, 3|2, 1|3, and update 1,2,3, note that '1' appears
⍝ twice on the left-hand side of rules, '3' appears once and '2' doesn't
⍝ appear. Thus 1 must be the 'greatest' number, 3 follows it and 2 is last

⍝ Algorithm based on the above: For each incorrectly ordered update
⍝ 1. Find all the rules with both numbers in the update: r/⍨(∧/⍵∊⍨⊢)¨r
⍝ 2. Take the first number of each such rule and combine with the update: ⍵,⊃¨
⍝ 3. Sort update based on number of occurrences: {⍵[;2][⍒⍵]}¨{{(≢⍵) ⍺}⌸
ro←{⍵[;2][⍒⍵]}¨{{(≢⍵) ⍺}⌸⍵,⊃¨r/⍨(∧/⍵∊⍨⊢)¨r}¨n
⎕←+/{⍵[⌈2÷⍨≢⍵]}¨ro


⍝ TODO: Why is the fixed-point matrix approach not working?
⍝ m←100 100⍴0
⍝ {m[⍎⊃1⌷⍵;⍎⊃2⌷⍵]←1}¨{'|'(≠⊆⊢)⍵}¨({'|'∊⍵}¨s)/s
⍝ f←{⍵∨⍵∨.×⍵}⍣≡m
⍝ u←⍎¨¨{','(≠⊆⊢)⍵}¨({','∊⍵}¨s)/s
⍝ +/{⍎⊃⍵[2÷⍨((≢⍵) + 1)]}¨u/⍨(∧/2{f[⍺;⍵]}/⍎¨)¨u
