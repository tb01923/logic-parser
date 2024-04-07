# logic-parser
Parser for propositional logic, with recommended application of laws and substitution. more to come. Currently
porting JavaScript to ReScript (flavor of ocaml) to ease working with the abstract syntax tree, before adding
more capabilities.  No tests yet, JS version was more of a spike, i haven't learned unit tests on ReScript side yet,
its on the todo list.


## execution
This is a simple example that reduces in one step: `a and b or not(a and b)` this is fairly easy for a human to see 
as true `⊤`.  There are infinite variants of this propositional equation including more complex 
(`(a and b and c) or not(a and b and c)`) and simpler (`a or not a`).  Execution of these statements are correctly 
reduced to `⊤`, and the output shows the path to get there:
```
Step                        Complexity   Representation
--------------------------  ----------   -----------------------------
initial:                    10           ((a ∧ b) ∨ ¬(a ∧ b))
abstraction:                5            ([c/(a ∧ b)] ∨ ¬[c/(a ∧ b)])
Complement<or>:             6            ((p ∨ ¬p) ≡ ⊤)
transformation:             0            ⊤
```
The reduction engine takes the following steps:
1. parses the input into the abstract syntax tree with implicit grouping of `a and b` added
2. forms all possible abstract representations of the statement at this step (including keeping the intial version)
2a. attempt to abstract all binary operations into a variable (in this case `a and b` is abstracted to `c`)
3. find all applicable laws that apply to any of the possible abstraactions (including the initial version) 
     and apply the one that results in the lowest complexity
3a. in this case it matches the `Complement` law for `or` which is stated in the laws representation `((p ∨ ¬p) ≡ ⊤)`
4. it then transforms the step based on that law by taking the right hand side of the law  `⊤` 
5. since the complexity is 0 there are no possible remaining reductions so it is done.

The slightly more complex version of `a and b and c or not(a and b and c)` has a similar tree:
```
Step                        Complexity   Representation
--------------------------  ----------   -----------------------------
initial:                    15           ((a ∧ (b ∧ c)) ∨ ¬(a ∧ (b ∧ c)))
abstraction:                6            ([e/(a ∧ (b ∧ c))] ∨ ¬[e/(a ∧ (b ∧ c))])
Complement<or>:             6            ((p ∨ ¬p) ≡ ⊤)
transformation:             0            ⊤
```
a slight difference is that the reduction engine abstracted the `a and b and c` to the variable `e` which indicates
that it found another abstraction that it would have labeled as `d`  as less important.  in this case 
(based on where the parens are) it was the statement `b and c`. The path to an optimal solution didn't leverage 
that abstraction.

Once less note on abstraction.  I adopted the notation for abstraction that is similar to that of substituion:
`[c/(a ∧ b)]` to mean `c` stands for the proposition `a ∧ b` or more generally `[c/d]`c stands for the proposition `d`

### Laws
## truth table based laws (all booleans)
```
truth-table-and:            ((⊤ ∧ ⊤) ≡ ⊤)
truth-table-and:            ((⊤ ∧ ⊥) ≡ ⊥)
truth-table-and:            ((⊥ ∧ ⊤) ≡ ⊥)
truth-table-and:            ((⊥ ∧ ⊥) ≡ ⊥)
truth-table-or:             ((⊤ ∨ ⊤) ≡ ⊤)
truth-table-or:             ((⊤ ∨ ⊥) ≡ ⊤)
truth-table-or:             ((⊥ ∨ ⊤) ≡ ⊤)
truth-table-or:             ((⊥ ∨ ⊥) ≡ ⊥)
truth-table-implies:        ((⊤ -> ⊤) ≡ ⊤)
truth-table-implies:        ((⊤ -> ⊥) ≡ ⊥)
truth-table-implies:        ((⊥ -> ⊤) ≡ ⊤)
truth-table-implies:        ((⊥ -> ⊥) ≡ ⊤)
truth-table-biconditional:  ((⊤ <=> ⊤) ≡ ⊤)
truth-table-biconditional:  ((⊤ <=> ⊥) ≡ ⊥)
truth-table-biconditional:  ((⊥ <=> ⊤) ≡ ⊥)
truth-table-biconditional:  ((⊥ <=> ⊥) ≡ ⊤)
truth-table-negate:         (¬⊥ ≡ ⊤)
truth-table-negate:         (¬⊤ ≡ ⊥)
```
## Laws that resolve to booleans
```
Domination<or>:             ((p ∨ ⊤) ≡ ⊤)
Domination<and>:            ((p ∧ ⊥) ≡ ⊥)
Tautology<imp>:             ((p -> p) ≡ ⊤)
Complement<or>:             ((p ∨ ¬p) ≡ ⊤)
Complement<and>:            ((p ∧ ¬p) ≡ ⊥)
```
## laws that resolve to a single variable
```
Identity<or>:               ((p ∨ ⊥) ≡ p)
Identity<and>:              ((p ∨ ⊤) ≡ p)
Idempotence<or>:            ((p ∨ p) ≡ p)
Idempotence<and>:           ((p ∧ p) ≡ p)
Tautology<bi>:              ((p <=> p) ≡ p)
Double Negation:            (¬¬p ≡ p)
Absorbtion<or>:             ((p ∨ (p ∧ q)) ≡ p)
Absorbtion<and>:            ((p ∧ (p ∨ q)) ≡ p)
```
# laws that transform propositional statements to other forms
```
Commutative<and>:           ((p ∧ q) ≡ (q ∧ p))
Commutative<or>:            ((p ∨ q) ≡ (q ∨ p))
DeMorgan<not(or)>:          (¬(p ∨ q) ≡ (¬p ∧ ¬q))
DeMorgan<not(and)>:         (¬(p ∧ q) ≡ (¬p ∨ ¬q))
Associative<and>:           (((p ∧ q) ∧ r) ≡ (p ∧ (q ∧ r)))
Associative<or>:            (((p ∨ q) ∨ r) ≡ (p ∨ (q ∨ r)))
Distributive<or(and)>:      ((p ∨ (q ∧ r)) ≡ ((p ∨ q) ∧ (p ∨ r)))
Distributive<and(or)>:      ((p ∧ (q ∨ r)) ≡ ((p ∧ q) ∨ (p ∧ r)))
biconditional equivalence:  (((p ∧ q) ∨ (¬p ∧ ¬q)) ≡ (p <=> q))
implication equivalence:    ((p ∨ ¬q) ≡ (p -> q))
```

### ReScript version
```
npm install
npm install -g ReScript
rescript build
```
This will print something - but there is no REPL mode
```
node src/main.bs.js
```

### todo
* three variable laws do not work over statements with two variables
  * `("Distributive<or(and)>`: `p or (q and r) = (p or q) and (p or r)")`
  * should apply to `p or (p and q)` 
* add tests
* add REPL interaction
* a BSAT solver: https://en.wikipedia.org/wiki/Boolean_satisfiability_problem