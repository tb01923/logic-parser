# logic-parser
Parser for propositional logic, with recommended application of laws and substitution. more to come. Currently
porting JavaScript to ReScript (flavor of ocaml) to ease working with the abstract syntax tree, before adding
more capabilities.  No tests yet, JS version was more of a spike, i haven't learned unit tests on ReScript side yet,
its on the todo list.


## execution


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

todo:
* modify identify laws to work over statements with abstractions
* create a use case, to take a statement, generate all abstract variants, and generate all possible law matches for each variant
* create "replace" capacity