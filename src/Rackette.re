open CS17SetupRackette;
open Read.Reader;
open Types;

/* Initial top-level environment:
   contains built in procedures such as "+", "-", and "cons" */

let initialTle: environment = [
  [
    (
      Name("+"),
      BuiltinV({
        printedRep: "builtIn: plus",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => NumV(a + b)
          | _ => failwith("'+' expects two number values")
          },
      }),
    ),
    (
      Name("-"),
      BuiltinV({
        printedRep: "builtIn: minus",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => NumV(a - b)
          | _ => failwith("'-' expects two number values")
          },
      }),
    ),
    (
      Name("*"),
      BuiltinV({
        printedRep: "builtIn: multiplication",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => NumV(a * b)
          | _ => failwith("'*' expects two number values")
          },
      }),
    ),
    (
      Name("/"),
      BuiltinV({
        printedRep: "builtIn: division",
        bProc: alon =>
          switch (alon) {
          | [NumV(_), NumV(0)] => failwith("cannot divide by zero")
          | [NumV(a), NumV(b)] => NumV(a / b)
          | _ => failwith("'/' expects two number values")
          },
      }),
    ),
    (
      Name("remainder"),
      BuiltinV({
        printedRep: "builtIn: remainder",
        bProc: alon =>
          switch (alon) {
          | [NumV(_), NumV(0)] => failwith("cannot divide by zero")
          | [NumV(a), NumV(b)] => NumV(a mod b)
          | _ => failwith("'remainder' expects two number values")
          },
      }),
    ),
    (
      Name("="),
      BuiltinV({
        printedRep: "builtIn: equalMath",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => BoolV(a == b)
          | _ => failwith("'=' expects two number values")
          },
      }),
    ),
    (
      Name("<"),
      BuiltinV({
        printedRep: "builtIn: less than",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => BoolV(a < b)
          | _ => failwith("< expects two number values")
          },
      }),
    ),
    (
      Name(">"),
      BuiltinV({
        printedRep: "builtIn: more than",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => BoolV(a > b)
          | _ => failwith("> expects two number values")
          },
      }),
    ),
    (
      Name("<="),
      BuiltinV({
        printedRep: "builtIn: less than or equal",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => BoolV(a <= b)
          | _ => failwith("<= expects two number values")
          },
      }),
    ),
    (
      Name(">="),
      BuiltinV({
        printedRep: "builtIn: more than or equal",
        bProc: alon =>
          switch (alon) {
          | [NumV(a), NumV(b)] => BoolV(a >= b)
          | _ => failwith(">= expects two number values")
          },
      }),
    ),
    (
      Name("equal?"),
      BuiltinV({
        printedRep: "builtIn: isEqual",
        bProc: alod =>
          switch (alod) {
          | [BuiltinV(a), BuiltinV(b)] => BoolV(a == b)
          | [ClosureV(a), ClosureV(b)] => BoolV(a == b)
          | [NumV(a), NumV(b)] => BoolV(a == b)
          | [BoolV(a), BoolV(b)] => BoolV(a == b)
          | [ListV(a), ListV(b)] => BoolV(a == b)
          | _ => failwith("equal? expects two values of the same type")
          },
      }),
    ),
    (
      Name("number?"),
      BuiltinV({
        printedRep: "builtIn: isNum",
        bProc: alod =>
          switch (alod) {
          | [NumV(_num)] => BoolV(true)
          | [_hd] => BoolV(false)
          | _ => failwith("number? expects one value")
          },
      }),
    ),
    (
      Name("zero?"),
      BuiltinV({
        printedRep: "builtIn: isZero",
        bProc: alon =>
          switch (alon) {
          | [NumV(num)] =>
            if (num == 0) {
              BoolV(true);
            } else {
              BoolV(false);
            }
          | _ => failwith("zero? expects a number value")
          },
      }),
    ),
    (
      Name("cons"),
      BuiltinV({
        printedRep: "builtIn: cons",
        bProc: alod =>
          switch (alod) {
          | [a, ListV(lst)] => ListV([a, ...lst])
          | _ =>
            failwith("cons expects inputs of type 'a followed by List('a)")
          },
      }),
    ),
    (
      Name("first"),
      BuiltinV({
        printedRep: "builtIn: first",
        bProc: alod =>
          switch (alod) {
          | [ListV([hd, ..._tl])] => hd
          | _ => failwith("First expects a list with at least 1 element")
          },
      }),
    ),
    (
      Name("rest"),
      BuiltinV({
        printedRep: "builtin: rest",
        bProc: alod =>
          switch (alod) {
          | [ListV([_hd, ...tl])] => ListV(tl)
          | _ => failwith("rest expects a list with at least 1 element")
          },
      }),
    ),
    (
      Name("empty?"),
      BuiltinV({
        printedRep: "builtIn: isEmpty",
        bProc: alod =>
          switch (alod) {
          | [ListV([])] => BoolV(true)
          | [ListV([_hd, ..._tl])] => BoolV(false)
          | _ => failwith("empty? expects an argument of type list('a)")
          },
      }),
    ),
    (
      Name("cons?"),
      BuiltinV({
        printedRep: "builtIn: isCons",
        bProc: lst =>
          switch (lst) {
          | [ListV([])] => BoolV(false)
          | [ListV([_hd, ..._tl])] => BoolV(true)
          | _ => failwith("cons? expects an argument of type list('a)")
          },
      }),
    ),
    (
      Name("not"),
      BuiltinV({
        printedRep: "builtIn: not",
        bProc: bool =>
          switch (bool) {
          | [BoolV(exp)] => BoolV(!exp)
          | _ => failwith("not expects an argument of type bool")
          },
      }),
    ),
  ],
];

/* parseExpression (rec):
   Input: a concreteProgramPiece which has been determined to be an expression.
   Output: an expression representing the semantic content of the
     concreteProgramPiece. We convert concrete syntax into an abstract 
     expression,
     handle special forms (if, cond, lambda, let, and, or), and applications

   Ex:
   - ListC([SymbolC("+"), NumberC(1), NumberC(2)]) =>
   ApplicationE([NameE(Name("+")), NumE(1), NumE(2)])

     Recursion Diagrams:
    OI: ListC([SymbolC("if"), SymbolC("true"), NumberC(1), NumberC(2)])
      RI: SymbolC("true") | NumberC(1) | NumberC(2)
      RO: BoolE(true)     | NumE(1)    | NumE(2)
      Ideation: Parse the condition and both branches recursively
    OO: IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(2)})

    OI: ListC([SymbolC("+"), NumberC(1), NumberC(2)])
      RI: NumberC(1) | NumberC(2)
      RO: NumE(1)    | NumE(2)
      Ideation: Parse operator as NameE, map parseExpression over arguments
    OO: ApplicationE([NameE(Name("+")), NumE(1), NumE(2)])

    OI: ListC([SymbolC("let"), ListC([ListC([SymbolC("x"), NumberC(5)])]),
    SymbolC("x")])
      RI: NumberC(5) | SymbolC("x")
      RO: NumE(5)    | NameE(Name("x"))
      Ideation: Parse bindings and body recursively
    OO: LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(5)}],
              letBody: NameE(Name("x"))})
    */

let rec parseExpression: concreteProgramPiece => expression =
  input =>
    switch (input) {
    | SymbolC(x) =>
      switch (x) {
      | "empty" => EmptyE
      | "true" => BoolE(true)
      | "false" => BoolE(false)
      | _ => NameE(Name(x))
      }
    | NumberC(num) => NumE(num)
    | ListC(lst) =>
      switch (lst) {
      | [SymbolC("and"), expr1, expr2] =>
        AndE(parseExpression(expr1), parseExpression(expr2))
      | [SymbolC("or"), expr1, expr2] =>
        OrE(parseExpression(expr1), parseExpression(expr2))
      | [SymbolC("if"), boolExpr, exprT, exprF] =>
        IfE({
          boolExpr: parseExpression(boolExpr),
          trueExpr: parseExpression(exprT),
          falseExpr: parseExpression(exprF),
        })
      | [SymbolC("cond"), ...tl] =>
        CondE(
          List.map(
            pair =>
              switch (pair) {
              | ListC([condExpr, resExpr]) => {
                  conditionExpr: parseExpression(condExpr),
                  resultExpr: parseExpression(resExpr),
                }
              | _ => failwith("cond expects pairs of expressions")
              },
            tl,
          ),
        )
      | [SymbolC("lambda"), ListC(names), expr] =>
        LambdaE({
          nameList:
            List.map(
              x =>
                switch (x) {
                | SymbolC(n) => Name(n)
                | _ => failwith("lambda expects valid names")
                },
              names,
            ),
          lambdaBody: parseExpression(expr),
        })
      | [SymbolC("let"), ListC(pairlst), expr] =>
        LetE({
          letPairs:
            List.map(
              pair =>
                switch (pair) {
                | ListC([SymbolC(name), expr]) => {
                    pairName: Name(name),
                    pairExpr: parseExpression(expr),
                  }
                | _ => failwith("let expects valid name-expression pairs")
                },
              pairlst,
            ),
          letBody: parseExpression(expr),
        })
      | [SymbolC(name), ...tl] =>
        switch (tl) {
        | [] => NameE(Name(name))
        | _ =>
          ApplicationE([
            NameE(Name(name)),
            ...List.map(parseExpression, tl),
          ])
        }
      | [ListC(lst), ...tl] =>
        ApplicationE([
          parseExpression(ListC(lst)),
          ...List.map(parseExpression, tl),
        ])
      | _ => failwith("wrong syntax")
      }
    };

/* parseDefinition:
   Input: A concreteProgramPiece which has been determined to be a definition.
   Output: A definition tuple (name, expression) in abstract syntax
   Ex:
   - parseDefinition(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])) =>
   (Name("x"), NumE(5))
    */

let parseDefinition: concreteProgramPiece => definition =
  input =>
    switch (input) {
    | ListC([_, SymbolC(name), expr]) => (
        Name(name),
        parseExpression(expr),
      )
    | _ => failwith("wrong definition syntax")
    };

/* parsePiece:
   Input: A concreteProgramPiece representation of a rackette program
   Output: An abstractProgramPiece, produced after the program determines if a
   concreteProgramPiece is a definition or expression and parses accordingly

   Ex:
   - parsePiece(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])) =>
   Definition((Name("x"), NumE(5)))
   - parsePiece(NumberC(42)) =>
   Expression(NumE(42))
    */

let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* parse:
   Input: A concreteProgram, which is a list of concreteProgramPieces
   Output: Parses an entire program by mapping parsePiece over all pieces to
     produce an abstractProgram, which is a list of abstractProgramPieces
   Ex:
   - parse([NumberC(1), ListC([SymbolC("+"), NumberC(2), NumberC(3)])]) =>
   [Expression(NumE(1)),
   Expression(ApplicationE([NameE(Name("+")), NumE(2), NumE(3)]))]
    */

let parse: concreteProgram => abstractProgram =
  input => List.map(parsePiece, input);

let emptyEnv: environment = [[]];

let emptyBindingList: bindingList = [];

/* addBindingToList
   Input: A binding, bd, and a bindingList, bdLst
   Output: A new bindingList with the binding prepended
   Ex:
    - addBindingToList((Name("x"), NumV(5)), [(Name("y"), NumV(3))]) =>
   [(Name("x"), NumV(5)), (Name("y"), NumV(3))]
    */

let addBindingToList: (binding, bindingList) => bindingList =
  (bd, bdLst) => [bd, ...bdLst];

/* addBinding:
   Input: A binding, bd and an environment, env
   Output: A new environment with the binding added to env
   Ex:
    - addBinding((Name("x"), NumV(5)), [[(Name("y"), NumV(3))], []]) =>
   [[(Name("x"), NumV(5)), (Name("y"), NumV(3))], []]
    */

let addBinding: (binding, environment) => environment =
  (bd, env) => [addBindingToList(bd, List.hd(env)), ...List.tl(env)];

/* extendEnv:
   Input: A bindingList, bdLst and an environment, env
   Output: A new environment with the bindingList added to env
   Ex:
    - extendEnv([(Name("x"), NumV(5))], [[(Name("y"), NumV(3))]]) =>
   [[(Name("x"), NumV(5))], [(Name("y"), NumV(3))]]
    */

let extendEnv: (bindingList, environment) => environment =
  (bdLst, env) => [bdLst, ...env];

/* lookup (rec):
   Input: A name, called name, and an environment, env
   Output: Searches environments from most-recent to oldest and returns
     option(value) — Some(value) if binding found, and None otherwise
   Ex:
    - lookup(Name("x"), [[(Name("x"), NumV(1))], []]) => Some(NumV(1))

   Recusion Diagrams:
   OI: (Name("y"), [[("x", NumV(5)), ("y", NumV(3))], [("z", NumV(7))]])
     RI: (Name("y"), [[("y", NumV(3))], [("z", NumV(7))]])
     Ideation: "x" != "y", skip this binding and recurse with rest of frame
     RO: Some(NumV(3))
   OO: Some(NumV(3))

   OI: (Name("x"), [[("x", NumV(5)), ("y", NumV(3))], [("z", NumV(7))]])
     Ideation: First binding matches, return its value
   OO: Some(NumV(5))
   */

let rec lookup: (name, environment) => option(value) =
  (name, env) =>
    switch (env) {
    | [] => None
    | [[], ...tl] => lookup(name, tl)
    | [[(nm, value), ...tl1], ...tl2] =>
      if (nm == name) {
        Some(value);
      } else {
        lookup(name, [tl1, ...tl2]);
      }
    };

/* eval (rec):
   Input: tle, the top level environment, env, the local environment at time of
     evaluation, and expr, an expression to evaluate
   Output: A value produced upon evaluating expr in the environments tle, env
   Ex.
    - eval([[Name("x"), NumV(4)]], [[]], NameE(Name(("x"))) => NumV(4)

   Recursion Diagrams:

   OI: ([], [], AndE(BoolE(false), NumE(5)))
     RI: ([], [], BoolE(false))
     Ideation: First expression evaluates to false, short-circuit
     RO: BoolV(false)
   OO: BoolV(false)

   OI: ([], [], OrE(BoolE(true), NumE(5)))
     RI: ([], [], BoolE(true))
     Ideation: First expression evaluates to true, short-circuit
     RO: BoolV(true)
   OO: BoolV(true)

   OI: (initialTle, [], ApplicationE([NameE(Name("+")), NumE(2), NumE(3)]))
     RI: (initialTle, [], NameE(Name("+")))
     RO: BuiltinV({...})
     RI: (initialTle, [], NumE(2)) | (initialTle, [], NumE(3))
     RO: NumV(2) | NumV(3)
     Ideation: Apply builtin + to [NumV(2), NumV(3)]
   OO: NumV(5)

   OI: (initialTle, [[("x", NumV(10))]], NameE(Name("x")))
     Ideation: Lookup Name("x") in List.append(env, tle)
     RI: lookup(Name("x"), [[("x", NumV(10))], ...initialTle...])
     RO: Some(NumV(10))
   OO: NumV(10)
   */

let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
    switch (expr) {
    | NumE(n) => NumV(n)
    | BoolE(b) => BoolV(b)
    | EmptyE => ListV([])
    | NameE(nm) =>
      switch (lookup(nm, List.append(env, tle))) {
      | Some(vlu) => vlu
      | None => failwith("undefined procedure")
      }
    | AndE(expr1, expr2) =>
      switch (eval(tle, env, expr1)) {
      | BoolV(true) => eval(tle, env, expr2)
      | BoolV(false) => BoolV(false)
      | _ => failwith("Not valid 'and' expression")
      }
    | OrE(expr1, expr2) =>
      switch (eval(tle, env, expr1)) {
      | BoolV(true) => BoolV(true)
      | BoolV(false) => eval(tle, env, expr2)
      | _ => failwith("Not valid 'or' expression")
      }
    | IfE(ifDta) =>
      switch (eval(tle, env, ifDta.boolExpr)) {
      | BoolV(true) => eval(tle, env, ifDta.trueExpr)
      | BoolV(false) => eval(tle, env, ifDta.falseExpr)
      | _ => failwith("if expressions expect boolean conditionals")
      }
    | CondE(lstCdDta) =>
      switch (lstCdDta) {
      | [] => failwith("no conditions matched")
      | _ =>
        switch (eval(tle, env, List.hd(lstCdDta).conditionExpr)) {
        | BoolV(true) => eval(tle, env, List.hd(lstCdDta).resultExpr)
        | BoolV(false) => eval(tle, env, CondE(List.tl(lstCdDta)))
        | _ => failwith("condition must evaluate to a boolean")
        }
      }
    | LambdaE(lmdaDta) =>
      ClosureV({
        cNameList: lmdaDta.nameList,
        cExpr: lmdaDta.lambdaBody,
        cEnv: env,
      })
    | LetE(ltDta) =>
      let lclBds = [
        List.map(
          pr => (pr.pairName, eval(tle, env, pr.pairExpr)),
          ltDta.letPairs,
        ),
      ];
      eval(tle, List.append(lclBds, env), ltDta.letBody);
    | ApplicationE(lstOfExpr) =>
      let procVal = eval(tle, env, List.hd(lstOfExpr));
      let argVals = List.map(x => eval(tle, env, x), List.tl(lstOfExpr));
      switch (procVal) {
      | BuiltinV(biV) => biV.bProc(argVals)
      | ClosureV(clsDta) =>
        let formalActuals =
          List.map2((a, b) => (a, b), clsDta.cNameList, argVals);
        eval(tle, extendEnv(formalActuals, clsDta.cEnv), clsDta.cExpr);
      | _ => failwith("first argument must be a procedure")
      };
    };

/* addDefinition:
   Input: env, the current environment,
     and (id, expr), a tuple of a name and an expression.
   Output: a new enviroment with the binding (id, expr) if the name id is not
     already bound
   */

let addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) =>
    switch (lookup(id, env)) {
    | Some(_) => failwith("definition already exists")
    | None => addBinding((id, eval(env, [], expr)), env)
    };

/* stringOfValue (rec):
   Input: A value, aValue
   Output: a string which represents the value aValue.

   OI: ListV([NumV(1), NumV(2), NumV(3)])
     RI: stringOfValue(NumV(1)) | stringOfValue(NumV(2)) | stringOfValue(NumV(3))
     Ideation: Map stringOfValue over list, concatenate with spaces
     RO: "1" | "2" | "3"
     Ideation: Join with " " and wrap with "(list " and ")"
   OO: "(list 1 2 3)"

   OI: ListV([ListV([NumV(1), NumV(2)]), NumV(3)])
     RI: stringOfValue(ListV([NumV(1), NumV(2)])) | stringOfValue(NumV(3))
       RI (nested): stringOfValue(NumV(1)) | stringOfValue(NumV(2))
       RO (nested): "1" | "2"
       Ideation: Join and wrap
       RO (nested): "(list 1 2)"
     RO: "(list 1 2)" | "3"
     Ideation: Join with " " and wrap
   OO: "(list (list 1 2) 3)"
   */

let rec stringOfValue: value => string =
  aValue =>
    switch (aValue) {
    | NumV(n) => string_of_int(n)
    | BoolV(b) => string_of_bool(b)
    | ListV(alov) =>
      switch (alov) {
      | [] => "empty"
      | _ =>
        "(list " ++ String.concat(" ", List.map(stringOfValue, alov)) ++ ")"
      }
    | BuiltinV(biDta) => biDta.printedRep
    | ClosureV(_clsDta) => "user-defined procedure"
    };

/* process:
   Input: An abstract program, pieces, which is a list abstract program pieces:
     either definitions or expressions
   Output: A list of values, produced upon evaluating each abstract program 
      piece
   Ex:
    - process([Definition(Name("x"), NumE(3)), NameE(Name("x"))]) => NumV(3)
   */

let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

/* rackette:
   Input: A string representation of a rackette program.
   Output: A string representation of the output of the rackette program
   Ex:
    - rackette("(+ 3 4)") => "7"
   */

let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse empty expression",
);
// sample test: parseExpression with read
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "read and parse empty expression",
);

/* additional check-expects */

checkExpectExpression(
  parseExpression(
    ListC([
      ListC([SymbolC("if"), SymbolC("true"), SymbolC("+"), SymbolC("-")]),
      NumberC(1),
      NumberC(2),
    ]),
  ),
  ApplicationE([
    IfE({
      boolExpr: BoolE(true),
      trueExpr: NameE(Name("+")),
      falseExpr: NameE(Name("-")),
    }),
    NumE(1),
    NumE(2),
  ]),
  "parse: application with if as operator (if true + -) 1 2",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("/")), NumE(7), NumE(3)]),
  ),
  NumV(2),
  "eval: integer division 7 / 3 -> 2",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("remainder")), NumE(7), NumE(3)]),
  ),
  NumV(1),
  "eval: remainder 7 mod 3 -> 1",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("<")), NumE(2), NumE(3)]),
  ),
  BoolV(true),
  "eval: < 2 3 -> true",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("<=")), NumE(3), NumE(3)]),
  ),
  BoolV(true),
  "eval: <= 3 3 -> true",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name(">=")), NumE(4), NumE(5)]),
  ),
  BoolV(false),
  "eval: >= 4 5 -> false",
);

let nestedListExpr =
  ApplicationE([
    NameE(Name("cons")),
    NumE(1),
    ApplicationE([NameE(Name("cons")), NumE(2), EmptyE]),
  ]);
checkExpect(
  eval(initialTle, [], nestedListExpr),
  ListV([NumV(1), NumV(2)]),
  "eval: cons 1 (cons 2 empty) -> (1 2)",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("first")), nestedListExpr]),
  ),
  NumV(1),
  "eval: first of (1 2) -> 1",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("rest")), nestedListExpr]),
  ),
  ListV([NumV(2)]),
  "eval: rest of (1 2) -> (2)",
);

checkExpect(
  eval(initialTle, [], ApplicationE([NameE(Name("not")), BoolE(false)])),
  BoolV(true),
  "eval: not false -> true",
);

checkExpect(
  process([
    Definition((Name("a"), NumE(3))),
    Expression(
      ApplicationE([NameE(Name("+")), NameE(Name("a")), NumE(4)]),
    ),
  ]),
  [NumV(7)],
  "process: define a=3 then evaluate (+ a 4) -> 7",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("equal?")), NumE(2), NumE(3)]),
  ),
  BoolV(false),
  "eval: equal? 2 3 -> false",
);

checkExpectExpression(
  parseExpression(
    ListC([
      ListC([SymbolC("if"), SymbolC("true"), SymbolC("+"), SymbolC("-")]),
      NumberC(1),
      NumberC(2),
    ]),
  ),
  ApplicationE([
    IfE({
      boolExpr: BoolE(true),
      trueExpr: NameE(Name("+")),
      falseExpr: NameE(Name("-")),
    }),
    NumE(1),
    NumE(2),
  ]),
  "parse: application with if as operator (if true + -) 1 2",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("/")), NumE(7), NumE(3)]),
  ),
  NumV(2),
  "eval: integer division 7 / 3 -> 2",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("remainder")), NumE(7), NumE(3)]),
  ),
  NumV(1),
  "eval: remainder 7 mod 3 -> 1",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("<")), NumE(2), NumE(3)]),
  ),
  BoolV(true),
  "eval: < 2 3 -> true",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("<=")), NumE(3), NumE(3)]),
  ),
  BoolV(true),
  "eval: <= 3 3 -> true",
);
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name(">=")), NumE(4), NumE(5)]),
  ),
  BoolV(false),
  "eval: >= 4 5 -> false",
);

checkExpect(
  eval(initialTle, [], ApplicationE([NameE(Name("not")), BoolE(false)])),
  BoolV(true),
  "eval: not false -> true",
);

let someClosure =
  eval(
    initialTle,
    [],
    LambdaE({nameList: [Name("x")], lambdaBody: NameE(Name("x"))}),
  );
checkExpect(
  stringOfValue(someClosure),
  "user-defined procedure",
  "stringOfValue on ClosureV -> user-defined procedure",
);

checkExpect(
  process([
    Definition((Name("a"), NumE(3))),
    Expression(
      ApplicationE([NameE(Name("+")), NameE(Name("a")), NumE(4)]),
    ),
  ]),
  [NumV(7)],
  "process: define a=3 then evaluate (+ a 4) -> 7",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("equal?")), NumE(2), NumE(3)]),
  ),
  BoolV(false),
  "eval: equal? 2 3 -> false",
);

checkExpect(
  stringOfValue(ListV([])),
  "empty",
  "stringOfValue: empty list -> \"empty\"",
);

checkExpect(
  stringOfValue(ListV([NumV(1), NumV(4)])),
  "(list 1 4)",
  "stringOfValue: (list 1 4)",
);

checkExpect(
  stringOfValue(ListV([ListV([]), ListV([NumV(4), NumV(5)])])),
  "(list empty (list 4 5))",
  "stringOfValue: nested list",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      LambdaE({
        nameList: [Name("f")],
        lambdaBody: ApplicationE([NameE(Name("f")), NumE(3), NumE(4)]),
      }),
      NameE(Name("+")),
    ]),
  ),
  NumV(7),
  "higher-order: pass + as argument and apply to 3 4 -> 7",
);

checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([
      ApplicationE([
        LambdaE({
          nameList: [Name("a")],
          lambdaBody:
            LambdaE({
              nameList: [Name("x")],
              lambdaBody:
                ApplicationE([
                  NameE(Name("+")),
                  NameE(Name("x")),
                  NameE(Name("a")),
                ]),
            }),
        }),
        NumE(5),
      ]),
      NumE(2),
    ]),
  ),
  NumV(7),
  "higher-order: closure capture a=5 then apply x=2 -> 7",
);

checkExpect(rackette("(+ 1 2)"), ["3"], "rackette: (+ 1 2) -> [\"3\"]");

checkExpect(
  rackette("(define a 3) (+ a 4)"),
  ["7"],
  "rackette: define a then (+ a 4) -> [\"7\"]",
);

checkExpect(
  rackette("((lambda (x) (+ x 1)) 5)"),
  ["6"],
  "rackette: immediate lambda application -> [\"6\"]",
);

checkExpect(
  rackette("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"),
  ["34"],
  "rackette: practice on paper #1",
);

checkExpect(
  rackette(
    "(let ((x 0) (y 18)) (let ((f (lambda (a b) (+ x b))) (x 17)) (f y x)))",
  ),
  ["17"],
  "rackette: practice on paper #2",
);

checkExpect(
  rackette("(define y 17) (let ((y 3)) (+ y 7))"),
  ["10"],
  "rackette: practice on paper #4",
);

checkExpect(
  rackette(
    "(define double-all (lambda (alon)
  (cond
    ((empty? alon) alon)
    ((cons? alon) (cons (* 2 (first alon)) (double-all (rest alon)))))))

  (double-all (cons 3 (cons 4 (cons 5 empty))))",
  ),
  ["(list 6 8 10)"],
  "rackette: double-all",
);

/* my-member? */
checkExpect(
  rackette(
    "(define my-member? (lambda (num alon)
    (cond
      ((empty? alon) false)
      ((cons? alon) (or (equal? num (first alon)) (my-member? num (rest alon))))
    ))) (my-member? 4 (cons 3 (cons 4 (cons 5 empty))))",
  ),
  ["true"],
  "my-member?: found in middle -> true",
);

checkExpect(
  rackette(
    "(define my-member? (lambda (num alon)
    (cond
      ((empty? alon) false)
      ((cons? alon) (or (equal? num (first alon)) (my-member? num (rest alon))))
    ))) (my-member? 2 (cons 3 (cons 4 empty)))",
  ),
  ["false"],
  "my-member?: not present -> false",
);

checkExpect(
  rackette(
    "(define my-member? (lambda (num alon)
    (cond
      ((empty? alon) false)
      ((cons? alon) (or (equal? num (first alon)) (my-member? num (rest alon))))
    ))) (my-member? 3 empty)",
  ),
  ["false"],
  "my-member?: empty list -> false",
);

checkExpect(
  rackette(
    "(define my-member? (lambda (num alon)
    (cond
      ((empty? alon) false)
      ((cons? alon) (or (equal? num (first alon)) (my-member? num (rest alon))))
    ))) (my-member? 3 (cons 3 empty))",
  ),
  ["true"],
  "my-member?: single-element list -> true",
);

/* my-fold */
checkExpect(
  rackette(
    "(define my-fold (lambda (proc bv alod)
    (cond
      ((empty? alod) bv)
      ((cons? alod) (proc (first alod) (my-fold proc bv (rest alod))))))) 
      (my-fold (lambda (x acc) (+ x acc)) 0 (cons 1 (cons 2 (cons 3 empty))))",
  ),
  ["6"],
  "my-fold: sum 1+2+3 -> 6",
);

checkExpect(
  rackette(
    "(define my-fold (lambda (proc bv alod)
    (cond
      ((empty? alod) bv)
      ((cons? alod) (proc (first alod) (my-fold proc bv (rest alod))))
    ))) (my-fold (lambda (x acc) (* x acc)) 1 (cons 2 (cons 3 empty)))",
  ),
  ["6"],
  "my-fold: product 2*3 -> 6",
);

checkExpect(
  rackette(
    "(define my-fold (lambda (proc bv alod)
    (cond
      ((empty? alod) bv)
      ((cons? alod) (proc (first alod) (my-fold proc bv (rest alod))))
    ))) (my-fold (lambda (x acc) (cons x acc)) empty (cons 1 (cons 2 empty)))",
  ),
  ["(list 1 2)"],
  "my-fold: reconstruct list using cons -> (list 1 2)",
);

checkExpect(
  rackette(
    "(define my-fold (lambda (proc bv alod)
    (cond
      ((empty? alod) bv)
      ((cons? alod) (proc (first alod) (my-fold proc bv (rest alod))))
    ))) (my-fold (lambda (x acc) (+ x acc)) 42 empty)",
  ),
  ["42"],
  "my-fold: fold over empty returns base value",
);

/* flip */
checkExpect(
  rackette(
    "(define flip (lambda (alop)
    (cond
      ((empty? alop) empty)
      ((cons? alop)
        (cons (cons (first (rest (first alop))) 
        (cons (first (first alop)) empty))
              (flip (rest alop))))
    ))) (flip (cons (cons 1 (cons 2 empty)) 
    (cons (cons 3 (cons 4 empty)) empty)))",
  ),
  ["(list (list 2 1) (list 4 3))"],
  "flip: flip two pairs -> ((2 1) (4 3))",
);

checkExpect(
  rackette(
    "(define flip (lambda (alop)
    (cond
      ((empty? alop) empty)
      ((cons? alop)
        (cons (cons (first (rest (first alop))) 
        (cons (first (first alop)) empty))
              (flip (rest alop))))
    ))) (flip (cons (cons 5 (cons 6 empty)) empty))",
  ),
  ["(list (list 6 5))"],
  "flip: single pair -> ((6 5))",
);

checkExpect(
  rackette(
    "(define flip (lambda (alop)
    (cond
      ((empty? alop) empty)
      ((cons? alop)
        (cons (cons (first (rest (first alop))) 
        (cons (first (first alop)) empty))
              (flip (rest alop))))
    ))) (flip empty)",
  ),
  ["empty"],
  "flip: empty input -> empty",
);

/* find */

checkExpect(
  rackette(
    "(define find (lambda (pred alod) (cond ((empty? alod) empty) ((cons? alod)
    (if (pred (first alod)) (first alod) (find pred (rest alod))))))) 
    (find (lambda (x) true) empty)",
  ),
  ["empty"],
  "find: empty list -> empty",
);

checkExpect(
  rackette(
    "(define find (lambda (pred alod) (cond ((empty? alod) empty) ((cons? alod) 
    (if (pred (first alod)) (first alod) (find pred (rest alod))))))) 
    (find (lambda (x) (> x 2)) (cons 3 (cons 1 empty)))",
  ),
  ["3"],
  "find: predicate true on first element -> returns first",
);

checkExpect(
  rackette(
    "(define find (lambda (pred alod) (cond ((empty? alod) empty) 
    ((cons? alod) (if (pred (first alod)) (first alod) 
    (find pred (rest alod))))))) (find (lambda (x) (= x 2)) 
    (cons 1 (cons 2 (cons 3 empty))))",
  ),
  ["2"],
  "find: predicate true on later element -> returns that element",
);
