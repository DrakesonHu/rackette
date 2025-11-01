open CS17SetupRackette;
open Read.Reader;
open Types;

/* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? */
let plus: list(value) => value = alon =>
switch (alon) {
  | [NumV(a), NumV(b)] => NumV(a + b)
  | _ => failwith("'+' expects two number values")
};

let minus: list(value) => value = alon =>
switch (alon) {
  | [NumV(a), NumV(b)] => NumV(a - b)
  | _ => failwith("'-' expects two number values")
};

let mult: list(value) => value = alon =>
switch (alon) {
  | [NumV(a), NumV(b)] => NumV(a * b)
  | _ => failwith("'*' expects two number values")
};

let div: list(value) => value = alon =>
switch (alon) {
  | [NumV(a), NumV(b)] => NumV(a / b)
  | _ => failwith("'/' expects two number values")
};

let rem: list(value) => value = alon =>
switch (alon) {
  | [NumV(a), NumV(b)] => NumV(a - (b * (a / b)))
  | _ => failwith("'remainder' expects two number values")
};

let numEq: list(value) => value = alon =>
switch (alon) {
  | [NumV(a), NumV(b)] => BoolV(a == b)
  | _ => failwith("'=' expects two number values")
};

let lessThan: list(value) => value = alon => 
switch (alon) {
  | [NumV(a), NumV(b)] => BoolV(a < b)
  | _ => failwith("< expects two number values")
};

let moreThan: list(value) => value = alon => 
switch (alon) {
  | [NumV(a), NumV(b)] => BoolV(a > b)
  | _ => failwith("> expects two number values")
};

let lEq: list(value) => value = alon => 
switch (alon) {
  | [NumV(a), NumV(b)] => BoolV(a <= b)
  | _ => failwith("<= expects two number values")
};

let mEq: list(value) => value = alon => 
switch (alon) {
  | [NumV(a), NumV(b)] => BoolV(a >= b)
  | _ => failwith(">= expects two number values")
};

let isEq: list(value) => value = alod => 
switch (alod) {
  | [NumV(a), NumV(b)] => BoolV(a == b)
  | ([BoolV(a), BoolV(b)]) => BoolV(a == b)
  | ([ListV(a), ListV(b)]) => BoolV(a == b)
  | _ => failwith("equal? expects two values of the same type")
};

let isNum: list(value) => value = alod => 
switch (alod) {
  | [NumV(_num)] => BoolV(true)
  | [_hd] => BoolV(false)
  | _ => failwith("num? expects one value")
};

let isZero: list(value) => value = alon => 
switch (alon) {
  | [NumV(num)] =>
    if (num == 0) {
      BoolV(true)
    } else {
      BoolV(false)
    }
  | _ => failwith("zero? expects a number value")
};

let cons: list(value) => value = alod =>
switch (alod) {
  | [a, ListV(lst)] => ListV([a, ... lst])
  | _ => failwith("cons expects inputs of type 'a followed by List('a)")
};

let first: list(value) => value = alod =>
switch (alod) {
  | [ListV([hd, ... _tl])] => hd
  | _ => failwith("First expects a list with at least 1 element; arguments don't match")
};

let rest: list(value) => value = alod =>
switch (alod) {
  | [ListV([_hd, hd2, ... tl])] => ListV([hd2, ... tl])
  | _ => failwith("Rest expects a list with at least 2 elements; arguments don't match")
};

let isEmpty: list(value) => value = alod =>
switch (alod) {
  | [ListV([])] => BoolV(true)
  | [ListV([_hd, ... _tl])] => BoolV(false)
  | _ => failwith("empty? expects an argument of type list('a)")
};

let isCons: list(value) => value = lst =>
switch (lst) {
  | [ListV([])] => BoolV(false)
  | [ListV([_hd, ... _tl])] => BoolV(true)
  | _ => failwith("cons? expects an argument of type list('a)")
};

let not: list(value) => value = bool =>
switch (bool) {
  | [BoolV(exp)] => BoolV(!exp)
  | _ => failwith("not expects an argument of type bool")
};

let initialTle: environment = 
[[(Name("+"), 
  BuiltinV({printedRep: "builtIn: plus", bProc: plus})),
  (Name("-"), 
  BuiltinV({printedRep: "builtIn: minus", bProc: minus})),
  (Name("*"), 
  BuiltinV({printedRep: "builtIn: multiplication", bProc: mult})),
  (Name("/"), 
  BuiltinV({printedRep: "builtIn: division", bProc: div})),
  (Name("remainder"), BuiltinV({printedRep: "builtIn: remainder", bProc: rem})),
  (Name("="), 
  BuiltinV({printedRep: "builtIn: equalMath", bProc: numEq})),
  (Name("<"), 
  BuiltinV({printedRep: "builtIn: less than", bProc: lessThan})),
  (Name(">"), 
  BuiltinV({printedRep: "builtIn: more than", bProc: moreThan})),
  (Name("<="), 
  BuiltinV({printedRep: "builtIn: less than or equal", bProc: lEq})),
  (Name(">="), 
  BuiltinV({printedRep: "builtIn: more than or equal", bProc: mEq})),
  (Name("equal?"), 
  BuiltinV({printedRep: "builtIn: isEqual", bProc: isEq})),
  (Name("number?"), 
  BuiltinV({printedRep: "builtIn: isNum", bProc: isNum})),
  (Name("zero?"), 
  BuiltinV({printedRep: "builtIn: isZero", bProc: isZero})),
  (Name("cons"), 
  BuiltinV({printedRep: "builtIn: cons", bProc: cons})),
  (Name("first"), 
  BuiltinV({printedRep: "builtIn: first", bProc: first})),
  (Name("rest"), 
  BuiltinV({printedRep: "builtin: rest", bProc: rest})),
  (Name("empty?"), 
  BuiltinV({printedRep: "builtIn: isEmpty", bProc: isEmpty})),
  (Name("cons?"), 
  BuiltinV({printedRep: "builtIn: isCons", bProc: isCons})),
  (Name("not"), 
  BuiltinV({printedRep: "builtIn: not", bProc: not}))]];

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseExpression */
let rec parseExpression: concreteProgramPiece => expression =
  input => failwith("parseExpression is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseDefinition */
let parseDefinition: concreteProgramPiece => definition =
  input => failwith("parseDefinition is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parsePiece */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      failwith("definitions not yet parsed")
    | _ => failwith("expressions not yet parsed")
    };

/* TODO: write the header comment parts required by the Design Recipe
 * for parse */
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/* TODO: write the header comment parts required by the Design Recipe
 * and implement eval */
let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
    /* NOTE: tle is top level environment and env is local environment */
    failwith("eval is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe */
let addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) => failwith("addDefinition is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement stringOfValue*/
let rec stringOfValue: value => string =
  aValue => failwith("stringOfValue is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe */
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl) 
        /* TODO: replace this with correct definition-
                      handling code and a recursive call to processHelper */
        | [Expression(e), ...tl] => 
        [eval(tle, [], e), ... processHelper(tle, tl)]
        /* TODO: replace this with correct expression-
                      handling code and a recursive call to processHelper */
        };
    processHelper(initialTle, pieces);
  };

/* TODO: write the header comment parts required by the Design Recipe */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* TODO: Test Cases (we have included a few sample check-expects) */
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
