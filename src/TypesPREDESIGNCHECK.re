type rawProgram = string; //"(define x 3) (+ x 5)"
type concreteProgramPiece =
  | NumberC(int) //NumberC(5)
  | SymbolC(string) //SymbolC("+")
  | ListC(list(concreteProgramPiece)); 
    //ListC([SymbolC("+"), SymbolC("x"), NumberC(5)])
type concreteProgram = list(concreteProgramPiece);
//[ListC([SymbolC("define"), SymbolC("x"), NumberC(3)]),
// ListC([SymbolC("+"), SymbolC("x"), NumberC(5)])]

/* a Rackette name */
type name =
  | Name(string); // Name("x")
/* a Rackette expression */
type expression =
  | NumE(int) // NumE(5)
  | BoolE(bool) // BoolE(true)
  | EmptyE // EmptyE
  | NameE(name) // NameE(Name("x"))
  | AndE(expression, expression) 
    /* AndE(ApplicationE([NameE(Name(">")), NameE(Name("x")), NumE(5)],
    BoolE(true)))
    "(and (> x 5) True)" */
  | OrE(expression, expression)
    /* OrE(ApplicationE([NameE(Name("=")), NameE(Name("x")), NumE(2)]), 
    ApplicationE([NameE(Name(">")), NameE(Name("x")), NumE(2)]) */
  | IfE(ifData)
    /* IfE({booleanExpr: ApplicationE([NameE(Name(">")), NameE(Name("x")), 
    NumE(5)]), resultTExpr: ApplicationE([NameE(Name("+")), NameE(Name("x")), 
    NumE(1)]), resultFExpr: NameE(Name("x"))}) */
  | CondE(list(condData))
    /* CondE(conditionExpr: ApplicationE(NameE(Name("<")), NameE(Name("x")), 
    NumE(5)), resultExpr: ApplicationE(NameE(Name("+")), NumE(3), 
    NumE(2))) */
  | LambdaE(lambdaData)
    /* LambdaE(nameList: list(Name("x"), Name("y")), 
    lambdaBody: ApplicationE(NameE("*"), NumE("x"), numE("y"))) */
  | LetE(letData)
    /* LetE(letPairs: list(Name()))
    */
  | ApplicationE(list(expression))
  and ifData = { 
    booleanExpr: expression, /* booleanExpr: ApplicationE([NameE(Name(">")), 
    NameE(Name("x")), NumE(5)]) */
    resultTExpr: expression, /* trueExpr: ApplicationE([NameE(Name("+")), 
    NameE(Name("x")), NumE(1)]) */
    resultFExpr: expression, // falseExpr: NameE(Name("x"))
  } // for example: "(if (> x 5) (+ x 1) x)"

  and condData = { 
    conditionExpr: expression, 
    resultExpr: expression,
  }
  and lambdaData = {
    nameList: list(name),
    lambdaBody: expression,
  } 
  and letPair = {
    pairName: name, 
    pairExpr: expression,  
  }
  and letData = {
    letPairs: list(letPair),
    letBody: expression, 
  };
  
/* a Rackette definition */


type definition = (name, expression);
/* a piece of Rackette that can be processed:
 * either a definition or an expression 
 * Ex. (Name(x), NumE(5)) 
 * Ex 2. (Name(f), AndE(ApplicationE([NameE(Name(">")), NameE(Name("x")), 
 * NumE(5)], BoolE(true))))*/
type abstractProgramPiece = // Definition(Name("x"), NumE(5))
  | Definition(definition)
  | Expression(expression);
/* a representation of a Rackette program -
 * any number of pieces 
 * Ex. Definition((Name(x), NumE(5)))
 * Ex2. Expression(NumE(5)) */
type abstractProgram = list(abstractProgramPiece);
/* a Rackette value: the result of evaluating a Rackette expression 
 * Ex. [Definition((Name(x), NumE(5))), Expression(NameE(5))*/
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
/* Ex. NumV(5)
 * BuiltinV({printedRep: "builtIn: cons", bProc: cons})
 * ClosureV({cNameList: [Name("x")], cExpr: NamE(Name("x")), 
 * cEnv: [*initialTLE*]})
 * */
  and builtinData = { 
    printedRep: string,
    bProc: list(value) => value,
  }
  and closureData = {
    cNameList: list(name),
    cExpr: expression, 
    cEnv: environment,
  }
  /* Environments, bindingLists, and bindings aren't values
     But we use "and" here so closures have access to environments,
     bindings have access to values, etc. */
  and environment = (list(bindingList))
  and bindingList = (list(binding))
  and binding = (name, value);
