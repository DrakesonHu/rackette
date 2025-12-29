Rackette: A Racket Interpreter in ReasonML

A functional programming language interpreter implementing a subset of Racket, 
built in ReasonML for Brown University's CSCI 0170 (Computer Science: an Integrated Introduction), fall 2025.

================================================================================
ARCHITECTURE
================================================================================

The interpreter implements a three-stage pipeline:

1. Reading (Read.re) - Converts raw program strings into concrete programs 
   (lists of concrete program pieces)

2. Parsing (Rackette.re) - Transforms concrete programs into abstract programs, 
   recognizing special forms (if, cond, lambda, let, and, or)

3. Evaluation (Rackette.re) - Processes abstract programs using environment-based 
   evaluation with lexical scoping and closures

================================================================================
FEATURES
================================================================================

Core Language Constructs:
- First-class functions with closure semantics
- Lexical scoping via environments  
- Conditionals (if, cond) and local bindings (let)
- Built-in procedures: arithmetic, comparison, list operations, boolean logic

Type System:
Values: NumV, BoolV, ListV, ClosureV (user-defined procedures), BuiltinV (built-ins)

================================================================================
IMPLEMENTATION
================================================================================

Environments:
Separate top-level environment (TLE) for global definitions and local environment 
for function scopes. Closures capture their defining environment for proper lexical 
scoping.

Closure Structure:
  type closureData = {
    cNameList: list(name),    // Formal arguments
    cExpr: expression,        // Body expression
    cEnv: environment,        // Captured environment
  }

Environment Structure:
  type environment = list(bindingList)
  type bindingList = list(binding)
  type binding = (name, value)

================================================================================
EXAMPLE
================================================================================

;; Higher-order function with closure
(define make-adder
  (lambda (n)
    (lambda (x) (+ x n))))

(define add5 (make-adder 5))
(add5 10)  ;; Returns 15

================================================================================
KEY LEARNINGS
================================================================================

- Environment structure, scope interactions with recursion.
- Proper closure semantics (capture at definition time, not call time)
- Pattern matching for processing concrete and abstract programs

================================================================================

Course: CSCI 0170 Computer Science: an Integrated Introduction, Brown University
Implementation: ReasonML