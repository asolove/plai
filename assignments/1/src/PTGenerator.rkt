#lang racket

'((program -> (definition* program))
  (definition* -> () (definition definition*))
  (definition -> defvar deffun)
  (defvar -> ("defvar " id " = " expression " in "))
  (deffun -> ("deffun " id "(" params ")" fnbody " in "))
  (id -> "a" "b" "c" "d" "e" "f")
  (program -> (" { " statement* " } "))
  (statement* -> () (statement statement*))
  (statement -> (statementPart ";"))
  (statementPart -> id ifStatement funCall)
  (ifStatement -> ("if " statementPart " then " statementPart " else " statementPart))
  (funCall -> (funVal "(" arguments ")"))
  (funVal -> lambdaExpression id objProp)
  (objProp -> 
  