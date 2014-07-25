module Assembly where

type Literal = Integer

data Instr a
        = LDC Literal
        | LD Literal Literal
        | ADD
        | SUB
        | MUL
        | DIV
        | CEQ
        | CGT
        | CGTE
        | ATOM
        | CONS
        | CAR
        | CDR
        | SEL a a
        | JOIN
        | LDF 
        
