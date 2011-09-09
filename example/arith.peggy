-- Simple Arithmetic Expression Parser

expr = expr "+" fact { $1 + $2 }
     / expr "-" fact { $1 - $2 }
     / fact

fact = fact "*" term { $1 * $2 }
     / fact "/" term { $1 / $2 }
     / term

term = "(" expr ")"
     / number

number = [1-9] [0-9]* { read ($1 : $2) }

-- alnum = [a-zA-Z0-9]
