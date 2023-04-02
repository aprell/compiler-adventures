%{
  open Program

  let register_of_char = function
    | 'w' -> W
    | 'x' -> X
    | 'y' -> Y
    | 'z' -> Z
    | _ -> invalid_arg "register_of_char"
%}

%token <int> LITERAL
%token <char> REGISTER
%token INP ADD MUL DIV MOD EQL
%token EOF

%type <Program.instruction list> program
%type <Program.instruction> instruction
%type <Program.operand> operand
%type <Program.register> register

%start program

%%

program:
  | instruction* EOF { $1 }
  ;

instruction:
  | INP register { Inp $2 }
  | ADD register operand { Add ($2, $3) }
  | MUL register operand { Mul ($2, $3) }
  | DIV register operand { Div ($2, $3) }
  | MOD register operand { Mod ($2, $3) }
  | EQL register operand { Eql ($2, $3) }
  ;

operand:
  | register { Register $1 }
  | LITERAL { Literal $1 }
  ;

register:
  | REGISTER { register_of_char $1 }
  ;
