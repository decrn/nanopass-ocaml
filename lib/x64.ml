open Sexplib.Std

type triv = 
  [ `Reg of String
  | `Int of int ]

type x64 =
  [ `Mov of triv * triv
  | `Add of triv * triv
  | `Imul of triv * triv ]

type program =
  [ `Seq of program * program
  | `Instr of x64 ]
