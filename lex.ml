(*lexer*)

type token = CID of string | VID of string | NUM of string
             | TO | IS | QUIT | OPEN | EOF | ONE of char
module P = Printf

exception End_of_system

let _ISTREAM = ref stdin

let ch = ref []
let read () = match !ch with [] -> input_char !_ISTREAM
| h::rest -> (ch := rest; h)
let unread c = ch := c::!ch
let lookahead () = try let c = read () in unread c; c with End_of_file -> '$'

(*let rec integer i =
    let c = lookahead () in
      if (c>='0' && c<='9') then integer (10*i+(Char.compare (read()) '0'))
      else i*)


let rec integer i =
   let c = lookahead () in
      if (c>='0' && c<='9') then integer (i^(Char.escaped (read ())))
      else i

and identifier id =
let  c = lookahead () in
if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
(c >= '0' && c <= '9') || c == '_') then
identifier (id^(Char.escaped (read ())))
else id

(*native lexer*)
and native_token () =
   let c = lookahead () in
      if (c>='a' && c<='z') then
      let id = identifier "" in
         match id with
           "is"   -> IS
         | "quit" -> QUIT
         | "open" -> OPEN
         | _      -> CID (id)
      else if (c>='A' && c<='Z') then VID(identifier "")
      else if (c >= '0' && c <= '9') then NUM (integer "")
      else if (c = ':') then
                if (read (); lookahead()) = '-' then let _ = read() in TO
                else ONE (':')
      else ONE (read())

(*skip blank*)
and gettoken () =
   try
       let token = native_token () in
             match token with
              ONE ' ' -> gettoken ()
            | ONE '\t' -> gettoken ()
            | ONE '\n' -> gettoken ()
            | _ -> token
 with End_of_file -> EOF

let print_token tk = match tk with (CID i) -> P.printf "CID(%s)" i | (VID i) -> P.printf "VID(%s)" i | (NUM i) -> P.printf "NUM(%s)" i | (TO) -> P.printf ":-" | (QUIT) -> P.printf "quit" | (OPEN) -> P.printf "open" | (IS) -> P.printf "is"
| (EOF) -> P.printf "eof" | (ONE c) -> P.printf "ONE(%c)" c

