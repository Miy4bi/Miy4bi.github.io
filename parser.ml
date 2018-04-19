module L = Lex
module E = Engine

let revTok = ref ([] : L.token list)
let getToken () = match !revTok with
[] -> L.gettoken ()
| h::tl -> (revTok := tl; h)
(*if (!revTok = []) then L.gettoken ()
else let (h::tl) = !revTok in (revTok := tl; h)*)
let tok = ref (L.ONE ' ')
let revToken t = (revTok := (!tok)::(!revTok); tok := t)
let advance () = (tok := getToken(); L.print_token (!tok))
exception Syntax_error
let error () = raise Syntax_error
let check t = match !tok with
 L.CID _ -> if (t = (L.CID "")) then () else error()
| L.VID _ -> if (t = (L.VID "")) then () else error()
| L.NUM _ -> if (t = (L.NUM "")) then () else error()
| tk -> if (tk=t) then () else error()

let eat t = (check t; advance())

let prog = ref [[E.Var ""]]

let rec clauses() = match !tok with
L.EOF -> []
| _ -> (let c = clause() and d = clauses() in c::d)

and clause() = match !tok with
L.ONE '('  -> (let c =term() in eat(L.ONE '.'); [c])
| _ -> (let c = predicate() and d = to_opt() in eat(L.ONE '.');c::d)

and to_opt () = match !tok with
                L.TO -> (eat(L.TO); let c = terms() in c)
               | _ -> []

and command () = match !tok with
                L.QUIT -> exit 0
               | L.OPEN -> (eat(L.OPEN);
                  match !tok with
                    L.CID s -> (eat(L.CID ""); check (L.ONE '.');
                     L._ISTREAM := open_in (s^".pl");
                     advance(); 
                     prog := clauses(); close_in (!L._ISTREAM))
                  | _       -> error())
               |_       -> let t = term() in
                               (check(L.ONE '.');
                                 let _ = E.eval(!prog,t) in ())

and term () = match !tok with L.ONE('(') ->
                               (eat(L.ONE('('));let c = term() in 
                                     eat(L.ONE(')'));c)
                            |  L.CID s -> (tok:=getToken();match !tok with
                                    L.ONE ')' -> error()
                  | _       -> (revToken(L.CID s);let c =  predicate() in c))

and terms () = (let c = term() in let d = terms'() in [c]@d)
and terms' () = match !tok with L.ONE(',') -> (eat(L.ONE(','));let c = term()
                                               in let d = terms'() in [c]@d)
                              |  _ -> []

and predicate () = (let c = funname() in eat(L.ONE('('));let d = args() in 
                          eat(L.ONE(')'));E.App(c,d))

and args() = (let c = expr() in let d = args'() in [c]@d)
and args'() = match !tok with
               L.ONE ',' -> (eat(L.ONE ',');let c = expr() in let d =  args'() in
                                    [c]@d)
             | _ -> []

and expr() = match !tok with
                   L.ONE '(' -> expr_non_term()
                 | L.ONE '[' -> expr_non_term()
                 | L.VID  _   -> id()
                 | L.NUM  _   -> id()
                 | L.CID s   -> (try term() with
                                     Syntax_error -> (revToken (L.CID s); id()))
                 | _ -> term()


and expr_non_term () = match !tok with
             L.ONE('(') -> (eat(L.ONE('('));let c = expr_non_term() in
                            eat(L.ONE(')'));c)
           | _  -> (eat(L.ONE(('[')));let c = list() in eat(L.ONE(']'));c)

and list() = match !tok with
                           L.ONE ']' -> E.Atom "nil"
                         | _ -> (let c = expr() and d = list_opt() in
                                  E.App("cons",[c;d]))
and list_opt () = match !tok with
                   L.ONE '|' -> (eat(L.ONE '|'); id())
                 | L.ONE ',' -> (eat(L.ONE ','); list())
                 | _ -> E.Atom "nil"

and id () =  match !tok with L.CID s -> (eat(L.CID "");E.Atom s)
                           | L.VID s -> (eat(L.VID "");E.Var s)
                           | L.NUM s -> (eat(L.NUM(""));E.Atom s)

and funname () = match !tok with L.CID s -> (eat(L.CID "");s)
                              





let rec run() =
          print_string "?- ";
          while true do
          flush stdout; L._ISTREAM := stdin;
          advance();command();print_string "\n?- "
  done


let _ = run()
