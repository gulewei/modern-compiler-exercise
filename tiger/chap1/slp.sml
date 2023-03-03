type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp
(* a := 5+3; b := (print(a, a-1), 10*a); print(b) *)
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))


(* 1. Write an ML function (maxargs : stm^int) that tells the maximum
number of arguments of any print statement within any subexpression of
a given statement. For example, maxargs (prog) is 2. *)
fun maxargs (stm) = 
    let
        fun max (a, b) = if a > b then a else b
        fun searchExp (exps) =
            let
                fun f ([], stms) = stms
                  | f (EseqExp(stm, exp) :: exps, stms) = f(exp :: exps, stm :: stms)
                  | f (OpExp(left, bin, right) :: exps, stms) = f(left :: left :: exps, stms)
                  | f ((NumExp _) :: exps, stms) = f(exps, stms)
                  | f ((IdExp _) :: exps, stms) = f(exps, stms)
            in
                f(exps, [])
            end
        fun f ([], n) = n
          | f (CompoundStm(s1, s2) :: stms, n) = f(s1 :: s2 :: stms, n)
          | f (AssignStm(id, exp) :: stms, n) = f(searchExp([exp]) @ stms, n)
          | f ((PrintStm exps) :: stms, n) = f(searchExp(exps) @ stms, max(n, length(exps)))
    in
        f([stm], 0)
    end


(* 2. Write an ML function interp : stm^unit that "interprets" a program
in this language. To write in a "functional" style -
without assignment (: =) or
arrays -
maintain a list of (variable,integer) pairs, and produce new versions
of this list at each As s ignS tm. *)
fun interprets (stm) =
    let
        val hint = " >>> "
        fun value2string (value) = hint ^ Int.toString(value)
        fun table2string (table) =
            let
                fun f ([], text) = hint ^ "table: " ^ text
                  | f ((id: string, value: int)::table, text) = f(table, "(" ^ id ^ ", " ^ Int.toString(value) ^ "); " ^ text)
            in
                f(table, "")
            end
        fun update (table, id, value) = (id, value) :: table
        fun lookup ([], id) = NONE
          | lookup ((n, v) :: table, id) = if n = id then SOME(v) else lookup(table, id)
        fun opEval (Plus, a, b) = a + b
          | opEval (Minus, a, b) = a - b
          | opEval (Times, a, b) = a * b
          | opEval (Div, a, b) = a div b (* TODO float point number *)
        fun interpStm (stm, table) =
            let
                fun interpExp (IdExp id, table) = (getOpt(lookup(table, id), 0), table) (* TODO raise an exception instead of default to 0 *)
                  | interpExp (NumExp n, table) = (n, table)
                  | interpExp (EseqExp(stm, exp), table) = interpExp(exp, interpStm(stm, table))
                  | interpExp (OpExp(left, bin, right), table) =
                    let
                        val (vl, t1) = interpExp(left, table)
                        val (vr, t2) = interpExp(right, t1)
                    in
                        (opEval(bin, vl, vr), t2)
                    end
                fun expsEval ([], (values, table)) = (values, table)
                  | expsEval (exp :: exps, (values, table)) =
                    let
                        val (v, t) = interpExp(exp, table)
                    in
                        expsEval(exps, (values @ [v], t))
                    end
            in
                case stm of
                    CompoundStm(stm1, stm2) => interpStm(stm2, interpStm(stm1, table))
                  | AssignStm(id, exp) =>
                    let
                        val (v, t) = interpExp(exp, table)
                    in
                        update(t, id, v)
                    end
                  | PrintStm(exps) =>
                    let
                        val (values, table) = expsEval(exps, ([], table))
                    in
                        table before app((print o value2string))(values)
                    end
            end
    in 
        print(table2string(interpStm(stm, [])))
    end

(* tests *)
val test1 = maxargs(prog)
val test2 = interprets(prog)