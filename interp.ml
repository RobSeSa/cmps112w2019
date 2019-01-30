(* $Id: interp.ml,v 1.6 2019-01-24 13:15:38-08 - - $ *)

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Absyn.Number number -> number
    | Absyn.Memref memref -> (match memref with 
        | Absyn.Variable ident -> Hashtbl.find 
                                  Tables.variable_table ident
        | Absyn.Arrayref (ident, expr) ->
            Array.get (Hashtbl.find Tables.array_table ident) 
                      (int_of_float (eval_expr expr)))
    | Absyn.Unary (oper, expr) -> 
        (Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr)
    | Absyn.Binary (oper, expr1, expr2) -> 
        (Hashtbl.find Tables.binary_fn_table oper) 
        (eval_expr expr1) (eval_expr expr2)

let interp_goto (label : Absyn.label) =
    Hashtbl.find Tables.label_table label

let interp_if ((expr : Absyn.expr), (label : Absyn.label)) = 
    match (eval_expr expr) with
        | 0. -> None
        | 1. -> interp_goto label

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_char ' ';
         match item with
         | Absyn.String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Absyn.Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number (memref : Absyn.memref) =
        try let number = Etc.read_number ()
            in match memref with
            | Absyn.Variable ident -> 
                Hashtbl.add Tables.variable_table ident number
            | Absyn.Arrayref (ident, expr) -> 
                Hashtbl.add Tables.variable_table ident (eval_expr expr)
        with End_of_file -> 
             (Hashtbl.replace Tables.variable_table "eof" 1.)
    in List.iter input_number memref_list

let rec interp_stmt (stmt : Absyn.stmt) = match stmt with
    | Absyn.Dim (ident, expr) -> unimpl "Dim (ident, expr)"
    | Absyn.Let (memref, expr) -> unimpl "Let (memref, expr)"
    | Absyn.Goto label -> interp_goto label
    | Absyn.If (expr, label) -> interp_if (expr, label)
    | Absyn.Print print_list -> interp_print print_list
    | Absyn.Input memref_list -> interp_input memref_list

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> (let next_line = interp_stmt stmt
                            in match next_line with
                            | None -> interpret otherlines
                            | Some program -> interpret program)

let interpret_program program =
    (Tables.init_label_table program; 
     interpret program)

