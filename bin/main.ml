let binary_of_name name =
  match name with | "+" -> ( +.) | "-" -> ( -.) | "x" -> ( *.) | "/" -> ( /.)
                  | "xx" -> ( ** )
                  | _ -> raise Not_found
let unary_of_name name =
  match name with | "sin" -> sin | "cos" -> cos | "tan" -> tan
                  | "sqrt" -> sqrt | "cbrt" -> Float.cbrt
                  | _ -> raise Not_found

let stack : float Stack.t = Stack.create ()

let calc arg : unit =
  try let op = binary_of_name arg in
      let y = Stack.pop stack in
      let x = Stack.pop stack in
      Stack.push (op x y) stack
  with
  | Stack.Empty -> raise (Failure "Provide more arguments")
  | Not_found ->
     try let op = unary_of_name arg in
         let x = Stack.pop stack in
         Stack.push (op x) stack
     with
     | Stack.Empty -> raise (Failure "Provide more arguments")
     | Not_found -> try Stack.push (float_of_string arg) stack
                    with Failure _ -> raise (Failure ("Invalid argument " ^ arg))

let () = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.iter calc
let () = if Stack.length stack > 1 then raise (Failure "Too many arguments")
         else Stack.pop stack |> print_float |> print_newline
