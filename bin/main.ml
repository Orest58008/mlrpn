let binary_ops = Hashtbl.create 5;; (* Please increase when adding new operations *)
let open Hashtbl in                 (* It isn't critical, but it speeds things up *)
    add binary_ops "+"  Float.add;
    add binary_ops "-"  Float.sub;
    add binary_ops "x"  Float.mul;
    add binary_ops "/"  Float.div;
    add binary_ops "xx" Float.pow;
;;

let unary_ops = Hashtbl.create 3;; (* Please increase when adding new operations *)
let open Hashtbl in                (* It isn't critical, but it speeds things up *)
    add unary_ops "sin" Float.sin;
    add unary_ops "cos" Float.cos;
    add unary_ops "tan" Float.tan;
;;

let tbl_contains tbl key =
  match Hashtbl.find_opt tbl key with
  | None -> false | Some _ -> true ;;

let is_float str =
  match float_of_string_opt str with
  | None -> false | Some _ -> true ;;

let rec calc stack args =
  match args with
  | [] -> if List.length stack > 1 then
            raise (Failure "Too many arguments!!")
          else if List.length stack < 1 then
            raise (Failure "Provide some arguments!!")
          else
            print_endline (string_of_float (List.hd stack))
  | arg::unprocd when tbl_contains binary_ops arg ->
     (match stack with
      | second::first::rest -> let res = ((Hashtbl.find binary_ops arg) first second) in
                               calc (res::rest) unprocd
      | _ -> raise (Failure "Provide more arguments!!"))
  | arg::unprocd when tbl_contains unary_ops arg ->
     (match stack with
      | first::rest -> let res = ((Hashtbl.find unary_ops arg) first) in
                               calc (res::rest) unprocd
      | _ -> raise (Failure "Provide more arguments!!"))
  | arg::unprocd when is_float arg -> calc ((float_of_string arg)::stack) unprocd
  | arg::_ -> raise (Invalid_argument arg)
;;

calc [] (List.tl (Array.to_list Sys.argv));;
