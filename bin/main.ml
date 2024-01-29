let binary_of_name name =
  match name with
  | "+"  -> Float.add
  | "-"  -> Float.sub
  | "x"  -> Float.mul
  | "/"  -> Float.div
  | "xx" -> Float.pow
  | _ -> raise (Invalid_argument name)
;;

let rec calc stack args =
  match args with
  | [] -> if List.length stack > 1 then
            raise (Failure "Too many arguments!!")
          else if List.length stack < 1 then
            raise (Failure "Provide some arguments!!")
          else
            print_endline (string_of_float (List.hd stack))
  | arg :: unprocd -> (match stack with
                       | first::second::rest -> (try
                                                   let res = (binary_of_name arg) second first
                                                   in calc (res::rest) unprocd
                                                 with
                                                 | Invalid_argument _ ->
                                                    let num = float_of_string arg
                                                    in calc (num::stack) unprocd
                                                ) 
                       | _ -> let num = float_of_string arg
                              in calc (num::stack) unprocd
                      );;

calc [] (List.tl (Array.to_list Sys.argv));;
