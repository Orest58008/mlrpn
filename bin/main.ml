let rec calc stack args =
  match args with
  | [] -> if List.length stack > 1 then
            raise (Failure "Too many arguments!!")
          else if List.length stack < 1 then
            raise (Failure "Provide some arguments!!")
          else
            print_endline (string_of_float (List.hd stack))
  | arg :: unprocd -> match arg with
                      | "+" -> (match stack with
                                | first :: second :: rest ->
                                   calc ((Float.add first second) :: rest) unprocd
                                | _ -> raise (Failure "Provide more arguments!!"))
                      | other -> (match float_of_string_opt other with
                                  | None -> raise (Invalid_argument other)
                                  | Some num -> calc (num :: stack) unprocd)
;;

calc [] (List.tl (Array.to_list Sys.argv));;
