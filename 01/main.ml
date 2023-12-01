let rec fold_lines prev func = 
    try
        let curr = func prev (read_line ()) in
            fold_lines curr func
    with
        End_of_file -> prev;;

let is_digit char =
    match char with    
    | '0' .. '9' -> true
    | _ -> false;;

let first_char str =
    match str with
    | "" -> None
    | _ -> Some (str.[0]);;

let last_char str =
    let length = String.length str in
        match length with
        | 0 -> None
        | _ -> Some str.[length - 1];;

let str_tail str =
    let length = String.length str in
        match length with
        | 0 -> None
        | 1 -> None
        | _ -> Some (String.sub str 1 (length - 1));;

let str_rtail str =
    let length = String.length str in
        match length with
        | 0 -> None
        | 1 -> None
        | _ -> Some (String.sub str 0 (length - 1));;


let rec str_find str func =
    match first_char str with
    | None -> None
    | Some ch ->
        if func ch
            then Some ch
            else match str_tail str with
            | Some tail -> str_find tail func
            | None -> None;;

let rec str_rfind str func =
    match last_char str with
    | None -> None
    | Some ch ->
        if func ch
            then Some ch
            else match str_rtail str with
            | Some tail -> str_rfind tail func
            | None -> None;;

let int_of_digit digit = (int_of_char digit) - 48;;

let first_digit str = str_find str is_digit;;

let last_digit str = str_rfind str is_digit;;

let combine_digits str = match ((first_digit str), (last_digit str)) with
    | (Some(first), Some(last)) -> int_of_string ((String.make 1 first) ^ (String.make 1 last))
    | _ -> 0;;

let sum_of_calibration_values = fold_lines 0 (fun acc line -> acc + (combine_digits line));;

print_int sum_of_calibration_values;;
print_newline ();;