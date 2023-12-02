let digit_strings = [|
    "zero";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
    "0";
    "1";
    "2";
    "3";
    "4";
    "5";
    "6";
    "7";
    "8";
    "9";
|];;

let digit_value digit = match digit with
    | "zero" -> 0
    | "one" ->  1
    | "two" ->  2
    | "three" ->  3
    | "four" ->  4
    | "five" ->  5
    | "six" ->  6
    | "seven" ->  7
    | "eight" ->  8
    | "nine" ->  9
    | "0" -> 0
    | "1" -> 1
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | "5" -> 5
    | "6" -> 6
    | "7" -> 7
    | "8" -> 8
    | "9" -> 9
    | _ -> raise (Invalid_argument digit);;

let rec fold_lines prev func =
    try
        let curr = func prev (read_line ()) in
            fold_lines curr func
    with
        End_of_file -> prev;;

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
        | 0 -> ""
        | 1 -> ""
        | _ -> String.sub str 1 (length - 1);;

let str_rtail str =
    let length = String.length str in

    match length with
    | 0 -> ""
    | 1 -> ""
    | _ -> String.sub str 0 (length - 1);;

let rec str_starts_with str to_match =
    if to_match = ""
        then true
    else
        match ((first_char str), (first_char to_match)) with
        | ((Some a), (Some b)) -> (a = b) && str_starts_with (str_tail str) (str_tail to_match)
        | _ -> false;;

let rec str_ends_with str to_match =
    if to_match = ""
        then true
    else
        match ((last_char str), (last_char to_match)) with
        | ((Some a), (Some b)) -> (a = b) && str_ends_with (str_rtail str) (str_rtail to_match)
        | _ -> false;;

let get_digit_from_beginning str =
    Array.find_map (fun digit_string ->
        if str_starts_with str digit_string
            then Some (digit_value digit_string)
            else None
    ) digit_strings;;

let get_digit_from_end str =
    Array.find_map (fun digit_string ->
        if str_ends_with str digit_string
            then Some (digit_value digit_string)
            else None
    ) digit_strings;;

let rec find_first_digit str = 
    if str = "" then 0
    else
        match get_digit_from_beginning str with
        | Some value -> value
        | None -> find_first_digit (str_tail str);;

let rec find_last_digit str =
        if str = "" then 0
        else
            match get_digit_from_end str with
            | Some value -> value
            | None -> find_last_digit (str_rtail str);;

let calibration_value str =
    let first_digit = find_first_digit str in
    let last_digit = find_last_digit str in
    first_digit * 10 + last_digit;;

let sum_of_calibration_values = fold_lines 0 (fun acc line -> acc + (calibration_value line));;

print_int sum_of_calibration_values;;
print_newline ();;