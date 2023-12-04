(* 
    Input grammar:
        S -> Game <num>: A
        A -> B
        A -> B, A
        A -> B; A
        B -> <num> T
        T -> green
        T -> blue
        T -> red

    The input language is regular, but I am too lazy to specify it using a regular grammar.
*)

type cube =
| Red of int
| Green of int
| Blue of int

let make_cube color_type count =
    match color_type with
    | "red" -> Red(count)
    | "green" -> Green(count)
    | "blue" -> Blue(count)
    | _ -> raise (Invalid_argument ("Invalid color type " ^ color_type))

let get_or_else or_else opt =
    match opt with
    | Some(v) -> v
    | None -> or_else ()

let read_line_opt a = 
    try
        Some (read_line a)
    with
        End_of_file -> None 

let fold_lines func =
    let rec fold_lines_rec prev =
        match read_line_opt () with
        | Some(line) ->
            let curr = func prev line in
            fold_lines_rec curr 
        | None -> prev
    in
    fold_lines_rec

let str_tail str =
    let length = String.length str in
        match length with
        | 0 -> ""
        | 1 -> ""
        | _ -> String.sub str 1 (length - 1)

let parse_str_opt token str =
    if String.starts_with ~prefix:token str
        then 
            let token_length = (String.length token) in
            Some(String.sub str token_length ((String.length str) - token_length))
        else None

let parse_str token str =
    match parse_str_opt token str with
        | Some(str) -> str
        | None -> raise (Invalid_argument ("Could not parse token '" ^ token ^ "'" ^ "in string: '" ^ str ^ "'"))

let int_of_digit c =
    (int_of_char c) - 48

let parse_num =
    let rec parse_num_rec res str = 
        if str = ""
            then (res, str)
        else
            let digit = str.[0] in
            match digit with
            | '0' .. '9' -> parse_num_rec (res * 10 + (int_of_digit digit)) (str_tail str)
            | _ -> (res, str)
    in
    parse_num_rec 0

let parse_game_id str =
    let game_id, remaining_str = parse_str "Game " str |> parse_num in
    let cube_str = parse_str ": " remaining_str in
    (game_id, cube_str)

let parse_cube_type cube_type str =
     parse_str_opt cube_type str
     |> (Option.map (fun remaining_str -> ((make_cube cube_type), remaining_str)))
    
    
let parse_cube_types str =
    parse_cube_type "red" str
        |> get_or_else (fun _ ->
            parse_cube_type "green" str
            |> get_or_else (fun _ ->
                match parse_cube_type "blue" str with
                | Some(res) -> res
                | None -> raise (Invalid_argument ("Could not parse cube type: '" ^ str ^ "'"))
                )
        )
    

let parse_cube str = 
    let count, remaining_str = parse_num str in
    let make_cube, remaining_str = parse_str " "  remaining_str |> parse_cube_types in
    (make_cube (count), remaining_str)

let parse_cube_separator str = 
    parse_str_opt ", " str
    |> get_or_else (fun _ ->
        match parse_str_opt "; " str with
        | Some(str) -> str
        | None -> str
    )

let parse_cubes =
    let rec parse_cubes_rec cubes str =
        if str = "" then cubes
        else
            let cube, remaining_str = parse_cube_separator str |> parse_cube in
            parse_cubes_rec (cube :: cubes) remaining_str
    in parse_cubes_rec []

let parse_line str = 
    let game_id, str = parse_game_id str in
    let cubes = parse_cubes str in
    (game_id, cubes)

let red_count = 12
let green_count = 13
let blue_count = 14

let answer = fold_lines (fun acc line ->
    let game_id, cubes = parse_line line in
    let all_cubes_satisfy = List.for_all (fun cube ->
        match cube with
        | Red(count) -> count <= red_count
        | Green(count) -> count <= green_count
        | Blue(count) -> count <= blue_count
    ) cubes in

    if all_cubes_satisfy then acc + game_id
    else acc
) 0
            
let () = print_endline (string_of_int answer)