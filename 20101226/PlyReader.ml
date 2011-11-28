open List;;

type vertex = { x : float;
                y : float;
                z : float;
              };;
let vertex_x v = v.x;;
let vertex_y v = v.y;;
let vertex_z v = v.z;;

type face = { i : int;
              j : int;
              k : int;
            };;
let face_i f = f.i;;
let face_j f = f.j;;
let face_k f = f.k;;

let rec split_string c x =
  if String.contains x c
  then let ix = String.index x c in
       let part = String.sub x 0 ix in
       let rest = String.sub x (ix + 1) (String.length x - ix - 1) in
         part :: split_string c rest
  else [x];;

let element_vertex_p l =
  nth l 0 = "element" && nth l 1 = "vertex";;

let element_face_p l =
  nth l 0 = "element" && nth l 1 = "face";;

let end_header_p l =
  hd l = "end_header";;

let read_header ic =
  let nvert = ref 0 in
  let nface = ref 0 in
  let end_header = ref false in
    while not !end_header do
      let l = split_string ' ' (input_line ic) in
        if element_vertex_p l
        then nvert := int_of_string (nth l 2)
        else if element_face_p l
        then nface := int_of_string (nth l 2)
        else if end_header_p l
        then end_header := true
    done;
    print_endline "read_ply: read_header done";
    ( !nvert, !nface );;

let read_vertex ic nvert =
  let ret = Array.make nvert { x = 0.; y = 0.; z = 0.; } in
    for i = 0 to nvert - 1 do
      let l = split_string ' ' (input_line ic) in
        ret.(i) <- { x = float_of_string (nth l 0);
                     y = float_of_string (nth l 1);
                     z = float_of_string (nth l 2);
                   }
    done;
    print_endline "read_ply: read_vertex done";
    ret;;

let read_face ic nface =
  let ret = Array.make nface { i = 0; j = 0; k = 0 } in
    for i = 0 to nface - 1 do
      let l = split_string ' ' (input_line ic) in
        ret.(i) <- { i = int_of_string (nth l 1);
                     j = int_of_string (nth l 2);
                     k = int_of_string (nth l 3);
                   }
    done;
    print_endline "read_ply: read_face done";
    ret;;

let read_body ic nvert nface =
  let array_vertex = read_vertex ic nvert in
  let array_face = read_face ic nface in
    ( array_vertex, array_face );;
    

let read_ply file_name =
  let ic = open_in file_name in
  let (nvert, nface) = read_header ic in
  let ret = read_body ic nvert nface in
    close_in ic;
    ret;;
