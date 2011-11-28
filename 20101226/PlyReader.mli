type vertex
val vertex_x : vertex -> float
val vertex_y : vertex -> float
val vertex_z : vertex -> float

type face
val face_i : face -> int
val face_j : face -> int
val face_k : face -> int

val read_ply : string -> vertex array * face array
