open Lazy;;
open List;;

exception Undefined;;

(*
 * Declarations for vec3
 *)

type vec3 = { x : float;
              y : float;
              z : float;
              };;

let (+:) v w = { x = v.x +. w.x;
                 y = v.y +. w.y;
                 z = v.z +. w.z;
                 };;

let (-:) v w = { x = v.x -. w.x;
                 y = v.y -. w.y;
                 z = v.z -. w.z;
                 };;

let ( *:) v a = { x = v.x *. a;
                  y = v.y *. a;
                  z = v.z *. a;
                 };;

let (/:) v a = { x = v.x /. a;
                 y = v.y /. a;
                 z = v.z /. a;
                 };;

let vmag v = sqrt( v.x*.v.x +. v.y*.v.y +. v.z*.v.z );;

let vdot v w = v.x*.w.x +. v.y*.w.y +. v.z*.w.z;;

(*
 * Constants
 *)
let dt          = 0.004;;
let simscale    = 0.004;;
let h           = 0.01;;
let visc        = 0.2;;
let restdensity = 600.;;
let intstiff    = 1.0;;
let pmass       = 0.00020543;;
let pdist       = (pmass /. restdensity) ** (1.0 /. 3.0);;
let radius      = 0.004;;
let extstiff    = 10000.;;
let extdamp     = 256.;;
let epsilon     = 0.00001;;
let limit       = 200.;;
let pi          = 3.1415926535;;
let g           = { x =  0.; y = -9.8; z =   0. };;
let box_min     = { x =  0.; y =  0. ; z = -10. };;
let box_max     = { x = 20.; y = 50. ; z =  10. };;
let init_min    = { x =  0.; y =  0. ; z = -10. };;
let init_max    = { x = 10.; y = 20. ; z =  10. };;


(*
 * Utilities
 *)

let rec range low high d =
  if low <= high then low :: (range (low+.d) high d)
                 else [];;

let sum xs = fold_left (+.) 0. xs;;

let sum_vec3 xs = fold_left (+:) {x=0.;y=0.;z=0.} xs;;

let ($) f g = function x -> f(g(x));;

let rjust width s = 
  let s' = string_of_int s
  in String.make (width - (String.length s')) '0' ^ s';;
let rjust8 = rjust 8;;


(*
 * Declarations for Stream
 *)

type 'a stream = Nils
               | Cons of 'a * 'a stream lazy_t;;

let rec stream_take (n : int) (s : 'a stream) : 'a stream =
  match (n,s) with
  | (_,Nils)       -> Nils
  | (0,Cons(x,_ )) -> Nils
  | (n,Cons(x,xs)) -> Cons(x, lazy(stream_take (n-1) (force xs)));;


(*
 * Declarations for particles
 *)

type particle = { pos : vec3;
                  vel : vec3;
                  rho : float;
                  prs : float;
                  }

let ps0 : particle list = 
  let d  = pdist /. simscale *. 0.87 in
  let xs = let minx = init_min.x in
           let maxx = init_max.x
           in range (minx+.d) (maxx-.d) d in
  let ys = let miny = init_min.y in
           let maxy = init_max.y
           in range (miny+.d) (maxy-.d) d in
  let zs = let minz = init_min.z in
           let maxz = init_max.z
           in range (minz+.d) (maxz-.d) d
  in [ { pos = { x = x +. (Random.float 0.1) -. 0.05;
                 y = y +. (Random.float 0.1) -. 0.05;
                 z = z +. (Random.float 0.1) -. 0.05 };
         vel = { x = 0.; y = 0.; z = 0. };
         rho = 0.;
         prs = 0. }
       | x <- xs; y <- ys; z <- zs ];;


(*
 * Declarations for neighbor map
 *)

type idx = int * int * int;;
type neighbor_map = (idx, particle) Hashtbl.t;;

let neighbor_idx (r : vec3) : idx =
  let to_ix x = int_of_float( floor( x *. simscale /. h) )
  in (to_ix r.x, to_ix r.y, to_ix r.z);;

let mk_neighbor_map (ps : particle list) : neighbor_map = 
  let rec _mk_neighbor_map ps nbr_map =
    match ps with
    | []    -> nbr_map
    | p::ps -> Hashtbl.add nbr_map (neighbor_idx p.pos) p;
               _mk_neighbor_map ps nbr_map
  in _mk_neighbor_map ps (Hashtbl.create 10);;

let neighbor (nbr_map : neighbor_map) (r : vec3) : particle list = 
  let (_ix,_iy,_iz) = neighbor_idx r in
  let ix_min = let (ix,iy,iz) = neighbor_idx box_min 
               in (ix-1,iy-1,iz-1) in
  let ix_max = let (ix,iy,iz) = neighbor_idx box_max
               in (ix+1,iy+1,iz+1) in
  in flatten [ (try Hashtbl.find_all nbr_map (ix,iy,iz) with
                    Not_found -> [])
               | ix <- [_ix-1; _ix; _ix+1]
               ; iy <- [_iy-1; _iy; _iy+1]
               ; iz <- [_iz-1; _iz; _iz+1]
               ; ix_min <= (ix,iy,iz)
               ; (ix,iy,iz) <= ix_max ];;


(*
 * Functions for simulation
 *)

let rap_visc_kernel (x : vec3) : float =
  let r = vmag x
  in if r <= h then 45. /. (pi*.(h**6.)) *. (h-.r)
               else 0.;;

let grad_spiky_kernel (x : vec3) : vec3 =
  let r = vmag x
  in if r <= h then x *: (-45. /. (pi*.(h**6.)) *. (h-.r)**2. /. r)
               else {x=0.;y=0.;z=0.};;

let poly6_kernel (x : vec3) : float =
  let r = vmag x
  in if r <= h then 315. /. (64.*.pi*.(h**9.)) *. (h**2.-.r**2.)**3.
               else 0.;;

let boundary_condition (p : particle) (a : vec3) =
  let accel_limit a =
    let speed = vmag a
    in if speed > limit then a *: (limit /. speed)
                        else a in
  let wall d norm a =
    let diff = 2. *. radius -. d *. simscale in
    let adj  = extstiff *. diff -. extdamp *. (vdot norm p.vel)
    in if diff > epsilon then a +: (norm *: adj)
                         else a in
  let x_wall_max = wall (box_max.x -. p.pos.x  ) {x= -1.;y=  0.;z=  0.} in
  let x_wall_min = wall (p.pos.x   -. box_min.x) {x=  1.;y=  0.;z=  0.} in
  let y_wall_max = wall (box_max.y -. p.pos.y  ) {x=  0.;y= -1.;z=  0.} in
  let y_wall_min = wall (p.pos.y   -. box_min.y) {x=  0.;y=  1.;z=  0.} in
  let z_wall_max = wall (box_max.z -. p.pos.z  ) {x=  0.;y=  0.;z= -1.} in
  let z_wall_min = wall (p.pos.z   -. box_min.z) {x=  0.;y=  0.;z=  1.}
  in x_wall_max(
       x_wall_min(
         y_wall_max(
           y_wall_min(
             z_wall_max(
               z_wall_min(
                 accel_limit a))))));;

let viscosity_term ps pi = 
  let exp pj =
    let dr = (pi.pos -: pj.pos) *: simscale
    in (pj.vel -: pi.vel) *: (pmass /. pj.rho *. (rap_visc_kernel dr))
  in (sum_vec3 (map exp (filter ((!=) pi) ps))) *: visc;;

let pressure_term ps pi = 
  let exp pj =
    let dr = (pi.pos -: pj.pos) *: simscale
    in (grad_spiky_kernel dr) *: (pmass *. (pi.prs +. pj.prs) /. (2. *. pj.rho))
  in (sum_vec3 (map exp (filter ((!=) pi) ps))) *: (-1.);;

let calc_force ps pi = pressure_term ps pi +: viscosity_term ps pi;;

let density ps pi = 
  let exp pj =
    let dr = (pi.pos -: pj.pos) *: simscale
    in pmass *. (poly6_kernel dr)
  in sum (map exp ps);;
 
let advance ps nbr_map =
  let _advance p =
    let f  = calc_force (neighbor nbr_map p.pos) p in
    let a  = g +: (boundary_condition p (f /: p.rho)) in
    let v' = p.vel +: a *: dt in
    let r' = p.pos +: v' *: dt /: simscale
    in { p with pos = r'; vel = v' }
  in map _advance ps;;

let calc_amount ps nbr_map = 
  let _calc_amount p =
    let rho = density (neighbor nbr_map p.pos) p in
    let prs = (rho -. restdensity) *. intstiff
    in { p with rho = rho; prs = prs }
  in map _calc_amount ps;;

let rec simulation ps = 
  let ps'  = calc_amount ps (mk_neighbor_map ps) in
  let ps'' = advance ps' (mk_neighbor_map ps')
  in Cons(ps, lazy(simulation ps''));;


(*
 * Functions for output
 *)

let print_number_of ps = print_endline (string_of_int (length ps));;

let output_povs ps i =
  let sphere p = "sphere {\n"
               ^ "  <"
               ^ (string_of_float p.pos.x) ^ ","
               ^ (string_of_float p.pos.y) ^ ","
               ^ (string_of_float p.pos.z)
               ^ ">,0.5\n"
               ^ "  texture {\n"
               ^ "    pigment { color Yellow }\n"
               ^ "  }\n"
               ^ "}\n" in
  let body ps = "#include \"colors.inc\"\n"
              ^ "camera {\n"
              ^ "  location <10, 30, -40.0>\n"
              ^ "  look_at <10, 10, 0.0>\n"
              ^ "}\n"
              ^ "light_source { <0, 30, -30> color White }\n" in
  let file_name i = "result" ^ (rjust8 i) ^ ".pov" in
  let write ps i =
    let oc = open_out (file_name i)
    in output_string oc (body ps);
       iter (output_string oc $ sphere) ps;
       close_out oc
  in print_endline ("processing " ^ (file_name i) ^ " ...");
     write ps i;;

let stream_output pss =
  let rec _output s i =
    match s with
      Cons(ps,pss) -> output_povs ps i;
                      _output (force pss) (i+1)
    | Nils         -> ()
  in _output pss 0;;


(*
 * Main
 *)

let main =
  let n = int_of_string (Sys.argv.(1))
  in print_number_of ps0;
     stream_output (stream_take n (simulation ps0));;
