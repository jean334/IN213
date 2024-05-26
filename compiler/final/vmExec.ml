open Graphics ;;
open Unix ;;


exception Computation_success of VmBytecode.vm_val ;;
exception Computation_failure ;;

(* L� o� aller chercher le code des fonctions lors des appels.
   Initialis� uen fosi pour toute lorsque le bytecode est charg�. *)
let all_funs = ref ([] : (string * VmBytecode.vm_code) list) ;;


(* Print a state of the virtual machine. *)
let pp_state ppf state =
  Printf.fprintf ppf "------\n" ;
  Printf.fprintf ppf "Register r: %a\n%!"
    PrintByteCode.pp_value state.VmBytecode.register ;
  Printf.fprintf ppf "Code:\n%aEnd of code%!\n"
    PrintByteCode.pp_code state.VmBytecode.code ;
  Printf.fprintf ppf "Stack:\n" ;
  List.iter
    (fun v -> Printf.fprintf ppf " %a%!" PrintByteCode.pp_value v)
    state.VmBytecode.stack ;
  Printf.fprintf ppf "\nEnd of stack\n%!" ;
  Printf.fprintf ppf "Env:\n" ;
  List.iter
    (fun addr -> Printf.fprintf ppf " @%i%!" addr) state.VmBytecode.env ;
  Printf.fprintf ppf "\nEnd of env\n%!" ;
  Printf.fprintf ppf "Mem:\n" ;
  for i = 0 to Mem.mem.VmBytecode.next_free - 1 do
    Printf.fprintf ppf " @%i: %a%!"
      i PrintByteCode.pp_value Mem.mem.VmBytecode.data.(i)
  done ;
  Printf.fprintf ppf "\nEnd of mem\n%!" ;
;;


(* Compute the next state of the machine from the current state. *)
let rec next_state state =
  match (state.VmBytecode.register,
         state.VmBytecode.code,
         state.VmBytecode.stack) with
  | (_, ((VmBytecode.VMI_Loadi i) :: c), s) ->
      (* Int constant. *)
      { VmBytecode.register = VmBytecode.VMV_int i ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Loadb b) :: c), s) ->
      (* Bool constant. *)
      { VmBytecode.register = VmBytecode.VMV_bool b ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Loads str) :: c), s) ->
      (* String constant. *)
      { VmBytecode.register = VmBytecode.VMV_string str ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }

  | (_, ((VmBytecode.VMI_Loadf f) :: c), s) ->
      (* String constant. *)
      { VmBytecode.register = VmBytecode.VMV_float f ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
| ((VmBytecode.VMV_int i), (VmBytecode.VMI_Plus :: c), ((VmBytecode.VMV_int j):: s')) ->
  (* Addition for integers. *)
  { VmBytecode.register = VmBytecode.VMV_int (i + j) ;
    VmBytecode.code = c ;
    VmBytecode.stack = s' ;
    VmBytecode.env = state.VmBytecode.env }
| ((VmBytecode.VMV_float f), (VmBytecode.VMI_Plus :: c), ((VmBytecode.VMV_float g):: s')) ->
  (* Addition for floats. *)
  { VmBytecode.register = VmBytecode.VMV_float (f +. g) ;
    VmBytecode.code = c ;
    VmBytecode.stack = s' ;
    VmBytecode.env = state.VmBytecode.env }

  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Sub :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Subtraction.
          Be careful, subtraction not being commutative, we must take care
          of the "inverted" order of operands between register and stack
          hence we do j - i and not i - j. *)
       { VmBytecode.register = VmBytecode.VMV_int (j - i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
| ((VmBytecode.VMV_int i), (VmBytecode.VMI_Mult :: c), ((VmBytecode.VMV_int j):: s')) ->
  (* Multiplication for integers. *)
  { VmBytecode.register = VmBytecode.VMV_int (i * j) ;
    VmBytecode.code = c ;
    VmBytecode.stack = s' ;
    VmBytecode.env = state.VmBytecode.env }
| ((VmBytecode.VMV_float f), (VmBytecode.VMI_Mult :: c), ((VmBytecode.VMV_float g):: s')) ->
  (* Multiplication for floats. *)
  { VmBytecode.register = VmBytecode.VMV_float (f *. g) ;
    VmBytecode.code = c ;
    VmBytecode.stack = s' ;
    VmBytecode.env = state.VmBytecode.env }

  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Div :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Division.
          Same remark about non-commutativity of division than for
          subtraction. *)
       { VmBytecode.register = VmBytecode.VMV_int (j / i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Equal :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Comparison = assumed to be between 2 integers. *)
       { VmBytecode.register = VmBytecode.VMV_bool (i = j) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool i), (VmBytecode.VMI_Equal :: c),
     ((VmBytecode.VMV_bool j):: s')) ->
       (* Comparison = assumed to be between 2 booleans. *)
       { VmBytecode.register = VmBytecode.VMV_bool (i = j) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_string str1), (VmBytecode.VMI_Equal :: c),
     ((VmBytecode.VMV_string str2):: s')) ->
       (* Comparison = assumed to be between 2 strings. *)
       { VmBytecode.register = VmBytecode.VMV_bool (str1 = str2) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Lt :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Comparison < assumed to be between 2 integers. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j < i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool i), (VmBytecode.VMI_Lt :: c),
     ((VmBytecode.VMV_bool j):: s')) ->
       (* Comparison < assumed to be between 2 booleans. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j < i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_string str1), (VmBytecode.VMI_Lt :: c),
     ((VmBytecode.VMV_string str2):: s')) ->
       (* Comparison < assumed to be between 2 strings. *)
       { VmBytecode.register = VmBytecode.VMV_bool (str2 < str1) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
   | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Gt :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Comparison > assumed to be between 2 integers. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j > i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool i), (VmBytecode.VMI_Gt :: c),
     ((VmBytecode.VMV_bool j):: s')) ->
       (* Comparison > assumed to be between 2 booleans. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j > i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_string str1), (VmBytecode.VMI_Gt :: c),
     ((VmBytecode.VMV_string str2):: s')) ->
       (* Comparison > assumed to be between 2 strings. *)
       { VmBytecode.register = VmBytecode.VMV_bool (str2 > str1) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool true), ((VmBytecode.VMI_Branch (c1, _)) :: c),
     s) ->
       (* Branch if true. *)
      { VmBytecode.register = state.VmBytecode.register ;
        VmBytecode.code = c1 @ c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool false), ((VmBytecode.VMI_Branch (_, c2)) :: c),
     s) ->
       (* Branch if false. *)
      { VmBytecode.register = state.VmBytecode.register ;
        VmBytecode.code = c2 @ c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (r, (VmBytecode.VMI_Push :: c), s) ->
      (* Push onto the stack. *)
      { VmBytecode.register = r ;
        VmBytecode.code = c ;
        VmBytecode.stack = r :: s ;
        VmBytecode.env = state.VmBytecode.env }
| (_, (VmBytecode.VMI_Pop :: c), v :: s) ->
    (* Pop from the stack. *)
    let new_state = { VmBytecode.register = v ;
                      VmBytecode.code = c ;
                      VmBytecode.stack = s ;
                      VmBytecode.env = state.VmBytecode.env } in
    new_state
  | (_, ((VmBytecode.VMI_Read n) :: c),
     s) ->
       (* Read identifier. *)
       let addr =
        try List.nth state.VmBytecode.env n with
        | Failure msg when msg = "nth" ->
            raise (Failure ("VMI_Read out of env : " ^ (string_of_int n)))
        | Failure _ -> raise (Failure "Unexpected failure in VMI_Read") in
       if addr >= Mem.mem.VmBytecode.heap_base + Mem.mem.VmBytecode.size then
         raise (Failure "Access out of memory") ;
       { VmBytecode.register = Mem.mem.VmBytecode.data.(addr) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int bsize), (VmBytecode.VMI_Mkblock :: c),
     _) ->
       (* Allocation dynamique. Prend la taille du bloc � allouer dans
          le registre et met l'adresse allou�e dans le registre. *)
       let (addr, state') = Mem.new_block state bsize in
       { state' with
         VmBytecode.register = VmBytecode.VMV_addr addr ;
         VmBytecode.code = c }
  | ((VmBytecode.VMV_addr addr), (VMI_Envext :: c),
     s) ->
       (* Extension de l'environnement. Prend l'adresse � lier dans le
          registe.*)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = addr :: state.VmBytecode.env }
  | (r, ((VmBytecode.VMI_Assign n) :: c),
     s) ->
       (* Affectation d'une variable. Prend la valeur � affecter dans le
          registre. *)
    let addr =
      try List.nth state.VmBytecode.env n with
      | Failure msg when msg = "nth" ->
          raise (Failure ("VMI_Read out of env : " ^ (string_of_int n)))
      | Failure _ -> raise (Failure "Unexpected failure in VMI_Read") in
       if addr >= Mem.mem.VmBytecode.heap_base + Mem.mem.VmBytecode.size then
         raise (Failure "Access out of memory") ;
       Mem.mem.VmBytecode.data.(addr) <- r ;
       { VmBytecode.register = r ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
| (r, (VmBytecode.VMI_Print :: c), s) ->
    (* Affichage. Prend la valeur à afficher dans le registre. *)
    Printf.printf "valeur : %a " PrintByteCode.pp_value r ;
    let stack_str = 
        s 
        |> List.map (fun v -> match v with
                            | VmBytecode.VMV_int i -> string_of_int i
                            | VmBytecode.VMV_addr a -> string_of_int a
                            | _ -> "other")
        |> String.concat ", " in
    Printf.printf "stack: [%s]\n" stack_str;
    { VmBytecode.register = r ;
      VmBytecode.code = c ;
      VmBytecode.stack = s ;
      VmBytecode.env = state.VmBytecode.env }
         
  | ((VmBytecode.VMV_bool true),
     ((VmBytecode.VMI_Loop (code_cond, code_body)) :: c),
     s) ->
       (* Boucle lorsque la condition est vraie. *)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code =
           code_body @ code_cond @ [VmBytecode.VMI_Loop (code_cond, code_body)]
           @ c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool false),
     ((VmBytecode.VMI_Loop (_, _)) :: c),
     s) ->
       (* Boucle lorsque la condition est vraie. *)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Call f_name) :: c),
     s) ->
       (* Appel de fonction. Push le code restant sur la pile avant de sauter
          dans le code de la fonction. *)
       let f_code =
         (try List.assoc f_name !all_funs with
         | Not_found -> raise (Failure ("Undefined function " ^ f_name))) in
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = f_code ;
         VmBytecode.stack = (VMV_code_addr c) :: s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int index), (VmBytecode.VMI_Indxread :: c),
     (VmBytecode.VMV_addr base_addr) :: s) ->
       (* Lecture dans un tableau. L'indice o� aller lire est dans
          le registre. L'adresse de base du tableau est au sommet de la pile. *)
       if base_addr + index >=
           Mem.mem.VmBytecode.heap_base + Mem.mem.VmBytecode.size then
         raise (Failure "Access out of memory") ;
       { VmBytecode.register = Mem.mem.VmBytecode.data.(base_addr + index) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
| ((VmBytecode.VMV_int index), (VmBytecode.VMI_Indxwrite :: c),
   v :: (VmBytecode.VMV_addr base_addr) :: s) ->
    (* Affectation dans une case de tableau. L'indice est dans le registre
       et la valeur à affectée est prise au sommet de la pile et
       l'adresse de base du tableau juste en dessous. *)
    if base_addr + index >=
       Mem.mem.VmBytecode.heap_base + Mem.mem.VmBytecode.size then
      raise (Failure "Access out of memory") ;
    Mem.mem.VmBytecode.data.(base_addr + index) <- v ;
    let new_state = { VmBytecode.register = state.VmBytecode.register ;
                      VmBytecode.code = c ;
                      VmBytecode.stack = v :: (VmBytecode.VMV_addr base_addr) :: s ;
                      VmBytecode.env = state.VmBytecode.env } in
    new_state
  | (_, ((VmBytecode.VMI_Return) :: c),
     s) ->
       (* Retour de fonction. On ignore tout le code restant � ex�cuter.
          La VM ira cherher le code plac� en attente sur la pile lors de
          l'appel de la fonction courante que l'on quitte.. *)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = [] ;
         VmBytecode.stack = state.VmBytecode.stack ;
         VmBytecode.env = state.VmBytecode.env }
  | (r, (VmBytecode.VMI_Le :: c),
     s) -> failwith "TODO Le"
  | (r, (VmBytecode.VMI_Ge :: c),
     s) -> failwith "TODO Ge"
  | (r, (VmBytecode.VMI_Pushenv :: c),
     s) ->
       (* Sauvegarde de l'environnement sur la pile. *)
      { VmBytecode.register = r ;
        VmBytecode.code = c ;
        VmBytecode.stack = (VmBytecode.VMV_env state.VmBytecode.env) :: s ;
        VmBytecode.env = state.VmBytecode.env }
  | (r, VmBytecode.VMI_Popenv :: c,
     ((VmBytecode.VMV_env e) :: s)) ->
       (* Restoration de l'environnement depuis la pile. *)
       { VmBytecode.register = r ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = e }

  | ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_Window) ::c), s) ->
    let p1 = match Mem.mem.VmBytecode.data.(r) with
      | VmBytecode.VMV_int i -> i
      | _ -> failwith "Expected an integer" in
    let p2 = match Mem.mem.VmBytecode.data.(r + 1) with
      | VmBytecode.VMV_int i -> i
      | _ -> failwith "Expected an integer" in
      Graphics.open_graph (Printf.sprintf " %dx%d" p1 p2);
      { VmBytecode.register = VmBytecode.VMV_addr r ;
      VmBytecode.code = c ;
      VmBytecode.stack = s ;
      VmBytecode.env = state.VmBytecode.env }

  | ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_Rect) ::c), s) ->
    (*let p5 = match Mem.mem.VmBytecode.data.(r + 4) with
    | VmBytecode.VMV_string s -> s
    | _ -> failwith "Expected an string" in
    Graphics.set_color p5;*)
    Graphics.set_color black;
    let p1 = match Mem.mem.VmBytecode.data.(r) with
      | VmBytecode.VMV_int i -> i
      | _ -> failwith "Expected an integer" in
    let p2 = match Mem.mem.VmBytecode.data.(r + 1) with
      | VmBytecode.VMV_int i -> i
      | _ -> failwith "Expected an integer" in
    let p3 = match Mem.mem.VmBytecode.data.(r + 2) with
      | VmBytecode.VMV_int i -> i
      | _ -> failwith "Expected an integer" in
    let p4 = match Mem.mem.VmBytecode.data.(r + 3) with
      | VmBytecode.VMV_int i -> i
      | _ -> failwith "Expected an integer" in
      Graphics.draw_rect p1 p2 p3 p4 ;
      Graphics.fill_rect p1 p2 p3 p4 ;
      { VmBytecode.register = VmBytecode.VMV_addr r ;
      VmBytecode.code = c ;
      VmBytecode.stack = s ;
      VmBytecode.env = state.VmBytecode.env }
  
| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_RectMove) ::c), p1::p2::p3::p4::p5::s) ->
  let to_int = function
    | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
    | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
    | _ -> failwith "Expected a float"
  in
  Mem.mem.VmBytecode.data.(r) <- to_int p1;
  Mem.mem.VmBytecode.data.(r + 1) <- to_int p2;
  Mem.mem.VmBytecode.data.(r + 2) <- to_int p3;
  Mem.mem.VmBytecode.data.(r + 3) <- to_int p4;
  Mem.mem.VmBytecode.data.(r + 4) <- to_int p5;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_RectChangeX) ::c), p1::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float"
  in
  Mem.mem.VmBytecode.data.(r) <- to_int p1;
  Printf.printf "RectChangeX %a\n" PrintByteCode.pp_value p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_RectChangeY) ::c), p1::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float"
  in
  Mem.mem.VmBytecode.data.(r+1) <- to_int p1;
  Printf.printf "RectChangeY %a\n" PrintByteCode.pp_value p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_RectChangeW) ::c), p1::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float"
  in
  Mem.mem.VmBytecode.data.(r+2) <- to_int p1;
  Printf.printf "RectChangeW %a\n" PrintByteCode.pp_value p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_RectChangeH) ::c), p1::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float"
  in
  Mem.mem.VmBytecode.data.(r+3) <- to_int p1;
  Printf.printf "RectChangeH %a\n" PrintByteCode.pp_value p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_RectChangeC) ::c), p1::s) ->
    Graphics.set_color blue ;
    Printf.printf "RectChangeC %a\n" PrintByteCode.pp_value p1;
    { VmBytecode.register = VmBytecode.VMV_addr r ;
    VmBytecode.code = c ;
    VmBytecode.stack = s ;
    VmBytecode.env = state.VmBytecode.env }

| (VmBytecode.VMV_int r, ((VmBytecode.VMI_FPS) ::c), s) ->
  let fps = r in
  (*Graphics.clear_graph ();*)
  Unix.sleepf (1.0 /. float_of_int fps);
  { VmBytecode.register = VmBytecode.VMV_int r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| (r, ((VmBytecode.VMI_Background) ::c), (VmBytecode.VMV_int c1)::(VmBytecode.VMV_int c2)::(VmBytecode.VMV_int c3)::s) ->
  let color = rgb c1 c2 c3 in
  Graphics.set_color color;
  Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
  { VmBytecode.register = r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_Circle) ::c), s) ->
  Graphics.set_color black;
  let p1 = match Mem.mem.VmBytecode.data.(r) with
    | VmBytecode.VMV_int i -> i
    | _ -> failwith "Expected an integer" in
  let p2 = match Mem.mem.VmBytecode.data.(r + 1) with
    | VmBytecode.VMV_int i -> i
    | _ -> failwith "Expected an integer" in
  let p3 = match Mem.mem.VmBytecode.data.(r + 2) with
    | VmBytecode.VMV_int i -> i
    | _ -> failwith "Expected an integer" in
  (*let p4 = match Mem.mem.VmBytecode.data.(r + 3) with
    | VmBytecode.VMV_int i -> i
    | _ -> failwith "Expected an integer" in*)
    Graphics.draw_circle p1 p2 p3;
    { VmBytecode.register = VmBytecode.VMV_addr r ;
    VmBytecode.code = c ;
    VmBytecode.stack = s ;
    VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_CircleMove) ::c), p1::p2::p3::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float test"
  in
  Mem.mem.VmBytecode.data.(r) <- to_int p3;
  Mem.mem.VmBytecode.data.(r + 1) <- to_int p2;
  Mem.mem.VmBytecode.data.(r + 2) <- to_int p1;
  Printf.printf "CircleMove %a\n" PrintByteCode.pp_value p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_CircleChangeX) ::c), p1::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float"
  in
  Printf.printf "CircleChangeX floattant %a\n" PrintByteCode.pp_value (to_int p1);
  Mem.mem.VmBytecode.data.(r) <- to_int p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_CircleChangeY) ::c), p1::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float"
  in
  Mem.mem.VmBytecode.data.(r+1) <- to_int p1;
  Printf.printf "CircleChangeY %a\n" PrintByteCode.pp_value p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }

| ((VmBytecode.VMV_addr r), ((VmBytecode.VMI_CircleChangeR) ::c), p1::s) ->
  let to_int = function
  | VmBytecode.VMV_float f -> VmBytecode.VMV_int (int_of_float f)
  | VmBytecode.VMV_int i -> VmBytecode.VMV_int i
  | _ -> failwith "Expected a float"
  in
  Mem.mem.VmBytecode.data.(r+2) <- to_int p1;
  Printf.printf "CirlceChangeR %a\n" PrintByteCode.pp_value p1;
  { VmBytecode.register = VmBytecode.VMV_addr r ;
  VmBytecode.code = c ;
  VmBytecode.stack = s ;
  VmBytecode.env = state.VmBytecode.env }


    (*| (VmBytecode.VMV_int r, ((VmBytecode.VMI_FPS) ::c), s) ->
      let fps = r in
      (*Graphics.clear_graph ();*)
      Unix.sleepf (1.0 /. float_of_int fps);
      { VmBytecode.register = VmBytecode.VMV_int r ;
      VmBytecode.code = c ;
      VmBytecode.stack = s ;
      VmBytecode.env = state.VmBytecode.env }*)

  | (r, ((VmBytecode.VMI_AssignTrig n) :: c),
  s) ->
    (* Affectation d'une variable. Prend la valeur � affecter dans le
        registre. *)
  let addr =
    try List.nth state.VmBytecode.env n with
    | Failure msg when msg = "nth" ->
        raise (Failure ("VMI_Read out of env : " ^ (string_of_int n)))
    | Failure _ -> raise (Failure "Unexpected failure in VMI_Read") in
    if addr >= Mem.mem.VmBytecode.heap_base + Mem.mem.VmBytecode.size then
      raise (Failure "Access out of memory") ;
    Mem.mem.VmBytecode.data.(addr) <- r ;
    { VmBytecode.register = r ;
      VmBytecode.code = c ;
      VmBytecode.stack = s ;
      VmBytecode.env = state.VmBytecode.env }

  | (r, ((VmBytecode.VMI_Sin) ::c), p1::s) ->
    let result = sin (match p1 with
      | VmBytecode.VMV_float x -> x
      | VmBytecode.VMV_int x -> float_of_int x
      | _ -> failwith "Expected a float") in
    { VmBytecode.register = VmBytecode.VMV_float result ;
    VmBytecode.code = c ;
    VmBytecode.stack = s ;
    VmBytecode.env = state.VmBytecode.env }

  | (r, ((VmBytecode.VMI_Cos) ::c), p1::s) ->
    let result = cos (match p1 with
      | VmBytecode.VMV_float x -> x
      | VmBytecode.VMV_int x -> float_of_int x
      | _ -> failwith "Expected a float") in
    { VmBytecode.register = VmBytecode.VMV_float result ;
    VmBytecode.code = c ;
    VmBytecode.stack = s ;
    VmBytecode.env = state.VmBytecode.env }

  | ((VmBytecode.VMV_string r), ((VmBytecode.VMI_MathFunc) ::c), ((VmBytecode.VMV_int n)::s)) ->
      let rec get_args n s =
        if n <= 0 then [], s
        else match s with
        | arg::s' ->
          let args, s'' = get_args (n-1) s' in
          arg::args, s''
        | [] -> failwith "Not enough arguments on stack"
      in
      let args, s' = get_args n s in
      let result = match r, args with
      | "sin", [VmBytecode.VMV_float x] -> sin x
      | "cos", [VmBytecode.VMV_float x] -> cos x
      | _ -> failwith "Unknown function or wrong number of arguments"
      in
      { VmBytecode.register = VmBytecode.VMV_float result ;
      VmBytecode.code = c ;
      VmBytecode.stack = s' ;
      VmBytecode.env = state.VmBytecode.env }


  | (r, [(* Return *)], ((VmBytecode.VMV_code_addr c) :: s)) ->
      (* Return from function call, continue pending code. *)
      { VmBytecode.register = r ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (r, [(* Return *)], []) ->
      (* No more code to execute : the end. *)
      raise (Computation_success r)
  | (_, _, _) ->
      Printf.eprintf "State trace:\n%a%!" pp_state state ;
      raise Computation_failure
;;
