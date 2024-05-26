let rec __pp_code ppf depth code = List.iter (__pp_instr ppf depth) code

and __pp_instr ppf depth instr =
  (* Print indentation. *)
  let curr_indent = String.make (2 * depth) ' ' in
  Printf.fprintf ppf "%s" curr_indent ;
  match instr with
  | VmBytecode.VMI_Loadi i -> Printf.fprintf ppf "Loadi %d\n%!" i
  | VmBytecode.VMI_Loadb b -> Printf.fprintf ppf "Loadb %b\n%!" b
  | VmBytecode.VMI_Loads s ->
      Printf.fprintf ppf "Loads \"%s\"\n%!" (String.escaped s)
  | VmBytecode.VMI_Loadf f -> Printf.fprintf ppf "Loadf %f\n%!" f
  | VmBytecode.VMI_Plus -> Printf.fprintf ppf "Plus\n%!"
  | VmBytecode.VMI_Sub -> Printf.fprintf ppf "Sub\n%!"
  | VmBytecode.VMI_Mult -> Printf.fprintf ppf "Mult\n%!"
  | VmBytecode.VMI_Div -> Printf.fprintf ppf "Div\n%!"
  | VmBytecode.VMI_Equal -> Printf.fprintf ppf "Equal\n%!"
  | VmBytecode.VMI_Lt -> Printf.fprintf ppf "Lt\n%!"
  | VmBytecode.VMI_Le -> Printf.fprintf ppf "Le\n%!"
  | VmBytecode.VMI_Gt -> Printf.fprintf ppf "Gt\n%!"
  | VmBytecode.VMI_Ge -> Printf.fprintf ppf "Ge\n%!"
  | VmBytecode.VMI_Call f_name -> Printf.fprintf ppf "Call %s\n%!" f_name
  | VmBytecode.VMI_Read i -> Printf.fprintf ppf "Read %d\n%!" i
  | VmBytecode.VMI_Indxread -> Printf.fprintf ppf "Indxread\n%!"
  | VmBytecode.VMI_Return -> Printf.fprintf ppf "Return\n%!"
  | VmBytecode.VMI_Mkblock -> Printf.fprintf ppf "Mkblock\n%!"
  | VmBytecode.VMI_Print -> Printf.fprintf ppf "Print\n%!"
  | VmBytecode.VMI_Assign i -> Printf.fprintf ppf "Assign %d\n%!" i
  | VmBytecode.VMI_Indxwrite -> Printf.fprintf ppf "Indxwrite\n%!"
  | VmBytecode.VMI_Branch (c1, c2) ->
      Printf.fprintf ppf "Branch then\n%!" ;
      __pp_code ppf (depth + 1) c1 ;
      Printf.fprintf ppf "%sBranch else\n%!" curr_indent ;
      __pp_code ppf (depth + 1) c2 ;
      Printf.fprintf ppf "%sBranch end\n%!" curr_indent
  | VmBytecode.VMI_Push -> Printf.fprintf ppf "Push\n%!"
  | VmBytecode.VMI_Pushenv -> Printf.fprintf ppf "Pushenv\n%!"
  | VmBytecode.VMI_Pop -> Printf.fprintf ppf "Pop\n%!"
  | VmBytecode.VMI_Popenv -> Printf.fprintf ppf "Popenv\n%!"
  | VmBytecode.VMI_Envext -> Printf.fprintf ppf "Envext\n%!"
  | VmBytecode.VMI_Window -> Printf.fprintf ppf "Window\n%!"
  | VmBytecode.VMI_Rect -> Printf.fprintf ppf "Rect\n%!"
  | VmBytecode.VMI_RectMove -> Printf.fprintf ppf "RectMove test\n%!"
  | VmBytecode.VMI_RectChangeX -> Printf.fprintf ppf "RectChangeX\n%!"
  | VmBytecode.VMI_RectChangeY -> Printf.fprintf ppf "RectChangeY\n%!"
  | VmBytecode.VMI_RectChangeW -> Printf.fprintf ppf "RectChangeWidth\n%!"
  | VmBytecode.VMI_RectChangeH -> Printf.fprintf ppf "RectChangeHeight\n%!"
  | VmBytecode.VMI_RectChangeC -> Printf.fprintf ppf "RectChangeColor\n%!"
  
  | VmBytecode.VMI_Circle -> Printf.fprintf ppf "Circle\n%!"
  | VmBytecode.VMI_CircleMove -> Printf.fprintf ppf "CircleMove\n%!"
  | VmBytecode.VMI_CircleChangeX -> Printf.fprintf ppf "CircleChangeX\n%!"
  | VmBytecode.VMI_CircleChangeY -> Printf.fprintf ppf "CircleChangeY\n%!"
  | VmBytecode.VMI_CircleChangeR -> Printf.fprintf ppf "CircleChangeWidth\n%!"

  | VmBytecode.VMI_Line -> Printf.fprintf ppf "Line\n%!"
  |VmBytecode.VMI_FPS -> Printf.fprintf ppf "FPS\n%!"
  |VmBytecode.VMI_Background -> Printf.fprintf ppf "Background\n%!"
  |VmBytecode.VMI_Sin -> Printf.fprintf ppf "Sin\n%!"
  |VmBytecode.VMI_Cos -> Printf.fprintf ppf "Cos\n%!"
  |VmBytecode.VMI_AssignTrig i -> Printf.fprintf ppf "AssignTrig %d\n%!" i
  |VmBytecode.VMI_MathFunc -> Printf.fprintf ppf "MathFunc\n%!"
  | VmBytecode.VMI_Loop (_, i) ->
      Printf.fprintf ppf "Loop\n%!" ;
      __pp_code ppf (depth + 1) i ;
      Printf.fprintf ppf "Loop end\n%!"
;;


let rec pp_separated_list ppf printer = function
  | [] -> ()
  | [last] -> Printf.fprintf ppf "%a" printer last
  | h :: q ->
      Printf.fprintf ppf "%a, " printer h ;
      pp_separated_list ppf printer q
;;


(* Print a virtual machine value. *)
let rec pp_value ppf = function
  | VmBytecode.VMV_int i -> Printf.fprintf ppf "%d" i
  | VmBytecode.VMV_bool b -> Printf.fprintf ppf "%b" b
  | VmBytecode.VMV_string s -> Printf.fprintf ppf "%s" s
  | VmBytecode.VMV_addr i -> Printf.fprintf ppf "@%d" i
  | VmBytecode.VMV_float f -> Printf.fprintf ppf "%f" f
  | VmBytecode.VMV_code_addr c ->
      Printf.fprintf ppf "<code>\n" ;
      __pp_code ppf 4 c ;
      Printf.fprintf ppf "</code>\n"
  | VmBytecode.VMV_env _ -> Printf.fprintf ppf "<env>\n"
;;


(* Print a virtual machine bytecode program. Only exported function
   to hide the extra indentation parameter that must always be 0 at
   the initiall call. *)
let pp_code ppf code = __pp_code ppf 0 code ;;
