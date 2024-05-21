open Graphics ;;

Graphics.open_graph " 1024x768" ;;
let blue = Graphics.rgb 0 0 255 ;;
Graphics.set_color blue ;
Graphics.draw_rect 50 100 100 100 ;;
Graphics.moveto 20 250 ;;
Graphics.draw_string "Foobar" ;;
while true do () done ;;

(*
J'ai pu compiler le petit programme que vous m'avez joint. Mais je ne voit pas vraiment comment je vais pouvoir interfacer ma machine 
virutelle avec la bibiothèque Graphics. De mon côté, j'ai déjà codé le lexer et le parser pour reconnaître des instructions comme 

rect r(x,y,w,h,color); et le compilateur pour faire les bons appels machines et générer le bytecode pour que le rectangles soit enregistré dans la mémoire 
du programme : 

let compile_rect rho (r_name, params) = 
  let ptr_code =
    [VmBytecode.VMI_Loadi 1 ;
      VmBytecode.VMI_Mkblock; VmBytecode.VMI_Envext] in
  let alloc_code = [VmBytecode.VMI_Loadi 5 ; VmBytecode.VMI_Mkblock; VmBytecode.VMI_Print] in
  let decl_alloc_code = ptr_code @ alloc_code @ [VmBytecode.VMI_Assign 0] @ [VmBytecode.VMI_Push] in (*là dans le registre on à l'adresse*)
  let idx = 0 in
  let r_code_params =
    let rec compile_params idx = function
      | [] -> []
      | h :: t ->
        compile_expr rho h @ [VmBytecode.VMI_Print] @ [VmBytecode.VMI_Push] @ [VmBytecode.VMI_Print] @
        [VmBytecode.VMI_Loadi (idx)] @ [VmBytecode.VMI_Print] @ [VmBytecode.VMI_Indxwrite] @ [VmBytecode.VMI_Print] @ [VmBytecode.VMI_Pop] @ [VmBytecode.VMI_Print] @ compile_params (idx + 1) t in
    compile_params idx params in
  (r_name::rho, decl_alloc_code @ r_code_params @ [VmBytecode.VMI_Pop])
      ;;

La mémoire du programme suite à l'appel rect r(10,11,12,13,14); est la suivante :

Mem:
 @0: 1 @1: @3 @2: 5 @3: 10 @4: 11 @5: 12 @6: 13 @7: 14
End of mem


Mon objectif avec ce projet était de faire quelque chose de bas niveau pour mieux comprendre les mécanismes de compilation et découvrir comment 
on pouvait faire intéragir notre machine virutelle avec le système pour par exemple ouvrir une fenêtre et dessiner dedans. Je ne voulais pas 
juste coder un lexer et un parser pour qu'une ligne de code exécute quelques lignes tirées d'une bibliothèque.  

être capable d'appeler du code externe 
on donne à la vm d'exec du code binaire arbitraire, il faut écrire des bindings mais trop dur 
on va étendre la machine virtuelle pour qu'elle puisse appeler des fonctions externes 
*)