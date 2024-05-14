(* La m�moire globale. Initialement de taille 256, au hasard, remplie avec la
   valeur 0. S'il n'y a vraiment plus de place, on devrait r�allouer de la
   m�moire. Pour simplifier, ici, on �chouera simplement. *)
let mem = {
  VmBytecode.size = 256 ;   (* Chaque tas fait initialement 256. *)
  VmBytecode.next_free = 0 ;
  VmBytecode.heap_base = 0 ; (* Sera 0 ou size pour dire dans quel tas on
                                alloue. *)
  (* Comme on a 2 tas, on alloue le tableau de 2 fois la taille. *)
  VmBytecode.data = Array.make (2 * 256) (VmBytecode.VMV_int 0)
} ;;


(* Compteur global permettant, lors de la phase de copie, de savoir o� l'on
   doit copier dans le tas destination. C'est plus simple de l'avoir en
   global plut�t que de devoir se trimbaler cet index partout.
   ATTENTION: doit �tre r�initialis� en d�but de GC, � la valeur de base
   du tas destination de la copie. *)
let next_free_in_to = ref 0 ;;


(* from_addr = addresse de bloc. On r�cup�re la taille dans la case
   pr�c�dente. *)
let copy_block from_addr block_size =
  (* Copie de la taille du bloc. *)
  mem.data.(!next_free_in_to) <- VmBytecode.VMV_int block_size ;
  next_free_in_to := !next_free_in_to + 1 ;
  (* On m�morise l'adresse du nouveau bloc relog�. *)
  let new_addr = !next_free_in_to in
  (* Copie du contenu du bloc. *)
  for i = 0 to block_size - 1 do
    mem.data.(!next_free_in_to) <- mem.data.(from_addr + i) ;
    next_free_in_to := !next_free_in_to + 1
  done ;
  new_addr
;;


(* Transf�re la m�moire se trouvant � [addr] dans le tas TO si �a n'a pas
   d�j� �t� fait et met un pointeur distant dans le bloc dans le tas FROM.
   Pour savoir si le bloc se trouvant � [addr] a d�j� �t� transf�r�, il
   suffit de regarder s'il contient une adresse distante, donc si dans
   mem[addr] on trouve une VMV_addr d�signant une adresse dans le bloc TO. *)
let rec transfer_pointer addr =
  (* On va regarde en m�moire � l'adresse [addr]... *)
  let is_foreign =
    (match mem.VmBytecode.data.(addr) with
    | VmBytecode.VMV_addr a ->
        (* Est-ce que l'adresse est dans le bloc TO ? *)
        if (a >= mem.size && mem.heap_base = 0) ||
           (a < mem.size && mem.heap_base = mem.size) then Some a  (* Oui. *)
        else None                                                  (* Non. *)
    | _ -> None     (* Pas une adresse, donc bloc � recopier. *)) in
  match is_foreign with
  | Some a -> a
  | None -> (
      (* Ne pointe pas vers une adresse distante. R�cup�ration de la taille
         du bloc. *)
      let block_size =
        (match mem.data.(addr - 1) with
        | VmBytecode.VMV_int s -> s
        | _ -> raise (Failure "copy_val : unexpected non-int block size")) in
      (* Il faut faire une copie du bloc. *)
      let new_addr = copy_block addr block_size in
      (* Modifier l'ancienne valeur � l'adresse avec cette adresse distante. *)
      mem.VmBytecode.data.(addr) <- VmBytecode.VMV_addr new_addr ;
      (* Puis, on r�curse sur le bloc copi�. *)
      for i = 0 to block_size - 1 do
        match mem.data.(new_addr + i) with
        | VmBytecode.VMV_addr a ->
            mem.data.(new_addr + i) <- VmBytecode.VMV_addr (transfer_pointer a)
        | _ -> ()  (* Pas un pointeur, pas besoin de suivre. *)
      done ;
      (* Retourner l'adresse o� le bloc a �t� relog�. *)
      new_addr
     )
;;


(* Copie des racines. Retourne la valeur de la nouvelle racine et d�clenche
   Si n�cessaire la copie et le relogement de pointeur si la racine en est
   un. *)
let copy_root v =
  match v with
  | VmBytecode.VMV_int _ | VmBytecode.VMV_bool _ | VmBytecode.VMV_string _
  | VMV_code_addr _ ->
      (* Remarque : il n'y a pas d'adresses dans le code, pas de pointeurs,
         donc rien � copier ni reloger. *)
      v
  | VMV_addr addr ->
      (* La racine est un pointeur, il faut recopier le bloc vers lequel il
         pointe si �a n'a pas d�j� �t� fait. *)
      VmBytecode.VMV_addr (transfer_pointer addr)
  | VMV_env env ->
      (* Un environnement est en fait une liste d'adresse. Donc il faut
         suivre reloger tous ces pointeurs. *)
      VMV_env (List.map transfer_pointer env)
;;


(* D�clenchement du GC. *)
let gc vm_state =
  Printf.printf "******* GC *******\n" ;
  (* Copy. *)
  (* On doit recopier tous les blocs vivants dans l'autre tas. On d�termine
     donc l'adresse de l'autre tas, qui, � la fin, deviendra le tas courant.
     Comme la m�moire est coup�e en 2 parties �gales de taille size, les
     adresses de d�but de tas sont soit 0 soit size. *)
  let new_base = if mem.heap_base = 0 then mem.size else 0 in
  Printf.printf "* @FROM: %d @TO: %d\n" mem.heap_base new_base ;
  (* R�initialise l'index de copie au d�but du tas destination de la copie. *)
  next_free_in_to := new_base ;
  (* On copie tout � partir des racines. *)
  let register' = copy_root vm_state.VmBytecode.register in
  let stack' = List.map copy_root vm_state.VmBytecode.stack in
  let env' = List.map transfer_pointer vm_state.VmBytecode.env in
  (* On met � jour la structure m�moire : l'indice du prochain bloc libre et
     la base du tas. *)
  mem.next_free <- !next_free_in_to ;
  mem.heap_base <- new_base ;
  Printf.printf "* Next free block: %d\n# living blocks: %d\n"
    mem.next_free (mem.next_free - new_base) ;
  Printf.printf "******* END GC *******\n" ;
  (* On retourne le nouvel �tat de la VM o� les racines ont �t� mises �
     jour. *)
  { VmBytecode.register = register' ;
    VmBytecode.code = vm_state.VmBytecode.code ;
    VmBytecode.stack = stack' ;
    VmBytecode.env = env' }
;;


(* Allocation d'un bloc m�moire de taille donn�e. S'il n'y a plus assez de
   m�moire dans le tas, d�clenche un GC. *)
let new_block vm_state alloc_size =
  (* L'allocation effective sera de 1 de plus pour pouvoir m�moriser
     la taille du bloc. *)
  let vm_state' =
    if (mem.next_free - mem.heap_base) + alloc_size + 1 >= mem.size then (
      (* Besoin de d�clencher un GC car plus assez de m�moire. *)
      let state = gc vm_state in
      (* On v�rifie s'il reste bien de la m�moire apr�s le GC, sinon on
         �choue. *)
      if (mem.next_free - mem.heap_base) + alloc_size + 1 >= mem.size then
        raise (Failure "Really no more memory") ;
      state
     )
    else vm_state in
  (* L'adresse du bloc est celle juste apr�s son champ de taille. C'est cette
     adresse que l'on retourne � "l'utilisateur". *)
  let tmp = mem.next_free + 1 in
  (* On m�morise la taille du bloc. *)
  mem.data.(mem.next_free) <- VmBytecode.VMV_int alloc_size ;
  (* On fait progresser l'indice de prochain bloc libre. Comme on a pris une
     case pour mettre la taille du bloc, il ne faut pas oublier le + 1. *)
  mem.next_free <- mem.next_free + alloc_size + 1 ;
  (* On retourne l'adresse du bloc allou�. *)
  (tmp, vm_state')
;;
