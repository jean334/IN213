(* La mémoire globale. Initialement de taille 256, au hasard, remplie avec la
   valeur 0. S'il n'y a vraiment plus de place, on devrait réallouer de la
   mémoire. Pour simplifier, ici, on échouera simplement. *)
let mem = {
  VmBytecode.size = 256 ;   (* Chaque tas fait initialement 256. *)
  VmBytecode.next_free = 0 ;
  VmBytecode.heap_base = 0 ; (* Sera 0 ou size pour dire dans quel tas on
                                alloue. *)
  (* Comme on a 2 tas, on alloue le tableau de 2 fois la taille. *)
  VmBytecode.data = Array.make (2 * 256) (VmBytecode.VMV_int 0)
} ;;


(* Compteur global permettant, lors de la phase de copie, de savoir où l'on
   doit copier dans le tas destination. C'est plus simple de l'avoir en
   global plutôt que de devoir se trimbaler cet index partout.
   ATTENTION: doit être réinitialisé en début de GC, à la valeur de base
   du tas destination de la copie. *)
let next_free_in_to = ref 0 ;;


(* from_addr = addresse de bloc. On récupère la taille dans la case
   précédente. *)
let copy_block from_addr block_size =
  (* Copie de la taille du bloc. *)
  mem.data.(!next_free_in_to) <- VmBytecode.VMV_int block_size ;
  next_free_in_to := !next_free_in_to + 1 ;
  (* On mémorise l'adresse du nouveau bloc relogé. *)
  let new_addr = !next_free_in_to in
  (* Copie du contenu du bloc. *)
  for i = 0 to block_size - 1 do
    mem.data.(!next_free_in_to) <- mem.data.(from_addr + i) ;
    next_free_in_to := !next_free_in_to + 1
  done ;
  new_addr
;;


(* Transfère la mémoire se trouvant à [addr] dans le tas TO si ça n'a pas
   déjà été fait et met un pointeur distant dans le bloc dans le tas FROM.
   Pour savoir si le bloc se trouvant à [addr] a déjà été transféré, il
   suffit de regarder s'il contient une adresse distante, donc si dans
   mem[addr] on trouve une VMV_addr désignant une adresse dans le bloc TO. *)
let rec transfer_pointer addr =
  (* On va regarde en mémoire à l'adresse [addr]... *)
  let is_foreign =
    (match mem.VmBytecode.data.(addr) with
    | VmBytecode.VMV_addr a ->
        (* Est-ce que l'adresse est dans le bloc TO ? *)
        if (a >= mem.size && mem.heap_base = 0) ||
           (a < mem.size && mem.heap_base = mem.size) then Some a  (* Oui. *)
        else None                                                  (* Non. *)
    | _ -> None     (* Pas une adresse, donc bloc à recopier. *)) in
  match is_foreign with
  | Some a -> a
  | None -> (
      (* Ne pointe pas vers une adresse distante. Récupération de la taille
         du bloc. *)
      let block_size =
        (match mem.data.(addr - 1) with
        | VmBytecode.VMV_int s -> s
        | _ -> raise (Failure "copy_val : unexpected non-int block size")) in
      (* Il faut faire une copie du bloc. *)
      let new_addr = copy_block addr block_size in
      (* Modifier l'ancienne valeur à l'adresse avec cette adresse distante. *)
      mem.VmBytecode.data.(addr) <- VmBytecode.VMV_addr new_addr ;
      (* Puis, on récurse sur le bloc copié. *)
      for i = 0 to block_size - 1 do
        match mem.data.(new_addr + i) with
        | VmBytecode.VMV_addr a ->
            mem.data.(new_addr + i) <- VmBytecode.VMV_addr (transfer_pointer a)
        | _ -> ()  (* Pas un pointeur, pas besoin de suivre. *)
      done ;
      (* Retourner l'adresse où le bloc a été relogé. *)
      new_addr
     )
;;


(* Copie des racines. Retourne la valeur de la nouvelle racine et déclenche
   Si nécessaire la copie et le relogement de pointeur si la racine en est
   un. *)
let copy_root v =
  match v with
  | VmBytecode.VMV_int _ | VmBytecode.VMV_bool _ | VmBytecode.VMV_string _
  | VMV_code_addr _ ->
      (* Remarque : il n'y a pas d'adresses dans le code, pas de pointeurs,
         donc rien à copier ni reloger. *)
      v
  | VMV_addr addr ->
      (* La racine est un pointeur, il faut recopier le bloc vers lequel il
         pointe si ça n'a pas déjà été fait. *)
      VmBytecode.VMV_addr (transfer_pointer addr)
  | VMV_env env ->
      (* Un environnement est en fait une liste d'adresse. Donc il faut
         suivre reloger tous ces pointeurs. *)
      VMV_env (List.map transfer_pointer env)
;;


(* Déclenchement du GC. *)
let gc vm_state =
  Printf.printf "******* GC *******\n" ;
  (* Copy. *)
  (* On doit recopier tous les blocs vivants dans l'autre tas. On détermine
     donc l'adresse de l'autre tas, qui, à la fin, deviendra le tas courant.
     Comme la mémoire est coupée en 2 parties égales de taille size, les
     adresses de début de tas sont soit 0 soit size. *)
  let new_base = if mem.heap_base = 0 then mem.size else 0 in
  Printf.printf "* @FROM: %d @TO: %d\n" mem.heap_base new_base ;
  (* Réinitialise l'index de copie au début du tas destination de la copie. *)
  next_free_in_to := new_base ;
  (* On copie tout à partir des racines. *)
  let register' = copy_root vm_state.VmBytecode.register in
  let stack' = List.map copy_root vm_state.VmBytecode.stack in
  let env' = List.map transfer_pointer vm_state.VmBytecode.env in
  (* On met à jour la structure mémoire : l'indice du prochain bloc libre et
     la base du tas. *)
  mem.next_free <- !next_free_in_to ;
  mem.heap_base <- new_base ;
  Printf.printf "* Next free block: %d\n# living blocks: %d\n"
    mem.next_free (mem.next_free - new_base) ;
  Printf.printf "******* END GC *******\n" ;
  (* On retourne le nouvel état de la VM où les racines ont été mises à
     jour. *)
  { VmBytecode.register = register' ;
    VmBytecode.code = vm_state.VmBytecode.code ;
    VmBytecode.stack = stack' ;
    VmBytecode.env = env' }
;;


(* Allocation d'un bloc mémoire de taille donnée. S'il n'y a plus assez de
   mémoire dans le tas, déclenche un GC. *)
let new_block vm_state alloc_size =
  (* L'allocation effective sera de 1 de plus pour pouvoir mémoriser
     la taille du bloc. *)
  let vm_state' =
    if (mem.next_free - mem.heap_base) + alloc_size + 1 >= mem.size then (
      (* Besoin de déclencher un GC car plus assez de mémoire. *)
      let state = gc vm_state in
      (* On vérifie s'il reste bien de la mémoire après le GC, sinon on
         échoue. *)
      if (mem.next_free - mem.heap_base) + alloc_size + 1 >= mem.size then
        raise (Failure "Really no more memory") ;
      state
     )
    else vm_state in
  (* L'adresse du bloc est celle juste après son champ de taille. C'est cette
     adresse que l'on retourne à "l'utilisateur". *)
  let tmp = mem.next_free + 1 in
  (* On mémorise la taille du bloc. *)
  mem.data.(mem.next_free) <- VmBytecode.VMV_int alloc_size ;
  (* On fait progresser l'indice de prochain bloc libre. Comme on a pris une
     case pour mettre la taille du bloc, il ne faut pas oublier le + 1. *)
  mem.next_free <- mem.next_free + alloc_size + 1 ;
  (* On retourne l'adresse du bloc alloué. *)
  (tmp, vm_state')
;;
