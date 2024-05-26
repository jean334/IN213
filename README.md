# GOLEM : Graphical Objet Simulation and Animation CoMpilEr 

Ce language permet de rapidement afficher et mettre en mouvement des formes géométrique dans une fenêtre. 

3 codes exemples sont fourni:

## exemple1.gol

Ce programme désinnent progressivement une forme assez joli (on dirait un trou noir). Au départ je voulais faire en sorte que deux planètes orbites autour d'une étoile mais suite à quelques erreur, un autre pattern est apparu.

## exemple2.gol

Les deux orbites voulues 

## exemple3.gol

juste une démonstration avec un rectangle.

## Structure d'un programme

- On commence par définir une fenêtre avec sa taille `win w(1200,1200)`
- on définit optionnelement un fond `background(r,g,b)`
- On définit ensuite les formes que l'on souhaite `circle c(x,y,radius)`, `rect r(x,y,w,h,color)` (color est un artéfact pour la construction d'un rectangle, je n'ai au final pas implémenter cette option). Ces fonctions enregistre les formes dans la mémoire du programme à la manière d'une struct en c, ce qui permet par la suite de modifier les formes sans avoir à en créer de nouvelles.
- on peut changer les dimensions et les positions des formes avec la syntaxe `ucircle c(new_x, new_y, new_radius)`, `urect r(new_x, new_y, new_w, new_height, new_color)`, `urect r.x(new_x)` ... .J'ai été contraint de rajouter les mots clefs `urect` et `ucircle` puisque sinon le parser ne faisait pas la différence entre les rectangles et les cercles avec la syntaxe `r.x()` et `c.x()`. Comme ces deux formes n'ont pas les mêmes arguments, les fonctions `.x()` ne sont pas interchangeables ce qui causait des erreurs. Une autre solution aurait été de remplacer les paramètres des cercles par `(x,y,radius,radius)`. De cette manière on aurait pu ne pas faire la distinction entre le `.x()` d'un rectangle et celui d'un cercle.
- Avec une boucle while, on peut changer la position de ces formes en fonction du temps, la fonction `fps(n_fps)` permet simplement de réguler le nombre de passage dans la boucle while par seconde à n_fps.

## Compilation
La compilation se déroule de la même façon que d'habitude:
- `touch depend`
- `make depend`
- `make` 

Le make lève parfois des erreurs, il suffit de relancer la commande 2 ou 3 fois pour que tout compile sans erreur. Je n'ai pas d'explication à ce problème.

## Remarque
Il y a probablement une certaine quantité de code mort, en particuier dans le parser et le constructeur d'arbre syntaxique. Je n'ai malheureusement pas le temps de tout nettoyer en raison du PRE qui me prend beaucoup de temps.




