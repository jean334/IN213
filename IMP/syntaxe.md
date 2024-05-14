# Définition de la syntaxe complète du programme

**Définissons la syntaxe du programme**

- ouvrir une fenêtre, définir sa taille, sa position
> draw(x,y,w,h), retourne un pointeur vers cette fenêtre

- créer des formes primitives (rect, cicle, triangle...)
> rect r(x,y,w,h,col)
- définir des forces
> force f(type, intensité, dir_x, dir_y)
> type : GLOBAL ou un pointeur vers un objet ou un ensemble.
- Les formes doivent pouvoir être affecter par les forces, se déplacer...
>leur position et calculé à chaque pas de temps  
- modifier des propriétés (couleurs, tailles...) de ces formes
> par une notation pointée, les propriétés modifiables sont :
> la taille, la position, la couleur 
- définir des ensembles d'objets et les modifier

**Exemple de code**

win w(x,y,w,h)
w.x = 500
w.y = 500

rect r(x,y,w,h, col)
circle c(x,y,r, col)
line l(x1,x2,y1,y2,col)
force f1(GLOBAL, 40, 1,1)
force f2(c, 40,1,1)

