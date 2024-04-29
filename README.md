# Notes sur le projet

- Est-ce qu'on se base sur imp ou sur pcf -> plutôt imp
- En quoi sont convertie les éléments de la VM -> l'interpréteur de la VM est codé en ocaml, c'est un lexer en faite, et après il s'occupe de faire
la traduction en instructions assembleurs
- est ce qu'il faut repartir de 0 ou se baser sur les derniers TPs de 

# Description du projet

Le programme doit :
- par défaut, ouvrir une fenêtre dans laquelle on peut draw (comme processing)
- on doit pouvoir rapidement faire des formes primitives --> rect, circle, triangle ...
- on doit pouvoir faire en sorte qu'elles soit soumisent à la gravité
- on doit pouvoir définir des ensembles d'objet primitifs
- on doit pouvoir définir des forces (pensenteur, frottements)
- il faut pouvoir fill, changer la couleur, redéfinir des propriétés de bases de ces formes --> notation pointée ?


# Syntaxe du projet

draw(x,y,w,h) --> définit une fenêtre, retourne un pointeur vers cette fenêtre 
rect(x,y,w,h) --> la syntaxe pour définir des formes primitives
assemble([liste de formes]) --> permet de grouper les formes
