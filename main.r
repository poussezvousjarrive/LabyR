library(TurtleGraphics)
source('classes/Loggerhead.r')

# Exemple d'utilisation
turtle <- Loggerhead$new()

# Définir un carré de côté 1
square <- Polygon$fromVertices(list(
  c(0, 0),
  c(1, 0),                     
  c(1, 1),
  c(0, 1)
))

# Ajouter un layer contenant un carré
turtle$addLayer(list(
   square
))

# Afficher le dernier layer
turtle$display()