library(TurtleGraphics)

source('classes/Loggerhead.r')
source('classes/Path.r')
source('classes/Polygon.r')

# Exemple d'utilisation
turtle <- Loggerhead$new()

# Définir un carré de côté 1
path <- Path$new()

for (i in 1:4) {
  path$forward(50)
  path$turn(90)
}

square <- Polygon$new(path)

# Ajouter un calque contenant le carré
turtle$addLayer()

turtle$buildShapes(list(
  square
))

# Afficher le dernier calque
# turtle$display()