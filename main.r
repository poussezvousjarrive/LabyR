library(TurtleGraphics)

Loggerhead <- setRefClass(
  "Loggerhead",
  fields = list(
    matrix = "list"
  ),
  methods = list(
    initialize = function() {
      matrix <<- list()
      turtle_init() # Initialize turtle graphics
      turtle_hide()
    },

   # Méthode pour ajouter un nouveau layer
    addLayer = function() {
      matrix <<- append(matrix, list(list()))  # Ajouter un layer vide
    },

    # Méthode publique pour dessiner en 3 dimensions
    goto3d = function(x, y, z, fill, delta) {
      # Calcul du nombre de layers nécessaires
      num_layers <- ceiling(abs(z) / delta)  
      
      dz <- z / num_layers
      dx <- x / num_layers
      dy <- y / num_layers

      # Assurer qu'il y a suffisamment de layers
      while (length(matrix) < num_layers) addLayer()

      # Réinitialiser l'environnement graphique
      turtle_reset()

      # Découpage du mouvement en couches 2D
      for (i in seq_len(num_layers)) {
        current_x <- dx * i
        current_y <- dy * i
        current_layer <- i

        # Ajout du déplacement sur le layer actuel
        goto2d(
          current_layer,
          current_x, 
          current_y, 
          fill = fill
        )

        turtle_goto(current_x, current_y)
        if (i %% 2 == 0) {
          turtle_right(90)
        } else {
          turtle_left(90)
        }
      }
    },

    # Méthode publique pour ajouter une position à un layer spécifique
    goto2d = function(layerIndex, x, y, fill = TRUE) {
      if (length(matrix) < layerIndex || layerIndex < 1) {
        stop("Invalid layer index")
      }
      position <- c(x, y, fill)
      matrix[[layerIndex]] <<- append(matrix[[layerIndex]], list(position))
    }
  )
)

# Exemple d'utilisation
turtle <- Loggerhead$new()

# Ajouter deux layers
turtle$addLayer()
turtle$addLayer()

# Ajouter des positions dans les layers
turtle$goto3d(1, 5, 5, TRUE, 0.5)

print(turtle$matrix)