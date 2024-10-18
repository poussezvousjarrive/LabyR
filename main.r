Loggerhead <- setRefClass(
  "Loggerhead",
  fields = list(
    matrix = "list"
  ),
  methods = list(
    initialize = function() {
      matrix <<- list()
    },

   # Méthode pour ajouter un nouveau layer
    addLayer = function() {
      matrix <<- append(matrix, list(list()))  # Ajouter un layer vide
    },

    # Méthode publique pour dessiner en 3 dimensions
    goto = function(x, y, z, delta) {
      # Calcul du nombre de layers nécessaires
      num_layers <- ceiling(abs(z) / delta)  
      
      dz <- z / num_layers
      dx <- x / num_layers
      dy <- y / num_layers

      # Assurer qu'il y a suffisamment de layers
      while (length(matrix) < num_layers) addLayer()

      # Découpage du mouvement en couches 2D
      for (i in seq_len(num_layers)) {
        current_x <- dx * i
        current_y <- dy * i
        current_layer <- i

        # Ajout du déplacement sur le layer actuel
        .goto(
          current_layer,
          current_x, 
          current_y, 
          fill = TRUE
        )
      }
    },

    # Méthode privée pour ajouter une position à un layer spécifique
    .goto = function(layerIndex, x, y, fill = FALSE) {
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
turtle$.goto(1, 1, 2, TRUE)
turtle$.goto(1, 3, 4, FALSE)
turtle$.goto(2, 5, 6, TRUE)

print(turtle$matrix)