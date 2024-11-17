library(R6)

Polygon <- R6Class("Polygon",
  class = TRUE,
  
  public = list(
    vertices = NULL, # Liste des sommets du polygone

    initialize = function(path) {
      if (!inherits(path, "Path")) {
        stop("Object passed as first parameter isn't of type 'Path'")
      }
      
      movements <- path$movements
      if (length(movements) < 3) {
        stop("No polygon can be built with less than 3 vertices")
      }
      
      first_point <- movements[[1]][1:2]
      last_point <- movements[[length(movements)]][1:2]

      if (!all(first_point == last_point)) {
        stop("No polygon can be built from an open path")
      }

      self$vertices <- lapply(movements, function(m) m[1:2])
    },

    # Méthode publique pour renvoyer un tracé optimisé
    toPrintPath = function() {
      new_path <- Path$new()
      
      for (i in 1:length(self$vertices)) {
        vertex <- self$vertices[[i]]

        if (i == 1) {
          new_path$forward(0)
        } else {
          dx <- vertex[1] - new_path$x
          dy <- vertex[2] - new_path$y
          norm <- sqrt(dx**2 + dy**2)
          new_path$set_facing(dx/norm, dy/norm)
          new_path$forward(norm)
        }
      }
      return(new_path)
    },

    # Surcharge de la fonction d'affichage
    print = function() {
      origin <- self$vertices[[1]]
      
      cat("<Polygon>",
          "\n\tVertices: ", length(self$vertices) - 1, " (+ 1)",
          "\n\tOrigin: (", origin[1], ', ', origin[2], ")",
          "\n",
      sep = "")
    }

  )
)