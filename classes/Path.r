library(R6)

Path <- R6Class("Path",
  public = list(
    x = 0,
    y = 0,
    facing = c(1, 0), # Direction initiale (vecteur x, y)
    movements = list(), # Liste pour stocker les mouvements

    initialize = function(x=0, y=0, facing=c(1, 0)) {
      self$x <- x
      self$y <- y
      self$facing <- facing
      self$movement(x, y, x, y)
    },

    # Déplace vers l'avant dans la direction actuelle
    forward = function(value, fill=1.0) {
      new_x <- self$x + value * self$facing[1]
      new_y <- self$y + value * self$facing[2]
      self$movement(self$x, self$y, new_x, new_y, fill)

      self$x <- new_x
      self$y <- new_y
    },

    # Tourne l'angle de direction de l'objet
    turn = function(angle) {
      radians <- angle * pi / 180
      new_facing_x <- cos(radians) * self$facing[1] - sin(radians) * self$facing[2]
      new_facing_y <- sin(radians) * self$facing[1] + cos(radians) * self$facing[2]
      self$facing <- c(new_facing_x, new_facing_y)
    }
    
  ),

  static = list(

    # Fonction statique capable de fusionner des tracés
    merge = function(paths) {
      final_path <- paths[[1]]

      for (i in 2:length(paths)) {
        final_path <- private$fusion(final_path, paths[[i]])
      }

      return(final_path)
    }
    
  )

  private = list(
    
    # Enregistre chaque déplacement avec (x, y, fill)
    movement = function(x1, y1, x2, y2, fill=1.0) {
      self$movements <- append(self$movements, list(c(x2, y2, fill)))
    }

    # Fonction privée pour fusionner deux tracés
    fusion = function(path1, path2) {
      if (!all(inherits(path1, "Path") & inherits(path2, "Path"))) {
        stop("All objects to be merge should be instances of 'Path'")
      }
                      
      last_point_path1 <- path1$movements[[length(path1$movements)]][1:2]
      first_point_path2 <- path2$movements[[1]][1:2]

      # Vérifie si le dernier point de P1 rejoint le premier point de P2
      if (all(last_point_path1 == first_point_path2)) {
        new_movements <- c(path1$movements, path2$movements[-1])
        new_path <- Path$new()
        new_path$movements <- new_movements
        return(new_path)
      } else {
        stop("Unable to fuse : the provided paths don't join")
      }
    }
    
  )
)