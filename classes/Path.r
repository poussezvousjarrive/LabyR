library(R6)

Path <- R6Class("Path",
  class = TRUE,

  public = list(
    x = 0,
    y = 0,
    facing = c(1, 0), # Direction initiale (vecteur x, y)
    movements = list(), # Liste pour stocker les mouvements

    initialize = function(x=0, y=0, facing=c(1, 0)) {
      self$facing <- facing
      self$move(x, y, x, y)
    },

    # Constructeur alternatif à partir de coordonnées
    fromCoords = function(coords) {
      path <- Path$new(coords[[1]][1], coords[[1]][2])

      for (i in 2:length(coords)) {
        point <- coords[[i]]
        path$move(path$x, path$y, point[1], point[2])
      }

      path$move(path$x, path$y, coords[[1]][1], coords[[1]][2])
      return(path)
    },

    # Méthode publique pour avancer dans la direction actuelle
    forward = function(value, fill=1.0) {
      new_x <- self$x + value * self$facing[1]
      new_y <- self$y + value * self$facing[2]
      
      self$move(self$x, self$y, new_x, new_y, fill)

      self$x <- new_x
      self$y <- new_y
    },

    # Méthode publique pour faire tourner la direction actuelle d'un certain angle
    turn = function(angle) {
      radians <- angle * pi / 180
      new_facing_x <- cos(radians) * self$facing[1] - sin(radians) * self$facing[2]
      cat(angle, sep='\n')
      new_facing_y <- sin(radians) * self$facing[1] + cos(radians) * self$facing[2]
      self$facing <- round(c(new_facing_x, new_facing_y), 10)
    },
    
    # Méthode publique pour modifier directement le vecteur direction actuel
    set_facing = function(new_facing_x, new_facing_y) {
      self$facing <- round(c(new_facing_x, new_facing_y), 10)
    },

    # Fonction publique pour fusionner deux tracés
    fusion = function(path1, path2) {
      if (!all(inherits(path1, "Path") && inherits(path2, "Path"))) {
        stop("All objects to be merged should be instances of 'Path'")
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
    },

    # Méthode publique pour enregistrer un mouvement
    move = function(x1, y1, x2, y2, fill=1.0) {
      self$movements <- append(self$movements, list(c(x2, y2, fill)))
      self$x <- x2
      self$y <- y2
    },

    # Surcharge de la fonction d'affichage
    print = function() {
      movement_strings <- sapply(self$movements, function(move) {
        paste0("(", move[1], ", ", move[2], ")")
      })

      movement_path <- paste(movement_strings, collapse = " -> ")
      cat("<Path>", movement_path, "\n", sep = " ")
    },
    
    
    # Méthode publique pour visualiser un chemin
    display = function() {
      turtle_init()
      turtle_hide()
      turtle_up()
      turtle_goto(0, 0)
      
      curr_pos <- self$movements[[1]]
      for (i in 1:(length(self$movements) - 1)) {
        next_pos <- self$movements[[i+1]]

        # Si ce vecteur a la mention FILL
        if (next_pos[3] != 0) {
          turtle_down()
          turtle_goto(next_pos[1], next_pos[2])
        } else { 
          turtle_up()
          turtle_goto(next_pos[1], next_pos[2])
        }

        pos <- next_pos 
      }
    }
  )
)

p <- Path$new()
p$move(0,0,0,0)
#p$move(0,0,50,0)
#p$move(50,0,50,50)
#p$move(50,50,50,0)
#p$move(50,0,50,0)
p$display()