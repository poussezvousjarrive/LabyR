library(R6)
library(TurtleGraphics)

Loggerhead <- R6Class("Loggerhead",
  class = TRUE,
 
  public = list(
    layers = NULL,
    activeLayer = NULL,

    initialize = function() {
      self$layers <- list()
      turtle_init() # Initialise turtle graphics
      turtle_hide()
    },

    # Méthode publique pour ajouter un nouveau calque
    addLayer = function() {
      layer <- Path$new()
      self$layers <- append(self$layers, list(layer))
      layerIndex <- length(self$layers)
      self$selectLayer(layerIndex)
    },

    # Méthode publique pour changer le calque actif
    selectLayer = function(layerIndex) {
      if (layerIndex < 1 || layerIndex > length(self$layers)) {
        stop("Layer index out of bounds")
      } else {
        self$activeLayer <- layerIndex
      }
    },

    # Méthode publique pour ajouter des polygones au calque
    buildShapes = function(polygons) {
      if(is.null(self$activeLayer)) stop("No active layer selected")

      layer <- self$layers[[self$activeLayer]]

      for (polygon in polygons) {
        if (!inherits(polygon, "Polygon")) {
          stop("All shapes must be of class 'Polygon'")
        } else {
          polygonPath <- polygon$toPrintPath()
          layer <- Path$new()$fusion(layer, polygonPath)
        }
      }

      self$layers[[self$activeLayer]] <- layer
    },

    # Méthode publique pour tracer librement
    freeDraw = function(moves) {
      if(!self$activeLayer) stop("No layer registered")
      # free draw
    },

    # Méthode publique
    # pour visualiser une couche 2D
    display = function(layerIndex) {
      layer <- matrix[[layerIndex]]

      pos <- c(0.0, 0.0, 0.0) # Origine du tracé (le 3ème = FILL)
     
      turtle_reset()
      turtle_hide()
     
      # Largeur du pinceau (pour tracer un point solitaire)
      # (comme un ver solitaire)
      lwd <- turtle_status()$DisplayOptions$lwd 
     
      for (i in range(1:length(layer))) {
        next_vec <- layer[[i]]
        next_pos <- c(pos[1] + next_vec[1],
                     pos[2] + next_vec[2],
                     next_vec[3])
        # Si ce vecteur a la mention FILL,
        # on affiche la ligne entre les deux
        if (next_pos[3]) {
          turtle_down()
          turtle_goto(next_pos[1], next_pos[2])
        } else { 
          # Sinon on se déplace de manière invisible 
          # et on affiche simplement le prochain point
          # sous forme d'une petit carré
          turtle_up()
          turtle_goto(next_pos[1], next_pos[2])
         
          # Carré côté largeur du pinceau
          turtle_forward(lwd/2)
          turtle_left(90)
         
          turtle_down()
          turtle_forward(lwd/2)
          for (j in 1:3) {
            turtle_left(90)
            turtle_forward(lwd)
          }
          turtle_left(90)
          turtle_forward(lwd/2)
         
          turtle_up()
          turtle_left(90)
          turtle_forward(lwd/2)
        }
       
        # La position suivante devient la position actuelle
        pos <- next_pos 
      }
    }
  )
)
