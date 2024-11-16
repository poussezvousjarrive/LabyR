library(R6)
library(TurtleGraphics)

Loggerhead <- R6Class("Loggerhead",
  class = TRUE,
 
  public = list(
    layers = NULL,
    activeLayer = NULL,
    delta = 0.5,
    flow_rate = 1,
    nozzle_diam = 0.25,
    fil_radius = 0.5,

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
    # Pour générer le GCode correspondant à toutes les couches
    genFile = function(filename = "out.gcode") {
      gcode_str <- "HEADER TEST\n"

      for (i in 1:length(self$layers)) {
        # Coordonnée Z absolue (i-1 fois la hauteur d'une couche, 
        # la première couche étant à Z = 0)
        z <- (i-1)*self$delta
  
        movs <- self$layers[[i]]$movements
        
        if (length(movs) > 0) {
          curr_p <- movs[[1]] 
          
          for (j in 1:(length(movs) - 1)) {
            next_p <- movs[[j+1]]
            # Mention FILL
            if (next_p[3] != 0) {
              ## Toute cette partie sert à calculer le taux d'extrusion pour un vecteur donné
              # Première étape : savoir la longueur de la ligne qu'on trace
              curr_line_l <- sqrt((next_p[1] - curr_p[1])**2 + (next_p[2] - curr_p[2])**2)
              # Volume de fil à extruder 
              extr_v <- self$delta * self$flow_rate * self$nozzle_diam * curr_line_l
              # On divise par la surface du filament (qui dépend de son rayon)
              # et on obtient la longueur
              extr_l <- extr_v / (pi * (self$fil_radius)**2)
              # Commande G1 : Mouvement de travail (avec extrusion)
              curr_str <- sprintf("G1 X%f Y%f Z%f E%f\n", next_p[1], next_p[2], z, extr_l)
            } else {
              # Si FILL = 0 : mouvement rapide G0 (mouvement rapide sans extrusion)
              curr_str <- sprintf("G0 X%f Y%f Z%f\n", next_p[1], next_p[2], z)
            }
            gcode_str <- paste(gcode_str, curr_str, sep='')
          }
        }
      }
      write(gcode_str, filename)
    }
  )
)
