Loggerhead <- setRefClass(
  "Loggerhead",
  fields = list(
    matrix = "list"
  ),
  methods = list(
    initialize = function() {
      matrix <<- list()
      turtle_init() # Initialise turtle graphics
      turtle_hide()
    },

   # Méthode pour ajouter un nouveau layer
    addLayer = function() {
      matrix <<- append(matrix, list(list()))  # Ajouter un layer vide
    },

    # Méthode publique pour ajouter une position à un layer spécifique
    goto = function(layerIndex, x, y, fill = TRUE) {
      if (length(matrix) < layerIndex || layerIndex < 1) {
        stop("Invalid layer index")
      }
      position <- c(x, y, fill)
      matrix[[layerIndex]] <<- append(matrix[[layerIndex]], list(position))
    }
  )
)