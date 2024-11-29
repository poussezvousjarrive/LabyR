intersect :
  
  # Pour pouvoir construire l'enveloppe convexe
  # (à la toute fin), il faut déjà ajouter les points 
  # sera erronée. Pour ça, je vais vérifier les intersections
  # entre les différents segments des polygones
  # On a besoin d'une fonction qui vérifie si deux lignes simples
  # s'entrechoquent (pas sûr du mot)
  intersect <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
    # Source : https://en.wikipedia.org/wiki/Line–line_intersection
    # L'algo se base sur les "parametres de bezier" t et u
    # t et u sont des fractions
    # Il y a intersection SSi 0<=t<=1 ET 0<=u<=1
    t_num <- (x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4)
    t_den <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
    
    u_num <- -((x1 - x2)*(y1 - y3) - (y1 - y2)*(x1 - x3))
    u_den <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
    
    if(t_den == 0 || u_den == 0) return(NULL) # Lignes parallèles...
    
    t <- t_num / t_den
    u <- u_num / u_den
    
    if ((t > 0 && t <= 1) && (u > 0 && u <= 1)) { # Intersection !
      p_x <- x1 + t*(x2 - x1)
      p_y <- y1 + t*(y2 - y1)
      return(c(p_x, p_y))
    } else {
      return(NULL)
    }
  }
  