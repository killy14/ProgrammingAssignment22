## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Cette fonction crée un objet spécial "matrice" qui peut mettre en cache son inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialisation de la variable pour stocker l'inverse en cache
        inv <- NULL
        
        # Fonction pour affecter une nouvelle valeur à la matrice
        set <- function(y) {
                x <<- y  # Affectation de la nouvelle valeur à la matrice
                inv <<- NULL  # Réinitialisation du cache de l'inverse
        }
        
        # Fonction pour obtenir la matrice actuelle
        get_matrix <- function() x
        
        # Fonction pour mettre en cache l'inverse de la matrice
        setinv <- function(solve) inv <<- solve
        
        # Fonction pour récupérer l'inverse mis en cache
        getinv <- function() inv
        
        # Retourner les fonctions définies dans une liste
        list(set = set, get_matrix = get_matrix,
             setinv = setinv, getinv = getinv
        )        
}


## Write a short comment describing this function

# Cette fonction calcule l'inverse de la "matrice" spéciale renvoyée par 
# makeCacheMatrix ci-dessus. Si l'inverse a déjà été calculé 
# (et que la matrice n'a pas changé), cacheSolve doit récupérer l'inverse 
# dans le cache.

cacheSolve <- function(x, ...) {
        # Récupération de l'inverse depuis le cache
        inv <- x$getinv()  # Appel de la fonction getinv pour obtenir l'inverse
        
        # Vérification si l'inverse est déjà en cache
        if (!is.null(inv)) {
                message("Getting cached data")  # Affichage d'un message pour indiquer que les données sont récupérées depuis le cache
                return(inv)  # Retour de l'inverse depuis le cache
        }
        
        # Si l'inverse n'est pas en cache, calcul de l'inverse de la matrice
        dt <- x$get_matrix()  # Récupération de la matrice
        inverse <- solve(dt, ...)  # Calcul de l'inverse de la matrice
        
        # Mise en cache de l'inverse calculé
        x$setinv(inverse)
        
        # Retour de l'inverse calculé
        inverse
}
