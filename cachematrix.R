## Las siguientes funciones almacenan en caché la inversa de una matriz.

## Esta función crea un objeto "matriz" especial (una lista en realidad) 
##que puede almacenar su inversa en el caché.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(inversa) s <<- inversa
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##Esta función calcula la inversa de la "matriz" especial
##devuelta por makeCacheMatrix  de arriba. Si la inversa ya se ha 
##calculado (y la matriz no ha cambiado), entonces la solución de 
##caché debería recuperar la inversa del caché.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
