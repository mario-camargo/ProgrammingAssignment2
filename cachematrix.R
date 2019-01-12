## Las siguientes funciones almacenan en caché la inversa de una matriz.

## Esta función crea un objeto "matriz" especial (una lista en realidad) 
##que puede almacenar su inversa en el caché.

makeCacheMatrix <- function(x = matrix()) { # el argumento de la función es una matriz invertible
        s <- NULL # la inversa de la matriz empieza como NULL
        set <- function(y) { 
                x <<- y # se le asigna de forma golbal una matrix a 'x'
                s <<- NULL
        }
        get <- function() x # función que regresa a la matriz 'x'
        setsolve <- function(inversa) s <<- inversa # se asiga de forma global el argumento a la matriz 's'
        getsolve <- function() s # función que regresa la matriz inversa 's'
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) # la función makeCacheMatrix regresa una lista con las funciones anteriores como elementos
}


##Esta función calcula la inversa de la "matriz" especial
##devuelta por makeCacheMatrix  de arriba. Si la inversa ya se ha 
##calculado (y la matriz no ha cambiado), entonces la solución de 
##caché debería recuperar la inversa del caché.

cacheSolve <- function(x, ...) { # función cuya entrada será la 'matriz' especial de la función de arriba
        s <- x$getsolve() # a 's' se le asigna el valor de la inversa de la matriz
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        } # si 's' no es NULL entonces regresa el valor de la inversa de la matriz guardada en la cache
        data <- x$get() # en otro caso, la matriz es asignada a la variable 'data'
        s <- solve(data, ...) # 's' ahora tiene asignada el valor de la inversa de la matriz
        x$setsolve(s) #  el valor de 's' se almacena en la cahce
        s # la función 'cacheSolve' regresa el valor de 's'
}
