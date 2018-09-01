## This file contains two functions: makeCacheMAtrix and cacheSolve


## makeCacheMArix creates a matrix object that caches its inverse


makeCacheMatrix <- function(x) {

        im <- NULL
        set <- function(y){
                x <<- y
                im <-- NULL
        }
        get <- function() x
        setinversematrix <- function(solve) im <<- solve
        getinversematrix <- function() im
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}

## cacheSolve requires an argument that is returned by makeCacheMatrix,
## if cached, retreves the inverse, and if not, executes solve() to return
## the inverse of the matrix


cacheSolve <- function(x, ...) {
        
        im <- x$getinversematrix()
        if(!is.null(im)){
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinversematrix(im)
        im
}
