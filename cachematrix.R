## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y){
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) Inv <<- solve
        getInv <- function() Inv
        list(set = set, get = get, setInv = setInv, 
             getInv = getInv)
}

## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        mtx <- x$get()
        Inv <- solve(mtx, ...)
        ## Return a matrix that is the inverse of 'x'
        x$setInverse(Inv)
        Inv
}
