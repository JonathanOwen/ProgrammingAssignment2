## Two functions, makeCacheMatrix and cacheSolve, are used to
## create and cache values for a matrix and its inverse.
## Assigning each output of makeCacheMatrix to a list object
## creates pointers to where each instance of the matrix,
## inverse matrix, and the functions for setting and retrieving
## them are stored in memory.
## These lists are passed as arguments to cacheSolve, which uses 
## the functions defined in makeCacheMatrix to return an inverse
## matrix from the list if one is present or calculate and store
## a new inverse if not.

 

## makeCacheMatrix defines 4 functions
## set(x), get(x), setinv(solve), & getinv(solve)
## and returns them in a list.
## The functions assign & return values for 'x' and 'solve'.
## 'x' is a matrix. 
## 'solve' should be the inverse of matrix 'x'
## (for cacheSolve to work properly)
## All functions are enclosed within makeCacheMatrix.
## values of 'x' and solve' are those of the enclosing
## environment, which allows them to be different from those in
## the global environment.
## NOTE: The function set(y) is optional to allow changing of a 
## stored matrix. If this function is called, it also assigns
## null to the inverse so that cacheSolve will be forced to
## recalculate it.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invm <<- solve
        getinv <- function() invm
        
        list( set = set,
              get = get,
              setinv = setinv,
              getinv = getinv)

}


## cacheSolve takes closure functions output from
## makeCacheMatrix as an argument.
## It first calls getinv(), which will return the cached
## inverse matrix if one is present.
## If there is no cached inverse, the original matrix
## is retrieved by get() and its inverse is then
## calculated using solve().
## After this is calculated setinv() stores the inverse
## in the cache.
## The calculated inverse matrix is returned.

cacheSolve <- function(x, ...) {        
        invm <- x$getinv()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinv(invm)
        invm
}
