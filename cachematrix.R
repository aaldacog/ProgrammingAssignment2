## Assignment 2
## Abraham Aldaco  abraham.aldaco@gmail.com

## This function basically receive as parameter a matrix
## it puts the matrix in memory "matrixx" and it defines 4 sub-functions
##   set - can put in memory a different matrix with out call makeCacheMatrix
##   get - read whatever matrix is in memory
##   setInverse - calls "solve" which is the inverse of a matrix
##   getInverse - which shows the matrix
##

makeCacheMatrix <- function(x = matrix()) {
    matrixx <- NULL
    set <- function(y){
        x <<- y
        matrixx <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) matrixx <<- solve
    getInverse <- function() matrixx
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function mainly returns the inverse of the matrix 
## if the inverse of the matrix already was computed previously,
## just it shows what is in memory (e.g, cached)
## otherwise, take the matrix, compute inverse and put it in memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrixx <- x$getInverse()
    if(!is.null(matrixx)) {
        message("getting cached matrixx")
        return(matrixx)
    }
    mdata <- x$get()
    matrixx <- solve(mdata, ...)
    x$setInverse(matrixx)
    matrixx
}
