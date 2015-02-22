## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL 
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x 
        
        ## Set the valueof the inverse of the matrix
        setInv <- function(inv) xinv <<- inv 
        
        ## Get the value of the inverse of the matriz
        getInv <- function() xinv 
        
        ## Return our list
        list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}
