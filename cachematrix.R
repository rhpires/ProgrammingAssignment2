## The following functions cache the inverse of a matrix.
## The first creates an R object that stores a matrix and its inverse.
## The second uses an argument from makecacheMatrix() to calculate the
## inverse or to retrieve it from the cached value which is stored in 
## makecacheMatrix.

## makecacheMatrix creates an R object containing: set() to set the 
## value of the matrix, get() to get the value of the matrix, 
## setinverse() to set the value of the inverse , and getmatrix() to
## set the value of the inverse.

makecacheMatrix <- function(x = matrix()) {
       inversematrix <- NULL
        set <- function(y) {
                x <<- y 
                inversematrix <<- NULL
        }        
        
        get <- function() x
        setinverse <- function(inv) inversematrix <<- inv
        getinverse <- function() inversematrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve uses arguments of makecacheMatrix to check if the 
## value of the inverse has been store in cache, if it's not, it will
## calculate the value of the inverse and sets the value in cache.

cacheSolve <- function(x, ...){
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)){
                message("getting cached data")
                return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data)
        x$setinverse(inversematrix)
        inversematrix
}
