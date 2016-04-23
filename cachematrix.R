##The functions below are a way to consume less resources in a computation process 
##to invert a matrix, does it using cache to store already inverted matrixes

## In this function a special matrix is created to invert and keep in cache 
## the value of a inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    
    set <- function(y){
        x <<- y
        inverted <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(toInverse){
        inverted <<- toInverse  
    } 
    
    getinverse <- function(){
        inverted
    }
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


##This function checks if the "matrix" created before in makeCacheMatrix, 
##has changed or  if it was inverted already. Then return the inverted value 
##of original matrix, using solve to invert a new matrix or return cached 
##value of already inverted matrix

## Tests:
## a <- matrix(c(4,1,3,2), nrow = 2, ncol = 2)
## m <- makeCacheMatrix(a)
## m$get()
##      [,1] [,2]
## [1,]    4    3
## [2,]    1    2
## cacheSolve(m)
##      [,1] [,2]
## [1,]  0.4 -0.6
## [2,] -0.2  0.8
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$getinverse()
    if(!is.null(inverted)){
        message("getting inverted matrix")
        inverted
    }
    matrix <- x$get()
    inverted <- solve(matrix,...)
    x$setinverse(inverted)
    inverted
}
