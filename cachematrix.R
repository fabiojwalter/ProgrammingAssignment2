## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
