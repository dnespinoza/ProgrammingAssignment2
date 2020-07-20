##  The function generate a list with four functions 
## this allows set and get a matrix and his inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<-y
        inv <<-NULL
    } 
    get <- function() x
    setinv<- function(inverse) inv <<- inverse
    getinv <- function(){
        inv
    }
    list(set = set, get=get, setinv = setinv , getinv = getinv)
}
## This function first verify that the matrix inverse doesn't exist in the
## environment, if is not then calculates the inverse and set it like 
## setinv in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
                        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx,...)
    x$setinv(inv)
    inv
}
