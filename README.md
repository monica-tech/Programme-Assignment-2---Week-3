# Programme-Assignment-2---Week-3
## Caching the Inverse of a Matrix


## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse, which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function (y){
      x<<-y
      inv<<-NULL
    }
get<-function()x
setInverse<-function(inverse) inv<<-inverse
getInverse<-function()inv
list(set=set, get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}



## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getInverse()
    if (!is.null(inv)) {
        message ("getting cached data")
        return(inv)
    }
    dataframe<-x$get()
    inv<-solve(dataframe,...)
    x$setInverse(inv)
    inv
}
