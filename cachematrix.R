## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## this function creates a special "matrix" object that can cache its inverse, and contains four steps
    ##1.set the value of the matrix x
    ##2.get the value of the matrix x
    ##3.set the value of m, the inverse of x
    ##4.get the value of m, the inverse of x
    m<-matrix(NA,nrow(x),nrow(x))
    set<-function(y){
        x<<-y
        m<<-matrix(NA,nrow(x),nrow(x))
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <-function() m
    list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <-x$getSolve()
    if(length(m[!is.na(m)])>0){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <-solve(data, ...)
    x$setSolve(m)
    m
}