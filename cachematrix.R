## Caching the inverse of a matrix

## function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        a<-NULL
        set <-function(y) {
                x<<-y
                a<<-NULL
                }
        get<- function() x
        setinverse<-function (inverse) a<<-inverse
        getinverse<-function() a
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        }


## function to calculate the inverse of matrix object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a<-x$getinverse()
        if(!is.null(a)){
                message("getting cached data")
                return(a)
                }
        data<-x$get()
        a<-solve(data,...)
        x$setinverse(a)
        a
}
