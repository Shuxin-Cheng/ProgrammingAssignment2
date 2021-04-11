## A pair of function to calculate&cache matrix inverses
## or retrieve cached matrix inverses.

## a function to creates a special matrix that can cache
## its inverse.


MakeCacheMatrix <- function(x) {
   n <- NULL
   set <- function(y){
           x <<- y
           n <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) n <<- inverse
   getinverse <- function()n
   list(set=set,get=get,
        setinverse=setinverse, 
        getinverse=getinverse)
}


## a function to compute&cache inverses or retrieve cached 
## inverses

cacheSolve <- function(x,...) {
    n <- x$getinverse()
    if(!is.null(n)) {message("getting cached data")
                    n          
    }
    data <- x$get()
    n <- solve(data,...)
    x$setinverse(n)
    n
}


