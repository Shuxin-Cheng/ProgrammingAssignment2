## A pair of function to calculate&cache matrix inverses
## or retrieve cached matrix inverses.


## 1. A function to creates a special matrix that can cache
## its inverse:


MakeCacheMatrix <- function(x) {

##defines 'i' as an empty value to be filled.
     i <- NULL
   
##defines 'set' function to set x and i 
   set <- function(y){
           x <<- y
           i <<- NULL
   }

##defines get function to return the matrix
   get <- function() x

##defines 'setinverse' function to set (cache) the value of i
   setinverse <- function(inverse) i <<- inverse
   
##defines 'getinverse' function to fetch cached i value
   getinverse <- function()i
   
## return a list of the functions defined above.
   list(set=set,get=get,
        setinverse=setinverse, 
        getinverse=getinverse)
}


## 2. A function to compute&cache inverses or retrieve cached 
## inverses

cacheSolve <- function(x,...) {

## try to find the cached value for i    
    i <- x$getinverse()
    
##return i if cached value is found
    if(!is.null(i)) {message("getting cached data")
                    return(i)          
    }
##if no value found from cache, calculate the inverse(i) and cache the value 
##then return i
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}




