makeCacheMatrix <- function(x = numeric()) {
        i <- NULL                                #set the inverse to NULL
        set <- function(y) {                 # set the matrix 
                x <<- y
               i <<- NULL
        }
        get <- function() x                  # get the matrix   
        setinverse <- function(inverse) i <<- inverse                             #set inverse of the matrix based on solve() from cacheSolve
        getinverse <- function() i                                                            #get inverse of the matrix based on the value in i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                                                                        #query the x matrix's cache  
        if(!is.null(i)) {                                                                                 #If already computed in cache then return the inverse without any computation
                message("getting cached data for inverse of the matrix")
                return(i)
        }
        data <- x$get()                                                                             #if there's no cache then use solve function in r to compute inverse
        i <- solve(data, ...)
        x$setinverse(i)                                                                            # set the cache with the inverse 
        i
}

