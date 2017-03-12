## Computing the inverse of a matrix is computationally expensive. 
## The function makeCacheMatrix allow you to cache the inverse of a matrix.  
## The function cacheSolve computes the inverse of the matrix if  
## it is not already cached in makeCacheMatrix

##Author: Jacques du Plessis
##Date: 12 March 2017

## This function creates a special matrix that caches it's inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the inverse matrix variable
        im <- NULL

        ## define the set function
        set <- function(y) {
                x <<- y
                #initiliaze cached inverse matrix when matrix changes with set
                im <<- NULL
        }		

        ## define the get function
        get <- function() x

        ## define the setinverse function
        setinverse <- function(inverse) im <<- inverse

        ## define the getinverse function
        getinverse <- function() im

        ## set the return value	
	  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of a invertable matrix. It will first check
## whether the inverse was previously cached.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  	  	
        ##check if already cached    
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }

       ##if not cached, compute the inverse of the matrix x
       mymatrix <- x$get()
       im <- solve(mymatrix, ...)

       ##update the cache
       x$setinverse(im)

       ##return the inverse matrix
	 im 
}
