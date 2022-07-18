## create a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 invM <- NULL                     # the inverse of x
 set <- function(y){
      x <<- y
      invM <<- NULL
 }
 get <- function() x             # return the matrix
 getinv <- function() invM        # returns the inverse
 setinv <- function(i) invM <<- i # sets the inverse
 list(set=set,get=get,getinv=getinv,setinv=setinv)
}


## compute the inverse of the "matrix" function returned by the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()        # reads the inverse of x
	if(!is.null(inv)){       # in case it exists
	   message("getting cached inverse")
	   return(inv)
	}
	matriz <- x$get()
	inv <- solve(matriz)
	x$setinv(inv)
	inv
}
