## In order to solve for matrices using a cache you need to call the makeCacheMatrix function to store and create the matrix into the environment.
## After that you can call cacheSolve to solve for the inverse of the matrix and if you already solved for cacheSolve will pull the inverse from
## makeCacheMatrix rather than solving for it again.

## This function stores a matrix object. It also allows the ability to store its inverse saved into m. 
## You can set the inverse by using setinverse and you can get it using set inverse. You can also 
## set and get the matrix by using set and get respectively. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function tries to get the inverse from the object by requesting the variable m from makeCacheMatrix using x's getinverse().
## If there is nothing stored in m it means the cache is empty and m will be equal to null. If it is the if statement will not run
## and solve will be called on the matrix and the result will be stored in m and m will be returned. If it is null then the message
## "getting cached data" will print and the cached matrix will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
	