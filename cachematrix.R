#### Programming Assignment #2 ####

#Prob 1: Create the makeCacheMatrix function which creates a special
#"matrix" object that can cache its inverse. Remember that it must 
#set the value of the matrix, get the value of the matrix, set the 
#value of the inverse, and get the value of the inverse.

#Prob 2: Create the cacheSolve function which computes the inverse
#of the special "matrix" returned by makeCacheMatrix. If the inverse
#has already been calculated (and the matrix has not changed), then the
#cacheSolve function should retrive the inverse from the cache.

#Note: Computing the inverse of a matrix can be done with the command
#solve(x) for any square invertible matrix x.

#### Answers ####

#Answer #1: This function makeCacheMatrix will create a special "matrix"
#object that can cache its inverse.

makeCacheMatrix <- function (){
        #create empty vector in which to store cached inverse matrix
        inv <- NULL
        #set the value of the matrix to x and reset inv
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse
        setinv <- function(inverse) inv <<- inverse
        #get the value of the inverse
        getinv <- function() inv
        #return the special "matrix" with these functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#Answer #2: This function cacheSolve will compute the inverse of the
#special "matrix" returned by the previous function if the inverse has 
#not already been calculated, otherwise it will return what is cached.

cacheSolve <- function (x, ...) {
        #retrieve the inverse value of matrix x and assign to inv
        inv <- x$getinv()
        #check if the inverse is already calculated and retrieve it
        #return a message that you are retrieving it if true
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #the inverse is not calculated so it must be calculated now
        data <- x$get()
        inv <- solve(data, ...)
        #cache the inverse 
        x$setinv(inv)
        #and then return the inverse
        inv
}