## This file contains 2 functions:

##  makeCacheMatrix - creates 4 objects (set, get, setinverse, getinverse) for handling 
##  a matrix and its inverse

##  cacheSolve - checks if inverse has been computed, and grabs from cache if so.  If
##  not, then inverse is computed usind solve()


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## iniitialize inverse
	inv <- NULL
	
## set value of matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

## get value of matrix
	get <- function() x

## set matrix inverse
	setinverse <- function(matrixinv) inv <<- matrixinv

## get matrix inverse
	getinverse <- function() inv

## make these functions available to other functions
	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## cacheSolve checks to see if inverse has already been computed.  if so
## it gets inverse from the cache.  if not, it computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

##  get the inverse of the matrix
	inv <- x$getinverse

##  if this is not null, then the inverse has already been computed.  Let's
##  give a message that we are retrieving from cache, then return inverse
	if(!is.null(inv)) {
		message('getting cached data')
		return(inv)  ## return inverse and exit function
	}

##  if the inverse wasn't in the cache, then we must calculate it

##  get the matrix
	data <- x$get()

## solve for the inv
	inv <- solve(data)

## set the inverse
	x$setinverse(inv)

## return inverse
	inv

}
