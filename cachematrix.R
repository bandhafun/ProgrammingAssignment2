## This program is to find an inverse of the matrix. 
## And in the event the inverse is already calculated to get it from the cache.

## Creates a list with functions to cahe and retrieve matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
		singular <<- NULL
		inv <<- NULL
		setdata <- function (y){
			x  <<- y
			singular <- FALSE
			inv <<- NULL
		}	
		getdata <- function() x
		setinverse <- function(y) inv <<- y
		getinverse <- function() inv
		setsingular <- function(chk) singular <<- chk
		getsingular <- function() singular
		list(setdata=setdata, getdata=getdata,setinverse = setinverse,getinverse=getinverse,setsingular=setsingular,getsingular=getsingular)		
}

## Returns an inverese of a matrix from the cache if present 
## else calculates and caches the inverse

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		if(x$getsingular()){
			message("The matrix is singular and hence the inverse does not exist")
			return(NA)
		}
		else return (inv)
	}
	data <- x$getdata()
	if(det(data)==0)
	{
		message("The matrix is singular and hence the inverse does not exist")	
		chkS <- TRUE
		x$setsingular(chkS)
		x$setinverse(NA)			
	}else{
		chkS <- FALSE
		x$setsingular(chkS)
		x$setinverse(solve(data))
	}
	x$getinverse()
}