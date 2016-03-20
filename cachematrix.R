## The first function lists the methods that set and get a matrix and its inverse.
## The second funciton is passed from the list and attempts the calculate and set its inverse.

## makeCacheMatrix will create a matrix x and then set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
	cachedInv <- NULL  ## initialise the inverse
	
	## set x in the parent environment with the desired value, if the inverse is already set, then ignore it
	set <- function(userValue = matrix()) {
		x <<- userValue
		cachedInv <- NULL
	}

	get <- function() x
	
	##set inverse variable in the parent environment to the desired value and return the value
	setInverse <- function(invVal) {
		cachedInv <<- invVal
		return(cachedInv)
	}  
	
	getInverse <- function() cachedInv
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Looking at the results of the first fuction, it will first check to see if an inverse exists already
## If none exists then it will attempt to calculate one

cacheSolve <- function(x=makeCacheMatrix(1:9, nrow=3, ncol=3),...) { ##matrix provided or creates a 3x3 matrix
        
        ## Check to see if an inverse already exists
        calculatedInverse <- x$getInverse()
        
        ## Check to see if there is a cached value and its matrix
        if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) {
        	message("We found an inverse matrix")
        	return(calculatedInverse)
        }
        
        ## Otherwise we need to get the matrix
        matrixToSolve <- x$get()
        
        ## Try to solve the matrix and find any errors or warnings
        calculatedInverse <- tryCatch( {
        	solve(matrixToSolve)
        } , warning=function(w) {
        	message("This may not be the correct result")
        	message(w)
        } , error=function(e) {
        	message("Something went wrong")
        	message(e)
        	message("\n")
        } )
        
        ##Regardless of what has happened, set the value of the inverse matrix (NULL if something went wrong)
        message("Here is the value of the inverse matrix:")
        x$setInverse(calculatedInverse) 
}

