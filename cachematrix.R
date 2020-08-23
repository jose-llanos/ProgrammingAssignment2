# function makeCacheMatrix: creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()){       
        i <- NULL											# initialize inverse
        set <- function(y) {								# method set			
                m <<- y
                i <<- NULL
        }
        get <- function() m									# method get
        set_inverse <- function(inverse) i <<- inverse		# method set inverse
        get_inverse <- function() i							# method get inverse
        list(set = set,										# list methods (return) 
             get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}

# ------------------------------------------------------------------------------------

# function cacheSolve: computes the inverse of the special "matrix" returned
cacheSolve <- function(x, ...) {
        m <- x$get_inverse()					# return matrix inverse
        if(!is.null(m)) {						# validates the calculation of the inverse matrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()							# get matrix
        m <- solve(data) %*% data				# calculates inverse matrix
        x$set_inverse(m)						# set inverse object				
        m										# return matrix
}	

