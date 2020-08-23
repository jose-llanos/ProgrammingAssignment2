# function makeCacheMatrix: creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()){   
	    
	    # initialize inverse    
        i <- NULL
        	
        # method set										
        set <- function(y) {											
                m <<- y
                i <<- NULL
        }
        
        # method get
        get <- function() m	
        
        # method set inverse							
        set_inverse <- function(inverse) i <<- inverse
        
        # method get inverse		
        get_inverse <- function() i
        
        # list methods (return) 							
        list(set = set,										
             get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}

# ------------------------------------------------------------------------------------

# function cacheSolve: computes the inverse of the special "matrix" returned
cacheSolve <- function(x, ...) {
	    
	    # return matrix inverse
        m <- x$get_inverse()	
        
        # validates the calculation of the inverse matrix				
        if(!is.null(m)) {						
                message("getting cached data")
                return(m)
        }
        
        # get matrix
        data <- x$get()
        
        # calculates inverse matrix							
        m <- solve(data) %*% data
        
        # set inverse object				
        x$set_inverse(m)
        	
        # return matrix										
        m										
}	
	

