# Step 1: Create a function to generate a special "vector" object with caching ability
makeCacheVector <- function(x = numeric()) {
  # Initialize a NULL variable to store the cached mean
  cached_mean <- NULL
  
  # Function to set the vector (and clear the cache)
  set <- function(y) {
    x <<- y
    cached_mean <<- NULL
  }
  
  # Function to get the vector
  get <- function() x
  
  # Function to set the cached mean
  setMean <- function(mean) cached_mean <<- mean
  
  # Function to get the cached mean
  getMean <- function() cached_mean
  
  # Return a list of functions
  list(set = set, get = get,
       setMean = setMean,
       getMean = getMean)
}

# Step 2: Create a function to compute the mean with caching

cacheMean <- function(v) {
  # Check if the mean is already cached
  cached_mean <- v$getMean()
  if (!is.null(cached_mean)) {
    message("Getting cached data")
    return(cached_mean)
  }
  
  # If not cached, compute the mean
  data <- v$get()
  cached_mean <- mean(data)
  
  # Cache the calculated mean
  v$setMean(cached_mean)
  
  cached_mean
}
# Create a new cache-enabled vector
v <- makeCacheVector(c(1, 2, 3, 4))

# Compute the mean (not cached initially)
cacheMean(v) # Outputs: 2.5

# Compute the mean again (this time it will use the cache)
cacheMean(v) # Outputs: 2.5, with message "Getting cached data"

# Update the vector
v$set(c(10, 20, 30, 40))

# Compute the mean of the new vector (cache reset)
cacheMean(v) # Outputs: 25
