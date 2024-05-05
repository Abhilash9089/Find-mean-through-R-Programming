# Find-mean-through-R-Programming
# Create a function that caches the result of a time-consuming computation
make_cacheable_function <- function(computation_function) {
  # Initialize an empty cache
  cache <- NULL
  
  # Create a function that checks the cache and computes if necessary
  cached_function <- function(...) {
    # Convert input arguments to a unique string for caching
    args <- list(...)
    cache_key <- paste(args, collapse = "_")
    
    # Check if the result is already in the cache
    if (!is.null(cache[[cache_key]])) {
      message("Using cached result.")
      return(cache[[cache_key]])
    } else {
      # Compute the result using the provided computation function
      result <- computation_function(...)
      
      # Cache the result
      cache[[cache_key]] <- result
      message("Computing and caching result.")
      return(result)
    }
  }
  
  # Return the cached function
  return(cached_function)
}

# Example usage:
# Create a time-consuming computation function (e.g., calculating the mean)
slow_mean <- function(x) {
  Sys.sleep(2)  # Simulate a time-consuming operation
  return(mean(x))
}

# Create a cached version of the slow_mean function
cached_mean <- make_cacheable_function(slow_mean)

# Test the cached function
print(cached_mean(c(1, 2, 3)))  # This will compute and cache the result
print(cached_mean(c(1, 2, 3)))  # This will use the cached result
