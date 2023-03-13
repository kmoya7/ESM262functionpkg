# Compute Fish Function
#' @param possible_fish = vector/txt file with list of names
#' function will return a list with the most common fish, the rarest fish, and the total count


compute_fish <- function(possible_fish) {
  fish_vect <- as.vector(possible_fish[,1])
  fish_fact <- as.factor(fish_vect)
  
  common_fish <- names(which.max(summary(fish_fact)))
  rarest_fish <- names(which.min(summary(fish_fact)))
  total_count <- sum(summary(fish_fact))
  
  fish_list <- c(common_fish, rarest_fish, total_count)
  
  return(fish_list)
}