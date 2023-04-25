alter_mean <- function(graph, attribute, mode="all"){				# mode can also be "in" or "out"
  mean_attr <- list(0)											# initializing a list
  for (i in 1:vcount(graph)){										# for all nodes in school
    ineighbors <- neighbors(graph, i, mode)						# find the neighbors
    attr <- get.vertex.attribute(graph, attribute, ineighbors)	# finding the neighbors' attribute
    mean_attr[[i]] <- mean(attr, na.rm=T)						# averaging over the list
  }
  return(unlist(mean_attr))										# returning it as a vector
}