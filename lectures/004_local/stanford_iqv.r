get_iqvs <- function(graph, attribute) {
  
  #we have now defined a function, get_iqvs, that will take the
  # graph "graph" and find the iqv statistic for the categorical
  # variable "attribute." Within this function whenever we use the 
  #variables graph or attribute they correspond to the graph and
  # variable we passed (provided) to the function
  
  mat <- get.adjacency(graph)
  
  # To make this function work on a wide variety of variables we
  # find out how many coded levels (unique responses) exist for
  # the attribute variable programatically
  
  attr_levels = get.vertex.attribute(graph,
                                     attribute,
                                     V(graph))
  
  num_levels = length(unique(attr_levels))
  iqvs = rep(0, nrow(mat))
  
  # Now that we know how many levels exist we want to loop
  # (go through) each actor in the network. Loops iterate through
  # each value in a range.  Here we are looking through each ego
  # in the range of egos starting at the first and ending at the
  # last.  The function nrow provides the number of rows in an
  # object and the ":" opperand specifies the range.  Between
  # the curly braces of the for loop ego will represent exactly
  # one value between 1 and the number of rows in the graph
  # object, iterating by one during each execution of the loop.
  
  for (ego in 1:nrow(mat)) {
    
    # initialize actor-specific variables
    alter_attr_counts = rep(0, num_levels)
    num_alters_this_ego = 0
    sq_fraction_sum = 0
    
    # For each ego we want to check each tied alter for the same
    # level on the variable attribute as the ego.
    
    for (alter in 1:ncol(mat)) {
      
      # only examine alters that are actually tied to ego
      if (mat[ego, alter] == 1) {
        
        num_alters_this_ego = num_alters_this_ego + 1
        
        # get the alter's level on the attribute 
        alter_attr = get.vertex.attribute(graph, 
                                          attribute, (alter - 1))
        
        # increment the count of alters with this level
        # of the attribute by 1
        alter_attr_counts[alter_attr + 1] =
          alter_attr_counts[alter_attr + 1] + 1
      }
    }
    
    # now that we're done looping through all of the alters,
    # get the squared fraction for each level of the attribute
    # out of the total number of attributes
    for (i in 1:num_levels) {
      attr_fraction = alter_attr_counts[i] /
        num_alters_this_ego
      sq_fraction_sum = sq_fraction_sum + attr_fraction ^ 2
    }
    
    # now we can compute the ego's blau index...
    blau_index = 1 - sq_fraction_sum
    
    # and the ego's IQV, which is just a normalized blau index
    iqvs[ego] = blau_index / (1 - (1 / num_levels))
  }
  
  # The final part of a function returns the calculated value.
  #  So if we called get_iqvs(testgraph, gender) return would
  # provide the iqvs for gender in the test graph.  If we are also
  # intersted in race we could simply change the function call
  # to get_iqvs(testgraph, race).  No need to write all this
  # code again for different variables.
  
  return(iqvs)
}


