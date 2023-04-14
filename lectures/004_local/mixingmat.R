##########################################################################
# 	author: 		jimi adams
#	last updated:	2015-05-19
#	This script computes a "Mixing Matrix" for a specified attribute
#	in a passed complete network, for a single attribute at a time. 
#	It assumes a numerically coded attribute variable.
#	This works for an igraph object. There is a built in function to do this in SNA.
#########################################################################

mixmat <- function(mygraph, attrib, use.density=TRUE) {
  
  require(igraph)
  
  # get unique list of characteristics of the attribute
  attlist <- sort(unique(get.vertex.attribute(mygraph,attrib)))
  
  numatts <- length(attlist)
  
  # build an empty mixing matrix by attribute
  mm <- matrix(nrow=numatts, 
               ncol=numatts,
               dimnames=list(attlist,attlist))
  
  
  # calculate edge density for each matrix entry by pairing type
  el <- get.edgelist(mygraph,names=FALSE)
  if(is.directed(mygraph)=="FALSE") el<-matrix(c(el, el[,2],el[,1]), ncol=2)
  for (i in 1:numatts) {
    for (j in 1:numatts) {
      mm[i,j] <- length(which(apply(el,1,function(x) { 
        get.vertex.attribute(mygraph, attrib, x[1] ) == attlist[i] && 
          get.vertex.attribute(mygraph, attrib, x[2] ) == attlist[j]  } )))
    }  
  }
  
  # convert to proportional mixing matrix if desired (ie by edge density)
  if (use.density) mm/ecount(mygraph) else mm
}


assortcoeff <- function(m) {
  tr <- 0
  for (k in 1:nrow(m)) tr <- tr + m[k,k]
  sumsq <- sum (rowSums(m)*colSums(m))
  (tr - sumsq) / (1 - sumsq)
}

