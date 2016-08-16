require(igraph)
require(stringr)

generateNetworkFiles <- function(g.list, dir = getwd(), subdir = "network_files") {
  
  ## create a directory to store the network files in
  if (!dir.exists(file.path(dir, subdir))) {
    dir.create(file.path(dir, subdir))
  }
  
  ## generate file names for the networks
  network.file.names <- 
    lapply(1:length(g.list), function(x) { 
      file.path(dir, subdir, paste0("network_", str_pad(as.character(x), 3, pad = "0"), ".graphml"))
      })  
  
  ## save the networks to the list of file names
  mapply(write_graph, 
         graph = g.list, 
         file = network.file.names, 
         format = "graphml", 
         prefixAttr = FALSE)  # need this so NetLogo can read attributes
  
  ## return the list of file names
  network.file.names  
}

generateNetwork <- function(num.nodes = 1000, num.lgbts = 50,
                            sampleNetwork = sample_smallworld_wrapper(),
                            distributeTrait = distributeTraitRandomly(), 
                            distributeSupport = distributeSupportNormally()) {
  
  ## generate a network of the selected type
  g <- sampleNetwork(num.nodes)
  
  ## assign trait to some nodes based on selected distribution
  g <- distributeTrait(g, num.nodes, num.lgbts)
  
  ## set out to false for LGBT nodes
  V(g)[!is.na(breed) & breed == "lgbts"]$out <- FALSE  

  ## set node support based on selected distribution
  V(g)$support <- distributeSupport(num.nodes)
  
  # note: could use set_vertex_attr and chaining to do this with pipes
  
  ## calculate network homophily
  g$homophily <- assortativity_nominal(g, types = V(g)$type)
  
  ## return the network
  g
}

# distributeSupport <- function(num.nodes, support.distribution = "normal") {
#   ## TODO: turn this into different distributeSupport functions
#   ## e.g. distributeSupportNormally, distributeSupportUniformly
#   ## that way, can include new functions on the fly
#   ## ... how can I handle different parameters? 
#   ## is this where '...' comes in handy?
#   ## answer: closures
#   
#   if (support.distribution == "normal") {
#     ## normal distribution between -1 and 1, centered on 0
#     support <- rnorm(num.nodes, sd = 0.33)
#     support[support > 1] <- 1  # truncate
#     support[support < -1] <- -1
#   } else if (support.distribution == "uniform") {
#     ## uniform distribution between -1 and 1
#     support <- runif(num.nodes, -1, 1)
#   }
#   
#   support
# }

distributeSupportNormally <- function(mean = 0, sd  = .33, min = -1, max = 1) {
  function(num.nodes) {
    ## normal distribution between min and max, centered on mean
    support <- rnorm(num.nodes, mean = mean, sd = sd)
    support[support > max] <- max  # truncate
    support[support < min] <- min
    
    ## return vector of support values
    support
  }
}

distributeSupportUniformly <- function(min = -1, max = 1) {
  function(num.nodes) {
    ## uniform distribution between min and max
    runif(num.nodes, min, max)
  }
}

distributeSupportMixtureModel <- function(mean1 = -.5, sd1 = .2, 
                                          mean2 = .5, sd2 = .2, 
                                          min = -1, max = 1, 
                                          prob = .8) {
  function(num.nodes) {
    ## mixture of two normal distributions
    ## prob is the weight for the first distribution
    ## R code sourced from a question on Stack Exchange: 
    ## http://stats.stackexchange.com/questions/70855/simulating-random-variables-from-a-mixture-of-normal-distributions
    components <- sample(1:2, prob = c(prob, 1 - prob), size = num.nodes, 
                         replace = TRUE)
    mus <- c(mean1, mean2)
    sds <- c(sd1, sd2)
    
    support <- rnorm(n = num.nodes, mean = mus[components], sd = sds[components])
    
    support[support > max] <- max  # truncate
    support[support < min] <- min
    
    ## return vector of support values
    support
  }
}

distributeSupport1988 <- function(means = c(-.8, -.4, 0, .4, .8), 
                                  stdevs = c(.1, .1, .1, .1, .1), 
                                  probs = c(.47, .26, .15, .09, .03), 
                                  min = -1, 
                                  max = 1) {
  function(num.nodes) {
    ## mixture of five normal distributions
    ## probs are the weights for each distribution
    ## R code sourced from a question on Stack Exchange: 
    ## http://stats.stackexchange.com/questions/70855/simulating-random-variables-from-a-mixture-of-normal-distributions
    components <- sample(1:5, prob = probs, size = num.nodes, 
                         replace = TRUE)
    mus <- means
    sds <- stdevs
    
    support <- rnorm(n = num.nodes, mean = mus[components], sd = sds[components])
    
    support[support > max] <- max  # truncate
    support[support < min] <- min
    
    ## return vector of support values
    support
  }
}

# distributeTrait <- function(g, trait.distribution = "random") {
#   
#   if (trait.distribution == "random") {
#     ## make random nodes LGBT
#     V(g)[sample(1:num.nodes, num.lgbts)]$breed <- "lgbts"  
#     # NetLogo calls types of agents "breeds"
#   }
# 
#   ## for igraph plots and calculations, it's easier to work with numeric types
#   ## so, set nodes with the trait to type 1, and other nodes to type 2
#   ## TODO: reverse this? 
#   V(g)$type <- ifelse(V(g)$breed %in% "lgbts", 1, 2)  
#   # use `%in%` rather than `==` so NAs are set to 2
#   
#   g
# }

distributeTraitRandomly <- function() {
  function(g, num.nodes, num.lgbts) {
    ## make random nodes LGBT
    V(g)[sample(1:num.nodes, num.lgbts)]$breed <- "lgbts"
    # NetLogo calls types of agents "breeds"
    
    V(g)$type <- ifelse(V(g)$breed %in% "lgbts", 1, 2)
    # use `%in%` rather than `==` so NAs are set to 2
    
    g
  }
}

distributeTraitPreferentially <- function(n = 1, prob = 1) {
  function(g, num.nodes, num.lgbts) {
    if (n > num.lgbts) n <- num.lgbts
    
    ## randomly set n seeds
    V(g)[sample(1:num.nodes, n)]$type <- 1
    V(g)[is.na(type)]$type <- 2
    
    ## select neighbors
    while(length(V(g)[type == 1]) < num.lgbts) {
      ## get list of vertices adjacent to vertices with {trait}
      vs <- do.call(igraph::union, adjacent_vertices(g, V(g)[type == 1]))[type == 2]
      ## should nodes with multiple {trait} neighbors be more likely to have the trait?
      
      ## create a logical vector to determine which neighbors to assign the trait to 
      ps <- runif(length(vs)) <= prob
      
      ## prevent overshooting num.lgbts
      remainder <- num.lgbts - length(V(g)[type == 1])
      overshoot <- length(which(ps)) - remainder
      if (overshoot > 0) ps[sample(which(ps), overshoot)] <- FALSE
      
      ## assign the trait to the selected vertices
      V(g)[vs[ps]]$type <- 1
    }
    
    ## name the trait appropriately for NetLogo
    V(g)$breed <- ifelse(V(g)$type == 1, "lgbts", NA)
    
    ## return the modified igraph object
    g
  }
}

getNeighborsOfTrait <- function(g, order = 1) {
  n <- neighborhood(graph = g, nodes = V(g)[type == 1], order = order)
  do.call(igraph::union, n)
}

plotNeighborsOfTrait <- function(g, n) {
  V(g)[n[type == 2]]$type <- 3
  plot(g, 
       vertex.color = V(g)$type, 
       vertex.label = NA, 
       vertex.size = 2, 
       vertex.frame.color = adjustcolor("white", alpha.f = 0))
}

plotGraphNoEdges <- function(g, n = NULL) {
  if (!is.null(n)) V(g)[n[type == 2]]$type <- 3
  plot(g, 
       vertex.color = V(g)$type, 
       vertex.label = NA, 
       vertex.size = 2, 
       vertex.frame.color = adjustcolor("white", alpha.f = 0), 
       edge.color = adjustcolor("darkgrey", alpha.f = 0), 
       layout = layout_randomly)  
}

# TODO: make LGBT nodes a distinctive color, make the other nodes dark gray. 
# maybe make them different sizes as well. 

plotGraphSupport <- function(g) {
  ## source for mapping continuous data onto a (diverging) gradient: 
  ## http://stackoverflow.com/questions/26176807/igraph-node-colors-according-to-gene-expression-values-attributes
  breaks <- c(-1, -.75, -.5, -.25, .25, .5, .75, 1)
  color.assignments <- cut(V(g)$support, breaks = breaks, include.lowest = TRUE)
  colors <- RColorBrewer::brewer.pal(7, "RdYlBu")
  
  V(g)$color <- colors[color.assignments]
  plot(g, 
       vertex.label = NA, 
       vertex.size = 2, 
       vertex.frame.color = adjustcolor("white", alpha.f = 0), 
       edge.color = adjustcolor("darkgrey", alpha.f = .2))
}

sample_smallworld_wrapper <- function(dim = 1, nei = 2, p = 0.10) {
  function(num.nodes) {
    sample_smallworld(dim = dim, size = num.nodes, nei = nei, p = p)
  }
}

sample_gnp_wrapper <- function(p) {
  ## p is approx average degree / (num.nodes - 1)
  function(num.nodes) {
    sample_gnp(n = num.nodes, p = p)
  }
}

sample_gnm_wrapper <- function(nei = 2) {
  ## nei is average degree / 2
  ## just like for sample_smallworld()
  function(num.nodes) {
    sample_gnm(num.nodes, num.nodes*nei)
  }
}

sample_pa_wrapper <- function(nei = 2) {
  ## m is the number of edges to add in each time step
  ## where the number of time steps is the number of nodes - 1
  ## so m = nei = average degree / 2
  function(num.nodes) {
    sample_pa(n = num.nodes, m = nei, directed = FALSE)
  }
}