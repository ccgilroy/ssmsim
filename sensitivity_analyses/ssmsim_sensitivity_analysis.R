##################
###   Setup    ###
##################
set.seed(123)
options(java.parameters = "-Xmx12288m")  # increase Java heap size. 
# default heap size is only 512 MB, increase to 12 GB 
# must do before loading rJava package

library(RNetLogo)  # loads rJava package
library(plyr)
library(dplyr)  # plyr MUST be loaded before dplyr
library(tidyr)
library(stringr)
library(ggplot2)
library(igraph)
library(yaml)
library(lattice)

## Note that you shouldn't change the working directory after
## starting NetLogo
project.path <- getwd()
nl.path <- file.path(project.path, "NetLogo") 
r.path <- file.path(project.path, "R")
model.path <- file.path(nl.path, "ssmsim.nlogo")
sa.path <- file.path(project.path, "sensitivity_analyses")

source(file.path(r.path, "ssmsim_netlogo.R"))
source(file.path(r.path, "ssmsim_networks.R"))
source(file.path(r.path, "ssmsim_data_cleaning.R"))
source(file.path(r.path, "ssmsim_output.R"))

run.config <- yaml.load_file(file.path(sa.path, "run_config.yml"))
run <- run.config$run_path[1]
run.path <- file.path(sa.path, run)
## read config file with parameters for specific set of model runs
config <- yaml.load_file(file.path(sa.path, "sensitivity_analysis_config.yml"))

## Each run path has a different yaml config file 
## with a different set of parameters
parameter.space <- 
  expand.grid(lambda = seq(config$lambda$min, config$lambda$max), 
              pct = seq(config$pct$min, config$pct$max)) %>%
  as.list()

## Output each run to a separate csv file
files <- 
  Map(function(lambda, pct) { 
    ## run simulations in NetLogo
    Sys.setenv(NOAWT=1)  # need to set this to run headless
    NLStart(nl.path, gui=FALSE)
    NLLoadModel(model.path)
    
    ##################
    ### Model Runs ###
    ##################
    
    ## pull parameters from config file and process as necessary
    trait_dist <- if(config$homophily) distributeTraitPreferentially() else distributeTraitRandomly()
    ally_delay <- if(config$allies) 0 else config$max_ticks
    support_dist <- eval(parse(text = config$support_dist))
    network_type <- eval(parse(text = config$network_type))
    num_lgbts <- floor(pct/100 * config$num_nodes)
    
    gs <- replicate(config$runs, 
                    generateNetwork(num.nodes = config$num_nodes, 
                                    num.lgbts = num_lgbts, 
                                    sampleNetwork = network_type,
                                    distributeTrait = trait_dist, 
                                    distributeSupport = support_dist), 
                    simplify = FALSE)
    network_files <- paste("lambda", lambda, "pct", pct, sep = "_")
    fs <- generateNetworkFiles(gs, dir = run.path, subdir = network_files)
    rm(gs)  # remove graph objects to save memory
    
    results <- 
      runSimulations(fs, 
                     ticks = config$max_ticks, 
                     growth.fn = "grow-exp", 
                     random.fn = "random-support", 
                     response.fn = "respond-transition-matrix", 
                     coming.out.delay = 0, 
                     ally.delay = ally_delay, 
                     lambda = lambda)
    
    NLQuit()
    Sys.unsetenv("NOAWT")  # unset environment variable
    
    run.name <- 
      Map(paste0, names(config[1:6]), c(lambda, pct, config[3:6])) %>% 
      Reduce(function(x, y) paste(x, y, sep = "_"), x = .) 
    
    ##################
    ### Model Data ###
    ##################
    
    ## save max value of proportion supportive
    ## save time point at which supportive > opposed
    metrics <- 
      results %>%
      group_by(run) %>%
      do(
        reportComparisonMetrics(., num.nodes = config$num_nodes) %>%
          transmute(lambda = lambda, 
                    percent = pct, 
                    num_nodes = config$num_nodes,
                    allies = config$allies, 
                    homophily = config$homophily,
                    degree = as.integer(config$degree),
                    variable = variable,
                    value = value)
      ) %>%
      ungroup()
    ## NOTE: `degree = config$degree` throws an error:
    ## "Error: invalid subscript type 'closure'"
    
    metrics.file.name <- file.path(run.path, 
                                   paste(run.name, "csv", sep = "."))
    write.csv(metrics, file = metrics.file.name, 
              row.names = FALSE)
    
    ## Remove network files to save space
    lapply(fs, file.remove)
    unlink(file.path(run.path, network_files), recursive = TRUE)
    rm(results)
    
    ## Return file name
    metrics.file.name
    },
    parameter.space$lambda,
    parameter.space$pct
  )

## Combine csv files
data.list <- lapply(files, read.csv)
data <- bind_rows(data.list)
sa.name <-
  Map(paste0, names(config[3:6]), c(config[3:6])) %>% 
  c("sensitivity", "analysis", .) %>%
  Reduce(function(x, y) paste(x, y, sep = "_"), x = .)
write.csv(data, file = file.path(run.path, paste(sa.name, "csv", sep = ".")), 
          row.names = FALSE)
lapply(files, file.remove)

## Create surface plot of max proportion supportive
data_max <- data %>% filter(variable == "max proportion supportive")
plot.title <- 
  sprintf(
    "Sensitivity analysis, homophily = %s, allies = %s", 
    config$homophily,
    config$allies
  )
png(filename = file.path(run.path, paste0("surface_plot_", sa.name, ".png")), 
    width = 8, height = 5, units = "in", res = 300)
wireframe(value ~ lambda * percent, data = data_max, 
          zlab = list("max proportion supportive", rot = 90), 
          main = plot.title,
          drape = TRUE, 
          colorkey = TRUE, 
          scales = list(arrows = FALSE))
dev.off()