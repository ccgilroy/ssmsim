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

# project.path <- "/Users/cgilroy/Documents/Research/ssmsim"
## Note that you shouldn't change the working directory after
## starting NetLogo
project.path <- getwd()
nl.path <- file.path(project.path, "NetLogo") 
r.path <- file.path(project.path, "R")
model.path <- file.path(nl.path, "ssmsim.nlogo")

run.config <- yaml.load_file(file.path(project.path, "run_config.yml"))
run.path <- file.path(project.path, run.config$run_path[1])

## TODO: add loop or vectorization so that the model runs for each run path

config <- yaml.load_file(file.path(run.path, "ssmsim_config.yml"))

networks.path <- file.path(run.path, "networks")
plots.path <- file.path(run.path, "plots")
output.path <- file.path(run.path, "output")

if (!dir.exists(networks.path)) { dir.create(networks.path) }
if (!dir.exists(plots.path)) { dir.create(plots.path) }
if (!dir.exists(output.path)) { dir.create(output.path) }

source(file.path(r.path, "ssmsim_netlogo.R"))
source(file.path(r.path, "ssmsim_networks.R"))
source(file.path(r.path, "ssmsim_data_cleaning.R"))
source(file.path(r.path, "ssmsim_output.R"))

## run simulations in NetLogo
Sys.setenv(NOAWT=1)  # need to set this to run headless
NLStart(nl.path, gui=FALSE)
NLLoadModel(model.path)

##################
### Model Runs ###
##################

## TODO: include replication of multiple networks
# results <- 
#   replicate(10, generateNetwork(10000, 500), simplify = FALSE) %>% 
#   generateNetworkFiles(dir = networks.path) %>% 
#   runSimulations() 

## pull parameters from config file and process as necessary
trait_dist <- if(config$homophily) distributeTraitPreferentially() else distributeTraitRandomly()
ally_delay <- if(config$allies) 0 else config$max_ticks
support_dist <- eval(parse(text = config$support_dist))
network_type <- eval(parse(text = config$network_type))
num_lgbts <- floor(config$pct/100 * config$num_nodes)

gs <- replicate(config$runs, 
                generateNetwork(num.nodes = config$num_nodes, 
                                num.lgbts = num_lgbts, 
                                sampleNetwork = network_type,
                                distributeTrait = trait_dist, 
                                distributeSupport = support_dist), 
                simplify = FALSE)
# g <- generateNetwork(num.nodes = config$num_nodes, 
#                      num.lgbts = num_lgbts, 
#                      sampleNetwork = network_type,
#                      distributeTrait = trait_dist, 
#                      distributeSupport = support_dist)
fs <- generateNetworkFiles(gs, dir = networks.path)

results <- 
  runSimulations(fs, 
                 ticks = config$max_ticks, 
                 growth.fn = "grow-exp", 
                 random.fn = "random-support", 
                 response.fn = "respond-transition-matrix", 
                 coming.out.delay = 0, 
                 ally.delay = ally_delay, 
                 lambda = config$lambda)

NLQuit()
Sys.unsetenv("NOAWT")  # unset environment variable

## unnest and save results
# results_unnested <- unnestNodeAttributes(results)
# write.csv(results_unnested, file.path(output.path, "results.txt")) 

run.name <- 
  Map(paste0, names(config[1:6]), config[1:6]) %>% 
  Reduce(function(x, y) paste(x, y, sep = "_"), x = .) 

##################
### Model Plots ##
##################

tick_list <- seq(0, config$max_ticks, by=10)
poll.plot.title <- 
  sprintf(
    "1%% Sample; Degree = %i; %% = %.0f; lambda = %.0f;\nhomophily = %s; allies = %s", 
    config$degree, config$pct, config$lambda, 
    config$homophily, config$allies
  )
poll.plot <- 
  results %>%
  sampleSupport(sample_proportion=0.01) %>% 
  pollSupport(mid_break=.2) %>% 
  filter(tick %in% tick_list) %>%
  group_by(tick, support_level) %>%
  summarise(count = mean(count)) %>%  # plot mean fraction over multiple runs
  plotPoll(num.nodes=.01*config$num_nodes) + geom_line() + 
  ggtitle(poll.plot.title)
poll.plot.file.name <- 
  file.path(plots.path, 
            paste0("poll_plot_", run.name, ".png"))
ggsave(filename = poll.plot.file.name, 
       plot = poll.plot, 
       width = 8, height = 5)

support.plot.title <- 
  sprintf(
    "Support; Degree = %i; %% = %.0f; lambda = %.0f;\nhomophily = %s; allies = %s", 
    config$degree, config$pct, config$lambda, 
    config$homophily, config$allies
  )
support.plot <- 
  results %>% 
  filter(run == "1") %>%  # only plot distribution from first run
  plotSupport(ticks = c(1, seq(50, config$max_ticks, by = 50))) + 
  ggtitle(support.plot.title)
support.plot.file.name <- 
  file.path(plots.path, 
            paste0("support_plot_", run.name, ".png"))
ggsave(filename = support.plot.file.name, 
       plot = support.plot, 
       width = 8, height = 5)

  
##################
### Model Data ###
##################

## save max value of proportion supportive
## save time point at which supportive > opposed

# metrics <- bind_rows(lapply(list(no_delay = results.no.delay, 
#                                  with_delay = results.with.delay, 
#                                  no_allies = results.no.allies), 
#                             reportComparisonMetrics), 
#                      .id = "run_type")

metrics <- 
  results %>%
  group_by(run) %>%
  do(
    reportComparisonMetrics(., num.nodes = config$num_nodes) %>%
    transmute(lambda = config$lambda, 
              percent = config$pct, 
              num_nodes = config$num_nodes,
              allies = config$allies, 
              homophily = config$homophily,
              degree = as.integer(config$degree),
              variable = variable,
              value = value)
  ) %>%
  ungroup()
#   reportComparisonMetrics(results, num.nodes = config$num_nodes) %>%
#   transmute(lambda = config$lambda, 
#             percent = config$pct, 
#             num_nodes = config$num_nodes,
#             allies = config$allies, 
#             homophily = config$homophily,
#             degree = as.integer(config$degree),
#             variable = variable,
#             value = value)
## NOTE: `degree = config$degree` throws an error:
## "Error: invalid subscript type 'closure'"

# metrics.run.name <- 
#   Map(paste0, names(config[1:6]), config[1:6]) %>% 
#   Reduce(function(x, y) paste(x, y, sep = "_"), x = .) %>%
#   paste(., "csv", sep = ".")
metrics.file.name <- file.path(output.path, 
                               paste(run.name, "csv", sep = "."))
write.csv(metrics, file = metrics.file.name, 
          row.names = FALSE)

## Remove network files to save space
lapply(fs, file.remove)