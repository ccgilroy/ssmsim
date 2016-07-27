##################
###   Setup    ###
##################

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

project.path <- "/Users/cgilroy/Documents/Research/ssmsim"
nl.path <- file.path(project.path, "NetLogo") 
r.path <- file.path(project.path, "R")
model.path <- file.path(nl.path, "ssmsim.nlogo")
run.path <- file.path(project.path)

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

set.seed(123)
## TODO: include replication of multiple networks
# results <- 
#   replicate(10, generateNetwork(10000, 500), simplify = FALSE) %>% 
#   generateNetworkFiles(dir = networks.path) %>% 
#   runSimulations() 
g <- generateNetwork(num.nodes = 100000, num.lgbts = 4000, nei = 10,
                     distributeTrait = distributeTraitRandomly(), 
                     distributeSupport = distributeSupport1988())
fs <- generateNetworkFiles(list(g), dir = networks.path, subdir = "full_model")

results.no.delay <- 
  runSimulations(fs, 
                 ticks = 400, 
                 growth.fn = "grow-exp", 
                 random.fn = "random-support", 
                 response.fn = "respond-transition-matrix", 
                 coming.out.delay = 0, 
                 ally.delay = 0, 
                 lambda = 2)

NLQuit()
Sys.unsetenv("NOAWT")  # unset environment variable

## unnest and save results
results_unnested <- unnestNodeAttributes(results)
output.path <- file.path(project.path, "output")
if (!dir.exists(output.path)) { dir.create(output.path) }
write.csv(results_unnested, file.path(output.path, "results.txt")) 

##################
### Model Plots ##
##################

## aggregate and plot results
poll_df <- pollSupport(results)
# poll_df2 <- pollSupport(results2)
# poll_df3 <- pollSupport(results3)

plotPoll(poll_df)
plots.path <- file.path(project.path, "plots")
if(!dir.exists(plots.path)) { dir.create(plots.path) }
ggsave(file.path(plots.path, "results.png"))
# plotPoll(poll_df2)
# ggsave("plot/results2.png")
# plotPoll(poll_df3)
# ggsave("plot/results3.png")


##################
### Model Data ###
##################

## save max value of proportion supportive
## save time point at which supportive > opposed

metrics <- bind_rows(lapply(list(no_delay = results.no.delay, 
                                 with_delay = results.with.delay, 
                                 no_allies = results.no.allies), 
                            reportComparisonMetrics), 
                     .id = "run_type")

metrics <- 
  metrics %>%
  transmute(lambda = 2, 
            percent = 4, 
            degree = 20,
            trait = "random", 
            run_type = run_type, 
            variable = variable,
            value = value)

write.csv(metrics, file = file.path(output.path, "d20_pct4_random_metrics.csv"), 
          row.names = FALSE)