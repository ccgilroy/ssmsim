require(RNetLogo)
require(dplyr)

runModel <- function(network_file, ticks = 100, 
                     growth.fn = "grow-exp", random.fn = "random-support", 
                     response.fn = "respond-max", 
                     coming.out.delay = 0, 
                     ally.delay = 0, 
                     lambda = 1.5, 
                     target.order = "default") {
  NLCommand("setup")
  NLCommand(paste0("nw:load-graphml \"", network_file, "\"")) 
  NLCommand("set-starting-conditions")
  NLCommand("set growth-function (task ", growth.fn, ")")
  NLCommand("set random-function (task ", random.fn, ")")
  NLCommand("set response-function (task ", response.fn, ")")
  NLCommand("set coming-out-delay ", coming.out.delay)
  NLCommand("set ally-delay ", ally.delay)
  NLCommand("set lambda ", lambda)
  NLCommand(paste0("set target-order \"", target.order, "\""))
  NLDoReport(ticks, "go", 
             c("ticks", 
               "map [[who] of ?] sort turtles", "list-support", 
               "map [[who] of ?] sort lgbts", "list-out"),
             as.data.frame = TRUE,
             df.col.names = c("tick", "turtle", "support", "lgbt", "out"))  # this data frame will be returned
}

# setupModel <- function(out = 0.20, support = 0) {
#   NLCommand("set initial-out-probability", out, 
#             "set initial-percent-support", support)
# }

runSimulations <- function(network_files, ticks = 100, 
                           growth.fn = "grow-exp",
                           random.fn = "random-support", 
                           response.fn = "respond-max", 
                           coming.out.delay = 0, 
                           ally.delay = 0, 
                           lambda = 1.5, 
                           target.order = "default") {
  # setupModel(out, support)
  bind_rows(lapply(network_files, runModel, ticks = ticks, growth.fn = growth.fn, 
                   random.fn = random.fn, response.fn = response.fn, 
                   coming.out.delay = coming.out.delay, ally.delay = ally.delay, 
                   lambda = lambda, target.order = target.order), 
            .id = "run")
}

