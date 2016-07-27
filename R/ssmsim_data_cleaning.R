require(dplyr)
require(tidyr)
require(stringr)

unnestNodeAttributes <- function(model_df) {
  df1 <- 
    model_df %>% 
    select(run, tick, turtle, support) %>%
    unnest(turtle, support) %>%
    spread(turtle, support)  # attempt to rename turtle with mutate was too slow
  df2 <- 
    model_df %>%
    select(run, tick, lgbt, out) %>%
    unnest(lgbt, out) %>%
    mutate(lgbt = sapply(lgbt, function(x){ 
      paste0("out", str_pad(as.character(x), 5, pad = "0"))
    })) %>%
    spread(lgbt, out)
  inner_join(df1, df2, by = c("run", "tick"))
}

pollSupport <- function(model_df, mid_break = .33) {
  df <- 
    model_df %>%
    mutate( 
      low_support = sapply(support, function(x) length(x[x < -mid_break])),
      mid_support = sapply(support, function(x) length(x[x > -mid_break & 
                                                           x < mid_break])),
      high_support = sapply(support, function(x) length(x[x > mid_break]))
    ) %>% 
    select(run, tick, low_support, mid_support, high_support) %>%
    gather(key = support_level, value = count, 
           high_support, mid_support, low_support)
}

sampleSupport <- function(model_df, sample_proportion = .01) {
  df <- 
    model_df %>%
    mutate(support = lapply(support, function(x) { 
      sample(x, size = sample_proportion * length(x))
      }))
}

reportComparisonMetrics <- function(model_df, mid_break = .2, num.nodes = 100000) {
  df <- 
    model_df %>%
    mutate( 
      low_support = sapply(support, function(x) length(x[x < -mid_break])),
      mid_support = sapply(support, function(x) length(x[x > -mid_break & 
                                                           x < mid_break])),
      high_support = sapply(support, function(x) length(x[x > mid_break]))
    ) %>% 
    select(run, tick, low_support, mid_support, high_support)
  
  df_cross <- filter(df, high_support > low_support)
  t_cross <- ifelse(length(df_cross$tick) > 0, min(df_cross$tick), NA)
  max_support <- max(df$high_support)/num.nodes
  
  data.frame(variable = c("cross-over time", "max proportion supportive"), 
             value = c(t_cross, max_support))
}