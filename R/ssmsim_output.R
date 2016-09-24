require(ggplot2)
require(plyr)
require(dplyr)

plotFinalState <- function(results) {
  final <- 
    results %>%
    filter(tick==100) %>%
    mutate(out = mapvalues(
      out,
      from = c(0,1),
      to = c("None out", "All out")
    ))
  
  ggplot(final, aes(x=support, fill=out)) + 
    geom_bar() + 
    theme_minimal() +
    ggtitle("Final Simulation States (@t=100)") +
    xlab("Support (%)") + 
    ylab("Number of simulations") +
    theme(legend.title=element_blank()) 
}

plotRuns <- function(results, filter = 25) {
  ggplot(filter(results, tick<=filter), aes(x=tick, y=support, color=run)) + 
    geom_line() +
    guides(color=FALSE) + 
    theme_minimal() + 
    ggtitle("Simulation Trajectories") + 
    xlab("time")
}

plotRunsSeparate <- function(results) {
  ggplot(filter(results, tick<=25), aes(x=tick)) + 
    geom_line(aes(y = support, colour = "support")) + 
    geom_line(aes(y = out*100, colour = "out")) +
    facet_wrap(~run) + 
    ylim(0,100) + 
    theme_minimal() + 
    theme(legend.title=element_blank(), 
          strip.text.x=element_blank())  # the runs are ordered alphabetically, 
                                         # not numerically, so hide text
}

plotPoll <- function(poll_df, num.nodes = 10000) {
  
  ## Colors from endpoints of RColorBrewer::brewer.pal(5, "RdBu")
  ## With the middle shade manually made darker
  colors <- c("#CA0020", "#7b7b7b", "#0571B0")
  
  ggplot(poll_df, aes(x = tick, y = count/num.nodes, color = support_level)) + 
    geom_point() + 
    ylim(0, 1) +
    xlab("time") + 
    ylab("fraction") +
    ggtitle("Support Poll") + 
    theme_minimal() + 
    theme(legend.title=element_blank()) + 
    scale_color_manual(breaks = c("high_support", "mid_support", "low_support"), 
                    labels = c("Supportive", "Neutral", "Opposed"), 
                    values = rev(colors))
#     scale_color_hue(breaks = c("low_support", "mid_support", "high_support"), 
#                     labels = c("Opposed", "Neutral", "Supportive"))
}

plotSupport <- function(model_df, ticks = c(1, 100)) {
  ## mid_break = .33
  support_df <- 
    model_df %>% 
    select(tick, support) %>% 
    filter(tick %in% ticks) %>%
    unnest(support)
  
#   support_df <-
#     support_df %>%
#     mutate(support_level = 
#              ifelse(support > mid_break, "high_support", 
#                     ifelse(support < -mid_break, "low_support", "mid_support")), 
#            support_level = as.factor(support_level))
  
  ggplot(support_df, aes(x = support)) + 
    facet_wrap(~ tick) + 
    #geom_histogram(aes(fill = support_level), binwidth = .05) + 
    geom_freqpoly(size = 1, binwidth = .05) + 
    xlim(-1, 1) +
    theme_minimal() + 
    theme(legend.title=element_blank()) + 
    ggtitle("Change in Support Distribution over Time")  
#     scale_color_hue(breaks = c("low_support", "mid_support", "high_support"), 
#                     labels = c("Opposed", "Neutral", "Supportive"))
}

