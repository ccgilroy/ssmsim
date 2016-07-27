# SSM Model

The SSM ("Same-Sex Marriage") Model is an agent-based model (ABM) that explores the effects of the distribution and visibility of a trait that characterizes some individuals in a network on other individuals' reactions to that trait. 

The prototypical case examined in this model is interaction between LGBT-identified people "coming out" to members of their social network and the subsequent change in how supportive those social ties are, both toward their LGBT tie specifically and toward LGBT rights in general. It is hypothesized that this micro-level process partially explains the recent rise in support for same-sex marriage on the national level in the United States, concomitant with the U.S. Supreme Court's 2015 ruling, Obergefell v. Hodges, guaranteeing the right to marry to same-sex couples.

## Conceptual Overview

Agents are nodes in a network connected by static edges, representing strong ties of kinship and friendship. The network is a small world (Watts and Strogatz 1998), with a high clustering coefficient and low average path length. The relevant trait ("LGBT") is randomly distributed among a small percentage of the agents. Agents' views of the trait ("support") are also randomly distributed, using numeric values from a specified distribution.   

A model run is a discrete series of timepoints ("ticks"). At each tick, the following may happen:

1. LGBT agents come out,
2. Agents with ties to newly out agents adjust their level of support in response, 
3. Other agents may also adjust their level of support. 

## Technical Notes

The model is written in R (R Project 2015) and NetLogo (Wilensky 1999), interfaced using the `RNetLogo` package (Thiele 2014). 

R version: 3.2.3  
NetLogo version: 5.2.1  
RNetLogo version: 1.0-1  

Note for Mac users: NetLogo 5.2.1 requires both Java 8 and Java 6 to be installed. This requires additional configuration as described [here](http://stackoverflow.com/questions/26618105/rnetlogo-not-working-on-mac-yosemite/) on Stack Overflow. Release notes for NetLogo indicate that this issue has been resolved in NetLogo 5.3.

## R Source Code

The general workflow is as follows: 

create networks -> run model -> clean up results -> plot and save

The relevant functions are organized into the following R source files: 

`SSM_Model_Networks.R`: Functions that build and save `igraph` networks in the graphML format, to be imported into NetLogo. This is where the size of the networks and the distribution of support are specified. 

`SSM_Model_NetLogo.R`: Functions that interact with NetLogo using `RNetLogo`, issuing NetLogo commands and returning the values that NetLogo reports as a data frame. This is where which NetLogo commands to use for agent actions are specified. 

`SSM_Model_Data_Cleaning.R`: Functions that transform the data frame from model runs to forms appropriate for saving (no loss of information) and plotting (aggregation of individual values) using `dplyr` and `tidyr`.

`SSM_Model_Output.R`: Functions that plot the results of running the model using `ggplot2`. 

In addition, code to start up NetLogo through R and source the above R files exists in both R script (`SSM_Model_Main.R`) and RMarkdown (`SSM_Model_Run_Template.Rmd`) form. 

## NetLogo Model

The model in NetLogo relies on importing a network generated in R using `nw:load-graphml`. This is much more efficient than attempting to generate the network in NetLogo. 

The model contains multiple commands (functions) describing possible actions agents might take. These must be bound to variable names as tasks (first-class functions) in R before the model is run. 

