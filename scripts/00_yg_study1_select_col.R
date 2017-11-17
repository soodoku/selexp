# 
# Preference for congenial sources for political/apolitical news
# 

# set dir
setwd(githubdir)
setwd("selexp/")

# Load libs
library(dplyr)
library(ggplot2)
library(scales)

# Load data
load("data/yg_codedlong.RData")
datal <- subset(datal, select = c("pid3", "lr1", "hs1", "weight"))

write.csv(datal, file = "data/yg_study1.csv", row.names = F)
