# 
# Preference for congenial sources for political/apolitical news
# 

# set dir
setwd(githubdir)
setwd("selexp/")

# Load libs
library(ggplot2)
library(goji)

# Load data
datal <- read.csv("data/yg_study1.csv")

# Recode
datal$pid3 <- relevel(as.factor(datal$pid3), ref = "Democrat")
datal$lr1 <- car::recode(datal$lr1, "'L' = 'Left'; 'M' = 'Neutral'; 'R' = 'Right'") 

a <- data.frame(prop.table(with(subset(datal, hs1 == 'Hard'), xtabs(weight ~lr1 + pid3)), 2), task = "Hard News Selection")
b <- data.frame(prop.table(with(subset(datal, hs1 == 'Soft'), xtabs(weight ~lr1 + pid3)), 2), task = "Soft News Selection")
colnames(a)[1] <- colnames(b)[1] <- "Slant" 

# Plot
fig1 <- rbind(a, b)

ggplot(fig1, aes(x = pid3, y = Freq, group = Slant)) + 
  geom_point(aes(colour = Slant), size = 3, alpha = .8) + 
  theme_minimal(base_size = 9) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = nolead0s(seq(0, 1, .2)), lim = c(0, 1)) +
  scale_colour_manual(name = "Outlet Slant", values = c("#42C4C7", "#CCAA22", "#A84E1C")) + 
  ylab("Proportion of Time Chosen") + 
  xlab("") + 
  theme(legend.position = "bottom") + 
  theme(panel.grid.major = element_line(color = "#F0F0F0", size = .25)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(plot.title = element_text(color = "#525252", size = 10, vjust = 1.25)) +
  theme(axis.text.x = element_text(size = 9, color = "#636363")) +
  theme(axis.text.y = element_text(size = 9, color = "#636363")) +
  theme(axis.title.x = element_text(size = 10, color = "#323232", vjust = 0)) +
  theme(axis.title.y = element_text(size = 10, color = "#323232", vjust = 1.25)) +
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
  facet_wrap(~ task, nrow = 2)

ggsave("figs/congenial_by_political_apolitical.pdf", dpi = 450)
