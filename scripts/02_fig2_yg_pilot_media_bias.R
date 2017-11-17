#    
# Perceptions of Media Bias by PID
# YG Pilot

# Set Working dir.
setwd(githubdir)
setwd("selexp/")

# Load libraries
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(goji)

# Load data
data <- read.csv("data/yg_pilot.csv")

# Bias 
bias <- names(data)[grep("bias", names(data))]
data[, bias] <- lapply(data[, bias], function(x) zero1(as.numeric(car::recode(x, "'1 - Liberal or pro-Democratic party bias' = 1; '4 - No bias at all' = 4; '7 - Conservative or pro-Republican party bias' = 7")), 1, 7))

# Get estimates
# Throws warning because of weight se
res <- 
data[, c(bias, "pid3", "weight")] %>%
mutate(pid3 = as.character(pid3)) %>%
filter(pid3 %in% c("Democrat ", "Republican ", "Independent ")) %>%
group_by(pid3) %>%
summarize_all(funs(avg = weighted.mean(., weight, na.rm = TRUE), se = se_prop(weighted.mean(., weight, na.rm = TRUE), n()))) %>%
select(-c(weight_avg, weight_se))

# Get data in long form	
resl <- res %>% 
  gather(key, value, -pid3) %>% 
  extract(key, into = c('show', 'param'), '(.*)(_[^_]+$)') %>% 
  spread(param, value)

# Fixing some aspects for pres. and plotting
names(resl) <- c("pid3", "show", "avg", "se")
resl$show   <- car::recode(toupper(gsub("bias_", "", resl$show)), "'HUF' = 'Huffington Post'; 'DRG' = 'Drudge Report'; 'USA' = 'USA Today'; 'FOX' = 'Fox News'")


# Plot
# ~~~~~~~~~
ggplot(data = resl, aes(y = show, x = avg, xmin = avg + 1.96*se, xmax = avg - 1.96*se, colour = pid3)) +
  geom_point() + 
  geom_vline(xintercept = .5, linetype = "dashed", colour = "#AAAAAA") + 
  geom_errorbarh(height = 0) + 
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .25, .5, .75, .95), labels = c("Liberal", "", "Unbiased", "", "Conservative")) + 
  scale_colour_manual("", values = c("#42C4C7", "#CCAA22", "#A84E1C")) +
  labs(x = "", y = "", size = 10) + 
  theme_minimal(base_size = 9) +
  theme(panel.grid.major = element_line(color = "#F0F0F0", size = .25)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(color = "#525252", size = 10, vjust = 1.25)) +
  theme(axis.text.x = element_text(size = 9, color = "#636363")) +
  theme(axis.text.y = element_text(size = 9, color = "#636363")) +
  theme(axis.title.x = element_text(size = 10, color = "#323232", vjust = 0)) +
  theme(axis.title.y = element_text(size = 10, color = "#323232", vjust = 1.25)) +
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
  theme(strip.background = element_rect(fill = 'white', color='white'))
			
ggsave(file = "figs/outlet_bias.pdf", dpi = 450)
