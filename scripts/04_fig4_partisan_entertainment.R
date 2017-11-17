# Partisan Entertainment

# Set directory
setwd(githubdir)
setwd("selexp/")

# Load libraries
library(dplyr)
library(goji)
library(ggplot2)

# Load data
naes <- foreign::read.dta("data/naes08_online_all_waves_data_full.dta")
naesweights <- foreign::read.dta("data/naes08_online_weights.dta")
naes <- merge(naes, naesweights, by = "rkey")

# Recode
# ---------
# PID
naes$pid3 <- car::recode(as.numeric(naes$ma01_1), "1:3 = 'Republican'; 5:7 = 'Democrat'; else = NA", as.factor = T)

# TV Shows
# EEB03rE Ellen DeGeneres Show; EB02rG BET News; EB05rA Big Love
to_vars   <- c("simpsons", "jayleno", "dave", "lawandorder", "twentyfour", "csi", "familyguy", "oprah", "scrubs", "bros",
	           "biglove", "ellen", "oreilly", "stewart")
from_vars <- c("eb02ra_2",  "eb02rn_2", "eb03rb_2", "eb03rf_2", "eb03rp_2", "eb04rc_2", "eb05rl_2", "eb04re_2", "eb05rd_2", "eb03rh_2",
               "eb05ra_2", "eb03re_2", "eb03rc_2", "eb02rq_2")
naes[, to_vars] <- lapply(naes[, from_vars], function(x) car::recode(x, "'yes' = 1; 'no' = 0; 'skipped' = NA", as.factor.result = FALSE))

# Total Democrats, Republicans watching each show 
pid_sums <- 
naes[, c("pid3", "wt_12345", to_vars)] %>%
  group_by(pid3) %>%
  summarise_all(funs(sum(.*wt_12345, na.rm = TRUE)))

# Limiting ourselves to partisans, proportion of the audience that is Democrat
prop_d  <- pid_sums[1, 3:16]/(pid_sums[1, 3:16] + pid_sums[2, 3:16])
prop_df <- data.frame(show = names(prop_d), score = t(prop_d))

# Reorder
prop_df$show <- car::recode(prop_df$show, '"simpsons" = "Simpsons"; "jayleno" = "Jay Leno"; "dave" = "David Letterman";
                                           "lawandorder" = "Law & Order"; "twentyfour" = "24"; "csi" = "CSI Miami";
                                           "familyguy" = "Family Guy"; "oprah" = "Oprah"; "scrubs" = "Scrubs";
                                           "bros" = "Brothers & Sisters"; "biglove" = "Big Love"; "ellen" = "Ellen DeGeneres";
                                           "oreilly" = "O\'Reilly Factor"; "stewart" = "John Stewart"')
prop_df$show <- factor(prop_df$show, levels = prop_df$show[order(prop_df$score)], ordered = TRUE) #reordering
prop_df$news <- as.factor(c(rep(0, 12), rep(1, 2)))

# Plot
ggplot(prop_df, aes(score, show)) +
  geom_point(aes(color = news), pch = 16, size = 3) + 
  scale_colour_manual(name = "", values = c("#333333", "#bbbbbb"), labels = c("Entertainment", "News or Political Satire")) + 
  theme_minimal(base_size = 9) +
  ylab("") + 
  xlab(expression(t[D] / (t[D] + t[R]))) + 
  scale_x_continuous(limits = c(.2, .9), breaks = c(.25, .5, .75), labels = c(".25", ".5", ".75")) + 
  theme(panel.grid.major = element_line(color = "#F0F0F0", size = .25)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(color = "#525252", size = 10, vjust = 1.25)) +
  theme(axis.text.x = element_text(size = 9, color = "#636363")) +
  theme(axis.text.y = element_text(size = 9, color = "#636363")) +
  theme(axis.title.x = element_text(size = 10, color = "#323232", vjust = 0)) +
  theme(axis.title.y = element_text(size = 10, color = "#323232", vjust = 1.25)) +
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))

ggsave("figs/entertainment.pdf", dpi = 450)
