#    
# Knowledge Gap
# ANES CDF

# Set Working dir.
setwd(githubdir)

# Load libraries
library(car)
library(haven)
library(goji) # devtools::install_github("soodoku/goji")
library(dplyr)
library(broom)
library(ggplot2)
library(directlabels)

# Load data
data <- read_dta("selexp/data/anes_timeseries_cdf_stata12.dta")

# Wait wait
data$VCF0009x

# Year of Study
# table(data$VCF0004, useNA = "always")
# str(data$VCF0004)
data$year <- as.factor(data$VCF0004) 

# Pre-Elex Interviewer Rated Knowledge
# VCF0050a: IWR OBSERVATION: Respondent Level of Political Info - Pre
# QUESTION: PRE INTERVIEWER OBSERVATION: Respondent's general level of information about politics and public affairs seemed:
table(data$VCF0050a, useNA = "always")
data$pk <- zero1(as.numeric(car::recode(data$VCF0050a, "9 = NA")), 5, 1)

# Education (8 = DK, 9 = NA)
data$ed7 <- zero1(as.numeric(car::recode(data$VCF0140a, "c(8, 9) = NA")))
data$ed2 <- zero1(as.numeric(car::recode(data$VCF0140a, "c(8, 9) = NA; c(1, 2, 3, 4) = 0; c(6, 7) = 1; else = NA")))

# Political Interest
data$polint <- zero1(as.numeric(car::recode(data$VCF0313, "c(0, 9) = NA")))
data$polint2 <- as.numeric(car::recode(data$VCF0313, "c(0, 9) = NA; c(1, 2) = 0; c(3, 4) = 1"))

# Mode of Interview, VCF0017, 0 = f2f

# Pre-Elex Interviewer vars.
# IWR gender: VCF0070a: (1 = Male)
# Interviewer Race: VCF0071a

# Sociodem
# Age VCF0101 (ignoring 97, 98, 99 issues)
data$age <- as.numeric(car::recode(data$VCF0101, "0 = NA"))

# Gender VCF0104 (1 = Male)

# Race/Ethnicity: VCF0105a (7 categories)

# Analysis
# ------------------

# Within-group/year-wise regressions

pk_int <- data %>%
filter(!is.na(polint) & !is.na(pk)) %>%
group_by(year) %>%
do(tidy(lm(pk ~ polint, weight = VCF0009x, .)))

with(pk_int[pk_int$term == 'polint', ], cor(as.numeric(year), estimate))

pk_int_10 <- data %>%
filter(!is.na(polint2) & !is.na(pk)) %>%
group_by(year) %>%
do(tidy(lm(pk ~ polint2, weight = VCF0009x, .)))

with(pk_int_10[pk_int_10$term == 'polint2', ], cor(as.numeric(year), estimate))

pk_ed7 <- data %>%
filter(!is.na(ed7) & !is.na(pk)) %>%
group_by(year) %>%
do(tidy(lm(pk ~ ed7, weight = VCF0009x, .)))

with(pk_ed7[pk_ed7$term == 'ed7', ], cor(as.numeric(year), estimate))

pk_ed2 <- data %>%
filter(!is.na(ed2) & !is.na(pk)) %>%
group_by(year) %>%
do(tidy(lm(pk ~ ed2, weight = VCF0009x, .)))

with(pk_ed2[pk_ed2$term == 'ed2', ], cor(as.numeric(year), estimate))

# Regressions
# --------------

# with respondent level covariates + mode of interview
with(data, summary(lm(pk ~ polint*zero1(as.numeric(year)) + age + I(age^2) + as.factor(VCF0104) + as.factor(VCF0105a) + as.factor(VCF0017))))
int_lm <- with(data, summary(lm(pk ~ polint*zero1(as.numeric(year)) + age + I(age^2) + as.factor(VCF0104) + as.factor(VCF0105a) + as.factor(VCF0017), weight = VCF0009x)))
round(coef(int_lm)[,1], 2)

with(data, summary(lm(pk ~ ed7*zero1(as.numeric(year)) + age + I(age^2) + as.factor(VCF0104) + as.factor(VCF0105a) + as.factor(VCF0017))))
ed_lm <- with(data, summary(lm(pk ~ ed7*zero1(as.numeric(year)) + age + I(age^2) + as.factor(VCF0104) + as.factor(VCF0105a) + as.factor(VCF0017), weight = VCF0009x)))
round(coef(ed_lm)[,1], 2)

# add mode of interview + interviewer level covariates
with(data, summary(lm(pk ~ polint*zero1(as.numeric(year)) + age + I(age^2) + as.factor(VCF0104) + as.factor(VCF0105a) + as.factor(VCF0017) + as.factor(VCF0070a) + as.factor(VCF0071a), weight = VCF0009x)))
with(data, summary(lm(pk ~ ed7*zero1(as.numeric(year)) + age + I(age^2) + as.factor(VCF0104) + as.factor(VCF0105a) + as.factor(VCF0017) + as.factor(VCF0070a) + as.factor(VCF0071a), weight = VCF0009x)))

# Plot
# --------------
dotplot_ed2 <- data.frame(pk_ed2[pk_ed2$term == "ed2", "estimate"],
	                      year = as.numeric(as.character(pk_ed2$year[seq(1, nrow(pk_ed2), 2)])))

ggplot(dotplot_ed2, aes(x = year, y = estimate)) +
geom_point() + 
geom_smooth(method = lm, se = FALSE, size = .8, alpha = .7) +
labs(x = "", y = "BS and Above \u2013 HS and Below") +
scale_x_continuous(breaks = dotplot_ed2$year, labels = dotplot_ed2$year) + 
scale_y_continuous(breaks = seq(0, .5, .1), labels = nolead0s(seq(0, .5, .1)), lim = c(0, .5)) +
theme_minimal(base_size = 9) +
theme(panel.grid.major = element_line(color = "#F0F0F0", size = .25)) +
theme(panel.grid.minor = element_blank()) +
theme(axis.ticks = element_blank()) +
theme(legend.position = "none") +
theme(plot.title = element_text(color = "#525252", size = 10, vjust = 1.25)) +
theme(axis.text.x = element_text(size = 12, color = "#636363", angle = 45, hjust = 1)) +
theme(axis.text.y = element_text(size = 12, color = "#636363")) +
theme(axis.title.x = element_text(size = 14, color = "#636363", vjust = 0)) +
theme(axis.title.y = element_text(size = 14, color = "#636363", vjust = 1.25)) +
theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
theme(strip.background = element_rect(fill = 'white', color='white'))

ggsave(file = "selexp/figs/know_gap_ed_lm_anes.pdf", dpi = 450)

dotplot_int2 <- data.frame(pk_int_10[pk_int_10$term == "polint2", "estimate"],
	                      year = as.numeric(as.character(pk_int_10$year[seq(1, nrow(pk_int_10), 2)])))

ggplot(dotplot_int2, aes(x = year, y = estimate)) +
geom_point() + 
geom_smooth(method = lm, se = FALSE, size = .8, alpha = .7) +
labs(x = "", y = "Follow Some of the Time or More \u2013 Follow Now or Then or Less") + 
scale_x_continuous(breaks = dotplot_int2$year, labels = dotplot_int2$year) +  
scale_y_continuous(breaks = seq(0, .5, .1), labels = nolead0s(seq(0, .5, .1)), lim = c(0, .5)) +
theme_minimal(base_size = 9) +
theme(panel.grid.major = element_line(color = "#F0F0F0", size = .25)) +
theme(panel.grid.minor = element_blank()) +
theme(axis.ticks = element_blank()) +
theme(legend.position = "none") +
theme(plot.title = element_text(color = "#525252", size = 10, vjust = 1.25)) +
theme(axis.text.x = element_text(size = 12, color = "#636363", angle = 45, hjust = 1)) +
theme(axis.text.y = element_text(size = 12, color = "#636363")) +
theme(axis.title.x = element_text(size = 14, color = "#636363", vjust = 0)) +
theme(axis.title.y = element_text(size = 14, color = "#636363", vjust = 1.25)) +
theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
theme(strip.background = element_rect(fill = 'white', color='white'))

ggsave(file = "selexp/figs/know_gap_int_lm_anes.pdf", dpi = 450)
