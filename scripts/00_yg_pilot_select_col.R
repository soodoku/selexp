#    
# Perceptions of Media Bias by PID
# YG Pilot

# Set Working dir.
setwd(githubdir)
setwd("selexp/")

# Load data
data <- foreign::read.spss("data/STAN0050_OUTPUT_relabel2.sav", to.data.frame = T, use.missing = T)

bias <- names(data)[grep("bias", names(data))]
data <- subset(data, select = c("pid3", "weight", bias))

write.csv(data, file = "data/yg_pilot.csv", row.names = F)
