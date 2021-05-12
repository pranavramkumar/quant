# TAD
# Uni: pr2576
# Converting csv files to rdata files for compression

# load starting database
setwd("C:/Users/Pranav Ramkumar/Desktop/Quant Series/TAD/PS/Final")
Editorials <- read.csv("Editorials.csv")
save(Editorials, file = "Editorials.RData")
Speeches <- read.csv("Speeches.csv")
save(Speeches, file = "Speeches.RData")