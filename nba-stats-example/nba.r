source("../gap.r")
source("../other.r")

set.seed(1)

## The data comes from basketball-reference.com with some small modification.

np = read.csv("nba2015player.csv")
nt = read.csv("nba2015team.csv")

np$Player = NULL
nt$Team = NULL

np[is.na(np)] = 0
nt[is.na(nt)] = 0

nbap = gap.stat(np)
nbat = gap.stat(nt)

plot(nbap$Tabs[,1], ylim = c(min(c(nbap$Tabs[,1], nbap$Tabs[,2])), max(c(nbap$Tabs[,1], c(nbap$Tabs[,2])))), main = "NBA Player Stats", xlab = "number of clusters k", ylab = "obs and exp log(Wk)")
lines(nbap$Tabs[,1])
points(nbap$Tabs[,2], pch = "E")
lines(nbap$Tabs[,2])

readline()

plot(nbap$Tabs[,4], ylim = c(min(nbap$Tabs[,4]), max(nbap$Tabs[,4])), main = "NBA Player Stats", xlab = "number of clusters k", ylab = "Gap")
lines(nbap$Tabs[,4])
segments(1:10, nbap$Tabs[,4] - nbap$Tabs[,3], 1:10, nbap$Tabs[,4] + nbap$Tabs[,3])
segments(1:10 - 0.1, nbap$Tabs[,4] - nbap$Tabs[,3], 1:10 + 0.1, nbap$Tabs[,4] - nbap$Tabs[,3])
segments(1:10 - 0.1, nbap$Tabs[,4] + nbap$Tabs[,3], 1:10 + 0.1, nbap$Tabs[,4] + nbap$Tabs[,3])

readline()

plot(nbat$Tabs[,1], ylim = c(min(c(nbat$Tabs[,1], nbat$Tabs[,2])), max(c(nbat$Tabs[,1], c(nbat$Tabs[,2])))), main = "NBA Team Stats", xlab = "number of clusters k", ylab = "obs and exp log(Wk)")
lines(nbat$Tabs[,1])
points(nbat$Tabs[,2], pch = "E")
lines(nbat$Tabs[,2])

readline()

plot(nbat$Tabs[,4], ylim = c(min(nbat$Tabs[,4]), max(nbat$Tabs[,4])), main = "NBA Team Stats", xlab = "number of clusters k", ylab = "Gap")
lines(nbat$Tabs[,4])
segments(1:10, nbat$Tabs[,4] - nbat$Tabs[,3], 1:10, nbat$Tabs[,4] + nbat$Tabs[,3])
segments(1:10 - 0.1, nbat$Tabs[,4] - nbat$Tabs[,3], 1:10 + 0.1, nbat$Tabs[,4] - nbat$Tabs[,3])
segments(1:10 - 0.1, nbat$Tabs[,4] + nbat$Tabs[,3], 1:10 + 0.1, nbat$Tabs[,4] + nbat$Tabs[,3])


