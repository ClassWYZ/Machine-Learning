
#Read & Split Data
data <- read.csv("cfb2014scores.csv", header = FALSE)
name <- read.csv("legend.txt", header = FALSE)

#Process Once
m <- matrix(0, nrow = 759, ncol = 759)
for (i in 1 : nrow(data)) {
  team1Index <- data[i, 1]
  team2Index <- data[i, 3]
  if (data[i, 2] > data[i, 4]) {
    m[team1Index, team1Index] = m[team1Index, team1Index] + 1 + (data[i, 2] / (data[i, 2] + data[i, 4]))
    m[team2Index, team2Index] = m[team2Index, team2Index] + (data[i, 4] / (data[i, 2] + data[i, 4]))
    m[team1Index, team2Index] = m[team1Index, team2Index] + (data[i, 4] / (data[i, 2] + data[i, 4]))
    m[team2Index, team1Index] = m[team2Index, team1Index] + 1 + (data[i, 2] / (data[i, 2] + data[i, 4]))
  }
  else {
    m[team1Index, team1Index] = m[team1Index, team1Index] + (data[i, 2] / (data[i, 2] + data[i, 4]))
    m[team2Index, team2Index] = m[team2Index, team2Index] + 1 + (data[i, 4] / (data[i, 2] + data[i, 4]))
    m[team1Index, team2Index] = m[team1Index, team2Index] + 1 + (data[i, 4] / (data[i, 2] + data[i, 4]))
    m[team2Index, team1Index] = m[team2Index, team1Index] + (data[i, 2] / (data[i, 2] + data[i, 4]))
  }
}
#Normalization
for (i in 1 : nrow(m)) {
  m[i,] <- m[i,] / (sum(m[i,]))
}
#Prediction
w <- matrix(1, nrow = 759, ncol = 1)
w <- w / nrow(w)
t <- 1000 # t = {10, 100, 200, 1000}
for (i in 1 : t) {
  w <- t(m) %*% w
}
top20 <- which(w %in% sort(w, decreasing = TRUE)[1:20])
top20name <- NULL
for (i in 1 : 20) {
  top20name <- cbind(top20name, as.character(name[top20[i],1][1]))
}
top20value <- NULL
for (i in 1 : 20) {
  top20value <- cbind(top20value, round(w[top20[i]], 5))
}
answer <- rbind(top20name, top20value)
#final w
eig <- eigen(t(m))
#eigen(t(m))$values[1]
u <-Re(eig$vectors[,2])
finalW <- u / sum(u)

#question 3
d <- NULL
w <- matrix(1, nrow = 759, ncol = 1)
w <- w / nrow(w)
for (i in 1 : 1000) {
  w <- t(m) %*% w
  distance <- sum(abs(w - finalW))
  d <- rbind(d, distance)
}
#plot
plot(d)
