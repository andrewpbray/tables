#### Both fixed
simA <- function(n) {
  gender <- rep(c("female", "male"), c(n/2, n/2))
  decision <- sample(rep(c("promote", "no promote"), c(0.25 * n, 0.75 * n)))
  matrix(c(gender, decision), ncol = 2)
}
#### One fixed
simB <- function(n, pPromote) {
  gender <- rep(c("female", "male"), c(n/2, n/2))
  decision <- sample(c("promote", "no promote"), size = n, replace = TRUE, prob = c(pPromote, 1 - pPromote))
  matrix(c(gender, decision), ncol = 2)
}
#### Neither fixed
simC <- function(n, pPromote) {
  gender <- sample(c("female", "male"), size = n, replace = TRUE, prob = c(.5, .5))
  decision <- sample(c("promote", "no promote"), size = n, replace = TRUE, prob = c(pPromote, 1 - pPromote))
  matrix(c(gender, decision), ncol = 2)
}


#### Model 1: maintain margins, test stat = female promoted cell
mod1 <- function(sim, it) {
  stat_obs <- table(sim[, 1], sim[, 2])[1, 2]
  stats_sim <- rep(NA, it)
  for(i in 1:it) {
    decision_shuffled <- sample(sim[, 2])
    stats_sim[i] <- sum(decision_shuffled[1:(n/2)] == "promote")
  }
  mean(stats_sim < stat_obs)
}

#### Model 2
## updated test statistic to difference in proportions

mod2 <- function(sim, it) {
  stat_obs <- diff(tapply(sim[,2]=="promote", sim[,1], mean))
  pHatPromote <- sum(table(sim[, 1], sim[, 2])[, 2])/n
  stats_sim <- rep(NA, it)
  for(i in 1:it) {
  decision <- sample(c("promote", "no_promote"), size = n, replace = TRUE,
                   prob = c(pHatPromote, 1 - pHatPromote))
  stats_sim[i] <- diff(tapply(decision == "promote", sim[,1], mean))
  }
  mean(stats_sim < stat_obs)
}

#### Model 3
## updated test statistic to difference in proportions

mod3 <- function(sim, it) {
  stat_obs <- diff(tapply(sim[,2]=="promote", sim[,1], mean))
  stats_sim <- rep(NA, it)
  for(i in 1:it) {
  decision <- sample(c("promote", "no_promote"), size = n, replace = TRUE,
                   prob = c(pPromote, 1 - pPromote))
  stats_sim[i] <- diff(tapply(decision == "promote", sim[,1], mean))
  }
  mean(stats_sim < stat_obs)
}

n <- 48
pPromote <- .25
it <-  500
nPvals <- 200
set.seed(49)

PvalMat <- matrix(rep(NA, nPvals * 9), ncol = 9)

for(j in 1:nPvals) {
  sim <- simA(n)
  PvalMat[j, 1] <- mod1(sim, it)
}

for(j in 1:nPvals) {
  sim <- simB(n, pPromote)
  PvalMat[j, 2] <- mod1(sim, it)
}

for(j in 1:nPvals) {
  sim <- simC(n, pPromote)
  PvalMat[j, 3] <- mod1(sim, it)
}


for(j in 1:nPvals) {
  sim <- simA(n)
  PvalMat[j, 4] <- mod2(sim, it)
}

for(j in 1:nPvals) {
  sim <- simB(n, pPromote)
  PvalMat[j, 5] <- mod2(sim, it)
}

for(j in 1:nPvals) {
  sim <- simC(n, pPromote)
  PvalMat[j, 6] <- mod2(sim, it)
}

for(j in 1:nPvals) {
  sim <- simA(n)
  PvalMat[j, 7] <- mod3(sim, it)
}

for(j in 1:nPvals) {
  sim <- simB(n, pPromote)
  PvalMat[j, 8] <- mod3(sim, it)
}

for(j in 1:nPvals) {
  sim <- simC(n, pPromote)
  PvalMat[j, 9] <- mod3(sim, it)
}

par(mfrow = c(3, 3))
hist(PvalMat[, 1], main = "simA : mod1", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 2], main = "simB : mod1", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 3], main = "simC : mod1", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 4], main = "simA : mod2", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 5], main = "simB : mod2", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 6], main = "simC : mod2", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 7], main = "simA : mod3", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 8], main = "simB : mod3", col = "grey", xlab = "", ylab = "")
hist(PvalMat[, 9], main = "simC : mod3", col = "grey", xlab = "", ylab = "")
