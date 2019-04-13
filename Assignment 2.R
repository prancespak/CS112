data <- c()
ind1 <- rnorm(999, mean=50, sd=10)
for (i in 0:999) {
  dep <- ind1[i]*2 + rnorm(1, mean=0, sd=10)
  data <- c(data, (dep))
}
df <- data.frame(ind1, data)
lm1 <- lm(data ~ ind1, data = df)
head(df)

data2 <- c(data, -1000)
ind2 <- c(ind1, 500)
df2 <- data.frame(ind2, data2)
lm2 <- lm(data2 ~ ind2, data = df2)
head(df)

summary(lm1)
summary(lm2)

plot(ind2, data2,
     main = "Regression of 999 and 1000 Points",
     xlab = "Independent Variable",
     ylab = "Dependent Variable")
abline(lm(data ~ ind1))
abline(lm(data2 ~ ind2))

#### PART 2
library(Matching)
library(arm)
library(formattable)

data("lalonde")
#only controls
data = lalonde[which(lalonde$treat == 0), ]
#regression model
lm1 <- lm(re78 ~ age + educ + re74 + re75 + I(educ*re74) + I(educ*re75) + I(age*re74) + I(age*re75) + I(age*age) + I(re74*re75), data = data)

#expected value of re78
get_expect <- function(coefs, person) {
  result <- coefs[1] + person[1]*coefs[2] +
    person[2]*coefs[3] +
    person[3]*coefs[4] + person[4]*coefs[5] + (person[2]*person[3])*coefs[6] +
    (person[2]*person[4])*coefs[7] + (person[1]*person[3])*coefs[8] +
    (person[1]*person[4])*coefs[9] + (person[1]*person[1])*coefs[10] +
    (person[3]*person[4])*coefs[11]
  return(result)
}

#predicted values
get_predict <- function(coefs, sig, person) {
  result <- coefs[1] + person[1]*coefs[2] +
    person[2]*coefs[3] +
    person[3]*coefs[4] + person[4]*coefs[5] + (person[2]*person[3])*coefs[6] +
    (person[2]*person[4])*coefs[7] + (person[1]*person[3])*coefs[8] +
    (person[1]*person[4])*coefs[9] + (person[1]*person[1])*coefs[10] +
    (person[3]*person[4])*coefs[11] + rnorm(1, 0, sig)
  return(result)
}

sim.lm <- sim(lm1, 10000)

# expected values
storage.matrix1 <- matrix(NA, nrow = 10000, ncol = 39)

for (age in c(17:55)) {
  for (i in 1:10000)
  {
    person_median <- c(age, median(data$educ), median(data$re74), median(data$re75))
    storage.matrix1[i, age - 16] <- get_expect(sim.lm@coef[i, ], person_median)
  }
}

storage.matrix2 <- matrix(NA, nrow = 10000, ncol = 39)

for (age in c(17:55)) {
  for (i in 1:10000)
  {
    person_75 <- c(age, quantile(data$educ, probs = 0.75), quantile(data$re74, probs = 0.75), quantile(data$re75, probs = 0.75))
    storage.matrix2[i, age - 16] <- get_expect(sim.lm@coef[i, ], person_75)
  }
}

#predicted values 

storage.matrix3 <- matrix(NA, nrow = 10000, ncol = 39)

for (age in c(17:55)) {
  for (i in 1:10000)
  {
    person_median <- c(age, median(data$educ), median(data$re74), median(data$re75))
    storage.matrix3[i, age - 16] <- get_predict(sim.lm@coef[i, ], sim.lm@sigma[i], person_median)
  }
}

storage.matrix4 <- matrix(NA, nrow = 10000, ncol = 39)
for (age in c(17:55)) {
  for (i in 1:10000)
    {
      person_75 <- c(age, quantile(data$educ, probs = 0.75), quantile(data$re74, probs = 0.75), quantile(data$re75, probs = 0.75))
      storage.matrix4[i, age - 16] <- get_predict(sim.lm@coef[i, ], sim.lm@sigma[i], person_75)
  }
}

#intervals

conf.intervals1 <- apply(storage.matrix1, 2, quantile, probs = c(0.025, 0.975))
conf.intervals2 <- apply(storage.matrix2, 2, quantile, probs = c(0.025, 0.975))
pred.intervals1 <- apply(storage.matrix3, 2, quantile, probs = c(0.025, 0.975))
pred.intervals2 <- apply(storage.matrix4, 2, quantile, probs = c(0.025, 0.975))

#plot
plot(x = c(1:14000), y = c(1:14000), type = "n", xlim = c(17,55),
     main = "Confidence Intervals for Re78", xlab = "Age", 
     ylab = "Re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf.intervals1[1, age - 16],
    x1 = age,
    y1 = conf.intervals1[2, age - 16],
    lty = 3,
    lwd = 2)
}
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf.intervals2[1, age - 16],
    x1 = age,
    y1 = conf.intervals2[2, age - 16],
    col = "red",
    lty = 1,
    lwd = 1)
}

plot(x = c(1:20000), y = c(1:20000), type = "n", xlim = c(17,55),
     main = "Prediction Intervals for Re78", xlab = "Age", 
     ylab = "Re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred.intervals1[1, age - 16],
    x1 = age,
    y1 = pred.intervals1[2, age - 16],
    lty = 3,
    lwd = 2)
}
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred.intervals2[1, age - 16],
    x1 = age,
    y1 = pred.intervals2[2, age - 16],
    col = "red",
    lty = 1,
    lwd = 1)
}

# for table
median.ages <- matrix(NA, nrow = 39, ncol = 3)
for (age in c(17:55)){
  person_median <- c(median(data$educ), median(data$re74), median(data$re75))
  median.ages[age-16,] <- person_median
}

### PART 3 ###
library(datasets)

plant.data <- PlantGrowth[-which(PlantGrowth$group == "trt2"),]
plant.data$group <- as.numeric(plant.data$group)
plant.data$group <- plant.data$group - 1

lm2 <- lm(weight ~ group, data = plant.data)

results <- rep(0, 10000)
for (i in 1:10000){
  sample1 <- sample(c(1:length(plant.data$group)), length(plant.data$group), replace = T)
  results[i] <- summary(lm(weight~group, data = plant.data[sample1,]))$coef[2]
}

quantile(results, probs = c(0.025, 0.975))


confint(lm2)
hist(results,
     main="Bootstrapped Coefficients", 
     xlab="Coefficients")

### Part 4 ###
sample2 <- sample(c(1:length(plant.data$group)), length(plant.data$group), replace = T)
lm3 <- lm(weight ~ group, data = plant.data[sample2,])
predicted <- predict(lm3)
actual <- plant.data[sample2,]$weight
r.squared <- 1 - sum((predicted - actual) ^ 2) / sum((actual - mean(actual)) ^ 2)
r.squared

### Part 5 ###
library(foreign)
nsw <- read_dta("nsw.dta")

lm4 <- glm(treat ~ age + education + black + hispanic + married + nodegree + re75, data = nsw, family = binomial)

treat_prob <- function(coefs, person) {
  logit <- coefs[1] + person[2]*coefs[2] +
    person[3]*coefs[3] +
    person[4]*coefs[4] + 
    person[5]*coefs[5] +
    person[6]*coefs[6] +
    person[7]*coefs[7] +
    person[8]*coefs[8]
  
  return(exp(logit) / (1 + exp(logit)))
}

treat.probs <- treat_prob(lm4$coefficients, nsw[which(nsw$treat == 1),])
control.probs <- treat_prob(lm4$coefficients, nsw[which(nsw$treat == 0),])
hist(treat.probs$treat, 
     main="Probabilities for Treatment (Treatment Group)", 
     xlab="Probability",
     col="red",
     xlim=c(0,0.45),
     ylim=c(0, 45))
hist(control.probs$treat,
     main="Probabilities for Treatment (Control Group)", 
     xlab="Probability",
     col="blue",
     xlim=c(0,0.45),
     ylim=c(0, 150))

mean(treat.probs$treat)
mean(control.probs$treat)
