
library(ggplot2)
library(corrplot)
library(dplyr)
library(lmvar)
library(Metrics)
library(GGally)

data <- read.csv("StudentsPerformance.csv")

################################################################### 1
# 1.A
n <- nrow(data)
n1 <- length(data$Fjob)
n2 <- length(data$Mjob)


p1 <- table(data$Mjob)[["at_home"]] / n
p2 <- table(data$Fjob)[["at_home"]] / n

p_diff <- p2 - p1

se = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
me = se * qnorm(0.975)

CI <- p_diff + c(-me, me)

cat("confidence interval = ", "(", CI[1], ",", CI[2], ")")


# 1.B
chisq.test(table(data$Fjob, data$Mjob))


################################################################### 2
set.seed(42)
SAMPLE_SIZE <- 15
p_hat <- sum(data[sample(1:n, SAMPLE_SIZE), ]$romantic == "yes") / SAMPLE_SIZE
cat("proportion in small sample =", p_hat)


n_sim <- 10e4
sim_dist <- replicate(n_sim,
                      mean(sample(c("yes", "no"),
                                 size=SAMPLE_SIZE,
                                 rep=T)=="yes"))

p_value <- mean(sim_dist > p_hat)

cat("p-vlaue =", p_value, '\n')
if(p_value > 0.05) {
  cat("We don't have enough evidence to reject H0.")
}
if(p_value <= 0.05) {
  cat("H0 is rejected in favor of HA.")
}

################################################################### 3

# 3.A
# probability distribution
expected_table <- table(data$Fjob) / n
print(round(expected_table, 2))

SAMPLE_SIZE = 100

random_sample <- data[sample(1:n, SAMPLE_SIZE), "Fjob"]
random_sample_table <- table(random_sample)
print(random_sample_table)

prb <- ifelse(data$Fjob=="teacher",0.9, 0.1)
biased_sample <- data[sample(n, SAMPLE_SIZE, prob = prb), "Fjob"]

biased_sample_table <- table(biased_sample)
print(biased_sample_table)

exptected_probs     <- matrix(expected_table)[1: length(expected_table)]
random_sample_probs <- matrix(random_sample_table)[1: length(random_sample_table)]
biased_sample_probs <- matrix(biased_sample_table)[1: length(biased_sample_table)]

chisq.test(random_sample_probs, p=exptected_probs)

chisq.test(biased_sample_probs, p=exptected_probs)


# 3.B
chisq.test(table(data$Fjob, data$Mjob))

################################################################### 4

# 4.B
model_by_study <- lm(formula = G1 ~ studytime, data)
print(summary(model_by_study))

p <- ggplot(data, aes(x = studytime, y = G1)) +
  geom_point() +
  labs(x="G1",
       y="studytime") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic() +  stat_smooth(method = lm, se = FALSE)

show(p)



model_by_G2<- lm(formula = G1 ~ G2, data)
print(summary(model_by_G2))

p <- ggplot(data, aes(x = G2, y = G1)) +
  geom_point() +
  labs(x="G1",
       y="G2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic() +  stat_smooth(method = lm, se = FALSE)

show(p)


plot(model_by_study)

plot(model_by_G2)

# 4.D

print(summary(model_by_study))

cat('#################################################\n\n')

print(summary(model_by_G2))



print(anova(model_by_study))

cat('#################################################\n\n')

print(anova(model_by_G2))


# 4.F
sample_100 <- data[sample(1:n, 100), ]

train.data <- sample_100[1:90, ]
test.data <- sample_100[91:100, ]

# 4.f.a
model_by_study_90<- lm(formula = G1 ~ studytime, train.data)
model_by_G2_90<- lm(formula = G1 ~ G2, train.data)

print(summary(model_by_study_90))

cat('#################################################\n\n')

print(summary(model_by_G2_90))

# 4.f.b
pe.studytime = summary(model_by_study_90)$coefficients[2]
se.studytime = summary(model_by_study_90)$coefficients[4]
me.studytime = qnorm(.975) * se.studytime
CI.studytime = pe.studytime + c(-me.studytime, me.studytime)
cat("confidence interval of slope of studytime is =", CI.studytime, '\n')


pe.G2 = summary(model_by_G2_90)$coefficients[2]
se.G2 = summary(model_by_G2_90)$coefficients[4]
me.G2 = qnorm(.975) * se.G2
CI.G2 = pe.G2 + c(-me.G2, me.G2)
cat("confidence interval of slope of G2 is =", CI.G2, '\n')


# 4.f.c
predictions.studytime <- model_by_study_90 %>% predict(test.data)
predictions.G2 <- model_by_G2_90 %>% predict(test.data)


# 4.f.d
cat("RMSE of the prediction by studytime model = ", rmse(predictions.studytime, test.data$G1),'\n')
cat("RMSE of the prediction by G2 model = ", rmse(predictions.G2, test.data$G1), '\n')
################################################################### 5
# 5.A

p1 <- ggpairs(data.frame(
                  "G1"=data$G1,
                  "age"=data$age,
                  "goout"=data$goout,
                  "studytime"=data$studytime,
                  "failures"= as.factor(data$failures)
                  ))
p2 <- ggpairs(data.frame(
                  "G1"=data$G1,
                  "health"=data$health,
                  "absences"=data$absences,
                  "G2"=data$G2,
                  "G3"=data$G3))
show(p1)
show(p2)

cor(data$failures, data$G1)


# 5.B
mlr <- lm(formula = G1 ~ G2 + G3 + failures, data, x=TRUE, y=TRUE)
summary(mlr)

plot(mlr)

anova(mlr)


# 5.E
create_formula = function(response, vars) {
  as.formula(paste(response, paste(vars, collapse=" + "), sep=" ~ "))
}


stepwise_selection = function(method) {
  full.vars <- c("school","sex","age","Fjob","Mjob","goout","internet","romantic","studytime","failures","health","absences","G2","G3")
  selected <- rep(0, length(full.vars))
  max_total_parameter <- 0
  for(i in seq(1, length(full.vars))) {
    remaining.vars <- full.vars[selected == 0]
    
    if(length(remaining.vars) == 0) {
      break
    }
    max_step_parameter <- 0
    max_step_var <- NULL
    for(j in seq(1, length(remaining.vars))) {
      if(method == "forward") {
        temp.vars <- c(full.vars[selected == 1], remaining.vars[j])
      }
      else if(method == "backward") {
        temp.vars <- setdiff(full.vars[selected == 0], remaining.vars[j])
      }
      formula <- create_formula("G1", temp.vars)
      model.temp.summary <- summary(lm(formula, data=data))
      if(model.temp.summary$adj.r.squared > max_step_parameter) {
        max_step_var <- remaining.vars[j]
        max_step_parameter <- model.temp.summary$adj.r.squared
      }
    }
    selected[which(full.vars == max_step_var)] = 1
    if(max_total_parameter < max_step_parameter) {
      max_total_parameter = max_step_parameter
    }
    else {
      break
    }
  }
  if(method == "forward")
    return(lm(create_formula("G1", full.vars[selected == 1]), data=data, y=TRUE, x=TRUE))
  else if(method == "backward")
    return(lm(create_formula("G1", full.vars[selected == 0]), data=data, y=TRUE, x=TRUE))
}

best_forward <- stepwise_selection(method="forward")
best_backward <- stepwise_selection(method="backward")

summary(best_forward)

summary(best_backward)

# 5.F
# linearity
plot(best_backward, 1)


# nearly normal residuals
plot(best_backward, 2)

# constant variability
plot(best_backward, 3)

# 5.G
cv.lm(mlr, k=5)

cv.lm(best_backward, k=5)

############################################################### 6
# 6.A
set.seed(42)
data["romanticYes"] = as.numeric(data["romantic"] == "yes")

train.size <- floor(2/3 * nrow(data))
train.ind <- sample(seq_len(nrow(data)), size = train.size)
train.data <- data[train.ind, ]
test.data <- data[-train.ind, ]



clf_part_A <- glm(formula = romanticYes ~ school + sex + age + goout + internet + studytime + failures + absences + G1,
              data=train.data,
              family=binomial)
summary(clf_part_A)


# 6.B
# refrence level = male

probFemale <- seq(0, 0.99, 0.01)

OR_RATIO = abs(summary(clf_part_A)$coefficients[3])
cat("OR RATIO =", OR_RATIO)
getY <- function(x) {
  return ((OR_RATIO*x/(1-x)) / (1 + (OR_RATIO*x/(1-x))))
}
probMale <- sapply(probFemale, getY)



plot(probMale, probFemale, type = "l", lty = 1, 
     main="odds ratio plot of Sex",
     xlab = "P(Romantic | Male)",
     ylab = "P(Romantic | Female)")
lines(seq(0, 1, 0.01), seq(0, 1, 0.01), col="green")


# 6.C
library(plotROC)
library(ggplot2)

plot_roc = function(clf) {
  train.data$prediction <- predict(clf, newdata=train.data)

  roc_curve <- ggplot(train.data,
                      aes(m = prediction,
                          d = romanticYes)) +
    geom_roc(n.cuts=20,
             labels=F) + 
    theme_classic() +
    geom_abline(slope=1, intercept = 0)
  
  
  show(roc_curve + annotate("text", x = .75, y = .25 , label =
                            paste("Aear Under Curve =",
                                  round(calc_auc(roc_curve)["AUC"], 3))))
  
  test.data$prediction <- predict(clf, newdata=test.data)
  
  roc_curve <- ggplot(test.data,
                      aes(m = prediction,
                          d = romanticYes)) +
    geom_roc(n.cuts=20,
             labels=F) + 
    theme_classic() + 
    geom_abline(slope=1, intercept = 0)
  
  show(roc_curve + annotate("text", x = .75, y = .25 , label =
                            paste("Aear Under Curve =",
                                  round(calc_auc(roc_curve)["AUC"], 3))))
} 


plot_roc(clf_part_A)

clf_part_E <- glm(formula = romanticYes ~ age + absences + studytime + internet,
              data=train.data,
              family=binomial)
summary(clf_part_E)

plot_roc(clf_part_E)

thresholds = seq(0, 1, 0.1)
U <- c()

for(i in seq(1, length(thresholds))) {
  predict_probs <- predict(clf_part_E, newdata=data)
  t <- confusionMatrix(data = as.factor(as.numeric(predict_probs > thresholds[i])),
                  reference=as.factor(data$romanticYes))$table
  
  TP = t[4]
  TN = t[1]
  FP = t[2]
  FN = t[3]
  U = c(U, TP + TN -20*FP - 10*FN)
}

plot(thresholds, U, type='l')

max_th = thresholds[which(U == max(U))]

cat("Maximum Unitility happens when threshold =", max_th)

############################################################### 7

data$academic_probation = as.numeric((data$G1 + data$G2 + data$G3) < 25)

aca_prob_model <- glm(formula = academic_probation ~ school+sex+
                        age+Fjob+Mjob+goout+internet+romantic+
                        studytime+failures+health++absences,
              data=data,
              family=binomial)
summary(aca_prob_model)