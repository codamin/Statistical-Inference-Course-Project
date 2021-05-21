library(dplyr)
library(ggplot2)
library(psych)
library(GGally)
library(reshape2)
library(scatterplot3d)
library(ggmosaic)
library(moments)
library(Hmisc)


data <- read.csv('StudentsPerformance.csv')

num_variables <- ncol(data)
num_cases <- nrow(data)

################################################################################
# Problem 1
################################################################################
# 1.a

var_1 <- 'absences'

p <- ggplot(data,
            aes_string(x=var_1)) +
  geom_histogram(aes(y=..density..),
                 binwidth=1,
                 fill='white',
                 color="black") +
  geom_density(alpha=.2,
               fill="#FF6666") +
  labs(title=paste("Histogram of ", var_1), x=var_1, y = "Count")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
show(p)
################################################################################
# 1.b
p <- ggplot(data,
            aes_string(sample=var_1)) +
  stat_qq() +
  stat_qq_line() +
  scale_shape_manual(values=c(4,13)) +
  scale_color_brewer(palette="Dark2") +
  theme_classic() +
  labs(title=paste("QQ-Plot for ", var_1)) +
  theme(plot.title = element_text(hjust = 0.5))
show(p)
################################################################################
# 1.c
library(moments)
cat("skewness of absences:", round(skewness(data$absences), 2), '\n')
################################################################################
# 1.d
box_plot <- boxplot(data$absences,
                    main = paste(var_1, " boxplot"))
cat('outliers are:', sort(box_plot$out), '\n')
################################################################################
# 1.e
col <- data$absences
cat("mean of absences:", mean(col), '\n')
cat("median of absences:", median(col), '\n')
cat("variance of absences:", var(col), '\n')
cat("standard deviation of absences:", sd(col), '\n')
################################################################################
# 1.f
p <- ggplot(data,
            aes_string(x=var_1)) + 
  geom_density() +
  geom_vline(aes(xintercept=mean(absences)),
              color="blue",
              linetype="dashed",
              size=1) +
  geom_vline(aes(xintercept=median(absences)),
             color="red",
             linetype="dashed",
             size=1) +
  theme_classic() +
  theme(
    legend.box.background = element_rect(color="red", size=2),
    legend.box.margin = margin(116, 6, 6, 6)
  ) +
  labs(title=paste("Mean and Median on Density of absences ", var_1),
       x="absences", y="density") +
  theme(plot.title = element_text(hjust = 0.5))


show(p)

################################################################################
# 1.g
pct <- round(c(sum(col <= 0.5*mean(col)),
                  sum(col > 0.5*mean(col) & col <= mean(col)),
                  sum(col > mean(col) & col <= 1.5*mean(col)),
                  sum(col > 1.5*mean(col) & col <= max(col)))
                  * 100 / num_cases,
                  2)

pie_df <- data.frame(group = c("First", "Second",
                               "Third", "Fourth"),
                     value = pct)
pie(pie_df$value,
    labels = paste(pie_df$group, sep = " ", pct, "%"),
    # col = c('red', 'blue', 'green', 'yellow'),
    col = c('red', "blue", "green", "yellow"),
    main = "absences Group Proportions")


# 1.h
Q1 = box_plot$stats[2]
Q3 = box_plot$stats[4]
lower_whisker_extreme = box_plot$stats[1]
upper_whisker_extreme = box_plot$stats[5]

cat("Q1=", Q1, '\n')
cat("Q3=", Q3, '\n')
cat("lower whisker=", lower_whisker_extreme, Q1, '\n')
cat("upper whisker=", Q3, upper_whisker_extreme, '\n')
cat("IQR=", Q3 - Q1, '\n')

################################################################################
# Problem 2
################################################################################

# 2.A. find the frequency of each category and its percentage.
school_groups = data %>% group_by(school) %>% summarize(count=n())
pct = round(school_groups$count / num_cases * 100, 2)
cat(paste(school_groups$school, "school frequency=", sep = " ", school_groups$count),  sep="\n")
cat(paste(school_groups$school, "school percentage=", sep = " ", pct, "%"),  sep="\n")
################################################################################
# 2.B. Plot a barplot for this variable and add percentage marks to it. Use different colors for each category.
p <- ggplot(data=school_groups,
            aes(x=school,
                y=count,
                fill=school)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste(pct, "%")),
            vjust=-0.3,
            size=3.5) +
  labs(title=paste("Bar plot of school"),
       x="school", y="frequency") +
  theme_classic()
show(p)
################################################################################
# 2.C. Sort the categories by their frequencies, then using a horizontal barplot to show the result.
school_groups = data %>% group_by(school) %>% summarize(count=n())
p <- ggplot(data=school_groups,
            aes(x=reorder(as.factor(school), count),
                y=count,
                fill=school)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste(pct, "%")),
            vjust=-0.3,
            size=3.5) +
  labs(title=paste("Horizontal Bar plot of school"),
       x="school", y="frequency") +
  theme_classic() +
  coord_flip()
show(p)
################################################################################
# 2.D. Plot a violin plot for this variable.
p <- ggplot(data) +
  geom_violin(aes(x=school,
                  y=absences,
                  fill=school)) +
  labs(title=paste("Violon plot of absences by school"),
       x="school", y="frequency") +
  theme_classic()
  
show(p)
################################################################################
# Question 3
################################################################################
# 3.B
p <- ggplot(data, aes(x=G1,
                 y=G2)) + 
  geom_point()+
  geom_smooth(method=lm,
              se=FALSE) +
  labs(title=paste("Scatter Plot of G1 and G2"), x="G1", y="G2")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
show(p)
################################################################################
# 3.C

cor.test(data$G1, data$G2)
################################################################################
# 3.F

p <- ggplot(data, aes(x=G1,
                      y=G2,
                      shape=sex,
                      col=sex)) + 
  geom_point() +
  geom_smooth(method=lm,
              se=F) +
  labs(title=paste("Scatter Plot of G1 and G2"), x="G1", y="G2")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

show(p)
################################################################################
# 3.G
hex_p <- ggplot(data, aes(G2, G3)) +
  geom_hex(bins=50) +
  stat_smooth(col='red',
              method = "loess") +
  labs(title=paste("Scatter Plot of G1 and G2"), x="G1", y="G2")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "left") +
  geom_point()

p <- ggMarginal(hex_p,
                type="histogram",
                fill = "slateblue")

show(p)


################################################################################
# Question 4
################################################################################
# 4.A

ords = data.frame("age"=data$age,
                  "goout"=data$goout,
                  "studytime"=data$studytime,
                  # "failures"=data$failures,
                  "health"=data$health,
                  "absences"=data$absences,
                  "G1"=data$G1,
                  "G2"=data$G2,
                  "G3"=data$G3)

ggpairs(ords, upper = list(continuous = "density"))
################################################################################
# 4.B

pvalmat <- rcorr(as.matrix(ords))$P
pvalmat[lower.tri(pvalmat)] <- NA
melted_pvalmat <- melt(round(pvalmat, 3), na.rm = TRUE)

cormat <- round(cor(ords),2)
cormat[lower.tri(cormat)] <- NA
melted_cormat <- melt(cormat, na.rm = TRUE) 
colnames(melted_cormat)[3] <- "Cor"
ggplot(data = melted_cormat[melted_cormat$Var1 != melted_cormat$Var2, ],
       aes(Var2,
           Var1,
           fill=Cor))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       limit = c(-1,1)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 25, size = 12))+
  coord_fixed() +
  geom_text(aes(label = paste(Cor, "\n", "p=", melted_pvalmat$value)),
            color = "black",
            size = 4) +
  labs(title=paste("Heatmap Correlogram"), x="", y="")
################################################################################
# 4.C

colors_3d <- c("red", "green")
colors_3d <- colors_3d[as.numeric(as.factor(data$sex))]
p_3d <- scatterplot3d(x=data$G1,
                      y=data$G2,
                      z=data$G3,
                      color = colors_3d,
                      pch = 16,
                      main="3D Scatterplot of G1,G2,G3 based on Gender",
                      xlab="G1",
                      ylab="G2",
                      zlab="G3")

legend(p_3d$xyz.convert(22, 3, 15),
       legend = levels(as.factor(data$sex)), title='sex',
       col =  c("red", "green"),
       pch = 16)




################################################################################
# Question 5
################################################################################
# 5.A

counts_5a <- as.data.frame.matrix(table(data$school, data$sex))
counts_5a %>% rowwise() %>% mutate(Total = sum(c(F, M)))
################################################################################
# 5.B

g <- ggplot(data, aes(Fjob,
                      fill=Mjob)) +
  geom_bar(position=position_dodge()) +
  geom_text(stat='count',
            aes(label=..count..),
            position = position_dodge(0.9),
            vjust=1.6,
            size=3.5) +
  labs(title=paste("Barplot of Father's Job to Mother's Job"))
show(g)

################################################################################
# 5.C

g <- ggplot(data, aes(romantic,
                      fill=sex)) +
  geom_bar() +
  geom_text(stat='count',
            aes(label=..count..),
            position = position_stack(0.5)) +
  labs(title=paste("Barplot of romantic in each sex"))

show(g)

################################################################################
# 5.D
counts_5d <- data %>%
  count(school, internet) %>%
  group_by(school) %>% 
  mutate(percent = round(n/sum(n), 3),
                           total = round(sum(n)))

p <- ggplot(data = counts_5d, aes(x = school,
                                  y = percent,
                                  fill = internet,
                                  width = sqrt(total/num_cases))) +
      geom_col(position = "fill") +
      geom_text(aes(label = paste(percent*100, '%')),
            position = position_stack(vjust = 0.5)) +
  labs(title="mosaic plot") +
  theme(plot.title = element_text(hjust = 0.5))

    
show(p)


################################################################################
# Problem 6
################################################################################

conf.interval = t.test(x = data$G3)$conf.int
conf.interval
################################################################################

# 6.c
p <- ggplot(data,
            aes(x=G3)) +
  geom_histogram(binwidth=1,
                 fill='white',
                 color="black") +
  geom_density(alpha=.2,
               fill="#FF6666") +
  geom_vline(aes(xintercept=mean(G3)),
             color="blue",
             linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=conf.interval[1]),
             color="red",
             linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=conf.interval[2]),
             color="red",
             linetype="dashed",
             size=1) +
  labs(title="Histogram of G3", x="G3", y = "Count") +
  
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
show(p)
################################################################################
mu_0 = 13
t.test(cars$speed, mu=mu_0)

################################################################################
# Problem 7
# A. By conducting a hypothesis test, explain whether there is a significant
# difference between the mean values of these two variables.

paired_sample <- data[sample(1:num_cases, 25), ] %>% select(G1, G2)

t.test(x=paired_sample$G1,y=paired_sample$G2, paired = T)

# B. Now, draw 100 independent samples from the dataset for each of these two
# variables. Then, conduct a hypothesis test to inspect whether there is a
# significant difference between the mean values of these two variables. Are
# the results of the test consistent with the 95% confidence interval?

first_100 <- data[sample(1:num_cases, 100), ] %>% select(G1)
second_100 <- data[sample(1:num_cases, 100), ] %>% select(G2)

t.test(first_100, second_100)

# Problem 8
# A. Calculate a 95% confidence interval for the median of this variable using the
# percentile method.
quantile(data$G3 , c(.025,.975))

# B. Pick a random sample of size 20. Then, using the bootstrapping method,
# calculate a 95% confidence interval for the mean of this variable using the
# standard error method.

sample_20 <- data[sample(1:num_cases, 20), 'G3']
                  
boot_dist <- replicate(1000, mean(sample(sample_20, 20, rep=T)))
boot_mean <- mean(boot_dist)
me <- se * qt(0.975, df=1000-1)
ci <- boot_mean + c(-me, me)
cat('boot strap confidence interval=', ci)


# C. Is there any noticeable difference between these two calculated confidence
# intervals? Explain your reasoning.

# Problem 9
data











