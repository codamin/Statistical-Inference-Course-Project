install.packages("hexbin")
library(dplyr)
library(ggplot2)

data <- read.csv('StudentsPerformance.csv')

num_variables <- ncol(data)
num_cases <- nrow(data)

# data %>% group_by(factor1, factor2) %>% summarize(count=n())
################################################################################
# 1.a
numeric_var <- 'age'
p <- ggplot(data,
            aes(x=G3)) +
  geom_histogram(aes(y=..density..),
                 binwidth=1,
                 fill='white',
                 color="black") +
  geom_density(alpha=.2,
               fill="#FF6666") +
  labs(title="Histogram of G3", x="G3", y = "Count")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
show(p)
################################################################################
# 1.b
p <- ggplot(data,
            aes(sample=G3)) +
  stat_qq() +
  stat_qq_line() +
  scale_shape_manual(values=c(4,13)) +
  scale_color_brewer(palette="Dark2") +
  theme_classic() +
  labs(title=paste("QQ-Plot for G3")) +
  theme(plot.title = element_text(hjust = 0.5))
show(p)
################################################################################
# 1.c
col = data$G3
skewness = (mean(col)-median(col)) / sd(col)
cat("skewness of G3:", skewness)
################################################################################
# 1.d
boxplot(data$G3)
################################################################################
# 1.e
cat("mean of G3:", mean(col), '\n')
cat("median of G3:", median(col), '\n')
cat("variance of G3:", var(col), '\n')
cat("standard deviation of G3:", sd(col), '\n')
################################################################################
# 1.f
p <- ggplot(data,
            aes(x=G3)) + 
  geom_density() +
  geom_vline(aes(xintercept=mean(G3)),
              color="blue",
              linetype="dashed",
              size=1) +
  geom_vline(aes(xintercept=median(G3)),
             color="red",
             linetype="dashed",
             size=1) +
  theme_classic()
show(p)
################################################################################
pct <- round(c(sum(col <= 0.5*mean(col)),
                  sum(col > 0.5*mean(col) & col <= mean(col)),
                  sum(col > mean(col) & col <= 1.5*mean(col)),
                  sum(col > 1.5*mean(col) & col <= max(col)))
                  * 100 / num_cases,
                  2)

pie_df <- data.frame(group = c("A", "B", "C", "D"),
                     value = pct)
pie(pie_df$value,
    labels = paste(pie_df$group, sep = " ", pct, "%"),
    col = rainbow(pct),
    main = "G3 Group Proportions")


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################



# 2.A. find the frequency of each category and its percentage.
school_groups = data %>% group_by(school) %>% summarize(count=n())
pct = round(school_groups$count / num_cases * 100, 2)
cat(paste(school_groups$school, "school frequency=", sep = " ", school_groups$count),  sep="\n")
cat(paste(school_groups$school, "school percentage=", sep = " ", pct, "%"),  sep="\n")
# GP school percentage= 88.35 %
# MS school percentage= 11.65 %
################################################################################
# 2.B. Plot a barplot for this variable and add percentage marks to it. Use different colors for each category.
p <- ggplot(data=school_groups, aes(x=school, y=count)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=count), vjust=-0.3, size=3.5) +
  theme_classic()
show(p)
################################################################################
# 2.C. Sort the categories by their frequencies, then using a horizontal barplot to show the result.
p <- ggplot(data=school_groups[order(school_groups$count)],
            aes(x=school,
                y=count,
                fill=school)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=count),
            vjust=-0.3,
            size=3.5) +
  theme_classic() +
  coord_flip()
show(p)
################################################################################
# D. Plot a violin plot for this variable.
# https://www.r-graph-gallery.com/95-violin-plot-with-ggplot2.html
p <- ggplot(data) +
  geom_violin(aes(x=school,
                  y=G3)) +
  ggtitle("Violin plot of body type")
p
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

ggplot(data, aes(x=G1,
                 y=G2)) + 
  geom_point()+
  geom_smooth(method=lm,
              se=FALSE)
################################################################################
cor.test(data$G1, data$G2)
################################################################################
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
p <- ggplot(data, aes(x=G1,
                      y=G2,
                      shape=sex,
                      col=sex)) + 
  geom_point() +
  geom_smooth(method=lm)


show(p)
################################################################################
p <- ggplot(data, aes(G1, G2), bins = 0.1) +
  geom_hex()

show(p)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


















