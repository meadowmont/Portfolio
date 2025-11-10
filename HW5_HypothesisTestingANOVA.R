# Meadow Monticello
# Prof Hurms
# 10/30/2025

## HW 5 ~ Hypothesis Testing and ANOVA (chewy)

### problem 1 ###
library(ISwR)
data("vitcap")
head(vitcap)

# perform some descriptive stat on vital.capacity variable
mean(vitcap$vital.capacity, na.rm = TRUE)
sd(vitcap$vital.capacity, na.rm = TRUE)

# histogram of vital.capacity
hist(vitcap$vital.capacity,
     main = "Histogram of Vital Capacity",
     xlab = "Vital Capacity (L)",
     ylab = "Frequency",
     col = "darkblue")

# test for Normality
shapiro.test(vitcap$vital.capacity)

# Conduct a hypothesis test to determine if the mean vital capacity for workers in the cadmium industry is significantly different than 4.7 liters.
t.test(vitcap$vital.capacity, mu = 4.7)

# visualize results
boxplot(vitcap$vital.capacity,
        main = "Vital Capacity of Workers vs. Hypothesized Mean",
        ylab = "Vital Capacity (liters)",
        col = "lightblue")


### problem 2 ###
data_path <- "C:/Users/Meadow/OneDrive - Saint Joseph's University/dss445/Datasets/marketing_performance.csv"
dataset <- read.csv(data_path)
head(dataset, 4)
summary(dataset)  # gives descriptive stats

#2b
library(ggplot2)

ggplot(dataset, aes(x= Strategy, y= SalesLift, fill= Region)) + 
  geom_boxplot() +
  labs(
    title = "Sales Lift by Strategy and Region",
    x = "Strategy",
    y = "Sales Lift",
    fill = "Region")


# 2c
fit_int <- aov(SalesLift ~ Strategy * Region, data=dataset)
summary(fit_int)

# 2d
shapiro.test(residuals(fit_int))
#bartlett.test(fit_int)  #AI QUEST this was my initial code
# error messgeage

# ai generated
# Bartlett's test of equal variances of SalesLift across Strategy
bartlett.test(SalesLift ~ factor(Strategy), data = dataset)

# (Optional) Bartlett's test of equal variances of SalesLift across Region
bartlett.test(SalesLift ~ factor(Region), data = dataset)

### problem 3 ###
data(iris)
iris

?iris

data(iris)
mean(iris$Sepal.Length[iris$Species == "virginica"], na.rm = TRUE)

### problem 4 ###
apply(iris[, 1:4], 2, mean)
