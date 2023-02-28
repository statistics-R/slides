library(ggplot2)

set.seed(123)

# Simulate data
n <- 100
educ <- rnorm(n, mean = 12, sd = 3)
age <- rnorm(n, mean = 35, sd = 10)
wage <- 1000 + 200*educ + 50*age + rnorm(n, mean = 0, sd = 100)

# Create data frame
data <- data.frame(educ = educ, age = age, wage = wage)

# Plot data
ggplot(data, aes(x = educ, y = wage)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Years of education", y = "Wage")



# Catehorical

library(ggplot2)

# generate simulated data
set.seed(123)
gender <- factor(rep(c("Male", "Female"), each = 50))
educ <- rnorm(100, mean = 12, sd = 2)
wage <- 20 + 5*as.numeric(gender) + 2*educ + rnorm(100, mean = 0, sd = 5)
data <- data.frame(gender, educ, wage)

# plot the data with linear regression lines by gender
ggplot(data, aes(x = educ, y = wage, color = gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Years of Education") +
  ylab("Wage") +
  ggtitle("Wage by Education Level and Gender") +
  scale_color_manual(values = c("blue", "red"))



# Ejemplos
# simulate data
set.seed(123)
gasto <- exp(rnorm(100))
voters <- 1 + 2 * log(x) + rnorm(100)

# plot the data
library(ggplot2)
ggplot(data.frame(x = gasto, y = voters), aes(x, y)) + 
  geom_point() + 
  xlab("Gasto") + 
  ylab("Votantes")

# fit a linear model
fit_lm <- lm(y ~ x)
summary(fit_lm)

# plot the linear fit
ggplot(data.frame(x = x, y = y), aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Gasto") + 
  ylab("Votantes")

# fit a linear-log model
fit_lmlg <- lm(y ~ log(x))
summary(fit_lmlg)

# plot the linear-log fit
ggplot(data.frame(x = x, y = y), aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ log(x)) +
  xlab("Gasto") + 
  ylab("Votantes")


# Log -x



# Simulate data
# Load libraries
library(ggplot2)
library(dplyr)

# Simulate data
set.seed(123)
exp <- runif(100, 0, 20)
wage <- exp^2 + rnorm(100, 0, 1)
data <- data.frame(exp, wage)

# Fit linear regression
fit_lin <- lm(wage ~ exp, data = data)
summary(fit_lin)

# Fit logarithmic regression
fit_log <- lm(log(wage) ~ exp, data = data)
summary(fit_log)

# Plot data and regression lines
library(ggplot2)
ggplot(data, aes(x = exp, y = wage)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  geom_smooth(method = "lm", formula = log(y) ~ x, se = FALSE, color = "blue") +
  labs(title = "Wage vs Experience",
       x = "Experience",
     y = "Wage")


# log log
set.seed(123)
n <- 100
x <- exp(2  + rnorm(n))
y <- exp(2 * log(x) + rnorm(n))
data <- data.frame(x = x, y = y)

# Linear regression
fit_lm <- lm(y ~ x, data = data)
summary(fit_lm)

# Logarithmic regression
fit_log <- lm(log(y) ~ log(x), data = data)
summary(fit_log)

# Plotting the results
library(ggplot2)
ggplot(data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  geom_smooth(method = "lm", formula = log(y) ~ log(x), se = FALSE, color = "blue") +
  scale_x_log10() +
  scale_y_log10()

