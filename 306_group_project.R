options(digits = 16)

library(leaps)
library(ggplot2)
library(ggcorrplot)
library(plotly)

insurance <- read.csv("insurance.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
insurance$index <- NULL

age <- insurance$age
charges <- insurance$charges
region <- insurance$region
bmi <- insurance$bmi

# plot age against charges
ggplot(insurance, aes(x = age)) + geom_point(aes(y = charges)) + labs(x = "Age", y = "Charges")

# plot the distribution of charges
ggplot(insurance, aes(x = charges)) + geom_histogram() + labs(x = "Charges", y = "Count")

# plot the distribution of charges across different regions
ggplot(insurance, aes(x = region, y = charges)) + geom_boxplot() + labs(x = "Region", y = "Charges")

# plot a heat map of the correlation matrix
corr <- cor(insurance[,c("age", "bmi", "children", "charges")])
ggcorrplot(corr, type = "full") + labs(color = "Correlation", y = "Charges")

# create an interactive scatter plot of age, BMI, and charges
plot_ly(insurance, x = age, y = bmi, z = charges, colors = charges, type = "scatter3d", mode = "markers", marker = list(size = 2)) %>%
  layout(scene = list(xaxis = list(title = "Age"),
                      yaxis = list(title = "BMI"),
                      zaxis = list(title = "Charges")))

# plot a pie chart for sex
sex_counts <- table(insurance$sex)
sex_percent <- round(prop.table(sex_counts) * 100, 1)
pie(sex_counts,
    main = "Distribution of Sex",
    col = c("lightblue", "lightpink"),
    labels = paste0(names(sex_counts), ": ", sex_percent, "%"),
    cex = 1.2)

# plot a pie chart for smoker
smoker_counts <- table(insurance$smoker)
smoker_percent <- round(prop.table(smoker_counts) * 100, 1)
pie(smoker_counts,
    main = "Distribution of Smoker Status in Insurance Dataset",
    col = c("lightblue", "lightpink"),
    labels = paste0(names(smoker_counts), ": ", smoker_percent, "%"),
    cex = 1.2)

model <- lm(charges ~ (.)^2, data = insurance)

set.seed(1342)
training_index <- sample.int(length(insurance$age) * 0.8)
training_set <- insurance[training_index,]
test_set <- insurance[-training_index,]

best_subset <- regsubsets(charges ~ (.)^2, data = training_set, nbest = 1, nvmax = 8, method = "forward")

reg <- summary(best_subset)
summary(best_subset)$which

plot(reg$bic, xlab = "Parameter", ylab = "BIC")
