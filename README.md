# College_Analysis
An analysis to investigate whether college major category has impact on earnings
### Load library
library(collegeIncome)

### Load data
data(college)

### Remove uncomplete cases
clean_college <- college[complete.cases(college),]

### Convert variables into their appropriate class
clean_college$major_code <- as.factor(clean_college$major_code)
clean_college$major_category <- as.factor(clean_college$major_category)

### Store clean_college in variable cl for convencience
cl <- clean_college

### Create matrix of means, grouped by major_category
cl_mean_matrix <- aggregate(cl[,7:ncol(cl)], list(cl$major_category), mean)

### View mean matrix
View(cl_mean_matrix)

### Create a column-subset of cl, containing only columns with interesting numeric values
clnum <- cl[, 7:ncol(cl)]

### Create correlation matrix
cl_cor_matrix <- cor(clnum, method = "pearson")

### View correlation matrix
View(cl_cor_matrix)

### Create a new clnum that only holds variables that are not highly correlated
clnum2 <- clnum[, c(1, 3, 6, 7, 9, 11, 13)]

### Make regression
fit <- lm(cl$median~cl$major_category+.-1, data = clnum2)

### Take a look at coefficients and their associated confidence interval
summary(fit)
