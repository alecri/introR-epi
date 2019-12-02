#' **Notes for an introduction to R with tidyverse**


# This is a comment: load the tidyverse package
library(tidyverse)

#' Read the data from the web
marathon <- read_csv("http://alecri.github.io/downloads/data/marathon.csv")

#' Basic data exploration
# how many obs and variables?
dim(marathon)
nrow(marathon) 
ncol(marathon)
colnames(marathon)
# quick summary of the variables
summary(marathon)
glimpse(marathon)
head(marathon)


#' Basic data manipulation

# create new variables
summary(marathon$runtime)
marathon = mutate(marathon, runtime_h = runtime/60)

# categorize a continuous variable
boxplot(marathon$age)
hist(marathon$age)
age_k = quantile(marathon$age, c(0, .25, .5, .75, 1), na.rm = T)
age_k
marathon = mutate(marathon, age_cat = cut(age, age_k, include.lowest = T))
table(marathon$age_cat, useNA = "ifany")
ggplot(marathon, aes(age_cat)) +
  geom_bar()

# changing levels of a factor variable
str(marathon$female)
marathon <- mutate(marathon, sex = factor(female, labels = c("woman", "men")))
table(marathon$female)
table(marathon$sex)

# select only variables
marathon_sub = select(marathon, id, na, age_cat, sex, bmi, runtime_h)
# and observations
women_25 <- filter(marathon_sub, sex == "woman", bmi > 25)
# sorting by (descending) na levels and timeh
arrange(women_25, desc(na), runtime_h)

#' The %>% operator
marathon %>% 
  mutate(
    runtime_h = runtime/60,
    sex = factor(female, labels = c("woman", "men"))
  ) %>% 
  select(id, na, age_cat, sex, bmi, runtime_h) %>% 
  filter(sex == "woman", bmi > 25) %>% 
  arrange(desc(na), runtime_h)


#' Some graphs with ggplot
p = ggplot(marathon, aes(wtdiff, na, col = runtime)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Weight change, kg", y = "Sodium concentration", col = "Time, min") +
  theme_classic() +
  facet_grid(~ sex)
p

library(plotly)
ggplotly(p)


#' Univariate summary statistics
summary(marathon$na)
sd(marathon$na)
ggplot(marathon, aes(na)) +
  geom_histogram(stat = "density", n = 2^5) +
  geom_line(stat = "density")
theme_set(theme_classic())
ggplot(marathon, aes(x = "", y = na)) +
  geom_boxplot() +
  labs(x = "", y = "Serum sodium concentration")

table(marathon$nas135)
prop.table(table(marathon$nas135))
ggplot(marathon, aes(nas135)) +
  geom_bar()


#' Bivariate summary statistics
ggplot(marathon, aes(female, na)) +
  geom_boxplot()
ggplot(marathon, aes(na, col = female)) +
  geom_line(stat = "density")

marathon %>% 
  group_by(female) %>% 
  summarise(mean = mean(na), sd = sd(na), median = median(na), range = max(na) - min(na))
marathon %>% 
  group_by(female) %>% 
  summarise_each(funs(min, max, median), na)

t.test(na ~ sex, data = marathon, var.equal = T)
fit1 <- lm(na ~ sex, data = marathon)
summary(fit1)
confint(fit1)

with(marathon, cor(na, wtdiff,  method = "pearson", use = "complete.obs"))
p = ggplot(marathon, aes(wtdiff, na)) +
  geom_point() +
  geom_smooth(method = "lm")

library(ggExtra)
ggMarginal(p, type = "histogram")

#' Binary outcome
table(marathon$nas135)

xtabs(~ nas135 + female, data = marathon) %>% 
  summary()
library(Epi)
stat.table(list(nas135, female), 
           list(count(), percent(female)), 
           marathon, margin = T)

marathon$nas135 <- factor(marathon$na <= 135, labels = c("no", "yes"))
fit2 = glm(nas135 ~ wtdiff + female, data = marathon, family = "binomial")
summary(fit2)
ci.exp(fit2)

mutate(marathon, p_pred = predict(fit2, newdata = marathon, type = "response")) %>% 
  ggplot(aes(wtdiff, p_pred, col = female)) +
  geom_line() +
  labs(x = "Weight change, kg", y = "Predicted probability", col = "Sex")
