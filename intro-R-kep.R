## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## ------------------------------------------------------------------------
library(tidyverse)
library(Epi)
library(knitr)
library(gridExtra)


## ------------------------------------------------------------------------
marathon <- read_csv("http://alecri.github.io/downloads/data/marathon.csv")
marathon


## ------------------------------------------------------------------------
# what is the dimension of the data  
dim(marathon)
# i.e. how many obs/rows
nrow(marathon)
# how many cols/variables?
ncol(marathon)
# what's the name of the variable
colnames(marathon)
# what is the structure of the data
glimpse(marathon)
# show the first rows
head(marathon)
# quick and dirty summary
summary(marathon)


## ------------------------------------------------------------------------
# run time in hour
marathon <- mutate(marathon, runtime_h = runtime/60)
# alternative code
# marathon$runtime_h <- marathon$runtime/60


## ------------------------------------------------------------------------
# modify a character variable into a factor variable
str(marathon$nas135)
marathon <- mutate(marathon, nas135 = factor(nas135, levels = c("na > 135", "na <= 135")))
str(marathon$nas135)


## ------------------------------------------------------------------------
# categorize a continuous variable
summary(marathon$age)
marathon <- mutate(marathon, age_cat = cut(age, breaks = c(19, 31, 38, 45, 75)))
table(marathon$age_cat)


## ------------------------------------------------------------------------
# some variables + variables containing wt (shortage for weight)
marathon_sub <- select(marathon, id, na, female, bmi, runtime_h, contains("wt"))
head(marathon_sub)


## ------------------------------------------------------------------------
female_27 <- filter(marathon_sub, female == "female", bmi > 27)
female_27


## ------------------------------------------------------------------------
arrange(female_27, desc(na), bmi)


## ------------------------------------------------------------------------
arrange(
   filter(
      select(
         mutate(marathon,
                runtime_h = runtime/60),
         id, na, female, bmi, runtime_h, contains("wt")),
      female == "female", bmi > 27), desc(na), bmi)


## ------------------------------------------------------------------------
marathon %>% 
   mutate(runtime_h = runtime/60) %>%
   select(id, na, female, bmi, runtime_h, contains("wt")) %>%
   filter(female == "female", bmi > 27) %>%
   arrange(desc(na), bmi)


## ------------------------------------------------------------------------
ggplot(marathon, aes(wtdiff, na, col = bmi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ female) +
  labs(x = "Weight change, kg", y = "Sodium concentration, mmol per liter") +
  theme_bw()
# change theme for next graphs
theme_set(theme_bw())


## ------------------------------------------------------------------------
# for a continuous variable
summary(marathon$na)
c(mean = mean(marathon$na), sd = sd(marathon$na))
quantile(marathon$na)
summarise(marathon, mean = mean(na), sd = sd(na), median = median(na), iqr = IQR(na))
# more concise
summarise_each(marathon, funs(mean, sd, median, IQR), na)
# histogram (counts)
ggplot(marathon, aes(na)) +
  geom_histogram() +
  labs(x = "Sodium concentration, mmol per liter")
# histogram (density)
ggplot(marathon, aes(na)) +
  geom_histogram(stat = "density", n = 2^5) +
  geom_line(stat = "density") +
    labs(x = "Sodium concentration, mmol per liter")
# boxplot
ggplot(marathon, aes(x = "", y = na)) +
  geom_boxplot() + 
  labs(x = "", y = "Sodium concentration, mmol per liter")


## ------------------------------------------------------------------------
# for a categorical variable
table(marathon$nas135)
prop.table(table(marathon$nas135))
# barplot (counts)
ggplot(marathon, aes(nas135)) +
  geom_bar()
# barplot (proportion)
ggplot(marathon, aes(nas135)) +
  geom_bar(aes(y = ..count../sum(..count..))) +
  labs(x = "", y = "Prevalence of hyponatremia")


## ------------------------------------------------------------------------
# Continuous + categorical
marathon %>% 
  group_by(female) %>% 
  summarise_each(funs(mean, sd, median, IQR), na)
ggplot(marathon, aes(female, na)) +
  geom_boxplot() +
  labs(x = "", y = "Sodium concentration, mmol per liter")
ggplot(marathon, aes(female, na, fill = female)) +
  geom_violin() +
  guides(fill = "none") +
    labs(x = "", y = "Sodium concentration, mmol per liter")
ggplot(marathon, aes(na, col = female)) +
  geom_line(stat = "density") +
  labs(x = "Sodium concentration, mmol per liter", col = "Sex")
# statistical tests
t.test(na ~ female, data = marathon, var.equal = FALSE)
wilcox.test(na ~ female, data = marathon)


## ------------------------------------------------------------------------
# 2-by-2 table
tab1 <- with(marathon, table(nas135, female))
tab1
prop.table(tab1, margin = 2)
ggplot(marathon, aes(nas135, fill = female)) +
  geom_bar(position = "dodge")
ggplot(marathon, aes(nas135, group = female, fill = female)) +
  geom_bar(aes(y = ..prop..), position = "dodge")
# statistical tests
chisq.test(tab1)


## ------------------------------------------------------------------------
# 2 continuous
cor_nawtd <- with(marathon, cor(na, wtdiff,  method = "pearson", use = "complete.obs"))
cor_nawtd
p1 <- ggplot(marathon, aes(wtdiff, na)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Weight change, kg", y = "Sodium concentration, mmol per liter")
library(ggExtra)
ggMarginal(p1, type = "histogram")


## ------------------------------------------------------------------------
library(table1)
table1(~ age + female + wtdiff + bmi + runtime | nas135, data = marathon)


## ------------------------------------------------------------------------
# univariate linear regression
fit1 <- lm(na ~ female, data = marathon)
summary(fit1)
ci.lin(fit1) %>% kable()


## ------------------------------------------------------------------------
# multivariable linear regression
fit2 <- lm(na ~ wtdiff + female, data = marathon)
ci.lin(fit2) %>% kable()
marathon <- mutate(marathon,
                   pred_fit2 = predict(fit2, newdata = marathon))


## ------------------------------------------------------------------------
# multivariable linear regression with interaction
fit3 <- lm(na ~ wtdiff*female, data = marathon)
ci.lin(fit3, ctr.mat = rbind(c(0, 1, 0, 0),
                             c(0, 1, 0, 1)))
marathon <- mutate(marathon,
                   pred_fit3 = predict(fit3, newdata = marathon))


## ------------------------------------------------------------------------
# graphical comparison
grid.arrange(
  ggplot(marathon, aes(wtdiff, pred_fit2, col = female)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted mean na"),
    ggplot(marathon, aes(wtdiff, pred_fit3, col = female)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted mean na"),
  ncol = 2
)


## ------------------------------------------------------------------------
# univariate logistic regression
fit4 <- glm(nas135 ~ wtdiff, data = marathon, family = binomial)
ci.exp(fit4)
marathon <- marathon %>% 
  mutate(pred_p = predict(fit4, newdata = marathon, type = "response"),
         pred_odds = predict(fit4, newdata = marathon, type = "link")) 
grid.arrange(
  ggplot(marathon, aes(wtdiff, pred_p)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted probability"),
  ggplot(marathon, aes(wtdiff, pred_odds)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted odds"),
  ncol = 2
)

## ------------------------------------------------------------------------
# multivariable logistic regression
fit5 <- glm(nas135 ~ wtdiff + female, data = marathon, family = binomial)
ci.exp(fit5)
mutate(marathon, pred = predict(fit5, newdata = marathon, type = "response")) %>% 
  ggplot(aes(wtdiff, pred, col = female)) +
  geom_line() +
  labs(x = "Weight change, kg", y = "Predicted probability")

