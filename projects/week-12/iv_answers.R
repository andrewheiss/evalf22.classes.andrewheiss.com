library(tidyverse)
library(stevedata)
library(estimatr)
library(broom)
library(modelsummary)


data(Dee04)

blah <- Dee04 %>% 
  group_by(register, college) %>% 
  summarize(total = n())

blah %>% 
  group_by(college) %>% 
  mutate(prop = total / sum(total))

ggplot(Dee04, aes(x = register))

naive_model <- lm(register ~ college, data = Dee04)
tidy(naive_model)

# 1. Relevance
model_check_relevance <- lm(college ~ distance, data = Dee04)

Dee04 %>% summarize(relevance = cor(distance, college))

cor.test(Dee04$distance, Dee04$college)

ggplot(Dee04, aes(x = distance, y = college)) +
  geom_point(alpha = 0.01) +
  geom_smooth(method = "lm")


# 2. Exclusion
model_check_exclusion <- lm(distance ~ register, data = Dee04)
tidy(model_check_exclusion)

Dee04 %>% summarize(relevance = cor(distance, register))

cor.test(Dee04$distance, Dee04$register)

ggplot(Dee04, aes(x = register, y = college)) +
  geom_point(alpha = 0.2, position = position_jitter()) +
  geom_smooth(method = "lm")

# 3. Exogeneity
# Tell a story that distance to college isn’t correlated with anything else in the model

# IN THEORY, living closer to a college should explain or increase the likelihood of attending college, but shouldn't in turn influence outcomes as an adult, like the propensity to vote.

# But in real life, that's not the case! Black and Hispanic Americans are more likely to live in urban areas, and there are more colleges in urban areas, and race/ethnicity are correlated with voting patterns

# 2SLS manually
first_stage <- lm(college ~ distance, data = Dee04)

predicted_college <- augment(first_stage, Dee04) %>% 
  rename(college_hat = .fitted)

second_stage <- lm(register ~ college_hat, data = predicted_college)
tidy(second_stage)

# 2SLS manually with controls
first_stage <- lm(college ~ distance + black + hispanic + female, data = Dee04)

predicted_college <- augment(first_stage, Dee04) %>% 
  rename(college_hat = .fitted)

second_stage <- lm(register ~ college_hat + black + hispanic + female, data = predicted_college)
tidy(second_stage)

# 2SLS automatically
model_2sls <- iv_robust(register ~ college + black + hispanic + female | 
                          distance + black + hispanic + female, 
                        data = Dee04)
tidy(model_2sls)


modelsummary(list(second_stage, model_2sls))
