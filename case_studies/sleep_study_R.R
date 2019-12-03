# Case Study using the sleepstudy data from the lme4 package
# Stats 506, Fall 2019
# 
# Updated: December 2, 2019
# Author: James Henderson
# 80: --------------------------------------------------------------------------

# libraries: -------------------------------------------------------------------
library(lme4); library(lmerTest)

# a quick plot to understand the data 
ggplot( sleepstudy, aes( x = Days, y = Reaction, color = Subject) ) +
  geom_line() +
  theme_bw()

# subject level slopes and intercepts are correlated
fit1 = lmer( Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
summary(fit1)

# subject level sopes and intercepts are uncorrelated
fit2 = lmer( Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), 
             data = sleepstudy)
summary(fit2)

#  check if days 2 is important
fit3 = lmer( Reaction ~ 
               Days + I(Days^2) + 
               (1 + Days | Subject), 
             data = sleepstudy %>% mutate(log_days = log(Days + 1)))
summary(fit3)
AIC(fit1, fit3)

# 80: --------------------------------------------------------------------------
