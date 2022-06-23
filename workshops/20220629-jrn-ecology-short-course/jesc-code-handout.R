knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)

anpp.annual <- read_csv('https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210011003/105/127124b0f04a1c71f34148e3d40a5c72')

##
## Comparing two samples
##

# Create a 2-year data subset
anpp.2019.2020 <- anpp.annual %>%
  dplyr::filter(year %in% 2019:2020)

# Summary statistics
anpp.2019.2020.stats <- anpp.2019.2020 %>%
  group_by(year) %>%
  summarise(n = n(),
            mean = mean(npp_g_m2),
            std.dev = sd(npp_g_m2)) %>%
  mutate(std.err = std.dev/sqrt(n))

anpp.2019.2020.stats

# Spread to wide form
anpp.2019.2020.wide <- anpp.2019.2020 %>%
  spread(year, npp_g_m2)

head(anpp.2019.2020.wide)

# A default 2-sample t-test in R
t.test(x = anpp.2019.2020.wide$`2019`, 
       y = anpp.2019.2020.wide$`2020`)

# t-test with equal variance
t.test(x = anpp.2019.2020.wide$`2019`, 
       y = anpp.2019.2020.wide$`2020`, 
       var.equal = TRUE)

# Paired t-test
t.test(x = anpp.2019.2020.wide$`2019`, 
       y = anpp.2019.2020.wide$`2020`, 
       paired = TRUE)

##
## Testing for unequal variance
##

# Create a 4-year data subset
anpp.2017.2020 <- anpp.annual %>%
  dplyr::filter(year %in% 2017:2020)

ggplot(anpp.2017.2020, aes(x = year, y = npp_g_m2, group = year)) +
  geom_boxplot()

library(car)

bartlett.test(npp_g_m2 ~ year, data = anpp.2017.2020)
fligner.test(npp_g_m2 ~ year, data = anpp.2017.2020)
leveneTest(npp_g_m2 ~ as.factor(year), data = anpp.2017.2020) # from car package

##
## Comparing samples with general linear models
##

# Two-sample mean comparison using a linear regression model
lm.ttest <- lm(npp_g_m2 ~ year, data = anpp.2019.2020)
summary(lm.ttest)

ggplot(data=anpp.2019.2020, aes(x=year, y=npp_g_m2)) +
  geom_point() +
  geom_smooth(method="lm")

?aov

anpp.2019.2020.f <- anpp.2019.2020
anpp.2019.2020.f$year <- factor(anpp.2019.2020.f$year)

# One-way ANOVA equivalent to the 2-sample t-test
aov.ttest <- aov(npp_g_m2 ~ year, data = anpp.2019.2020.f)
summary(aov.ttest)
car::Anova(lm.ttest, type = "III") # Not clear on difference in outputs, p value is same.

# Two-way ANOVA using year and vegetation zone
aov.2way <- aov(npp_g_m2 ~ year + zone, data = anpp.2019.2020.f)
summary(aov.2way)

# Two-way, repeated measures ANOVA using year and vegetation zone
aov.2way.rm <- aov(npp_g_m2 ~ year + zone + Error(zone/year),
                   data = anpp.2019.2020.f)
summary(aov.2way.rm)
