knitr::opts_chunk$set(echo = TRUE)

# Get tidyverse & the NPP data
library(tidyverse)

anpp <- read_csv('https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210011003/105/127124b0f04a1c71f34148e3d40a5c72')

# Then, make a summary figure
ggplot(anpp, aes(x = year, y = npp_g_m2, col = site, group = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

##
## Comparing two samples
##

# Create a 2-year data subset
anpp.sub1 <- anpp %>%
  dplyr::filter(year %in% 2019:2020)

# Summary statistics
anpp.sub1.stats <- anpp.sub1 %>%
  group_by(year) %>%
  summarise(n = n(),
            mean = mean(npp_g_m2),
            std.dev = sd(npp_g_m2)) %>%
  mutate(std.err = std.dev/sqrt(n))

anpp.sub1.stats

# Spread to wide form
anpp.sub1.wide <- anpp.sub1 %>%
  spread(year, npp_g_m2)

head(anpp.sub1.wide)

# A default 2-sample t-test in R
t.test(x = anpp.sub1.wide$`2019`, 
       y = anpp.sub1.wide$`2020`)

# t-test with equal variance
t.test(x = anpp.sub1.wide$`2019`, 
       y = anpp.sub1.wide$`2020`, 
       var.equal = TRUE)

# Paired t-test
t.test(x = anpp.sub1.wide$`2019`, 
       y = anpp.sub1.wide$`2020`, 
       paired = TRUE)

##
## Testing for unequal variance
##

# Create a 4-year data subset
anpp.sub2 <- anpp %>%
  dplyr::filter(year %in% 2017:2020)

ggplot(anpp.sub2, aes(x = year, y = npp_g_m2, group = year)) +
  geom_boxplot()

library(car)

bartlett.test(npp_g_m2 ~ year, data = anpp.sub2)
fligner.test(npp_g_m2 ~ year, data = anpp.sub2)
leveneTest(npp_g_m2 ~ as.factor(year), data = anpp.sub2) # from car package

##
## Comparing samples with general linear models
##

# Two-sample mean comparison using a linear regression model
lm.ttest <- lm(npp_g_m2 ~ year, data = anpp.sub1)
summary(lm.ttest)

ggplot(data=anpp.sub1, aes(x=year, y=npp_g_m2)) +
  geom_point() +
  geom_smooth(method="lm")

?aov

anpp.sub1.f <- anpp.sub1
anpp.sub1.f$year <- factor(anpp.sub1.f$year)

# One-way ANOVA equivalent to the 2-sample t-test
aov.ttest <- aov(npp_g_m2 ~ year, data = anpp.sub1.f)
summary(aov.ttest)
car::Anova(lm.ttest, type = "III") # Not clear on difference in outputs, p value is same.

# Two-way ANOVA using year and vegetation zone
aov.2way <- aov(npp_g_m2 ~ year + zone, data = anpp.sub1.f)
summary(aov.2way)

# One-way, repeated measures ANOVA using year and site
aov.1way.rm <- aov(npp_g_m2 ~ year + Error(site/year),
                   data = anpp.sub1.f)
summary(aov.1way.rm)

##
## Into the wilderness with linear mixed models
##
