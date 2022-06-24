knitr::opts_chunk$set(echo = TRUE)

# Get tidyverse & the NPP data
library(tidyverse)

anpp <- read_csv('https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210011003/105/127124b0f04a1c71f34148e3d40a5c72')

str(anpp)

# Then, make a summary figure
ggplot(anpp, aes(x = year, y = npp_g_m2, col = site, group = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

##
## Comparing two samples
##

# Create a 2-year data subset
anpp.19.20 <- anpp %>%
  dplyr::filter(year %in% 2019:2020)

# Summary statistics
anpp.19.20.stats <- anpp.19.20 %>%
  group_by(year) %>%
  summarise(n = n(),
            mean = mean(npp_g_m2),
            std.dev = sd(npp_g_m2)) %>%
  mutate(std.err = std.dev/sqrt(n))

anpp.19.20.stats

# Spread to wide form
anpp.19.20.wide <- anpp.19.20 %>%
  spread(year, npp_g_m2)

head(anpp.19.20.wide)

# A default 2-sample t-test in R
t.test(x = anpp.19.20.wide$`2019`, 
       y = anpp.19.20.wide$`2020`)

# t-test with equal variance
t.test(x = anpp.19.20.wide$`2019`, 
       y = anpp.19.20.wide$`2020`, 
       var.equal = TRUE)

# Paired t-test
t.test(x = anpp.19.20.wide$`2019`, 
       y = anpp.19.20.wide$`2020`, 
       paired = TRUE)

##
## Testing for unequal variance
##

# Create a 4-year data subset
anpp.17.20 <- anpp %>%
  dplyr::filter(year %in% 2017:2020)

ggplot(anpp.17.20, aes(x = year, y = npp_g_m2, group = year)) +
  geom_boxplot()

library(car)

bartlett.test(npp_g_m2 ~ year, data = anpp.17.20)
fligner.test(npp_g_m2 ~ year, data = anpp.17.20)
leveneTest(npp_g_m2 ~ as.factor(year), data = anpp.17.20) # from car package

##
## Comparing samples with general linear models
##

# Two-sample mean comparison using a linear regression model
lm.ttest <- lm(npp_g_m2 ~ year, data = anpp.19.20)
summary(lm.ttest)

ggplot(data=anpp.19.20, aes(x=year, y=npp_g_m2)) +
  geom_point() +
  geom_smooth(method="lm")

?aov

anpp.19.20.f <- anpp.19.20
anpp.19.20.f$year <- factor(anpp.19.20.f$year)

# One-way ANOVA equivalent to the 2-sample t-test
aov.ttest <- aov(npp_g_m2 ~ year, data = anpp.19.20.f)
summary(aov.ttest)
car::Anova(lm.ttest, type = "III") # Not clear on difference in outputs, p value is same.

# Two-way ANOVA using year and vegetation zone
aov.2way <- aov(npp_g_m2 ~ year + zone, data = anpp.19.20.f)
summary(aov.2way)

# One-way, repeated measures ANOVA using year and site
aov.1way.rm <- aov(npp_g_m2 ~ year + Error(site/year),
                   data = anpp.19.20.f)
summary(aov.1way.rm)

# ANOVA with 2017 data
# Differences between vegetation zones
anpp.2017 <- anpp %>%
  dplyr::filter(year == "2017") %>%
  mutate(zone = factor(zone))

# Graph data with boxplots
ggplot(anpp.2017, aes(x = zone, y = npp_g_m2, fill = zone)) +
  geom_boxplot()

# Run ANOVA with lm()
lm.ANOVA <- lm(npp_g_m2 ~ zone, data = anpp.2017) 
# Diagnostics
plot(lm.ANOVA)


# Test for normality of residuals
resid(lm.ANOVA) %>% shapiro.test()

# Look at model results.  Note that this reports Type I Sums of Squares
summary(lm.ANOVA)
# Note: recommend using Type III F tests from car::Anova()
# In this case they are the same as Type I
car::Anova(lm.ANOVA, type = "III", test.statistic = "F")

library(emmeans)
# Least squares means (estimated marginal means)
emmeans(lm.ANOVA, ~ zone)

# Notice there are n*(n-2)/2 possible pairwise comparisons
# Pairwise comparisons of least squares means
contrast(emmeans(lm.ANOVA, specs= ~zone), method = "pairwise")

# Letter separations (efficient way to display)
emmeans(lm.ANOVA, ~ zone) %>%
  multcomp::cld(Letters = LETTERS)

# Problem: emmeans switches to Sidak when adjust = "Tukey" is specified
emmeans(lm.ANOVA, ~ zone, adjust = "Tukey") %>%
  multcomp::cld(Letters = LETTERS)

# Compare results from Tukey and Scheffe
emmeans.Tukey <- emmeans(lm.ANOVA, specs = ~ zone) %>% 
  multcomp::cld(Letters = LETTERS) %>%
  as_tibble() %>%
  mutate(method = "Tukey")

emmeans.Scheffe <- emmeans(lm.ANOVA, specs = ~ zone) %>% 
  multcomp::cld(Letters = LETTERS, adjust = "scheffe") %>%
  as_tibble() %>%
  mutate(method = "Scheffe")

# Look at least squares means and 95% confidence intervals
bind_rows(emmeans.Tukey, emmeans.Scheffe) %>%
  mutate(.group = trimws(.group)) %>%
  ggplot(aes(x = zone, y = emmean, col = zone)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = zone, y = emmean, label = .group), hjust = - 0.5, show.legend = FALSE) +
  facet_wrap(~ method, ncol = 1) +
  ggtitle("2017 ANPP by Vegetation Zone", 
          subtitle = "means with the same letter are not different at alpha = 0.05") +
  ylab("Model-based mean ANPP (g/m^2) +/- 95% CI")


# Look at least squares means and standard errors
# This kind of figure is the one most commonly reported in scientific papers
bind_rows(emmeans.Tukey, emmeans.Scheffe) %>%
  mutate(.group = trimws(.group)) %>%
  ggplot(aes(x = zone, y = emmean, col = zone)) +
  geom_point() +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.1) +
  geom_text(aes(x = zone, y = emmean, label = .group), hjust = - 0.5, show.legend = FALSE) +
  facet_wrap(~ method, ncol = 1) +
  ggtitle("2017 ANPP by Vegetation Zone", 
          subtitle = "means with the same letter are not different at alpha = 0.05") +
  ylab("Model-based mean ANPP (g/m^2) +/- S.E.")

##
## Into the wilderness with linear mixed models
##

library(nlme)
library(lme4)

# Repeated measures: 2017-2020
# Compare Annual ANPP between 2019 and 2020
anpp.2017.2020 <- anpp %>%
  dplyr::filter(year %in% 2017:2020) 

# Plot the data
ggplot(anpp.2017.2020, aes(x = year, y = npp_g_m2, col = zone, group = site)) +
  geom_point() +
  geom_line(linetype="dashed", size = .05) +
  scale_x_continuous(breaks = 2017:2020)

# Variance is highest in 2017
anpp.2017.2020 %>%
  group_by(year) %>%
  summarise(std.dev = sd(npp_g_m2))

# Fit a repeated measures model (correlated errors with unstructured structure)
model.un <- gls(npp_g_m2 ~ zone*factor(year),
                corSymm(form = ~ 1 | site),
                weights=varIdent(form=~1|factor(year)),
                data = anpp.2017.2020)

# Fit a repeated measures model (correlated errors with autoregressive covariance structure)
model.ar <- gls(npp_g_m2 ~ zone*factor(year),  
                corAR1(form = ~ 1 | site), 
                weights=varIdent(form=~1|factor(year)), 
                data = anpp.2017.2020)

AIC(model.un)
AIC(model.ar)

# AR actually fits better than Unstructured
# Try using only 2 varaiance: parameters: one for 2017 when the varance in high 
# Set 2018, 2019, 2020 variances to be estimated as the same 

anpp.2017.2020 <- anpp.2017.2020 %>%
  mutate(anpp_level = if_else(year == 2017, "high", "low")) 


model.ar <- gls(npp_g_m2 ~ zone*factor(year),  
                      corAR1(form = ~ 1 | site), 
                      weights=varIdent(form=~1|anpp_level), 
                      data = anpp.2017.2020)

AIC(model.ar)

emmeans(model.ar, ~ zone*factor(year), lmer.df = "kenward-roger") 

# It's a little difficult to extract the correlationa and variance parameters
coef(model.un$modelStruct$corStruct, uncons = FALSE, allCoef = TRUE)
coef(model.un$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE) 

# Compare model results from 3 different mixed models specified similarly
# Note differnt spceficiations in the funtion arguments

# R-side model from nlme::gls()
model.ar.level.gls <- gls(npp_g_m2 ~ zone*factor(year),  
                          corAR1(form = ~ 1 | site), 
                          weights=varIdent(form=~1|anpp_level), 
                          data = anpp.2017.2020,
                          method = 'REML')

# G-side model from nlme::lme()
model.ar.level.lme <- lme(fixed = npp_g_m2 ~ zone*factor(year),  
                          random = ~ 1 | site,
                          weights = varIdent(form = ~ 1 | anpp_level),
                          data = anpp.2017.2020,
                          method = 'REML')

# G-side model from lme4::lmer()
model.ar.level.lmer <- lmer(npp_g_m2 ~ zone*factor(year) + 
                              (anpp_level | site),
                            data = anpp.2017.2020 %>%
                              mutate(anpp_level = factor(anpp_level),
                                     site = factor(site)))

AIC(model.ar.level.gls)
AIC(model.ar.level.lme)
AIC(model.ar.level.lmer)

mod.compare <- emmeans(model.ar.level.gls, ~zone*factor(year), lmer.df = "satterthwaite") %>%
  multcomp::cld(alpha = 0.05, Letters = LETTERS) %>%
  mutate(model = "gls") %>%
  bind_rows(emmeans(model.ar.level.lme, ~zone*factor(year), lmer.df = "satterthwaite") %>%
              multcomp::cld(alpha = 0.05, Letters = LETTERS) %>%
              mutate(model = "lme")) %>%
  bind_rows(emmeans(model.ar.level.lmer, ~zone*factor(year), lmer.df = "satterthwaite") %>%
              multcomp::cld(alpha = 0.05, Letters = LETTERS) %>%
              mutate(model = "lmer"))

# Compare means ... should all be the same
mod.compare %>%
  dplyr::select(zone, year, emmean, model) %>%
  spread(model, emmean)

# Compare standard errors
mod.compare %>%
  dplyr::select(zone, year, SE, model) %>%
  spread(model, SE)

# Compare mean comparison results
mod.compare %>%
  dplyr::select(zone, year, .group, model) %>%
  spread(model, .group)

pd <- position_dodge(0.5)
ggplot(mod.compare, aes(x = year, y = emmean, group = zone, colour = zone)) +
  geom_point(position=pd) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin = emmean - SE , ymax = emmean + SE), width=0.2, position=pd) +
  geom_text(aes(label = .group, y = emmean), 
            position = pd, hjust = -0.1, col = "black", size = 2.5) +
  facet_wrap(~ model, ncol = 1, strip.position = "right") +
  theme(legend.position = "top")

