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
## How do we know which comparison (t-test) to use?
##

# Create a slightly different data subset - 2017 and 2018
anpp.17.18 <- anpp %>%
  dplyr::filter(year %in% 2017:2018)

ggplot(anpp.17.18, aes(x = year, y = npp_g_m2, color = factor(year))) +
  geom_boxplot()

# Bartlett test for unequal variance (homescedasticity)
bartlett.test(npp_g_m2 ~ year, data = anpp.17.18)

# Test for normality of the data
shapiro.test(anpp.17.18$npp_g_m2)

# Fligner test for homoscedasticity
fligner.test(npp_g_m2 ~ as.factor(year), data = anpp.17.18)

# Levene test for homoscedasticity, which is in the `car` R package
car::leveneTest(npp_g_m2 ~ as.factor(year), data = anpp.17.18)

# Spread to wide form
anpp.17.18.wide <- anpp.17.18 %>%
  spread(year, npp_g_m2)

# Paired t-test
t.test(x = anpp.17.18.wide$`2017`, 
       y = anpp.17.18.wide$`2018`, 
       paired = TRUE,
       equal_variance=FALSE) # This is the default, we don't have to specify

##
## Comparing sample means with simple linear models
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
# We could also use car::Anova here, but the results are the same
#car::Anova(lm.ttest, type = "III")

# Two-way ANOVA using year and vegetation zone
aov.2way <- aov(npp_g_m2 ~ year + zone, data = anpp.19.20.f)
summary(aov.2way)

# One-way, repeated measures ANOVA using year and site
aov.1way.rm <- aov(npp_g_m2 ~ year + Error(site/year),
                   data = anpp.19.20.f)
summary(aov.1way.rm)

##
## Comparing group means with ANOVA results
##

# Subset to 2017
anpp.2017 <- anpp %>%
  dplyr::filter(year == "2017") %>%
  mutate(zone = factor(zone))

# Graph data by zone with boxplots
ggplot(anpp.2017, aes(x = zone, y = npp_g_m2, fill = zone)) +
  geom_boxplot()

# Run ANOVA with lm()
lm.ANOVA <- lm(npp_g_m2 ~ zone, data = anpp.2017)

anova(lm.ANOVA)

car::Anova(lm.ANOVA, type = "III", test.statistic = "F")

# Standard lm diagnostic plots - we could put them all on one page
#par(mfrow = c(2, 2), oma = c(0, 0, 2, 0)) -> opar
#plot(lm.ANOVA)

# But this gives us just the plot we need
qqnorm(resid(lm.ANOVA))
qqline(resid(lm.ANOVA))

# Test for normality of residuals
resid(lm.ANOVA) %>% shapiro.test()

##
## Post-hoc tests and pairwise comparisons
##

library(emmeans)
# Least squares means (estimated marginal means)
emmeans(lm.ANOVA, ~ zone)

# Pairwise comparisons of least squares means
contrast(emmeans(lm.ANOVA, specs= ~zone), method = "pairwise")

# Letter separations (efficient way to display)
emmeans.Tukey <- emmeans(lm.ANOVA, ~ zone) %>%
  multcomp::cld(Letters = LETTERS)

emmeans.Tukey

# Problem: emmeans switches to Sidak when adjust = "Tukey" is specified
# I actually found this from Russ Lenth:
#    https://stats.stackexchange.com/questions/508055/unclear-why-adjust-tukey-was-changed-to-sidak
# It seems the confidence limits are being adjusted when you specify adjust = 
# "Tukey" but not the means themselves. This is intended I guess, but I'm still
# fuzzy on the statistical meaning.
emmeans(lm.ANOVA, ~ zone, adjust = "Tukey") %>%
  multcomp::cld(Letters = LETTERS)

# First get comparisons from Scheffe
emmeans.Scheffe <- emmeans(lm.ANOVA, specs = ~ zone) %>% 
  multcomp::cld(Letters = LETTERS, adjust = "scheffe") %>%
  as_tibble() %>%
  mutate(method = "Scheffe")

# Put the Tukey results in a tibble
emmeans.Tukey <- emmeans.Tukey %>% as_tibble() %>% mutate(method = "Tukey")

# Look at least squares means and 95% confidence intervals for
# Tukey and Scheffe
bind_rows(emmeans.Scheffe, emmeans.Tukey) %>%
  mutate(.group = trimws(.group)) %>%
  ggplot(aes(x = zone, y = emmean, col = zone)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.1) +
  geom_text(aes(x = zone, y = emmean, label = .group),
            hjust = -0.5, show.legend = FALSE) +
  facet_wrap(~ method, ncol = 1) +
  ggtitle("2017 ANPP by Vegetation Zone", 
          subtitle = "Same-letter means are not different at alpha = 0.05") +
  ylab("Model-based mean ANPP (g/m^2) +/- 95% CI")

# Look at least squares means and standard errors
# This kind of figure is the one most commonly reported in scientific papers
bind_rows(emmeans.Tukey, emmeans.Scheffe) %>%
  mutate(.group = trimws(.group)) %>%
  ggplot(aes(x = zone, y = emmean, col = zone)) +
  geom_point() +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.1) +
  geom_text(aes(x = zone, y = emmean, label = .group), 
            hjust = - 0.5, show.legend = FALSE) +
  facet_wrap(~ method, ncol = 1) +
  ggtitle("2017 ANPP by Vegetation Zone", 
          subtitle = "Same-letter means are not different at alpha = 0.05") +
  ylab("Model-based mean ANPP (g/m^2) +/- S.E.")

##
## Into the wilderness with linear mixed models
##

library(nlme)
library(lme4)

# Subset the data between 2017 and 2020
anpp.17.20 <- anpp %>%
  dplyr::filter(year %in% 2017:2020) 

# Plot the data
ggplot(anpp.17.20, aes(x = year, y = npp_g_m2, col = zone, group = site)) +
  geom_point() +
  geom_line(linetype="dashed", size = .05) +
  scale_x_continuous(breaks = 2017:2020)

# Variance is highest in 2017
anpp.17.20 %>%
  group_by(year) %>%
  summarise(mean = mean(npp_g_m2),
            std.dev = sd(npp_g_m2))

# Fit a repeated measures model (unstructured correlated errors)
model.un <- gls(npp_g_m2 ~ zone + factor(year) + zone*factor(year),
                correlation = corSymm(form = ~ 1 | site),
                weights = varIdent(form=~1|factor(year)),
                data = anpp.17.20)

# Fit a repeated measures model (correlated errors with autoregressive
# covariance structure)
model.ar <- gls(npp_g_m2 ~ zone + factor(year) + zone*factor(year),  
                correlation = corAR1(form = ~ 1 | site), 
                weights = varIdent(form=~1|factor(year)), 
                data = anpp.17.20)

AIC(model.un)
AIC(model.ar)

# Assign 2017 to high variance group, other years to low
anpp.17.20 <- anpp.17.20 %>%
  mutate(anpp_level = if_else(year == 2017, "high", "low")) 

# Re-fit a model with a variance level
model.ar.level <- gls(npp_g_m2 ~ zone + factor(year) + zone*factor(year),  
                      correlation = corAR1(form = ~ 1 | site), 
                      weights = varIdent(form=~1|anpp_level), 
                      data = anpp.17.20)

AIC(model.ar.level)

emmeans(model.ar.level, ~ zone*factor(year), lmer.df = "kenward-roger") 



# R-side model from nlme::gls()
model.ar.level.gls <- gls(npp_g_m2 ~ zone + factor(year) + zone*factor(year),  
                          correlation = corAR1(form = ~ 1 | site), 
                          weights = varIdent(form=~1|anpp_level), 
                          data = anpp.17.20,
                          method = 'REML')

# G-side model from nlme::lme()
model.ar.level.lme <- lme(fixed = npp_g_m2 ~ zone + factor(year) +
                            zone*factor(year),  
                          random = ~ 1 | site,
                          weights = varIdent(form = ~ 1 | anpp_level),
                          data = anpp.17.20,
                          method = 'REML')

# G-side model from lme4::lmer()
model.ar.level.lmer <- lmer(npp_g_m2 ~ zone + factor(year) + zone*factor(year) + 
                              (anpp_level | site),
                            data = anpp.17.20)

AIC(model.ar.level.gls)
AIC(model.ar.level.lme)
AIC(model.ar.level.lmer)

mod.compare <- emmeans(model.ar.level.gls, ~zone*factor(year),
                       lmer.df = "satterthwaite") %>%
  multcomp::cld(alpha = 0.05, Letters = LETTERS) %>%
  mutate(model = "gls") %>%
  bind_rows(emmeans(model.ar.level.lme, ~zone*factor(year),
                    lmer.df = "satterthwaite") %>%
              multcomp::cld(alpha = 0.05, Letters = LETTERS) %>%
              mutate(model = "lme")) %>%
  bind_rows(emmeans(model.ar.level.lmer, ~zone*factor(year),
                    lmer.df = "satterthwaite") %>%
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

