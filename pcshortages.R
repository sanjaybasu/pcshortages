# Impact of Alleviating Primary Care Shortages on Geographic Inequalities in Mortality in the United States
# contact: sanjay_basu@hms.harvard.edu


##### INSTALL AND LOAD PACKAGES #####
# If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed from CRAN and then loaded.

# annotations for packages:
# tidyverse: The 'tidyverse' is a set of packagesf that work in harmony because they share common data representations and 'API' design. This package is designed to make it easy to install and load multiple 'tidyverse' packages in a single step. Learn more about the 'tidyverse' at <https://tidyverse.org>.
# mgcv: mgcv provides generalized additive modeling functions gam, predict.gam and plot.gam, which are very similar in use to the S functions of the same name designed by Trevor Hastie (with some extensions). However the underlying representation and estimation of the models is based on a penalized regression spline approach, with automatic smoothness selection. A number of other functions such as summary.gam and anova.gam are also provided, for extracting information from a fitted gamObject.
# lme4: lme4 provides functions for fitting and analyzing mixed models: linear (lmer), generalized linear (glmer) and nonlinear (nlmer.)
# stargazer: The stargazer command produces LaTeX code, HTML code and ASCII text for well-formatted tables that hold regression analysis results from several models side-by-side. It can also output summary statistics and data frame content. 
# tableone: The tableone package is an R package that eases the construction of “Table 1”, i.e., patient baseline characteristics table commonly found in biomedical research papers. The packages can summarize both continuous and categorical variables mixed within one table. Categorical variables can be summarized as counts and/or percentages. Continuous variables can be summarized in the “normal” way (means and standard deviations) or “nonnormal” way (medians and interquartile ranges).
# parameters:  Compute and extract model parameters.
# geeM: Calculate coefficients and nuisance parameters using generalized estimating equations. Link and Variance functions can be specified by the user. 
packages = c("tidyverse","mgcv", "lme4", "stargazer", "tableone", "parameters", "geeM")

## Now load or install and load all needed packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

##### LOAD AND DESCRIBE DATASET #####
load(url("https://github.com/sanjaybasu/pcshortages/blob/main/panel?raw=true"))

# Label and definition/units of variables listed below:
# le    Life expectancy, yrs, age-standardized
# mort  All-cause mortality, deaths per 100,000, age-standardized
# cvd   Cardiovascular disease mortality, deaths per 100,000, age-standardized
# ca    Cancer, deaths per 100,000, age-standardized
# infectInfectious diseases, deaths per 100,000, age-standardized
# resp  Respiratory tract diseases, deaths per 100,000, age-standardized
# subst Substance use or injury, deaths per 100,000, age-standardized
# pop   Population size
# eld   Elderly, %
# fem   Female, %
# blk   Black, %
# his   Hispanic, %
# nam   Native American, %
# urb   Urban population, urban-rural classification score [1-4 metropolitan of decreasing population, 5-6 non-metro of decreasing population] score%
# gim   General internal medicine per 100,000
# ped   General pediatrics per 100,000
# fp    Family practitioners per 100,000
# pc    Primary care, total, per 100,000
# spec  Specialists per 100,000
# tot   Total physicians per 100,000
# inc   Income, annual $
# pov   Population under poverty threshold, %
# ed    Education, % of people 25+ years old without a high school diploma years
# unins Uninsured, %
# unemp Unemployment rate, %
# poll  Pollution, days above air quality standard per month
# homeval Median home value, $US 2020
# hobed Hospital beds per 100,000
# medct Medical care costs, Medicare, risk adjusted, per capita
# mcare Insured by Medicare, %
vars <- c("le", "mort", "cvd", "ca", "infect", "resp", "subst", 
          "pop", "eld", "fem", "blk", "his", "nam", "urb",
          "gim", "ped", "fp", "pc", "spec", "tot",
          "inc", "pov", "ed", "unins", "unemp", "poll", "hobed", "medct", "mcare")

# Create an object summarizing all baseline variables (both continuous and categorical) in 2010, stratifying by hpsa: whether a location is a healthcare provider shortage area, defined as <1 primary care provider per 3,500 pop in the year 2010.
panel2017 = panel %>%
  filter(time==2017)
tableOne <- CreateTableOne(vars = vars, strata = c("hpsa"), data = panel2017)

# table 1: print method for the TableOne class objects created by CreateTableOne function. see help(CreateTableOne). nonnormal uses a character vector to specify the variables for which median and IQR are displayed instead of mean and SD
print(tableOne, quote = TRUE, noSpaces = TRUE, test = F, contDigits =1, nonnormal = c("le", "mort", "cvd", "ca", "infect", "resp", "subst", 
                                                                                      "pop", "eld", "fem", "blk", "his", "nam", "urb",
                                                                                      "gim", "ped", "fp", "pc", "spec", "tot",
                                                                                      "inc", "pov", "ed", "unins", "unemp", "poll", "hobed", "medct", "mcare"))

# scale the data by 2 SDs as suggested by Gelman to help with interpretability of the scaled regression coefficients, in that the coefficients can be interpreted as a change from one standard deviation below the mean to one standard deviation above the mean (Statist. Med. 2008; 27:2865–2873).
zpanel = panel 
zpanel$pc = (zpanel$pc-mean(na.omit(zpanel$pc)))/(2*sd(na.omit(zpanel$pc)))
zpanel$eld = (zpanel$eld-mean(na.omit(zpanel$eld)))/(2*sd(na.omit(zpanel$eld)))
zpanel$fem = (zpanel$fem-mean(na.omit(zpanel$fem)))/(2*sd(na.omit(zpanel$fem)))
zpanel$blk = (zpanel$blk-mean(na.omit(zpanel$blk)))/(2*sd(na.omit(zpanel$blk)))
zpanel$his = (zpanel$his-mean(na.omit(zpanel$his)))/(2*sd(na.omit(zpanel$his)))
zpanel$nam = (zpanel$nam-mean(na.omit(zpanel$nam)))/(2*sd(na.omit(zpanel$nam)))
zpanel$urb = (zpanel$urb-mean(na.omit(zpanel$urb)))/(2*sd(na.omit(zpanel$urb)))
zpanel$spec = (zpanel$spec-mean(na.omit(zpanel$spec)))/(2*sd(na.omit(zpanel$spec)))
zpanel$inc = (zpanel$inc-mean(na.omit(zpanel$inc)))/(2*sd(na.omit(zpanel$inc)))
zpanel$pov = (zpanel$pov-mean(na.omit(zpanel$pov)))/(2*sd(na.omit(zpanel$pov)))
zpanel$ed = (zpanel$ed-mean(na.omit(zpanel$ed)))/(2*sd(na.omit(zpanel$ed)))
zpanel$unins = (zpanel$unins-mean(na.omit(zpanel$unins)))/(2*sd(na.omit(zpanel$unins)))
zpanel$unemp = (zpanel$unemp-mean(na.omit(zpanel$unemp)))/(2*sd(na.omit(zpanel$unemp)))
zpanel$poll = (zpanel$poll-mean(na.omit(zpanel$poll)))/(2*sd(na.omit(zpanel$poll)))
zpanel$hobed = (zpanel$hobed-mean(na.omit(zpanel$hobed)))/(2*sd(na.omit(zpanel$hobed)))
zpanel$medct = (zpanel$medct-mean(na.omit(zpanel$medct)))/(2*sd(na.omit(zpanel$medct)))
zpanel$mcare = (zpanel$mcare-mean(na.omit(zpanel$mcare)))/(2*sd(na.omit(zpanel$mcare)))
zpanel$le = zpanel$le*365.25 # life exp in days

# create the the “de-meaned” time-varying variable as well as the “group-meaned” time-varying variable
zpanel <- cbind(
  zpanel,
  demean(zpanel, select = c("pc"), group = "county") # from the package "parameters"
)
# Now we have:
# pc_between : the mean of pc across all time-points, for each county



##### DESCRIPTIVE PLOTS #####

# Figure 1: relationship between pcp density and life expectancy
ggplot(panel, aes(pc, le) ) +
  geom_point(alpha = 0.05, color='#00aaff') +
  stat_smooth(method = gam, formula = y ~ s(x), color='#00aaff') + 
  xlim(quantile(panel$pc+1,c(.025)), quantile(panel$pc,c(.975)))+
  ylim(70,85)+
  theme_minimal()+
  labs(title="Primary care density versus life expectancy",
       x ="Primary care density (physicians per 100,000 pop)", y = "Life expectancy (age-adjusted, years)")

# Appendix Plots below:
# relationship between pcp density and all-cause mortality
ggplot(panel, aes(pc, mort) ) +
  geom_point(alpha = 0.05, color='#00aaff') +
  stat_smooth(method = gam, formula = y ~ s(x), color='#00aaff') + 
  xlim(quantile(panel$pc+1,c(.025)), quantile(panel$pc,c(.975)))+
  ylim(min(panel$mort-10), max(panel$mort+10))+
  theme_minimal()+
  labs(title="Primary care density versus all-cause mortality",
       x ="Primary care density (physicians per 100,000 pop)", y = "All-cause Mortality (deaths per 100,000)")

# relationship between pcp density and cvd mortality
ggplot(panel, aes(pc, cvd) ) +
  geom_point(alpha = 0.05, color='#00aaff') +
  stat_smooth(method = gam, formula = y ~ s(x), color='#00aaff') + 
  xlim(quantile(panel$pc+1,c(.025)), quantile(panel$pc,c(.975)))+
  ylim(min(panel$cvd-1), max(panel$cvd+1))+
  theme_minimal()+
  labs(title="Primary care density versus cardiovascular disease mortality",
       x ="Primary care density (physicians per 100,000 pop)", y = "Cardiovascular Mortality (deaths per 100,000)")

# relationship between pcp density and cancer mortality
ggplot(panel, aes(pc, ca) ) +
  geom_point(alpha = 0.05, color='#00aaff') +
  stat_smooth(method = gam, formula = y ~ s(x), color='#00aaff') + 
  xlim(quantile(panel$pc+1,c(.025)), quantile(panel$pc,c(.975)))+
  ylim(min(panel$ca-1), max(panel$ca+1))+
  theme_minimal()+
  labs(title="Primary care density versus cancer mortality",
       x ="Primary care density (physicians per 100,000 pop)", y = "Cancer Mortality (deaths per 100,000)")

# relationship between pcp density and infectious dz mortality
ggplot(panel, aes(pc, infect) ) +
  geom_point(alpha = 0.05, color='#00aaff') +
  stat_smooth(method = gam, formula = y ~ s(x), color='#00aaff') + 
  xlim(quantile(panel$pc+1,c(.025)), quantile(panel$pc,c(.975)))+
  ylim(min(panel$infect-10), max(panel$infect+10))+
  theme_minimal()+
  labs(title="Primary care density versus infectious disease mortality",
       x ="Primary care density (physicians per 100,000 pop)", y = "Infectious Disease Mortality (deaths per 100,000)")

# relationship between pcp density and resp dz mortality
ggplot(panel, aes(pc, resp) ) +
  geom_point(alpha = 0.05, color='#00aaff') +
  stat_smooth(method = gam, formula = y ~ s(x), color='#00aaff') + 
  xlim(quantile(panel$pc+1,c(.025)), quantile(panel$pc,c(.975)))+
  ylim(min(panel$resp-1), max(panel$resp+1))+
  theme_minimal()+
  labs(title="Primary care density versus respiratory disease mortality",
       x ="Primary care density (physicians per 100,000 pop)", y = "Respiratory Disease Mortality (deaths per 100,000)")

# relationship between pcp density and substance use mortality
ggplot(panel, aes(pc, subst) ) +
  geom_point(alpha = 0.05, color='#00aaff') +
  stat_smooth(method = gam, formula = y ~ s(x), color='#00aaff') + 
  xlim(quantile(panel$pc+1,c(.025)), quantile(panel$pc,c(.975)))+
  ylim(min(panel$subst-1), max(panel$subst+1))+
  theme_minimal()+
  labs(title="Primary care density versus substance use/injury",
       x ="Primary care density (physicians per 100,000 pop)", y = "Substance use/injury Mortality (deaths per 100,000)")





##### GENERALIZED ADDITIVE MODEL #####


# Fits a generalized additive model (GAM) to data, the term ‘GAM’ being taken to include any quadratically penalized GLM and a variety of other models estimated by a quadratically penalised likelihood type approach (see family.mgcv). The degree of smoothness of model terms is estimated as part of fitting. gam can also fit any GLM subject to multiple quadratic penalties (including estimation of degree of penalization). Confidence/credible intervals are readily available for any quantity predicted using a fitted model. The first part of the expression is a GAM formula, exactly like the formula for a GLM except that smooth term, s, can be added to the right hand side to specify that the linear predictor depends on smooth functions of predictors (or linear functionals of these). The family object specifies the distribution and link to use in fitting , which in this case is a log link to adjust for skew and prevent negative predictions of mortality. k is minimum starting basis for knots to estimate smoothing of the observed curvature between PCP density and life expectancy (Figure 1), choosing the minimum possible starting number to avoid overfitting. 
gam.1 = gam(le ~ s(pc,k=1)+s(pc_between,k=1)+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time, data = zpanel, family = gaussian(link=log)) # note that k is  a starting  basis dimension, which will be increased to minimum possible
gam.2 = gam(mort ~ s(pc,k=1)+s(pc_between,k=1)+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time, data = zpanel, family = gaussian(link=log))
gam.3 = gam(cvd ~ s(pc,k=1)+s(pc_between,k=1)+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time, data = zpanel, family = gaussian(link=log))
gam.4 = gam(ca ~ s(pc,k=1)+s(pc_between,k=1)+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time, data = zpanel, family = gaussian(link=log))
gam.5 = gam(infect ~ s(pc,k=1)+s(pc_between,k=1)+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time, data = zpanel, family = gaussian(link=log))
gam.6 = gam(resp ~ s(pc,k=1)+s(pc_between,k=1)+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time, data = zpanel, family = gaussian(link=log))
gam.7 = gam(subst ~ s(pc,k=1)+s(pc_between,k=1)+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time, data = zpanel, family = gaussian(link=log))

summary(gam.1$fitted.values) # This is the prediction of the dependent variable
summary(zpanel$le) # For comparison, observed values
summary(gam.1$residuals) # These are the residuals
summary(gam.2$fitted.values) 
summary(zpanel$mort) 
summary(gam.2$residuals) 
summary(gam.3$fitted.values) 
summary(zpanel$cvd) 
summary(gam.3$residuals) 
summary(gam.4$fitted.values) 
summary(zpanel$ca) 
summary(gam.4$residuals) 
summary(gam.5$fitted.values) 
summary(zpanel$infect) 
summary(gam.5$residuals) 
summary(gam.6$fitted.values) 
summary(zpanel$resp) 
summary(gam.6$residuals) 
summary(gam.7$fitted.values) 
summary(zpanel$subst) 
summary(gam.7$residuals) 

# Diagnostic test on the residuals
gam.1.res <- gam(gam.1$residuals~s(pc,k=1), data=zpanel, select=TRUE) 
plot.gam(gam.1.res, scheme=1)
summary(gam.1.res)
gam.2.res <- gam(gam.1$residuals~s(pc,k=1), data=zpanel, select=TRUE) 
plot.gam(gam.2.res, scheme=1)
summary(gam.2.res)
gam.3.res <- gam(gam.1$residuals~s(pc,k=1), data=zpanel, select=TRUE) 
plot.gam(gam.3.res, scheme=1)
summary(gam.3.res)
gam.4.res <- gam(gam.1$residuals~s(pc,k=1), data=zpanel, select=TRUE) 
plot.gam(gam.4.res, scheme=1)
summary(gam.4.res)
gam.5.res <- gam(gam.1$residuals~s(pc,k=1), data=zpanel, select=TRUE) 
plot.gam(gam.5.res, scheme=1)
summary(gam.5.res)
gam.6.res <- gam(gam.1$residuals~s(pc,k=1), data=zpanel, select=TRUE) 
plot.gam(gam.6.res, scheme=1)
summary(gam.6.res)
gam.7.res <- gam(gam.1$residuals~s(pc,k=1), data=zpanel, select=TRUE) 
plot.gam(gam.7.res, scheme=1)
summary(gam.7.res)

# Table 2: use model to predict outcomes for counties below a threshold given 2017 values of covariates, at goal levels of pcp density
# current 2017 data
goal = 100000/3500  # if goal is 1 PCP per 1500
predictdata_baseline  = zpanel %>%
  filter(pc < (goal-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))),
         time == 2017) 
#  scenario 1: 2017 data with pcp density raised to threshold for non-shortage counties
predictdata_goal = predictdata_baseline %>%
  mutate(pc = goal)
predictdata_goal$pc = (predictdata_goal$pc-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))) #rescale
#  scenario  2: 2017 data with pcp density raised to mean for non-shortage counties
goal2 = 100000/1500  # second [alternative] goal is rulemaking committee rec
predictdata_baseline2  = zpanel %>%
  filter(pc < (goal2-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))),
         time == 2017) 
predictdata_goal2 = predictdata_baseline2 %>%
  mutate(pc = goal2)
predictdata_goal2$pc = (predictdata_goal2$pc-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))) #rescale

# predict delta in outcome of increasing PCP density among shortage areas

#  scenario 1: if achieving 1 pcp  per 3,500 pop
predicted_delta1.1 = predict(gam.1, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.1, newdata=predictdata_baseline, type='response', se=T)$fit 
predicted_delta1.2 = predict(gam.2, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.2, newdata=predictdata_baseline, type='response', se=T)$fit 
predicted_delta1.3 = predict(gam.3, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.3, newdata=predictdata_baseline, type='response', se=T)$fit 
predicted_delta1.4 = predict(gam.4, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.4, newdata=predictdata_baseline, type='response', se=T)$fit 
predicted_delta1.5 = predict(gam.5, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.5, newdata=predictdata_baseline, type='response', se=T)$fit 
predicted_delta1.6 = predict(gam.6, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.6, newdata=predictdata_baseline, type='response', se=T)$fit 
predicted_delta1.7 = predict(gam.7, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.7, newdata=predictdata_baseline, type='response', se=T)$fit 
summary(predicted_delta1.1)
quantile(predicted_delta1.1,c(.025,.975))
summary(predicted_delta1.2)
quantile(predicted_delta1.2,c(.025,.975))
summary(predicted_delta1.3)
quantile(predicted_delta1.3,c(.025,.975))
summary(predicted_delta1.4)
quantile(predicted_delta1.4,c(.025,.975))
summary(predicted_delta1.5)
quantile(predicted_delta1.5,c(.025,.975))
summary(predicted_delta1.6)
quantile(predicted_delta1.6,c(.025,.975))
summary(predicted_delta1.7)
quantile(predicted_delta1.7,c(.025,.975))

#  scenario 2: if achieving 1 pcp  per 1,500 pop
predicted_delta2.1 = predict(gam.1, newdata=predictdata_goal2, type='response', se=T)$fit-predict(gam.1, newdata=predictdata_baseline2, type='response', se=T)$fit 
predicted_delta2.2 = predict(gam.2, newdata=predictdata_goal2, type='response', se=T)$fit-predict(gam.2, newdata=predictdata_baseline2, type='response', se=T)$fit 
predicted_delta2.3 = predict(gam.3, newdata=predictdata_goal2, type='response', se=T)$fit-predict(gam.3, newdata=predictdata_baseline2, type='response', se=T)$fit 
predicted_delta2.4 = predict(gam.4, newdata=predictdata_goal2, type='response', se=T)$fit-predict(gam.4, newdata=predictdata_baseline2, type='response', se=T)$fit 
predicted_delta2.5 = predict(gam.5, newdata=predictdata_goal2, type='response', se=T)$fit-predict(gam.5, newdata=predictdata_baseline2, type='response', se=T)$fit 
predicted_delta2.6 = predict(gam.6, newdata=predictdata_goal2, type='response', se=T)$fit-predict(gam.6, newdata=predictdata_baseline2, type='response', se=T)$fit 
predicted_delta2.7 = predict(gam.7, newdata=predictdata_goal2, type='response', se=T)$fit-predict(gam.7, newdata=predictdata_baseline2, type='response', se=T)$fit 
summary(predicted_delta2.1)
quantile(predicted_delta2.1,c(.025,.975))
summary(predicted_delta2.2)
quantile(predicted_delta2.2,c(.025,.975))
summary(predicted_delta2.3)
quantile(predicted_delta2.3,c(.025,.975))
summary(predicted_delta2.4)
quantile(predicted_delta2.4,c(.025,.975))
summary(predicted_delta2.5)
quantile(predicted_delta2.5,c(.025,.975))
summary(predicted_delta2.6)
quantile(predicted_delta2.6,c(.025,.975))
summary(predicted_delta2.7)
quantile(predicted_delta2.7,c(.025,.975))



##### MIXED EFFECTS MODEL #####

# mixed effects regressions for each outcome. glmer fits a generalized linear mixed effects model, (1+pc|county) fits a correlated random intercept and slope by county. parameters following 'control' are to fix a known issue: https://github.com/lme4/lme4/issues/173
mem.1alt <- glmer(le~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1+pc|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))

# leaving out the random slope term: does it make a diff? not per the likelihood ratio test or AIC
mem.1 <- glmer(le~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))
anova(mem.1alt, mem.1, test="Chisq")
mem.2 <- glmer(mort~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))
mem.3 <- glmer(cvd~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))
mem.4 <- glmer(ca~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))
mem.5 <- glmer(infect~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))
mem.6 <- glmer(resp~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))
mem.7 <- glmer(subst~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare+time+(1|county), data = zpanel, family = gaussian(link=log), control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2, check.conv.singular =.makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2))
# model fe  coefficients
stargazer(mem.1, mem.2, mem.3, mem.4, mem.5, mem.6, mem.7, mem.8,
          type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)

# appendix table 1: mixed effects model results
#  scenario 1: if achieving 1 pcp  per 3,500 pop
predicted_delta1.1 = predict(mem.1, newdata=predictdata_goal, type='response', se=T)-predict(mem.1, newdata=predictdata_baseline, type='response', se=T) 
predicted_delta1.2 = predict(mem.2, newdata=predictdata_goal, type='response', se=T)-predict(mem.2, newdata=predictdata_baseline, type='response', se=T) 
predicted_delta1.3 = predict(mem.3, newdata=predictdata_goal, type='response', se=T)-predict(mem.3, newdata=predictdata_baseline, type='response', se=T) 
predicted_delta1.4 = predict(mem.4, newdata=predictdata_goal, type='response', se=T)-predict(mem.4, newdata=predictdata_baseline, type='response', se=T) 
predicted_delta1.5 = predict(mem.5, newdata=predictdata_goal, type='response', se=T)-predict(mem.5, newdata=predictdata_baseline, type='response', se=T) 
predicted_delta1.6 = predict(mem.6, newdata=predictdata_goal, type='response', se=T)-predict(mem.6, newdata=predictdata_baseline, type='response', se=T) 
predicted_delta1.7 = predict(mem.7, newdata=predictdata_goal, type='response', se=T)-predict(mem.7, newdata=predictdata_baseline, type='response', se=T) 
summary(predicted_delta1.1)
summary(predicted_delta1.2)
summary(predicted_delta1.3)
summary(predicted_delta1.4)
summary(predicted_delta1.5)
summary(predicted_delta1.6)
summary(predicted_delta1.7)

#  scenario 2: if achieving 1 pcp  per 1,500 pop
predicted_delta2.1 = predict(mem.1, newdata=predictdata_goal2, type='response', se=T)-predict(mem.1, newdata=predictdata_baseline2, type='response', se=T) 
predicted_delta2.2 = predict(mem.2, newdata=predictdata_goal2, type='response', se=T)-predict(mem.2, newdata=predictdata_baseline2, type='response', se=T) 
predicted_delta2.3 = predict(mem.3, newdata=predictdata_goal2, type='response', se=T)-predict(mem.3, newdata=predictdata_baseline2, type='response', se=T) 
predicted_delta2.4 = predict(mem.4, newdata=predictdata_goal2, type='response', se=T)-predict(mem.4, newdata=predictdata_baseline2, type='response', se=T) 
predicted_delta2.5 = predict(mem.5, newdata=predictdata_goal2, type='response', se=T)-predict(mem.5, newdata=predictdata_baseline2, type='response', se=T) 
predicted_delta2.6 = predict(mem.6, newdata=predictdata_goal2, type='response', se=T)-predict(mem.6, newdata=predictdata_baseline2, type='response', se=T) 
predicted_delta2.7 = predict(mem.7, newdata=predictdata_goal2, type='response', se=T)-predict(mem.7, newdata=predictdata_baseline2, type='response', se=T) 
summary(predicted_delta2.1)
summary(predicted_delta2.2)
summary(predicted_delta2.3)
summary(predicted_delta2.4)
summary(predicted_delta2.5)
summary(predicted_delta2.6)
summary(predicted_delta2.7)


##### GEE #####
# generalized estimating equation with AR-1 correlation structure, robust standard errors [sandwich], and log link function
gee.1 = geem(le~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare, id = county, data = zpanel, corstr="ar1", sandwich = T, family = gaussian(link = "log"))
gee.2 = geem(mort~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare, id = county, data = zpanel, corstr="ar1", sandwich = T, family = gaussian(link = "log"))
gee.3 = geem(cvd~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare, id = county, data = zpanel, corstr="ar1", sandwich = T, family = gaussian(link = "log"))
gee.4 = geem(ca~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare, id = county, data = zpanel, corstr="ar1", sandwich = T, family = gaussian(link = "log"))
gee.5 = geem(infect~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare, id = county, data = zpanel, corstr="ar1", sandwich = T, family = gaussian(link = "log"))
gee.6 = geem(resp~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare, id = county, data = zpanel, corstr="ar1", sandwich = T, family = gaussian(link = "log"))
gee.7 = geem(subst~pc+pc_between+eld+fem+blk+his+nam+urb+spec+inc+pov+ed+unins+unemp+poll+hobed+medct+mcare, id = county, data = zpanel, corstr="ar1", sandwich = T, family = gaussian(link = "log"))

# for GEE predict function, have to run the predictions manually due to the predict function not being available in the geeM package
gee.1.beta = summary(gee.1)[1]$beta
gee.2.beta = summary(gee.2)[1]$beta
gee.3.beta = summary(gee.3)[1]$beta
gee.4.beta = summary(gee.4)[1]$beta
gee.5.beta = summary(gee.5)[1]$beta
gee.6.beta = summary(gee.6)[1]$beta
gee.7.beta = summary(gee.7)[1]$beta

# make matrix to multiply  by  beta coefs
predictdata_baseline = predictdata_baseline %>%
  select("pc","pc_between","eld","fem","blk","his","nam","urb","spec", "inc", "pov", "ed", "unins", "unemp", "poll", "hobed", "medct", "mcare")
predictdata_goal = predictdata_goal %>%
  select("pc","pc_between","eld","fem","blk","his","nam","urb","spec", "inc", "pov", "ed", "unins", "unemp", "poll", "hobed", "medct", "mcare")
predictdata_baseline2 = predictdata_baseline2 %>%
  select("pc","pc_between","eld","fem","blk","his","nam","urb","spec", "inc", "pov", "ed", "unins", "unemp", "poll", "hobed", "medct", "mcare")
predictdata_goal2 = predictdata_goal2 %>%
  select("pc","pc_between","eld","fem","blk","his","nam","urb","spec", "inc", "pov", "ed", "unins", "unemp", "poll", "hobed", "medct", "mcare")

# convert beta coef to matrix to multiply  by  values to get output
gee.1.betamatrix = matrix(rep(gee.1.beta[2:length(gee.1.beta)],dim(predictdata_baseline)[1]),nrow=dim(predictdata_baseline)[1],byrow=T)
gee.2.betamatrix = matrix(rep(gee.2.beta[2:length(gee.2.beta)],dim(predictdata_baseline)[1]),nrow=dim(predictdata_baseline)[1],byrow=T)
gee.3.betamatrix = matrix(rep(gee.3.beta[2:length(gee.3.beta)],dim(predictdata_baseline)[1]),nrow=dim(predictdata_baseline)[1],byrow=T)
gee.4.betamatrix = matrix(rep(gee.4.beta[2:length(gee.4.beta)],dim(predictdata_baseline)[1]),nrow=dim(predictdata_baseline)[1],byrow=T)
gee.5.betamatrix = matrix(rep(gee.5.beta[2:length(gee.5.beta)],dim(predictdata_baseline)[1]),nrow=dim(predictdata_baseline)[1],byrow=T)
gee.6.betamatrix = matrix(rep(gee.6.beta[2:length(gee.6.beta)],dim(predictdata_baseline)[1]),nrow=dim(predictdata_baseline)[1],byrow=T)
gee.7.betamatrix = matrix(rep(gee.7.beta[2:length(gee.7.beta)],dim(predictdata_baseline)[1]),nrow=dim(predictdata_baseline)[1],byrow=T)

#  scenario 1: if achieving 1 pcp  per 3,500 pop
# multiply betas by covar values and add intercept, then exp transform to account for log link
predicted_delta1.1 = exp(rowSums(gee.1.betamatrix*predictdata_goal) +  gee.1.beta[1])-exp(rowSums(gee.1.betamatrix*predictdata_baseline) +  gee.1.beta[1])
predicted_delta1.2 = exp(rowSums(gee.2.betamatrix*predictdata_goal) +  gee.2.beta[1])-exp(rowSums(gee.2.betamatrix*predictdata_baseline) +  gee.2.beta[1])
predicted_delta1.3 = exp(rowSums(gee.3.betamatrix*predictdata_goal) +  gee.3.beta[1])-exp(rowSums(gee.3.betamatrix*predictdata_baseline) +  gee.3.beta[1])
predicted_delta1.4 = exp(rowSums(gee.4.betamatrix*predictdata_goal) +  gee.4.beta[1])-exp(rowSums(gee.4.betamatrix*predictdata_baseline) +  gee.4.beta[1])
predicted_delta1.5 = exp(rowSums(gee.5.betamatrix*predictdata_goal) +  gee.5.beta[1])-exp(rowSums(gee.5.betamatrix*predictdata_baseline) +  gee.5.beta[1])
predicted_delta1.6 = exp(rowSums(gee.6.betamatrix*predictdata_goal) +  gee.6.beta[1])-exp(rowSums(gee.6.betamatrix*predictdata_baseline) +  gee.6.beta[1])
predicted_delta1.7 = exp(rowSums(gee.7.betamatrix*predictdata_goal) +  gee.7.beta[1])-exp(rowSums(gee.7.betamatrix*predictdata_baseline) +  gee.7.beta[1])
summary(predicted_delta1.1)
summary(predicted_delta1.2)
summary(predicted_delta1.3)
summary(predicted_delta1.4)
summary(predicted_delta1.5)
summary(predicted_delta1.6)
summary(predicted_delta1.7)

#  scenario 2: if achieving 1 pcp  per 1,500 pop
predicted_delta2.1 = exp(rowSums(gee.1.betamatrix*predictdata_goal2) +  gee.1.beta[1])-exp(rowSums(gee.1.betamatrix*predictdata_baseline2) +  gee.1.beta[1])
predicted_delta2.2 = exp(rowSums(gee.2.betamatrix*predictdata_goal2) +  gee.2.beta[1])-exp(rowSums(gee.2.betamatrix*predictdata_baseline2) +  gee.2.beta[1])
predicted_delta2.3 = exp(rowSums(gee.3.betamatrix*predictdata_goal2) +  gee.3.beta[1])-exp(rowSums(gee.3.betamatrix*predictdata_baseline2) +  gee.3.beta[1])
predicted_delta2.4 = exp(rowSums(gee.4.betamatrix*predictdata_goal2) +  gee.4.beta[1])-exp(rowSums(gee.4.betamatrix*predictdata_baseline2) +  gee.4.beta[1])
predicted_delta2.5 = exp(rowSums(gee.5.betamatrix*predictdata_goal2) +  gee.5.beta[1])-exp(rowSums(gee.5.betamatrix*predictdata_baseline2) +  gee.5.beta[1])
predicted_delta2.6 = exp(rowSums(gee.6.betamatrix*predictdata_goal2) +  gee.6.beta[1])-exp(rowSums(gee.6.betamatrix*predictdata_baseline2) +  gee.6.beta[1])
predicted_delta2.7 = exp(rowSums(gee.7.betamatrix*predictdata_goal2) +  gee.7.beta[1])-exp(rowSums(gee.7.betamatrix*predictdata_baseline2) +  gee.7.beta[1])
summary(predicted_delta2.1)
summary(predicted_delta2.2)
summary(predicted_delta2.3)
summary(predicted_delta2.4)
summary(predicted_delta2.5)
summary(predicted_delta2.6)
summary(predicted_delta2.7)



# table 3: Projected number of PCPs needed to achieve different thresholds of density per 100,000 population
goal = 100000/3500  # if goal is 1 PCP per 1500
short = panel %>%
  filter(time==2017) %>%
  mutate(shortfall = goal-pc) # if goal is 1 PCP per 3500
sum(short$shortfall[short$shortfall>0])
sum(short$shortfall[short$shortfall>0])/sum(short$shortfall>0)
#  Projected number of averted deaths per 100,000 per year if all counties met goal (95% CI)
predictdata_baseline  = zpanel %>%
  filter(pc < (goal-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))),
         time == 2017) 
#  scenario 1: 2017 data with pcp density raised to threshold for non-shortage counties
predictdata_goal = predictdata_baseline %>%
  mutate(pc = goal)
predictdata_goal$pc = (predictdata_goal$pc-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))) #rescale
# predict delta in outcome of increasing PCP density among shortage areas from GAM all-cause mort model
predicted_delta1.2 = predict(gam.2, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.2, newdata=predictdata_baseline, type='response', se=T)$fit 
summary(predicted_delta1.2) # per 100k
summary(predicted_delta1.2)*sum(panel$pop[panel$pc<goal & panel$time==2017])/100000 # nationwide abs population
  

goal = 100000/1500  # if goal is 1 PCP per 1500
short = panel %>%
  filter(time==2017) %>%
  mutate(shortfall = goal-pc)
sum(short$shortfall[short$shortfall>0])
predictdata_baseline  = zpanel %>%
  filter(pc < (goal-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))),
         time == 2017) 
predictdata_goal = predictdata_baseline %>%
  mutate(pc = goal)
predictdata_goal$pc = (predictdata_goal$pc-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))) #rescale
predicted_delta1.2 = predict(gam.2, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.2, newdata=predictdata_baseline, type='response', se=T)$fit 
summary(predicted_delta1.2)
summary(predicted_delta1.2)*sum(panel$pop[panel$pc<goal & panel$time==2017])/100000 # nationwide abs population



goal = 100000/2000  # if goal is 1 PCP per 2000
short = panel %>%
  filter(time==2017) %>%
  mutate(shortfall = goal-pc)
sum(short$shortfall[short$shortfall>0])
predictdata_baseline  = zpanel %>%
  filter(pc < (goal-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))),
         time == 2017) 
predictdata_goal = predictdata_baseline %>%
  mutate(pc = goal)
predictdata_goal$pc = (predictdata_goal$pc-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))) #rescale
predicted_delta1.2 = predict(gam.2, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.2, newdata=predictdata_baseline, type='response', se=T)$fit 
summary(predicted_delta1.2)
summary(predicted_delta1.2)*sum(panel$pop[panel$pc<goal & panel$time==2017])/100000 # nationwide abs population



goal = 100000/2500  # if goal is 1 PCP per 2500
short = panel %>%
  filter(time==2017) %>%
  mutate(shortfall = goal-pc)
sum(short$shortfall[short$shortfall>0])
predictdata_baseline  = zpanel %>%
  filter(pc < (goal-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))),
         time == 2017) 
predictdata_goal = predictdata_baseline %>%
  mutate(pc = goal)
predictdata_goal$pc = (predictdata_goal$pc-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))) #rescale
predicted_delta1.2 = predict(gam.2, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.2, newdata=predictdata_baseline, type='response', se=T)$fit 
summary(predicted_delta1.2)
summary(predicted_delta1.2)*sum(panel$pop[panel$pc<goal & panel$time==2017])/100000 # nationwide abs population


goal = 100000/3000  # if goal is 1 PCP per 3000
short = panel %>%
  filter(time==2017) %>%
  mutate(shortfall = goal-pc)
sum(short$shortfall[short$shortfall>0])
predictdata_baseline  = zpanel %>%
  filter(pc < (goal-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))),
         time == 2017) 
predictdata_goal = predictdata_baseline %>%
  mutate(pc = goal)
predictdata_goal$pc = (predictdata_goal$pc-mean(na.omit(panel$pc)))/(2*sd(na.omit(panel$pc))) #rescale
predicted_delta1.2 = predict(gam.2, newdata=predictdata_goal, type='response', se=T)$fit-predict(gam.2, newdata=predictdata_baseline, type='response', se=T)$fit 
summary(predicted_delta1.2)
summary(predicted_delta1.2)*sum(panel$pop[panel$pc<goal & panel$time==2017])/100000 # nationwide abs population

# fig 2: PCPS per 100000 vs averted national deaths per year
# PCPs per 100k per yr targets
x = c(100000/1500,
      100000/2000,
      100000/2500,
      100000/3000,
      100000/3500,
      0)
# mean outcomes from table 3
ymean = c(9404,
          2691,
          1002,
          462,
          230,
          0)
# 25th %tile from table 3
ymin =  c(7982,
          2135,
          704,
          268,
          103,
          0)
# 75th %tile from table 3
ymax = c(10689,
         3185,
         1258,
         634,
         349,
         0)

fig2  = as.data.frame(cbind(x,ymean,ymin,ymax))
ggplot(data = fig2)+
  aes(x = x,  y = ymean)+
  ggtitle("Primary care provider density versus averted mortality")+
  xlab("Primary care providers per 100,000 population")+
  ylab("Averted deaths per year")+
  geom_ribbon(aes(ymin=ymin, ymax =ymax),fill="#6699CC")+
  geom_line()+
  theme_minimal()+
  geom_vline(xintercept = 100000/3500, linetype="dashed", color = "#6699CC", size = 1)+
  geom_vline(xintercept = 100000/1500, linetype="dotted", color = "#6699CC", size = 1)


