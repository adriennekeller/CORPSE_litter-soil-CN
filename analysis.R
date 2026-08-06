### Set up, read in data ----
# load library and provide authorization
packload <- c('tidyverse', 'ggplot2','lme4', "lmerTest", "googlesheets4", "nlme")
lapply(packload, library, character.only=TRUE)

# read in data from google sheets (read_sheet only works with google sheets, and not other file types within Google Drive)
CNdat <- read_sheet("https://docs.google.com/spreadsheets/d/12edkr86p_gFbMxaKwGdy4NxqokZ3CEPUPhlxZ9SiaSs/edit?gid=606806724#gid=606806724",
                  sheet = "Data", skip = 1, col_names = T)
climNdat <- read_sheet("https://docs.google.com/spreadsheets/d/1aoN08UooY0muD2u6W6j8k4V4oyUAgNnkEsDeVOPq6hI/edit?gid=878516133#gid=878516133")

# join litter and soil C:N data with climate and soil data
df <- CNdat %>% left_join(climNdat, by = "recordID") %>%
  mutate(soilCN = ifelse(is.na(soilCN), soilC_pct/soilN_pct, soilCN),
         litterCN = if_else(is.na(litterCN), litterC_pct/litterN_pct, litterCN)) %>%
  filter(soilCN > 3) # filters out 4 observations

# clean up columns that were duplicated with left_join above
df <- df %>% dplyr::select(-c(lat.y, long.y, citation_num.y)) %>%
  dplyr::rename(lat = lat.x, lon = long.x, citation_num = citation_num.x)


### Explore data
# Examine distribution of single variables
hist(df$soilCN) # a few very high values, but we checked their veracity
hist(log(df$soilCN)) # log-transform improves normal distribution
hist(df$litterCN) # right skewed 
hist(log(df$litterCN)) # log-transform improves normal distribution
hist(df$litterC_pct) # some outliers (excepted value ~45-50%) but we checked their veracity
hist(df$litterN_pct) # some outliers but we checked their veracity

hist(df$MAT)
hist(df$CLAY)
hist(df$NDEP)
hist(df$MAOM_TOT)
hist(df$lat) # will want to take abs() when using latitude as a covariate
hist(df$pH)

# EDA plotting
# create latitude bins 
df$latbins <- cut(abs(df$lat), breaks = c(0,23.5,40,90), labels = c("0-23.5", "23.5-40", "40-90"))

#data exploration of subsets of data
df_temp <- filter(df, soilCN > 60 | soilCN < 5)
df_neon <- filter(df, citation == 'NEON')
df_non_neon <- filter(df, citation != 'NEON')
df_non_marambaia <- filter(df, citation_num != '1074')
df_fresh <- filter(df, fresh_litter_not_floor == TRUE)
df_latbin1 <- df %>% filter(latbins == "0-23.5")
df_latbin2 <- df %>% filter(latbins == "23.5-40")
df_latbin3 <- df %>% filter(latbins == "40-90")

### Bivariate relationship between litter and soil C:N (with global dataset and subsets)
#global dataset
ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  theme_bw()
ggsave("litter-soil-CN.png")

# neon and non-neon data
ggplot(df_neon, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  geom_point(data = df_non_neon, aes(x = log(litterCN), y = log(soilCN), color = 'red'))+ 
  geom_smooth(data = df_non_neon, method = "lm")+
  theme_bw()
# fresh litter vs forest floor litter
ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~fresh_litter_not_floor)+
  theme_bw()
#highlight influential site - marambaia
ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point(color = 'red') + geom_smooth(method = "lm") +
  geom_point(data = df_non_marambaia, aes(x = log(litterCN), y = log(soilCN)), color = 'black')+ 
  geom_smooth(data = df_non_marambaia, method = "lm") +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')+
  theme_bw()

# OLS regressions with global and subset data
mod1 <- lm(log(soilCN)~ log(litterCN), data = df); summary(mod1)
mod2 <- lm(log(soilCN)~ log(litterCN), data = df_neon); summary(mod2)
mod3 <- lm(log(soilCN)~ log(litterCN), data = df_non_neon); summary(mod3)
mod4 <- lm(log(soilCN)~ log(litterCN), data = df_non_marambaia); summary(mod4)
mod5 <- lm(log(soilCN)~ log(litterCN), data = filter(df, fresh_litter_not_floor == TRUE)); summary(mod5)
mod6 <- lm(log(soilCN)~ log(litterCN), data = filter(df, fresh_litter_not_floor == FALSE)); summary(mod6)

# bivariate relationship faceted by latitude bins
ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~latbins) +
  theme_bw()
ggsave("bivar_latbins.png")

### Building statistical model ---
# what are possible covariates?
df_no_na_cols <- df %>%
  select(where(~ !any(is.na(.))))
names(df_no_na_cols)

# bivars of climate/Ndep data - check for colinearity
ggplot(df, aes(x = MAP, y = MAT)) + geom_point(); cor.test(df$MAP, df$MAT) # r = 0.66
ggplot(df, aes(x = MAP, y = abs(lat))) + geom_point(); cor.test(df$MAP, abs(df$lat)) # r = -0.68
ggplot(df, aes(x = MAT, y = abs(lat))) + geom_point(); cor.test(df$MAT, abs(df$lat)) # r = -0.90
ggplot(df, aes(x = MAP, y = NDEP)) + geom_point(); cor.test(df$MAP, df$NDEP) # r = 0.12
#NDEP can be included along with one of MAP, MAT, or abs(lat) (the latter three are all highly correlated)
ggplot(df, aes(x = abs(lat), y = MAOM_TOT)) + geom_point(); cor.test(abs(df$lat), df$MAOM_TOT) # r = -0.11


## Fit linear mixed model for soilC:N ~ litterC:N (below, run through step 7/8 for different climate/lat variables given their colinearity)
# Step 1: linear model
mod.lm <- lm(log(soilCN) ~ log(litterCN), data = df)
summary(mod.lm)
plot(mod.lm)

# Step 2: Fit model with GLS (requires nlme::gsl())
library(nlme)
form <- formula(log(soilCN) ~ log(litterCN))
mod.gls <- gls(form, data = df)
summary(mod.gls)

# Step 3/4: Choose variance structure (add in random effects) and fit model
mod.lme <- nlme::lme(log(soilCN) ~ log(litterCN), random = ~1 | siteID, data = df,
                     method = "REML")
summary(mod.lme)

# Step 5: compare gls and lme models - does lme provide better fit?
anova(mod.gls, mod.lme) # random effects provide better fit

# Step 6: assess homogeneity of variance and independence
e2 <- resid(mod.lme, type = "normalized") # residuals from mod
f2 <- fitted(mod.lme) # fitted values from model
op <- par(mfrow = c(2,1), mar = c(2,2,1,1)) # set up plot
plot(x = e2, y = f2, xlab = "Fitted Values", ylab = "Residuals") # fitted vs resid plot
plot(e2 ~ log(litterCN), data = df, main = "log Litter CN", ylab = "Residuals") # residuals vs explanatory var

# Step 7/8 - optimal fixed structure
summary(mod.lme) # examine significance of regression parameters
mod.lme.ML <- nlme::lme(log(soilCN) ~ log(litterCN), random = ~ 1| siteID, 
                        data = df, method = "ML")

#run through full --> drop --> final model with MAT
mod.lme.ML.full <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + CLAY + 
                               NDEP + MAOM_TOT + sample_depth_cm,
                             random = ~ 1| siteID, 
                             data = df, method = "ML")
summary(mod.lme.ML.full)

mod.lme.ML.dropclay <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + NDEP + 
                                   MAOM_TOT + sample_depth_cm, random = ~ 1| siteID, 
                                 data = df, method = "ML")
summary(mod.lme.ML.dropclay)
anova(mod.lme.ML.full, mod.lme.ML.dropclay) # no diff - drop clay

mod.lme.ML.dropMAOM_TOT <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + 
                                       NDEP + sample_depth_cm,
                                     random = ~ 1| siteID, 
                                     data = df, method = "ML")
summary(mod.lme.ML.dropMAOM_TOT)
anova(mod.lme.ML.dropclay, mod.lme.ML.dropMAOM_TOT) # no diff - also drop MAOM_TOT

mod.lme.ML.dropsampledepth <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + 
                                          NDEP,
                           random = ~ 1| siteID, 
                           data = df, method = "ML")
summary(mod.lme.ML.dropsampledepth)
anova(mod.lme.ML.dropMAOM_TOT, mod.lme.ML.dropsampledepth) # no diff - also drop sample depth

mod.lme.final <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + NDEP,
                           random = ~ 1| siteID, 
                           data = df, method = "REML")
mod.lme.final.MAT <- mod.lme.final
summary(mod.lme.final.MAT) # final fitted model with REML method

#run through full --> drop --> final model with MAP
mod.lme.ML.full <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAP + CLAY + 
                               NDEP + MAOM_TOT + sample_depth_cm,
                             random = ~ 1| siteID, 
                             data = df, method = "ML")
summary(mod.lme.ML.full) 

mod.lme.ML.dropMAOM_TOT <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAP + NDEP + 
                                   CLAY + sample_depth_cm, random = ~ 1| siteID, 
                                 data = df, method = "ML")
summary(mod.lme.ML.dropMAOM_TOT)
anova(mod.lme.ML.full, mod.lme.ML.dropMAOM_TOT) # no diff - drop MAOM_TOT

mod.lme.ML.dropMAP <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                  CLAY + sample_depth_cm, random = ~ 1| siteID, 
                                data = df, method = "ML") 
summary(mod.lme.ML.dropMAP)
anova(mod.lme.ML.dropMAOM_TOT, mod.lme.ML.dropMAP) # no diff - also drop MAP

mod.lme.ML.dropsampledepth <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                       CLAY,
                                     random = ~ 1| siteID, 
                                     data = df, method = "ML")
summary(mod.lme.ML.dropsampledepth)
anova(mod.lme.ML.dropMAP, mod.lme.ML.dropsampledepth) # no diff - also drop sample depth

mod.lme.ML.dropclay <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP,
                                     random = ~ 1| siteID, 
                                     data = df, method = "ML")
summary(mod.lme.ML.dropclay)
anova(mod.lme.ML.dropsampledepth, mod.lme.ML.dropclay) # p = 0.08, marginal difference and clay is marginally sign

mod.lme.final <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP, random = ~ 1| siteID, 
                           data = df, method = "REML")
mod.lme.final.MAP <- mod.lme.final
summary(mod.lme.final.MAP) # final fitted model with REML method

#run through full --> drop --> final model with abs(lat)
mod.lme.ML.full <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + abs(lat) + CLAY + 
                               NDEP + MAOM_TOT + sample_depth_cm,
                             random = ~ 1| siteID, 
                             data = df, method = "ML")
summary(mod.lme.ML.full) 

mod.lme.ML.dropclay <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + abs(lat) + NDEP + 
                                   MAOM_TOT + sample_depth_cm, random = ~ 1| siteID, 
                                 data = df, method = "ML")
summary(mod.lme.ML.dropclay)
anova(mod.lme.ML.full, mod.lme.ML.dropclay) # no diff - drop clay

mod.lme.ML.dropMAOM_TOT <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                       abs(lat) + sample_depth_cm, random = ~ 1| siteID, 
                                     data = df, method = "ML") 
summary(mod.lme.ML.dropMAOM_TOT)
anova(mod.lme.ML.dropclay, mod.lme.ML.dropMAOM_TOT) # no diff - also drop MAOM_TOT

mod.lme.ML.dropsampledepth <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                             abs(lat) + sample_depth_cm, random = ~ 1| siteID, 
                           data = df, method = "ML")
summary(mod.lme.ML.dropsampledepth)
anova(mod.lme.ML.dropclay, mod.lme.ML.dropsampledepth) # no diff - drop sample depth

mod.lme.final <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                       abs(lat), random = ~ 1| siteID, 
                                     data = df, method = "REML") 
mod.lme.final.LAT <- mod.lme.final
summary(mod.lme.final.LAT) # final fitted model with REML method

## Run final full model structure for subset dfs
# NEON and non-NEON
mod.lme.NEON <-  nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                             abs(lat), random = ~ 1| siteID, 
                           data = df_neon, method = "REML") 
summary(mod.lme.NEON)

mod.lme.nonNEON <-  nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                             abs(lat), random = ~ 1| siteID, 
                           data = df_non_neon, method = "REML") 
summary(mod.lme.nonNEON)

# Non-Marambaia
mod.lme.nonMARAMBAIA <-  nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                abs(lat), random = ~ 1| siteID, 
                              data = df_non_marambaia, method = "REML") 
summary(mod.lme.nonMARAMBAIA)

# Fresh litter vs forest floor
mod.lme.fresh <-  nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                     abs(lat), random = ~ 1| siteID, 
                                   data = df_fresh, method = "REML") 
summary(mod.lme.fresh)

# Lat binned data
mod.lme.latbin1 <-  nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                              abs(lat), random = ~ 1| siteID, 
                            data = df_latbin1, method = "REML") 
summary(mod.lme.latbin1)

mod.lme.latbin2 <-  nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                abs(lat), random = ~ 1| siteID, 
                              data = df_latbin2, method = "REML") 
summary(mod.lme.latbin2)

mod.lme.latbin3 <-  nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                abs(lat), random = ~ 1| siteID, 
                              data = df_latbin3, method = "REML") 
summary(mod.lme.latbin3)



