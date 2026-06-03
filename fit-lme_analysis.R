### Fit linear mixed model for soilC:N ~ litterC:N

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
op <- par(mfrow, c(2,2), mar = c(4,4,3,2)) # set up plot
plot(x = e2, y = f2, xlab = "Fitted Values", ylab = "Residuals") # fitted vs resid plot
plot(e2 ~ log(litterCN), data = df, main = "log Litter CN", ylab = "Residuals") # residuals vs explanatory var

# Step 7/8 - optimal fixed structure
summary(mod.lme) # examine significance of regression parameters
mod.lme.ML <- nlme::lme(log(soilCN) ~ log(litterCN), random = ~ 1| siteID, 
                        data = df, method = "ML")

#run through full --> drop --> final model with MAT
mod.lme.ML.full <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + CLAY + 
                               NDEP + MAOM_TOT,
                             random = ~ 1| siteID, 
                             data = df, method = "ML")
summary(mod.lme.ML.full)

mod.lme.ML.dropclay <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + NDEP + 
                                   MAOM_TOT, random = ~ 1| siteID, 
                                 data = df, method = "ML")
summary(mod.lme.ML.dropclay)
anova(mod.lme.ML.full, mod.lme.ML.dropclay) # no diff - drop clay

mod.lme.ML.dropMAOM_TOT <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + NDEP,
                                     random = ~ 1| siteID, 
                                data = df, method = "ML")
summary(mod.lme.ML.dropMAOM_TOT)
anova(mod.lme.ML.dropclay, mod.lme.ML.dropMAOM_TOT) # no diff - also drop MAOM_TOT

mod.lme.final <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + NDEP,
                           random = ~ 1| siteID, 
                           data = df, method = "REML")
summary(mod.lme.final) # final fitted model with REML method

#run through full --> drop --> final model with MAP
mod.lme.ML.full <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAP + CLAY + 
                               NDEP + MAOM_TOT,
                             random = ~ 1| siteID, 
                             data = df, method = "ML")
summary(mod.lme.ML.full) 

mod.lme.ML.dropclay <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAP + NDEP + 
                                   MAOM_TOT, random = ~ 1| siteID, 
                                 data = df, method = "ML")
summary(mod.lme.ML.dropclay)
anova(mod.lme.ML.full, mod.lme.ML.dropclay) # no diff - drop clay

mod.lme.ML.dropMAP <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                  MAOM_TOT, random = ~ 1| siteID, 
                                data = df, method = "ML") # MAOM_TOT is almost marginally sign. so dropping MAP before MAOM_TOT
summary(mod.lme.ML.dropMAP)
anova(mod.lme.ML.dropclay, mod.lme.ML.dropMAP) # no diff - also drop MAP

mod.lme.ML.dropMAOM_TOT <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP,
                                     random = ~ 1| siteID, 
                                     data = df, method = "ML")
summary(mod.lme.ML.dropMAOM_TOT)
anova(mod.lme.ML.dropMAP, mod.lme.ML.dropMAOM_TOT) # dropping MAOM_TOT increases AIC and p = 0.09 --> keep MAOM_TOT

mod.lme.final <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                            MAOM_TOT, random = ~ 1| siteID, 
                           data = df, method = "REML")
summary(mod.lme.final) # final fitted model with REML method

#run through full --> drop --> final model with abs(lat)
mod.lme.ML.full <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + abs(lat) + CLAY + 
                               NDEP + MAOM_TOT,
                             random = ~ 1| siteID, 
                             data = df, method = "ML")
summary(mod.lme.ML.full) 

mod.lme.ML.dropclay <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + abs(lat) + NDEP + 
                                   MAOM_TOT, random = ~ 1| siteID, 
                                 data = df, method = "ML")
summary(mod.lme.ML.dropclay)
anova(mod.lme.ML.full, mod.lme.ML.dropclay) # no diff - drop clay

mod.lme.ML.dropMAOM_TOT <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                                  abs(lat), random = ~ 1| siteID, 
                                data = df, method = "ML") 
summary(mod.lme.ML.dropMAOM_TOT)
anova(mod.lme.ML.dropclay, mod.lme.ML.dropMAOM_TOT) # no diff - also drop MAOM_TOT

mod.lme.final <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + NDEP + 
                             abs(lat), random = ~ 1| siteID, 
                           data = df, method = "ML") 
summary(mod.lme.final) # final fitted model with REML method

