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

# Step 3: Choose variance structure (add in random effects)
# Step 4: fit model
mod1.lme <- nlme::lme(log(soilCN) ~ log(litterCN), random = ~1 | siteID, data = df,
                      method = "REML")
summary(mod1.lme)

# Step 5: compare gls and lme models - does lme provide better fit?
anova(mod.gls, mod1.lme)

# Step 6: assess homogeneity of variance and independence
e2 <- resid(mod1.lme, type = "normalized") # residuals from mod
f2 <- fitted(mod1.lme) # fitted values from model
op <- par(mfrow, c(2,2), mar = c(4,4,3,2)) # set up plot
plot(x = e2, y = f2, xlab = "Fitted Values", ylab = "Residuals") # fitted vs resid plot
plot(e2 ~ log(litterCN), data = df, main = "log Litter CN", ylab = "Residuals") # residuals vs explanatory var

# Step 7/8 - optimal fixed structure
summary(mod1.lme) # examine significance of regression parameters
mod.lme.ML <- nlme::lme(log(soilCN) ~ log(litterCN), random = ~ 1| siteID, data = df,
                        method = "ML")
mod.lme.ML.full <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + CLAY + NDEP 
                             + MAOM_TOT + lat,
                             random = ~ 1| siteID, data = df,
                             method = "ML")
summary(mod.lme.ML.full)
mod.lme.ML.dropclay <- nlme::lme(log(soilCN) ~ log(litterCN) + pH + MAT + NDEP + MAOM_TOT + lat, random = ~ 1| siteID, data = df,
                                 method = "ML")
summary(mod.lme.ML.dropclay)
anova(mod.lme.ML.full, mod.lme.ML.dropclay) # no diff - drop clay




