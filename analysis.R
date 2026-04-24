### Set up, read in data
# load library and provide authorization
packload <- c('tidyverse', 'ggplot2','lme4', "lmerTest", "googlesheets4")
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

### EDA plotting
# create latitude bins 
df$latbins <- cut(abs(df$lat.x), breaks = c(0,10,20,30,40,50,60,90), labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-90"))
df$latbins2 <- cut(abs(df$lat.x), breaks = c(0,23.5,40,60,90), labels = c("0-23.5", "23.5-40", "40-60", "60-90"))

#data exploration
hist(log(df$soilCN))
hist(log(df$litterCN))
hist(df$litterC_pct)
hist(df$litterN_pct)
df_temp <- filter(df, soilCN>60 | soilCN<5)
df_neon <- filter(df, citation == 'NEON')
df_non_neon <- filter(df, citation != 'NEON')
df_non_marambaia <- filter(df, citation_num.x != '1074')

### The main exhibit: Global bivariate relationship between litter and soil C:N
ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  theme_bw()
ggplot(df_neon, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  theme_bw()
ggplot(df_non_neon, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  theme_bw()
ggplot(df_non_marambaia, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  theme_bw()
#highlight influential site
ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point(color = 'red') + geom_smooth(method = "lm") +
  geom_point(data = df_non_marambaia, aes(x = log(litterCN), y = log(soilCN)), color = 'black')+
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')+
  theme_bw()

mod1 <- lm(log(soilCN)~ log(litterCN), data = df)
mod2 <- lm(log(soilCN)~ log(litterCN), data = df_neon)
mod3 <- lm(log(soilCN)~ log(litterCN), data = df_non_neon)
mod4 <- lm(log(soilCN)~ log(litterCN), data = df_non_marambaia)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~latbins) +
  theme_bw()

#Brainstorm: what is the model we want?
m1 <- lmer(soilCN ~ litterCN + MAT + MAP + CLAY + NDEP + MAOM_TOT + (1|citation), data = df)
summary(m1)
