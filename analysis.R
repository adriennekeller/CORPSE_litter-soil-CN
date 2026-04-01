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
ggplot(df, aes(x = log(litterCN), y = log(soilCN))) + geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~latbins)

