### Figure X: Bivariate relationship between litter CN and soil CN (log-log)
ggplot() +
  geom_smooth(data = df, aes(x = log(litterCN), y = log(soilCN)), 
              linetype = "dashed", method = "lm", colour = "grey", se = F) +
  geom_point(data = df, aes(x = log(litterCN), y = log(soilCN), 
                            colour = marambaia, fill = marambaia, shape = fresh_litter_not_floor)) +
  scale_colour_manual(values = c("#0072B2", "#56B4E9")) +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
#  geom_smooth(data = df_non_marambaia, aes(x = log(litterCN), y = log(soilCN), colour = marambaia), method = "lm") +
  scale_shape_manual(values = c(12, 22)) +
  geom_smooth(data = df_non_marambaia, aes(x = log(litterCN), y = log(soilCN), 
                                           linetype = fresh_litter_not_floor), 
              method = "lm", colour = "#0072B2", se = F) +
  geom_smooth(data = df_non_marambaia, aes(x = log(litterCN), y = log(soilCN)),
              linetype = "solid", method = "lm", colour = "black") +
  theme_bw()
ggsave("litter-soil-CN_detailed.png")
  
df_naturalforest <- df_non_marambaia %>% filter(naturalforest_plantation == "naturalforest")
df_plantation <- df_non_marambaia %>% filter(naturalforest_plantation == "plantation")
summary(lm(log(soilCN) ~ log(litterCN), data = df_naturalforest))
summary(lm(log(soilCN) ~ log(litterCN), data = df_plantation))
ggplot() + 
  geom_smooth(data = df, aes(x = log(litterCN), y = log(soilCN)), 
              linetype = "dashed", method = "lm", colour = "grey", se = F) +
  geom_point(data = df, aes(x = log(litterCN), y = log(soilCN), 
                            shape = marambaia, colour = naturalforest_plantation)) +
  scale_shape_manual(values = c(19, 1)) +
  scale_colour_manual(values = c("#5773CCFF", "#FFB900FF")) +
  scale_fill_manual(values = c("#5773CCFF", "#FFB900FF")) +
  geom_smooth(data = df_naturalforest, aes(x = log(litterCN), y = log(soilCN)), 
                                           linetype = "solid", method = "lm", colour = "#5773CCFF") +
  geom_smooth(data = df_plantation, aes(x = log(litterCN), y = log(soilCN)), 
                                           linetype = "solid", method = "lm", colour = "#FFB900FF") +
  theme_bw()
ggsave("litter-soil-CN_naturalforest-plantation.png")  
