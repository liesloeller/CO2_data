######################################

# Author: E. Oeller
# Stats first done June 10, 2021
# Last updated: Oct 14, 2021
# Purpose: For ESA 2021
# Evaluating aphid populations under ambient and elevated CO2 on three host plants
# Evaluating differences in plant nutrition under elevated and ambient CO2 for 3 host plants

######################################


# Looking at:
# How aphid populations differ on 3 plant hosts
# How aphid populations differ in ambient vs elevated CO2 levels
# How total carbohydrate levels differ on 3 plant hosts 
  # (to see if this explains aphid pop differences)
# How total carb levels differ in ambient vs elevated CO2 levels

# Set wd

library(ggplot2)
library(ggpubr)
library(agricolae)
library(tidyverse)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(plotrix)
library(dplyr)

# Read in the file
co2_data <- read.csv("CO2_aphid_pops.csv", sep= ",", header= T)
head(co2_data)
str(co2_data)


# Aphid pop ANOVA host plants test ---------------------------------------------

# Let us visualize this data with a box plot shall we

ggboxplot(co2_data, x= "plant_sp", y= "aphid_count",
          color= "plant_sp", palette= c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab= "Total Aphids", xlab= "Host Plant")

# ANOVA for all aphids in 3 plant treatments, CO2 levels are combined
aphidpop= aov(aphid_count~plant_sp, data= co2_data)
summary(aphidpop)
plot(aphidpop)
HSD.test(y=aphidpop, trt="plant_sp",group= T, console = T)

# I want separate ANOVAs for ambient and elevated CO2, 
# so make into 2 datasets (separated by hand, filter didn't work)


# Ambient Aphid pop -------------------------------------------------------

# We will look at how aphid populations of host plants are 
  # impacted by ambient CO2

#the simple aov method, no p-values

amb_aphid<- read.csv("ambient_aphid.csv")

View(amb_aphid)
amb_aphid_aov= aov(aphid_count~plant_sp, data= amb_aphid)
summary(amb_aphid)
HSD.test(y= amb_aphid_aov, trt="plant_sp", group=T, console=T)

#the glm method that Ben used for my gene data in chpt 2

amb_aphid_glm <- glm(aphid_count ~ plant_sp, data=amb_aphid)
summary(amb_aphid_glm)
Anova(amb_aphid_glm)
# ^ Here we get our p-value, we can see there are sig differences bet host plant treatments
amb_aphid_em <- emmeans(amb_aphid_glm, ~ plant_sp, type= "response", adjust = "none")
amb_aphid_em
# ^ Here we see the means for each plant treatment
cld(emmeans(amb_aphid_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the groupings for each plant treatment based on means
pairs(emmeans(amb_aphid_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the pairings and their p-values

# We see that foxtail barley and winter wheat have significantly higher
  # aphid populations than green foxtail.
# These values match what Xi sent me.

# Let's try to make a bar chart for the aphid populations of each host plant under ambient CO2
# First with Ben we will make a dataframe with the summary so we can take the average and error bars

amb_aph_summary <- amb_aphid %>% 
  group_by(plant_sp) %>% 
  summarize(mean_aph = mean(aphid_count),
            se_aph = std.error(aphid_count))

ggplot(amb_aph_summary, aes(x= plant_sp, y= mean_aph, fill= plant_sp))+
  geom_col()+
  labs(x="", y= "Avg aphids per plant", title= "Ambient CO2")+
  scale_x_discrete(labels = c("foxtail barley" = "Foxtail Barley", "green foxtail" = "Green Foxtail",
                              "winter wheat"= "Winter Wheat"))+
  geom_errorbar(data = amb_aph_summary,
                aes(x = plant_sp,
                    ymin = mean_aph - se_aph,
                    ymax = mean_aph + se_aph), width= 0.2)+
  scale_fill_manual(values= c("skyblue", "royalblue", "navy"))+
  theme_classic()+
  theme(legend.position= "none", axis.text.x= element_text(size= 12), 
        axis.text.y= element_text(size= 12),
        axis.title.y= element_text(size= 15))

 # Elevated Aphid pops -----------------------------------------------------

# We will look at how aphid populations of host plants are 
  # impacted by elevated CO2

# Getting straight into glm method

elev_aphid<- read.csv("elevated_aphid.csv")
View(elev_aphid)

elev_aphid_glm <- glm(aphid_count ~ plant_sp, data=elev_aphid)
summary(elev_aphid_glm)
Anova(elev_aphid_glm)
# ^ Here we get our p-value, we can see there are sig differences bet host plant treatments
elev_aphid_em <- emmeans(elev_aphid_glm, ~ plant_sp, type= "response", adjust = "none")
elev_aphid_em
# ^ Here we see the means for each plant treatment
cld(emmeans(elev_aphid_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the groupings for each plant treatment based on means
pairs(emmeans(elev_aphid_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the pairings and their p-values

# We see that foxtail barley and winter wheat have significantly higher
  # aphid populations than green foxtail, same as with ambient CO2.
# These values match what Xi sent me!!!

# Bar chart for aphid populations under elevated CO2 conditions

elev_aphid_summary <- elev_aphid %>% 
  group_by(plant_sp) %>% 
  summarize(mean_aph = mean(aphid_count),
            se_aph = std.error(aphid_count))

ggplot(elev_aphid_summary, aes(x= plant_sp, y= mean_aph, fill= plant_sp))+
  geom_col()+
  labs(x="", y= "Avg aphids per plant", title= "Elevated CO2")+
  scale_x_discrete(labels = c("foxtail barley" = "Foxtail Barley", "green foxtail" = "Green Foxtail",
                              "winter wheat"= "Winter Wheat"))+
  geom_errorbar(data = elev_aphid_summary,
                aes(x = plant_sp,
                    ymin = mean_aph - se_aph,
                    ymax = mean_aph + se_aph), width= 0.2)+
  scale_fill_manual(values= c("yellow4", "chartreuse3", "green4"))+
  theme_classic()+
  theme(legend.position= "none", axis.text.x= element_text(size= 12), 
        axis.text.y= element_text(size= 12),
        axis.title.y= element_text(size= 15))

# CO2 on Aphid Pop --------------------------------------------------------

# Was there a difference in aphid population between ambient and elevated CO2?

CO2_aphid<- read.csv("CO2_aphid_pops.csv")
View(CO2_aphid)
CO2_aphid_glm <- glm(aphid_count ~ CO2, data=CO2_aphid)
summary(CO2_aphid_glm)
Anova(CO2_aphid_glm)
# ^ Here we get our p-value, we can see there is a sig diff bet CO2 treatments
CO2_aphid_em <- emmeans(CO2_aphid_glm, ~ CO2, type= "response", adjust = "none")
CO2_aphid_em
# ^ Here we see the means for each CO2 treatment

# Plants in elevated CO2 conditions had sig higher aphid pops than plants in ambient CO2.

# Bar chart for aphid populations under elevated vs ambient CO2

CO2_aphid_summary <- CO2_aphid %>% 
  group_by(CO2) %>% 
  summarize(mean_aph = mean(aphid_count),
            se_aph = std.error(aphid_count))

ggplot(CO2_aphid_summary, aes(x= CO2, y= mean_aph, fill= CO2))+
  geom_col()+
  labs(x="", y= "Avg aphids per plant", title= "Ambient vs Elevated CO2")+
  scale_x_discrete(labels = c("Elevated" = "Elevated CO2", "Ambient" = "Ambient CO2"))+
  geom_errorbar(data = CO2_aphid_summary,
                aes(x = CO2,
                    ymin = mean_aph - se_aph,
                    ymax = mean_aph + se_aph), width= 0.2)+
  scale_fill_manual(values= c("darkcyan", "goldenrod3"))+
  theme_classic()+
  theme(legend.position= "none", axis.text.x= element_text(size= 12), 
        axis.text.y= element_text(size= 12),
        axis.title.y= element_text(size= 15))

# Ambient Carbs -----------------------------------------------------------

# Now we will look at how total carbohydrate levels of host plants are 
  # impacted by ambient CO2

amb_carb<- read.csv("ambient_carbs.csv")
View(amb_carb)

amb_carb_glm <- glm(total_carbs ~ plant_sp, data=amb_carb)
summary(amb_carb_glm)
Anova(amb_carb_glm)
# ^ Here we get our p-value, we can see there are sig differences bet host plant treatments
amb_carb_em <- emmeans(amb_carb_glm, ~ plant_sp, type= "response", adjust = "none")
amb_carb_em
# ^ Here we see the means for each plant treatment
cld(emmeans(amb_carb_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the groupings for each plant treatment based on means
pairs(emmeans(amb_carb_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the pairings and their p-values

# We see that winter wheat has the highest total carbs, then green foxtail, 
  # then foxtail barley. Green foxtail and winter wheat are not sig diff.

# Bar chart

amb_carb_summary <- amb_carb %>% 
  group_by(plant_sp) %>% 
  summarize(mean_carb = mean(total_carbs),
            se_carb = std.error(total_carbs))

ggplot(amb_carb_summary, aes(x= plant_sp, y= mean_carb, fill= plant_sp))+
  geom_col()+
  labs(x="", y= "Avg total sugars per plant", title= "Ambient CO2")+
  scale_x_discrete(labels = c("foxtail barley" = "Foxtail Barley", "green foxtail" = "Green Foxtail",
                              "winter wheat"= "Winter Wheat"))+
  geom_errorbar(data = amb_carb_summary,
                aes(x = plant_sp,
                    ymin = mean_carb - se_carb,
                    ymax = mean_carb + se_carb), width= 0.2)+
  scale_fill_manual(values= c("skyblue", "royalblue", "navy"))+
  theme_classic()+
  theme(legend.position= "none", axis.text.x= element_text(size= 12), 
        axis.text.y= element_text(size= 12),
        axis.title.y= element_text(size= 15))

# Elevated Carbs ----------------------------------------------------------

elev_carb<- read.csv("elevated_carbs.csv")
View(elev_carb)

elev_carb_glm <- glm(total_carbs ~ plant_sp, data=elev_carb)
summary(elev_carb_glm)
Anova(elev_carb_glm)
# ^ Here we get our p-value, we can see there are sig differences bet host plant treatments
elev_carb_em <- emmeans(elev_carb_glm, ~ plant_sp, type= "response", adjust = "none")
elev_carb_em
# ^ Here we see the means for each plant treatment
cld(emmeans(elev_carb_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the groupings for each plant treatment based on means
pairs(emmeans(elev_carb_glm, ~ plant_sp, type= "response", adjust = "none"))
# ^ Here we see the pairings and their p-values

# We see that winter wheat has the highest total carbs, then green foxtail, 
  # then foxtail barley. Green foxtail and winter wheat are not sig diff.

# Bar chart

elev_carb_summary <- elev_carb %>% 
  group_by(plant_sp) %>% 
  summarize(mean_carb = mean(total_carbs),
            se_carb = std.error(total_carbs))

ggplot(elev_carb_summary, aes(x= plant_sp, y= mean_carb, fill= plant_sp))+
  geom_col()+
  labs(x="", y= "Avg total sugars per plant", title= "Elevated CO2")+
  scale_x_discrete(labels = c("foxtail barley" = "Foxtail Barley", "green foxtail" = "Green Foxtail",
                              "winter wheat"= "Winter Wheat"))+
  geom_errorbar(data = elev_carb_summary,
                aes(x = plant_sp,
                    ymin = mean_carb - se_carb,
                    ymax = mean_carb + se_carb), width= 0.2)+
  scale_fill_manual(values= c("yellow4", "chartreuse3", "green4"))+
  theme_classic()+
  theme(legend.position= "none", axis.text.x= element_text(size= 12), 
        axis.text.y= element_text(size= 12),
        axis.title.y= element_text(size= 15))



# CO2 on Carbs ------------------------------------------------------------

# Was there a difference in total carbohydrates between ambient and elevated CO2?
CO2_carbs<- read.csv("CO2_carbs.csv")
View(CO2_carbs)
CO2_carbs_glm <- glm(total_carbs ~ CO2, data=CO2_carbs)
summary(CO2_carbs_glm)
Anova(CO2_carbs_glm)
# ^ Here we get our p-value, we can see there is NO sig diff bet CO2 treatments
CO2_carbs_em <- emmeans(CO2_carbs_glm, ~ CO2, type= "response", adjust = "none")
CO2_carbs_em
# ^ Here we see the means for each CO2 treatment

# Plants in elevated CO2 conditions have the same total carbohydrates as plants in ambient CO2.

CO2_carb_summary <- CO2_carbs %>% 
  group_by(CO2) %>% 
  summarize(mean_carb = mean(total_carbs),
            se_carb = std.error(total_carbs))

ggplot(CO2_carb_summary, aes(x= CO2, y= mean_carb, fill= CO2))+
  geom_col()+
  labs(x="", y= "Avg total sugars per plant", title= "Ambient vs Elevated CO2")+
  scale_x_discrete(labels = c("Elevated" = "Elevated CO2", "Ambient" = "Ambient CO2"))+
  geom_errorbar(data = CO2_carb_summary,
                aes(x = CO2,
                    ymin = mean_carb - se_carb,
                    ymax = mean_carb + se_carb), width= 0.2)+
  scale_fill_manual(values= c("darkcyan", "goldenrod3"))+
  theme_classic()+
  theme(legend.position= "none", axis.text.x= element_text(size= 12), 
        axis.text.y= element_text(size= 12),
        axis.title.y= element_text(size= 15))



# Aphid pop/Carbs regression ----------------------------------------------

# Here we will determine if total carbohydrates is a good predictor of aphid populations

aphid_carb_reg<- read.csv("CO2_carbs.csv")
View(aphid_carb_reg)

aphid_carb_lm<- lm(aphid_count~total_carbs, data= aphid_carb_reg)
summary(aphid_carb_lm)
# P= 0.02, but R^2 values are very low. 
  # "This combination indicates that the independent variables are correlated 
  # with the dependent variable, but they do not explain much of the variability
  # in the dependent variable."
# Now plot the residuals to see if significant?
residuals= resid(aphid_carb_lm)
plot(aphid_carb_reg$total_carbs, residuals)
# Residuals look evenly/randomly distributed
# We can say that total carbs is a good predictor of aphid populations?

hist(aphid_carb_lm$residuals)

# Now we plot aphid pop/carb regression

ggplot(aphid_carb_reg, aes(x= total_carbs, y= aphid_count))+
  geom_point()+
  stat_smooth(method= "lm", formula= y~x, color= "black")+
  labs(x= "Avg total sugars per plant", y= "Avg aphids per plant")+
  theme_classic()+
  theme(axis.title.x= element_text(size= 13),
        axis.title.y= element_text(size= 13))

# CO2 PCR------------------------------------------------------------------

# Read in the file
co2_pcr <- read.csv("CO2_PCR_stats.csv", sep= ",", header= T)
View(co2_pcr)
str(co2_pcr)

co2_pcr_glm <- glm(Mean_Cq ~ CO2, data=co2_pcr)
summary(co2_pcr_glm)
Anova(co2_pcr_glm)
# ^ Here we get our p-value, we can see there are sig differences bet host plant treatments
co2_pcr_em <- emmeans(co2_pcr_glm, ~ CO2, type= "response", adjust = "none")
co2_pcr_em

# No difference in viral titer between ambient and elevated, p= 0.18

