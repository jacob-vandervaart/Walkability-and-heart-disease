# Jacob Vandervaart
# 30Apr2024
# Data analysis portion

#install.packages("pacman")
library(pacman)

p_load(tidyverse)
p_load(tableone)
p_load(car)
p_load(gridExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

sdoh_df <- read.csv("Census tract health outcomes/CHD_and_SDOH_trimmed.csv")

glimpse(sdoh_df)
summary.data.frame(sdoh_df)
# lots of variables appeared to be skewed ex. work_no_car, publ_transit, walk_2work
# I am not very concerned with normality because of large sample size


# plot density for all variables and their log transformations
for (var in colnames(sdoh_df[-c(1:5)])) {
  plot1 <- sdoh_df |> ggplot(aes(!!as.name(var)))+
    geom_density()+
    labs(title = var)
  plot2 <-sdoh_df |> ggplot(aes(log(!!as.name(var))))+
    geom_density()+
    labs(title = paste("log",var))
  grid.arrange(plot1, plot2, ncol = 2)
}
#outcome variables  such as CHD and LPA obesity ect. appear to already be normally distributed
# communting variables and other appear to be no normal and would potentially benefit from log transformations
# CLT should make non normal data less of an issue.

# # basic plots of main variable of interest
# sdoh_df |> ggplot(aes(ACS_PCT_WALK_2WORK))+
#   geom_density()
# sdoh_df |> ggplot(aes(ACS_PCT_WALK_2WORK))+
#   geom_boxplot()
# sdoh_df |> ggplot(aes(log(ACS_PCT_WALK_2WORK)))+
#   geom_density()


# sdoh_df |> ggplot(aes(ACS_PCT_HU_NO_VEH))+
#   geom_density()
# sdoh_df |> ggplot(aes(ACS_PCT_HU_NO_VEH))+
#   geom_boxplot()
# sdoh_df |> ggplot(aes(log(ACS_PCT_HU_NO_VEH)))+
#   geom_density()

# data is very non normal, log transformed data visual appears to be normal

# table 1
table_one <- sdoh_df |> select(-c(X,TRACTFIPS,STATE,COUNTY)) %>% CreateTableOne(data = .)

table_one_format <- print(table_one, smd=TRUE, quote=FALSE, missing=TRUE)
table_one_format
write.csv(table_one_format,"Table1.csv")


# function to find correlation with CHD and other outcomes among other variables
cor_outcomes <- function(col){
  df <- c(obesity = cor.test(sdoh_df$OBESITY, col)$estimate,
             lpa = cor.test(sdoh_df$LPA, col)$estimate,
             bphigh = cor.test(sdoh_df$BPHIGH, col)$estimate,
             diabetes = cor.test(sdoh_df$DIABETES, col)$estimate,
             chd = cor.test(sdoh_df$CHD, col)$estimate)
  return(df)
}

# apply to numeric columns
outcomes_correlation <- apply(sdoh_df[-c(1:5)], MARGIN = 2, cor_outcomes)

# convert to a dataframe. 
# needed to transpose rows and columns
outcomes_correlation <- data.frame(rbind(outcomes_correlation)) |> t()

outcomes_correlation <- apply(outcomes_correlation, 2, FUN = function(x) round(x,2))
print(outcomes_correlation)
write.csv(outcomes_correlation, "outcomes corr.csv")

# there is a high correlation between all outcome measures, obesity, lpa, bphigh
# commuting characteristics such as drive to work, public transit and walk to work have moderate correlations


simple_model <- lm(data = sdoh_df, CHD ~ log(ACS_PCT_WALK_2WORK+0.01))
summary(simple_model)
# initially we see a moderate negative correlation. 
# a 10% increase in those walking to work corresponds to a 0.51 % reduction in the percentage of CHD
# move on to the full model to control for demographic characteristics

# create formula for linear model
variable_names <-colnames(sdoh_df)
# removing variables not to be included in model. state, county, region, and outcome measures ex obesity lpa
variable_names <-variable_names[-c(1:5,52:56)] 

formula_chd <- as.formula(paste("CHD ~ ", paste(variable_names, collapse= "+")))
formula_chd

chd_model <- lm(formula_chd, data = sdoh_df)
summary(chd_model)
# only age 50_64 is significant age category
# only per-capita income is significant out of income categories
# should evaluate for co-linearity
# for commuting characteristics not owing a car seems to be more important than alternate modes of transit

vif_values <- vif(chd_model)
vif_values
plot(vif_values)

# there are very large correlations seen in age, black and black non Hispanic, income and commuting variables

# removing highly correlated variables. Keeping only no vehicles and walk to work

 # variable_names2 <- sdoh_df |> 
#   select(ACS_AVG_HH_SIZE:ACS_MEDIAN_RENT,ACS_PCT_HU_NO_VEH,ACS_PCT_WALK_2WORK, ACS_PCT_WORK_NO_CAR) |> 
#   colnames()

# divide by 5 so that the beta is for a 5% point increase
sdoh_df <-  sdoh_df |> mutate(ACS_PCT_HU_NO_VEH5 = ACS_PCT_HU_NO_VEH / 5,
                              ACS_PCT_WALK_2WORK5 = ACS_PCT_WALK_2WORK / 5)

# variable_names2 <- sdoh_df |> 
#   select(ACS_AVG_HH_SIZE:ACS_PCT_FOREIGN_BORN, ACS_PCT_AGE_50_64, ACS_PCT_AIAN:ACS_PCT_BLACK,
#          ACS_PCT_HISPANIC:ACS_GINI_INDEX, ACS_PER_CAPITA_INC:ACS_MEDIAN_RENT,
#          ACS_PCT_HU_NO_VEH5,ACS_PCT_WALK_2WORK5) |> 
#   colnames()

variable_names2 <- sdoh_df |> 
  select(ACS_AVG_HH_SIZE:ACS_MEDIAN_RENT,
         ACS_PCT_HU_NO_VEH5,
         ACS_PCT_WALK_2WORK5) |> 
  colnames()

# after removing commuting co-linear parameters percentage walking to work is shown to be significant
# a 5% point increase in the population walking to work is correlated with a 0.16 point reduction in the percentage of the population with CHD
# HU no vehicles is significant but the magnitude is low 0.01 point increase in CHD for every 5% increase

# not doing this anymore keeping all co linear variables that are no related to commuting
# # no longer see high correlations in our main predictor walk_2work 
# # black and white seem to be correlated
# 
# # repeat but remove ACS_PCT_WHITE
# variable_names2 <- sdoh_df |> 
#   select(ACS_AVG_HH_SIZE:ACS_PCT_FOREIGN_BORN, ACS_PCT_AGE_50_64, ACS_PCT_AIAN:ACS_PCT_BLACK,
#          ACS_PCT_HISPANIC:ACS_GINI_INDEX, ACS_PER_CAPITA_INC:ACS_MEDIAN_RENT,
#          ACS_PCT_HU_NO_VEH5,ACS_PCT_WALK_2WORK5, -ACS_PCT_WHITE) |> 
#   colnames()
# formula_chd2 <- as.formula(paste("CHD ~ ", paste(variable_names2, collapse= "+")))
# formula_chd2
# 
# 
# chd_model2 <- lm(formula_chd2, data = sdoh_df)
# 
# vif_values2 <- vif(chd_model2)
# vif_values2
# plot(vif_values2)



formula_chd2 <- as.formula(paste("CHD ~ ", paste(variable_names2, collapse= "+")))
formula_chd2

chd_model2 <- lm(formula_chd2, data = sdoh_df)

vif_values2 <- vif(chd_model2)
vif_values2
plot(vif_values2)
# issues with co-linearity in commuting variables appears to be resolved. 
# walk to work and no vehicle both have low VIF values

summary(chd_model2)
# After removing other commuting variables walking to work appears to be significant
# The magnitude of the effect is small. 
# A 5% increase in those walking to work is associated with a -0.01% decrease in percentage of population with CHD

plot(chd_model2)
# Q-Q plot shows residuals are not normally distributed. There is some major deviation at the upper quantiles
# a few potential data points with large leverage, although this is not that concerning based on the total number of 

# investigating some specific points
# sdoh_df |> select(X:REGION,all_of(variable_names2),CHD) |> filter(`X` %in% c(159,1616,15148,30169))
# Olmsted County home to mayo clinic has a larger percentage of CHD and percentage disabled population,
# Tulsa county oklahoma has large per capita income, likely a type-o
# Sussex county Deleware has a moderately high CHD but otherwise seems normal
# Honolulu county has a very low educational level and high percentage native island



# data frame with only rows used in  model2
sdoh_drop_na <- sdoh_df |> select(STATE,COUNTY, all_of(variable_names2),CHD) |> na.omit()

# find all values of cooks D greater than 2*sqrt(k/n)
2*sqrt(ncol(sdoh_drop_na)/nrow(sdoh_drop_na))
cooks_d <- cooks.distance(chd_model2)
plot(cooks_d)
outliers <- which(cooks_d > 2*sqrt(ncol(sdoh_drop_na)/nrow(sdoh_drop_na)))
outliers

# find values of high influence
high_influence <- which(influence(chd_model2)$hat > 0.03)
# 0.03 determined visually from plot

sdoh_drop_na[outliers,]
# 30169 has a large per-capita income of 485,000. likely data entry error and should be 48,000
sdoh_drop_na[high_influence,]
# high influence points all have very large pacific islander populations

lm(formula_chd2, data = sdoh_df[-outliers,]) |> summary()
lm(formula_chd2, data = sdoh_df[-high_influence,]) |> summary()
# removing theses values does not significantly impact the model


# 30169 is clearly a data input error so I will manually correct it 
# even though it does not significantly impact model
sdoh_df[30169,]
sdoh_df[30169,'ACS_PER_CAPITA_INC'] <- sdoh_df[30169,'ACS_PER_CAPITA_INC'] / 10
sdoh_df[30169,]

# addressing Q-Q plot 
# creating new model with log transformed predictors

#log transforming 
sdoh_df <- sdoh_df |> mutate(ACS_PCT_WALK_2WORK_log = log(ACS_PCT_WALK_2WORK + 0.01),
                             ACS_PCT_HU_NO_VEH_log = log(ACS_PCT_HU_NO_VEH + 0.01))
# added a small value to avoid taking the log of a zero value

variable_names3 <- sdoh_df |> 
  select(ACS_AVG_HH_SIZE:ACS_MEDIAN_RENT,
         ACS_PCT_HU_NO_VEH_log,
         ACS_PCT_WALK_2WORK_log) |> 
  colnames()
formula_chd3 <- as.formula(paste("CHD ~ ", paste(variable_names3, collapse= "+")))
formula_chd3

chd_model3 <- lm(formula_chd3, data = sdoh_df)
summary(chd_model3)
plot(chd_model3)

# this does not seem to resolve the Q-Q plot skewdness for larger values of CHD
# large outlier 15148

# log transform all variables
sdoh_df_log <- apply(sdoh_df[,-c(1:5,57:60)], MARGIN = 2, FUN =function(x) log(x+0.01)) |> 
  data.frame()

# not log transforming CHD as plots show that it is already normally distributed
variable_names4 <- sdoh_df_log |>
  select(ACS_AVG_HH_SIZE:ACS_MEDIAN_RENT,
         ACS_PCT_HU_NO_VEH,
         ACS_PCT_WALK_2WORK) |>
  colnames()
formula_chd4 <- as.formula(paste("CHD ~ ", paste(variable_names4, collapse= "+")))
formula_chd4
chd_model4 <- lm(formula_chd4, data = sdoh_df_log)
summary(chd_model4)
plot(chd_model4)
# maybe the Q-Q plot is a bit better, hard to say. its better at the higher end but worse in the lower direction
# model is limited in ability to characterize high level CHD cases
# will use model 3 as 4 is not clearly any better

sdoh_df[19792,] # very old, high disabled pct


####################### selecting model 3 as main model
#apply same test of influence

sdoh_drop_na <- sdoh_df |> select(STATE,COUNTY, all_of(variable_names3),CHD) |> na.omit()
# find all values of cooks D greater than 2*sqrt(k/n)
2*sqrt(ncol(sdoh_drop_na)/nrow(sdoh_drop_na))
cooks_d <- cooks.distance(chd_model3)
plot(cooks_d)
outliers <- which(cooks_d > 2*sqrt(ncol(sdoh_drop_na)/nrow(sdoh_drop_na)))
outliers

# find values of high influence
high_influence <- which(influence(chd_model3)$hat > 0.03)
# 0.03 determined visually from plot

sdoh_drop_na[outliers,]
# 3 no outliers based on this cooks D
sdoh_drop_na[high_influence,]
# high influence points all have very large pacific islander populations

lm(formula_chd4, data = sdoh_df_log[-high_influence,]) |> summary()
# again these points do not have a significant influence on the model

summary(chd_model3$residuals)

delta_beta <- influence(chd_model3)$coef
plot(delta_beta)
summary(delta_beta)
# delta betas for no car and walk to work are low in comparison to the betas

outliers_bf <- outlierTest(chd_model3)
outliers_bf
sdoh_df[c(15148,31667,202,4754,27273,38638,29020,35481,28577,3231),]
# 15148 is near mayo clinic. this and other outliers all have very large CHD rates
# other points appear to be mostly low income low education

# testing model with and with out 2 commuting variables
base_variables <- sdoh_df |> 
  select(ACS_AVG_HH_SIZE:ACS_MEDIAN_RENT, ACS_PCT_HU_NO_VEH_log) |> 
  colnames()
base_formula <- as.formula(paste("CHD ~ ", paste(base_variables, collapse= "+")))
base_formula

chd_model_no_commute <- lm(base_formula, data = sdoh_df)
summary(chd_model_no_commute)
#adjusted rsquared of 0.7866
# model 3 adjusted rsquared 0.7867

# lack of effect on adjusted R squared and low magnitude of effect suggest these metrics are not strongly correlated with CHD

##### try other variables
formula_drive <- as.formula(paste("CHD ~ ", paste(base_variables, collapse= "+"), "+ ACS_PCT_DRIVE_2WORK")) 
lm(formula_drive, data = sdoh_df) |> summary()

formula_public <- as.formula(paste("CHD ~ ", paste(base_variables, collapse= "+"), "+ ACS_PCT_PUBL_TRANSIT")) 
lm(formula_public, data = sdoh_df) |> summary()

formula_taxi <- as.formula(paste("CHD ~ ", paste(base_variables, collapse= "+"), "+ ACS_PCT_TAXICAB_2WORK")) 
lm(formula_taxi, data = sdoh_df) |> summary()

formula_commute <- as.formula(paste("CHD ~ ", paste(base_variables, collapse= "+"), "+ log(ACS_PCT_COMMT_15MIN+0.01)")) 
lm(formula_commute, data = sdoh_df) |> summary()
# having a commute less than 15 minutes shows the highest magnitude of effect and biggest increase on adjusted r squared
# commuting times might be more important than mode

 ############# other outcomes
# use same set of hypothesized variables 

formula_obesity <- as.formula(paste("OBESITY ~ ", paste(variable_names3, collapse= "+")))
obesity_model <- lm(formula_obesity, data = sdoh_df)
summary(obesity_model)
plot(obesity_model)

formula_lpa <- as.formula(paste("LPA ~ ", paste(variable_names3, collapse= "+")))
lpa_model <- lm(formula_lpa, data = sdoh_df)
summary(lpa_model)
plot(lpa_model)


formula_bphigh <- as.formula(paste("BPHIGH ~ ", paste(variable_names3, collapse= "+")))
bphigh_model <- lm(formula_bphigh, data = sdoh_df)
summary(bphigh_model)
plot(bphigh_model)
# highest magnutide effect

formula_diabetes <- as.formula(paste("DIABETES ~ ", paste(variable_names3, collapse= "+")))
diabetes_model <- lm(formula_diabetes, data = sdoh_df)
summary(diabetes_model)
plot(diabetes_model)

######################################################

