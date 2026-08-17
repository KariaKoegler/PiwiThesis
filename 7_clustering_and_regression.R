# 1. Admin #####################################################################
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(stargazer)
library(tidyr)
library(stats)

#so far each column shows the mean of the last 10 years of each year. I now
#want to convert those columns so that only RowMeans is the column and it is
#fitting to the right year
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2013, merged_data_all_haildays_2cm$RowMeans2013, NA)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2015, merged_data_all_haildays_2cm$RowMeans2015, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2017, merged_data_all_haildays_2cm$RowMeans2017, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2018, merged_data_all_haildays_2cm$RowMeans2018, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2019, merged_data_all_haildays_2cm$RowMeans2019, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2020, merged_data_all_haildays_2cm$RowMeans2020, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2021, merged_data_all_haildays_2cm$RowMeans2021, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2022, merged_data_all_haildays_2cm$RowMeans2022, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2023, merged_data_all_haildays_2cm$RowMeans2023, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2024, merged_data_all_haildays_2cm$RowMeans2024, merged_data_all_haildays_2cm$RowMeans)
merged_data_all_haildays_2cm$RowMeans <- ifelse(merged_data_all_haildays_2cm$year == 2025, merged_data_all_haildays_2cm$RowMeans2025, merged_data_all_haildays_2cm$RowMeans)

merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[, -c(41:51)]

merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2013, merged_data_all_haildays_2cm$sumOfHaildays2013, NA)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2015, merged_data_all_haildays_2cm$sumOfHaildays2015, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2017, merged_data_all_haildays_2cm$sumOfHaildays2017, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2018, merged_data_all_haildays_2cm$sumOfHaildays2018, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2019, merged_data_all_haildays_2cm$sumOfHaildays2019, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2020, merged_data_all_haildays_2cm$sumOfHaildays2020, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2021, merged_data_all_haildays_2cm$sumOfHaildays2021, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2022, merged_data_all_haildays_2cm$sumOfHaildays2022, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2023, merged_data_all_haildays_2cm$sumOfHaildays2023, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2024, merged_data_all_haildays_2cm$sumOfHaildays2024, merged_data_all_haildays_2cm$sumOfHaildays)
merged_data_all_haildays_2cm$sumOfHaildays <- ifelse(merged_data_all_haildays_2cm$year == 2025, merged_data_all_haildays_2cm$sumOfHaildays2025, merged_data_all_haildays_2cm$sumOfHaildays)

merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[, -c(41:51)]

#Same for hailsize
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2013, merged_data_all_hailsize$RowMeans2013, NA)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2015, merged_data_all_hailsize$RowMeans2015, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2017, merged_data_all_hailsize$RowMeans2017, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2018, merged_data_all_hailsize$RowMeans2018, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2019, merged_data_all_hailsize$RowMeans2019, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2020, merged_data_all_hailsize$RowMeans2020, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2021, merged_data_all_hailsize$RowMeans2021, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2022, merged_data_all_hailsize$RowMeans2022, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2023, merged_data_all_hailsize$RowMeans2023, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2024, merged_data_all_hailsize$RowMeans2024, merged_data_all_hailsize$RowMeans)
merged_data_all_hailsize$RowMeans <- ifelse(merged_data_all_hailsize$year == 2025, merged_data_all_hailsize$RowMeans2025, merged_data_all_hailsize$RowMeans)

merged_data_all_hailsize <- merged_data_all_hailsize[, -c(41:51)]

merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2013, merged_data_all_hailsize$Hagelgross2013, NA)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2015, merged_data_all_hailsize$Hagelgross2015, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2017, merged_data_all_hailsize$Hagelgross2017, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2018, merged_data_all_hailsize$Hagelgross2018, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2019, merged_data_all_hailsize$Hagelgross2019, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2020, merged_data_all_hailsize$Hagelgross2020, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2021, merged_data_all_hailsize$Hagelgross2021, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2022, merged_data_all_hailsize$Hagelgross2022, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2023, merged_data_all_hailsize$Hagelgross2023, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2024, merged_data_all_hailsize$Hagelgross2024, merged_data_all_hailsize$Hagelgross)
merged_data_all_hailsize$Hagelgross <- ifelse(merged_data_all_hailsize$year == 2025, merged_data_all_hailsize$Hagelgross2025, merged_data_all_hailsize$Hagelgross)

merged_data_all_hailsize <- merged_data_all_hailsize[, -c(41:51)]

#Fixed effects as factors
merged_data_all_haildays_2cm$Betrieb <- as.factor(merged_data_all_haildays_2cm$Betrieb)
merged_data_all_haildays_2cm$plantation_year <- as.factor(merged_data_all_haildays_2cm$plantation_year)
merged_data_all_hailsize$Betrieb <- as.factor(merged_data_all_hailsize$Betrieb)
merged_data_all_hailsize$plantation_year <- as.factor(merged_data_all_hailsize$plantation_year)

#delete NAs in Betrieb
merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[!is.na(merged_data_all_haildays_2cm$Betrieb),]
merged_data_all_hailsize <- merged_data_all_hailsize[!is.na(merged_data_all_hailsize$Betrieb),]

#rename variables for better understanding
colnames(merged_data_all_haildays_2cm)[colnames(merged_data_all_haildays_2cm) == 'RowMeans'] <- 'MeanHaildays'
colnames(merged_data_all_hailsize)[colnames(merged_data_all_hailsize) == 'RowMeans'] <- 'MeanHailsize'
colnames(merged_data_all_hailsize)[colnames(merged_data_all_hailsize) == 'Hagelgross'] <- 'HailLargerthan2cm'

summary(lm(lag(Weinmerkmal,1) ~ MeanHaildays, data = merged_data_all_haildays_2cm))
confint(lm(lag(Weinmerkmal,1) ~ MeanHaildays, data = merged_data_all_haildays_2cm)) 
#the confidence intervals of the coefficients are close together --> good

#2. Run rergession ##############################################################
models <- list(
  m1 <- lm(lag(Weinmerkmal,1) ~ MeanHaildays, data = merged_data_all_haildays_2cm), #ohne FE
  m2 <- lm(lag(Weinmerkmal,1) ~ MeanHaildays + Betrieb + plantation_year, data = merged_data_all_haildays_2cm), #mit FE
  m3 <- lm(lag(Weinmerkmal,1) ~ MeanHailsize , data = merged_data_all_hailsize), #ohne FE
  m4 <- lm(lag(Weinmerkmal,1) ~ MeanHailsize + plantation_year + Betrieb, data = merged_data_all_hailsize), #mit FE 
  m5 <- lm(lag(Weinmerkmal,1) ~ MeanHailsize + HailLargerthan2cm + plantation_year + Betrieb, data = merged_data_all_hailsize) #mit FE und Hagelgross
)


#Clustering on farm level
clwb <- list(
  se1 <- vcovCL(models[[1]], cluster = ~ Betrieb),
  se2 <- vcovCL(models[[2]], cluster = ~ Betrieb),
  se3 <- vcovCL(models[[3]], cluster = ~ Betrieb),
  se4 <- vcovCL(models[[4]], cluster = ~ Betrieb),
  se5 <- vcovCL(models[[5]], cluster = ~ Betrieb)
)


#testing the SE on significance
ct <- list(
  ct1 <- coeftest(models[[1]], vcov = clwb[[1]])[, 2],
  ct2 <- coeftest(models[[2]], vcov = clwb[[2]])[, 2],
  ct3 <- coeftest(models[[3]], vcov = clwb[[3]])[, 2],
  ct4 <- coeftest(models[[4]], vcov = clwb[[4]])[, 2],
  ct5 <- coeftest(models[[5]], vcov = clwb[[5]])[, 2]
)

## Export as word
stargazer(c(models),
          
          se = c(ct),
          
          dep.var.labels = c("Uptake of PIWI variety"),
          
          omit.stat = c("LL","ser","f"),
          
          no.space = FALSE,
          
          align = TRUE,
          
          omit = c('[C][o][n][s][t][a][n][t]','[B][e][t][r][i][e][b]',
                   '[p][l][a][n][t][a][t][i][o][n]'),
          
          add.lines=list(c('Time fixed effects','No', 'Yes', 'No', 'Yes', 'Yes'),
                         
                         c('Farm fixed effects', 'No', 'Yes', 'No', 'Yes', 'Yes')),
          
          notes.align = "l",
          
          style = "qje",
          
          digits = 2,
          
          single.row = TRUE,
          
          notes.append = TRUE,
          
          column.sep.width = "-25pt",
          
          notes = "",
          
          type='html',
          
          font.size = "tiny",
          
          out="Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/3_output/timelag1Year.doc")


#Untersuchung der statistischen Power der Analyse
library(pwr)

# Post-hoc: find power given n, alpha, effect size
result <- pwr.t.test(
  d = 0.01, #observed effect size
  n = 37181,
  sig.level = 0.05,
  type = "two.sample"
)
cat("Achieved power:", round(result$power, 6))
# Achieved power: 1 for the 1. model, for the others is 0.28 --> 0.8 is necessary


#timelag 2 years
models <- list(
  m1 <- lm(lag(Weinmerkmal,2) ~ MeanHaildays, data = merged_data_all_haildays_2cm), #ohne FE
  m2 <- lm(lag(Weinmerkmal,2) ~ MeanHaildays + Betrieb + plantation_year, data = merged_data_all_haildays_2cm), #mit FE
  m3 <- lm(lag(Weinmerkmal,2) ~ MeanHailsize , data = merged_data_all_hailsize), #ohne FE
  m4 <- lm(lag(Weinmerkmal,2) ~ MeanHailsize + plantation_year + Betrieb, data = merged_data_all_hailsize), #mit FE 
  m5 <- lm(lag(Weinmerkmal,2) ~ MeanHailsize + HailLargerthan2cm + plantation_year + Betrieb, data = merged_data_all_hailsize) #mit FE und Hagelgross
)

# Cluster on farm level
clwb <- list(
  se1 <- vcovCL(models[[1]], cluster = ~ Betrieb),
  se2 <- vcovCL(models[[2]], cluster = ~ Betrieb),
  se3 <- vcovCL(models[[3]], cluster = ~ Betrieb),
  se4 <- vcovCL(models[[4]], cluster = ~ Betrieb),
  se5 <- vcovCL(models[[5]], cluster = ~ Betrieb)
)

# Test SE on significance
ct <- list(
  ct1 <- coeftest(models[[1]], vcov = clwb[[1]])[, 2],
  ct2 <- coeftest(models[[2]], vcov = clwb[[2]])[, 2],
  ct3 <- coeftest(models[[3]], vcov = clwb[[3]])[, 2],
  ct4 <- coeftest(models[[4]], vcov = clwb[[4]])[, 2],
  ct5 <- coeftest(models[[5]], vcov = clwb[[5]])[, 2]
)

## Export as word
stargazer(c(models),
          
          se = c(ct),
          
          dep.var.labels = c("Uptake of PIWI variety"),
          
          omit.stat = c("LL","ser","f"),
          
          no.space = FALSE,
          
          align = TRUE,
          
          omit = c('[C][o][n][s][t][a][n][t]','[B][e][t][r][i][e][b]',
                   '[p][l][a][n][t][a][t][i][o][n]'),
          
          add.lines=list(c('Time fixed effects','No', 'Yes', 'No', 'Yes', 'Yes'),
                         
                         c('Farm fixed effects', 'No', 'Yes', 'No', 'Yes', 'Yes')),
          
          notes.align = "l",
          
          style = "qje",
          
          digits = 2,
          
          single.row = TRUE,
          
          notes.append = TRUE,
          
          column.sep.width = "-25pt",
          
          notes = "",
          
          type='html',
          
          font.size = "tiny",
          
          out="Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/3_output/timelag2Years.doc")
