# 1. Admin #####################################################################
#install.packages("multiwayvcov")
#install.packages("lmtest")
#install.packages("stargazer")
#install.packages("stats")
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(stargazer)
library(tidyr)
library(stats)

#delete NAs in Betrieb
merged_data_all_haildays_2cm <- merged_data_all_haildays_2cm[!is.na(merged_data_all_haildays_2cm$Betrieb),]
merged_data_all_hailsize <- merged_data_all_hailsize[!is.na(merged_data_all_hailsize$Betrieb),]
merged_data_all_haildays <- merged_data_all_haildays[!is.na(merged_data_all_haildays$Betrieb),]


#2. Run rergession ##############################################################
models <- list(
  m1 <- lm(Weinmerkmal ~ RowMens, data = merged_data_all_haildays_2cm),
  m2 <- lm(Weinmerkmal ~ RowMens + Betrieb + plantation_year, data = merged_data_all_haildays_2cm),
  m3 <- lm(Weinmerkmal ~ RowMens , data = merged_data_all_hailsize),
  m4 <- lm(Weinmerkmal ~ RowMens + plantation_year + Betrieb, data = merged_data_all_hailsize),
  m5 <- lm(Weinmerkmal ~ RowMens, data = merged_data_all_haildays),
  m6 <- lm(Weinmerkmal ~ RowMens + plantation_year + Betrieb, data = merged_data_all_haildays)
)

# Here wird nach der Variable id (Betriebsid geclustert)

clwb <- list(
  se1 <- vcovCL(models[[1]], cluster = ~ Betrieb),
  se2 <- vcovCL(models[[2]], cluster = ~ Betrieb),
  se3 <- vcovCL(models[[3]], cluster = ~ Betrieb),
  se4 <- vcovCL(models[[4]], cluster = ~ Betrieb),
  se5 <- vcovCL(models[[5]], cluster = ~ Betrieb),
  se6 <- vcovCL(models[[6]], cluster = ~ Betrieb)
)

# Und dann der SE auf signifikanz getestet
ct <- list(
  ct1 <- coeftest(models[[1]], vcov = clwb[[1]])[, 2],
  ct2 <- coeftest(models[[2]], vcov = clwb[[2]])[, 2],
  ct1 <- coeftest(models[[3]], vcov = clwb[[3]])[, 2],
  ct1 <- coeftest(models[[4]], vcov = clwb[[4]])[, 2],
  ct1 <- coeftest(models[[5]], vcov = clwb[[5]])[, 2],
  ct1 <- coeftest(models[[6]], vcov = clwb[[6]])[, 2]
)

## Export as word
stargazer(c(models),
          
          se = c(ct),
          
          dep.var.labels = c("Uptake of PIWI variety"),
          
          omit.stat = c("LL","ser","f"),
          
          no.space = FALSE,
          
          align = TRUE,
          
          omit = c('[C][o][n][s][t][a][n][t]','[B][e][t][r][i][e][b]'),
          
          add.lines=list(c('Time fixed effects','No', 'Yes', 'No', 'Yes', 'No', 'Yes'),
                         
                         c('Farm fixed effects', 'No', 'Yes', 'No', 'Yes', 'No', 'Yes')),
          
          notes.align = "l",
          
          style = "qje",
          
          digits = 2,
          
          single.row = TRUE,
          
          notes.append = TRUE,
          
          column.sep.width = "-25pt",
          
          notes = "",
          
          type='html',
          
          font.size = "tiny",
          
          out="Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/3_output/test.doc")


