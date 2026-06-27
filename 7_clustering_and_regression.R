# 1. Admin #####################################################################
#install.packages("multiwayvcov")
#install.packages("lmtest")
#install.packages("stargazer")
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(stargazer)

View(merged_data_all_haildays)
View(merged_data_all_hailsize)

#2. Preparation ################################################################
merged_data_all_haildays$Weinmerkmal <- ifelse(merged_data_all_haildays$Weinmerkmal == "interspezifisch", 1,
                         ifelse(merged_data_all_haildays$Weinmerkmal == "europäisch", 0, NA))

merged_data_all_hailsize$Weinmerkmal <- ifelse(merged_data_all_hailsize$Weinmerkmal == "interspezifisch", 1,
                                               ifelse(merged_data_all_hailsize$Weinmerkmal == "europäisch", 0, NA))

#Betrieb as factor
merged_data_all_hailsize$Betrieb <- as.factor(merged_data_all_hailsize$Betrieb)
merged_data_all_haildays$Betrieb <- as.factor(merged_data_all_haildays$Betrieb)

#delete entries where reben = 0
merged_data_all_haildays = merged_data_all_haildays[merged_data_all_haildays$reben > 0, ]
#remove column reben
merged_data_all_haildays <- merged_data_all_haildays[,-c(10)]

merged_data_all_hailsize = merged_data_all_hailsize[merged_data_all_hailsize$reben > 0, ]
merged_data_all_hailsize <- merged_data_all_hailsize[,-c(10)]
dim(merged_data_all_haildays)

#delete Nas
merged_data_all_haildays <- merged_data_all_haildays[!is.na(merged_data_all_haildays$sumOfHaildays_2cm),]

#there are no datapoints with haildays left, so it never hailed larger than 2 cm
#in Zurich during the time frame

#test: Other variables work
summary(lm(Weinmerkmal ~ plantation_year, data = merged_data_all_haildays)) #the later it was planted,
#the more piwi -> time fixed effect
summary(lm(Weinmerkmal ~ year, data = merged_data_all_haildays))
summary(lm(Weinmerkmal ~ sumOfHaildays_2cm, data = merged_data_all_haildays))

view(merged_data_all_hailsize)

summary(lm(Weinmerkmal ~ meanHailsize + Hagelgross, data = merged_data_all_hailsize))
summary(lm(Weinmerkmal ~ Hagelgross + plantation_year + Betrieb, data = merged_data_all_hailsize))
summary(lm(Weinmerkmal ~ meanHailsize + Hagelgross + Betrieb + plantation_year, data = merged_data_all_hailsize))

merged_data_all_hailsize$plantation_year

# Run rergession
models <- list(
  #m1 <- lm(Weinmerkmal ~ sumOfHaildays_2cm, data = merged_data_all_haildays),
  
  m2 <- lm(Weinmerkmal ~ meanHailsize + Hagelgross, data = merged_data_all_hailsize),
  m3 <- lm(Weinmerkmal ~ Hagelgross + plantation_year, data = merged_data_all_hailsize),
  m4 <- lm(Weinmerkmal ~ Hagelgross + Betrieb, data = merged_data_all_hailsize),
  m5 <- lm(Weinmerkmal ~ Hagelgross + Betrieb + plantation_year, data = merged_data_all_hailsize),
  m6 <- lm(Weinmerkmal ~ meanHailsize + Betrieb + plantation_year, data = merged_data_all_hailsize)
)

# Here wird nach der Variable id (Betriebsid geclustert)

clwb <- list(
  #se1 <- vcovCL(models[[1]], cluster = ~ Betrieb),
  se2 <- vcovCL(models[[1]], cluster = ~ Betrieb),
  se3 <- vcovCL(models[[2]], cluster = ~ Betrieb),
  se4 <- vcovCL(models[[3]], cluster = ~ Betrieb),
  se5 <- vcovCL(models[[4]], cluster = ~ Betrieb),
  se6 <- vcovCL(models[[5]], cluster = ~ Betrieb)
)

# Und dann der SE auf signifikanz getestet
ct <- list(
  
  #ct1 <- coeftest(models[[1]], vcov = clwb[[1]])[, 2],
  ct2 <- coeftest(models[[1]], vcov = clwb[[1]])[, 2],
  ct3 <- coeftest(models[[2]], vcov = clwb[[2]])[, 2],
  ct4 <- coeftest(models[[3]], vcov = clwb[[3]])[, 2],
  ct5 <- coeftest(models[[4]], vcov = clwb[[4]])[, 2],
  ct6 <- coeftest(models[[5]], vcov = clwb[[5]])[, 2]
  
)

## Export as word
stargazer(c(models),
          
          se = c(ct),
          
          dep.var.labels = c("Uptake of PIWI variety"),
          
          omit.stat = c("LL","ser","f"),
          
          no.space = FALSE,
          
          align = TRUE,
          
          omit = c('[C][o][n][s][t][a][n][t]','[B][e][t][r][i][e][b]'),
          
          add.lines=list(c('Time fixed effects','No','Yes', 'No', 'Yes', 'Yes'),
                         
                         c('Farm fixed effects','No','No', 'Yes', 'Yes', 'Yes')),
          
          notes.align = "l",
          
          style = "qje",
          
          digits = 2,
          
          single.row = TRUE,
          
          notes.append = TRUE,
          
          column.sep.width = "-25pt",
          
          notes = "",
          
          type='html',
          
          font.size = "tiny",
          
          out="Y:/27_cadaster_canton_zurich/27_cadaster_canton_zurich/3_output/Regression_HS_vs_HD.doc")


