if(F){
  rm(list=ls())
sessionInfo(); memory.limit()
memory.limit(size=65000)
setwd("brfss")
library(foreign); library(dplyr); library(tidyr)
var <- c('X_STATE', 'IMONTH', 'IYEAR', 'MENTHLTH', 'MARITAL', 'X_EDUCAG', 'EMPLOY1',
         'CHILDREN', 'RENTHOM1', 'INCOME2', 'X_RACE_G1', 'SEX', 'X_AGEG5YR', 
         'SXORIENT', 'TRNSGNDR', 'QSTVER', 'PHYSHLTH', 'HLTHPLN1', 'VETERAN3')
data = data.frame(matrix(NA, nrow=0, ncol = length(var))); data
colnames(data) = var; data
for (i in 2014:2017){
  dt = read.xport(paste0('LLCP', i, '.XPT'))
  dt = subset(dt, select = var)
  data = rbind(data, dt)
  rm(dt); print(i)
}
table(data$IYEAR)
data = data %>% rename('sex' = 'SEX', 'income' = 'INCOME2', 'race' = 'X_RACE_G1')

data18 = read.xport('LLCP2018.XPT') 
var_base = c('X_STATE', 'IMONTH', 'IYEAR', 'MENTHLTH', 'MARITAL', 'X_EDUCAG', 'EMPLOY1', 'CHILDREN', 'RENTHOM1',
            'X_AGEG5YR', 'SOMALE', 'SOFEMALE', 'TRNSGNDR', 'QSTVER', 'PHYSHLTH', 'VETERAN3')
(var18 = c(var_base, c('INCOME2', 'X_RACE_G1', 'SEX1', 'HLTHPLN1')))
data18 = data18 %>% select(all_of(var18)) %>% rename('sex' = 'SEX1', 'income' = 'INCOME2', 'race' = 'X_RACE_G1')
data19 = read.xport('LLCP2019.XPT') 
(var19 = c(var_base, c('INCOME2', 'X_RACE_G1', 'X_SEX', 'HLTHPLN1')))
data19 = data19 %>% select(all_of(var19)) %>% rename('sex' = 'X_SEX', 'income' = 'INCOME2', 'race' = 'X_RACE_G1')
data20 = read.xport('LLCP2020.XPT') 
(var20 = c(var_base, c('INCOME2', 'X_RACEGR3', 'X_SEX', 'HLTHPLN1')))
data20 = data20 %>% select(all_of(var20)) %>% rename('sex' = 'X_SEX', 'income' = 'INCOME2', 'race' = 'X_RACEGR3')
data21 = read.xport('LLCP2021.XPT') 
(var21 = c(var_base, c('INCOME3', 'X_RACEGR3', 'X_SEX', 'X_HLTHPLN')))
data21 = data21 %>% select(all_of(var21)) %>% rename('sex' = 'X_SEX', 'income' = 'INCOME3', 
                                                     'race' = 'X_RACEGR3', 'HLTHPLN1'= 'X_HLTHPLN')
data22 = read.xport('LLCP2022.XPT') # X_RACEGR4
(var22 = c(var_base, c('INCOME3', 'X_RACEGR4', 'X_SEX', 'X_HLTHPLN')))
data22 = data22 %>% select(all_of(var22)) %>% rename('sex' = 'X_SEX', 'income' = 'INCOME3', 
                                                     'race' = 'X_RACEGR4', 'HLTHPLN1'= 'X_HLTHPLN')
data23 = read.xport('LLCP2023.XPT') # X_RACEGR3
(var23 = c(var_base, c('INCOME3', 'X_RACEGR3', 'X_SEX', 'X_HLTHPL1')))
data23 = data23 %>% select(all_of(var23)) %>% rename('sex' = 'X_SEX', 'income' = 'INCOME3', 
                                                     'race' = 'X_RACEGR3', 'HLTHPLN1'= 'X_HLTHPL1')
data24 = read.xport('LLCP2024.XPT') # X_HLTHPL2
# no transgender in 2024
(var24 = c(setdiff(var_base, "TRNSGNDR"), c('INCOME3', 'X_RACEGR3', 'X_SEX', 'X_HLTHPL2')))
data24 = data24 %>% select(all_of(var24)) %>% rename('sex' = 'X_SEX', 'income' = 'INCOME3', 
                                                     'race' = 'X_RACEGR3', 'HLTHPLN1'= 'X_HLTHPL2') %>% 
  mutate(TRNSGNDR = NA)
data1824 = rbind(data18, data19, data20, data21, data22, data23, data24); table(data1824$IYEAR)
rm(data18, data19, data20, data21, data22, data23, data24)
table(data1824$SOMALE, data1824$sex)
table(data1824$SOFEMALE, data1824$sex)
data1824$SXORIENT = ifelse(data1824$sex==1, data1824$SOMALE, data1824$SOFEMALE)
table(data1824$SXORIENT)
data1824 = data1824 %>% select(-SOMALE, -SOFEMALE)
data = rbind(data, data1824); rm(data1824); table(data$IYEAR)
data = data %>% filter(X_STATE<57) %>% rename('state' = "X_STATE", 'month' = "IMONTH", 'year' = "IYEAR" ); table(data$year)
save(data, file = 'brfss1424.RDa')
}
####################################
rm(list=ls())
load('brfss1424.RDa'); table(data$year)
table(data$SXORIENT, data$year); table(data$TRNSGNDR, data$year)
data1 = data %>% filter(year<2024) %>% filter(!is.na(SXORIENT) & !is.na(TRNSGNDR)) %>% filter(SXORIENT<4 & TRNSGNDR<=4)
data2 = data %>% mutate(year = ifelse(year==2025, 2024, year)) %>% # use 2025 as 2024
                          filter(year==2024) %>% filter(!is.na(SXORIENT)) %>% filter(SXORIENT<4)
data = rbind(data1, data2); rm(data1, data2)
data$year = as.numeric(as.character(data$year)); table(data$year)
# year 2014-2017 1 = straight 2 = gay 3 = bisexual
# year 2018-2024 1 = gay 2 = straight 3 = bisexual
table(data$year, data$SXORIENT)
data$gay = ifelse(data$year<=2017 & data$SXORIENT == 2, 1, 0); table(data$year, data$gay)
data$gay = ifelse(data$year>=2018 & data$SXORIENT == 1, 1, data$gay); table(data$year, data$gay)
data$bi = ifelse(data$SXORIENT == 3, 1, 0); table(data$year, data$bi)
# 2024 no question about transgender
# 1 Yes, Transgender, male-to-female
# 2 Yes, Transgender, female to male
# 3 Yes, Transgender, gender nonconforming
# 4 No
data$trans = ifelse(data$TRNSGNDR<4 & !is.na(data$TRNSGNDR), 1, 0); table(data$trans, data$year)
data$lgbt = ifelse(data$trans == 1 | data$gay==1 | data$bi==1, 1, 0); table(data$year, data$lgbt)
# cellphone
data$cell <- ifelse(data$QSTVER>=20, 1, 0); table(data$cell)
# sex
table(data$year, data$sex)
data = data %>% filter(sex <= 2) %>% mutate(female = ifelse(sex == 2, 1, 0)); table(data$sex, data$female)
# race
table(data$race, data$year)
data$white = ifelse(data$race == 1, 1, 0)
data$black = ifelse(data$race == 2, 1, 0)
data$hispanic = ifelse((data$race == 3 & data$year <= 2019) | (data$race == 5 & data$year > 2019), 1, 0)
data =  data %>% filter(race != 9) %>% mutate(other = ifelse(white == 0 & black == 0 & hispanic == 0, 1, 0))
# age
table(data$X_AGEG5YR, data$year)
data <- data[data$X_AGEG5YR!=14, ] # age 65> and refused or missing
data$age1829 <- ifelse(data$X_AGEG5YR==1|data$X_AGEG5YR==2, 1, 0)
data$age30s <- ifelse(data$X_AGEG5YR==3|data$X_AGEG5YR==4, 1, 0)
data$age40s <- ifelse(data$X_AGEG5YR==5|data$X_AGEG5YR==6, 1, 0)
data$age50s <- ifelse(data$X_AGEG5YR==7|data$X_AGEG5YR==8, 1, 0)
data$age60s <- ifelse(data$X_AGEG5YR==9|data$X_AGEG5YR==10, 1, 0)
data$age70s <- ifelse(data$X_AGEG5YR==11|data$X_AGEG5YR==12, 1, 0)
data$age80s <- ifelse(data$X_AGEG5YR==13, 1, 0)
# education
table(data$X_EDUCAG, data$year)
data <- data[data$X_EDUCAG!=9,]
data$noschool <- ifelse(data$X_EDUCAG==1, 1, 0)
data$highschool <- ifelse(data$X_EDUCAG==2, 1, 0)
data$somecollege <- ifelse(data$X_EDUCAG==3, 1, 0)
data$college <- ifelse(data$X_EDUCAG==4, 1, 0)
# marital
table(data$MARITAL, data$year)
data <- data[data$MARITAL!=9,]
data$married <- ifelse(data$MARITAL==1, 1, 0)
data$divorced <- ifelse(data$MARITAL==2, 1, 0)
data$widow <- ifelse(data$MARITAL==3, 1, 0)
data$separate <- ifelse(data$MARITAL==4, 1, 0)
data$single <- ifelse(data$MARITAL==5, 1, 0)
data$couple <- ifelse(data$MARITAL==6, 1, 0)
# children
table(data$CHILDREN, data$year)
data <- data[data$CHILDREN!=99,]
data$child <- ifelse(data$CHILDREN==88, 0, 1)
table(data$child, data$year)
# employment
table(data$EMPLOY1, data$year)
data <- data[data$EMPLOY1!=9,]
data$employed <- ifelse(data$EMPLOY1==1, 1, 0)
data$selfemp <- ifelse(data$EMPLOY1==2, 1, 0)
data$unemployed <- ifelse(data$EMPLOY1==3|data$EMPLOY1==4, 1, 0)
data$homemaker <- ifelse(data$EMPLOY1==5, 1, 0)
data$student <- ifelse(data$EMPLOY1==6, 1, 0)
data$retired <- ifelse(data$EMPLOY1==7, 1, 0)
data$disable <- ifelse(data$EMPLOY1==8, 1, 0)
# income changed since 2021 for $75K or more
table(data$income, data$year)
data$income1 <- ifelse(data$income==1, 1, 0)
data$income2 <- ifelse(data$income==2, 1, 0)
data$income3 <- ifelse(data$income==3, 1, 0)
data$income4 <- ifelse(data$income==4, 1, 0)
data$income5 <- ifelse(data$income==5, 1, 0)
data$income6 <- ifelse(data$income==6, 1, 0)
data$income7 <- ifelse(data$income==7, 1, 0)
data$income8 <- ifelse(data$income==8 | data$income==9 | data$income==10 | data$income==11, 1, 0)
data$income9 <- ifelse(data$income==77 | data$income==99, 1, 0)
data$income9 <- ifelse(is.na(data$income9), 1, data$income9)
incomevar = paste0('income', 1:8)
data[, incomevar] = lapply(data[, incomevar], function(x) ifelse(is.na(x), 0, x))
#mental
table(data$MENTHLTH)
data = data %>% filter(MENTHLTH != 77 & MENTHLTH != 99) %>% mutate(mental = ifelse(MENTHLTH == 88, 0, MENTHLTH))
table(data$mental)
data$distress = ifelse(data$mental==30, 1, 0); table(data$distress)
data$freq = ifelse(data$mental >= 14, 1, 0); table(data$freq)
# health insurance
data$insurance <- NA
data$insurance[data$HLTHPLN1==1] <- 1
data$insurance[data$HLTHPLN1==2] <- 0
# physical
data$physicalDay <- data$PHYSHLTH
data <- subset(data, physicalDay!=77 & physicalDay!=99)
data$physicalDay[data$physicalDay==88] <- 0
data$physical <- ifelse(data$physicalDay==30, 1, 0)
# home owner
data$home = ifelse(data$RENTHOM1 == 1, 1, 0)
# veteran
data$veteran = ifelse(data$VETERAN3 == 1, 1, 0)
#
data <- subset(data, select = c(gay, bi, trans, lgbt, mental, distress, freq,
                                female, age1829, age30s, age40s, age50s, age60s, age70s, age80s, 
                                noschool, highschool, somecollege, college,
                                employed, selfemp, unemployed, homemaker, student, retired, disable,
                                married, divorced, widow, separate, single, couple, child,
                                white, black,  hispanic, other,
                                income1, income2, income3, income4, income5, 
                                income6, income7, income8, income9,
                                year, month, state, cell, insurance, physical, home, veteran))
save(data, file = 'brfss1424clean.RDa')
####
load('brfss1424clean.RDa'); nrow(data); table(data$year)
data = data %>% select(distress, freq, lgbt, year, 
                       female,age1829,age30s,age40s,age50s,age60s,age70s,age80s,
                       white,black,hispanic,other,
                       employed,selfemp,unemployed,homemaker,student,retired,disable,
                       noschool,highschool,somecollege,college,
                       married,divorced,widow,separate,single,couple,
                       income1,income2,income3,income4,income5,income6,income7,income8, income9,
                       home,insurance,veteran)
data = na.omit(data); nrow(data); table(data$year)
write.csv(data, 'brfss1424clean.csv')