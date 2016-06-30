# load libraries
library(ggplot2)
library(MASS)
library(maps)
library(maptools)
library(sp)

# load data from Quantlet
ENRGENICS_ADDOTHER_PATH = "/Users/Brian/github/spl-enRgenics/ENRgenics_AddOther/ENRgenics_AddOther.r"
source(ENRGENICS_ADDOTHER_PATH)

# use for AddOther
ENRGENICS_IMPORTEIA_PATH = "/Users/Brian/github/spl-enRgenics/ENRgenics_ImportEIA/ENRgenics_ImportEIA.r"
ENRGENICS_CLIMATE_DATA_PATH = "/Users/Brian/github/spl-enRgenics/data/climdiv-tmpcst-v1.0.0-20160605"
PATH_VARS = c(ENRGENICS_IMPORTEIA_PATH, ENRGENICS_CLIMATE_DATA_PATH)

EIA_DATA_PATH = "/Users/Brian/github/spl-enRgenics/data/sales_revenue.csv.0"
file = EIA_DATA_PATH

# data formatting
raw_data = load_eia_data_with_all_others(file)
finalsdf = subset(raw_data, DataStatus=="Final")
res <- subset(raw_data, DataSatus=="Final" & Cat=="RESIDENTIAL")
ind = subset(raw_data, DataStatus=="Final" & Cat=="INDUSTRIAL")
com = subset(raw_data, DataStatus=="Final" & Cat=="COMMERCIAL")
other = subset(raw_data, DataStatus=="Final" & Cat=="OTHER")
total = subset(raw_data, DataStatus=="Final" & Cat=="TOTAL")

#Regressions by Category for Simple Model

fit_simpler <- lm(Sales~temp, data=total)
summary(fit_simpler)

qplot(temp,Sales, data=res) + geom_point(shape=1)+geom_smooth(method=lm), #Residential
qplot(temp,Sales, data=ind) + geom_point(shape=1)+geom_smooth(method=lm), #Industrial
qplot(temp,Sales, data=com) + geom_point(shape=1)+geom_smooth(method=lm), #Commercial
qplot(temp,Sales, data=other) + geom_point(shape=1)+geom_smooth(method=lm), #Other
qplot(temp,Sales, data=total) + geom_point(shape=1)+geom_smooth(method=lm), #Total

#Regressions by Category for Complex Model


fit3 <- lm(Sales~temp + Price + Area + Population, data=total)
summary(fit3)
qplot(temp + Price, Area, Popu, data=res) + geom_point(shape=1)+geom_smooth(method=lm),
qplot(temp + Price + Revenue,Sales, data=ind) + geom_point(shape=1)+geom_smooth(method=lm),
qplot(temp + Price + Revenue,Sales, data=com) + geom_point(shape=1)+geom_smooth(method=lm),
qplot(temp + Price + Revenue,Sales, data=other) + geom_point(shape=1)+geom_smooth(method=lm),
qplot(temp + Price + Revenue,Sales, data=total) + geom_point(shape=1)+geom_smooth(method=lm)

#ANOVA testing of model fit

fit_simple <- lm(Sales~temp,data=total)
fit_complex <- lm(Sales~temp + Price + Revenue, data=total)

summary(total)

anova(fit_simple,fit_simpler)
install.packages("relaimpo")
library(relaimpo)
calc.relimp(fit3,type=c("lmg","last","first","pratt"),
            rela=TRUE)

sapply(total,class)







