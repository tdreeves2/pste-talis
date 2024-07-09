# Import level-1 data
library(haven)
data <- read_sav("BTGUSAT3.sav")





# Select level-1 variables
data <- dplyr::select(data,
                      "IDSCHOOL",
                      "TCHWGT",
                      "TT3G12",
                      "TT3G36",
                      "TT3G37",
                      "TT3G06A1",
                      "TT3G06A2",
                      "TT3G06B1",
                      "TT3G06B2",
                      "TT3G06C1",
                      "TT3G06C2",
                      "TT3G06D1",
                      "TT3G06D2",
                      "TT3G06E1",
                      "TT3G06E2",
                      "TT3G06F1",
                      "TT3G06F2",
                      "TT3G06G1",
                      "TT3G06G2",
                      "TT3G06H1",
                      "TT3G06H2",
                      "TT3G06I1",
                      "TT3G06I2",
                      "TT3G06J1",
                      "TT3G06J2",
                      "TT3G11B",
                      "TT3G03",
                      "TRWGT1",
                      "TRWGT2",
                      "TRWGT3",
                      "TRWGT4",
                      "TRWGT5",
                      "TRWGT6",
                      "TRWGT7",
                      "TRWGT8",
                      "TRWGT9",
                      "TRWGT10",
                      "TRWGT11",
                      "TRWGT12",
                      "TRWGT13",
                      "TRWGT14",
                      "TRWGT15",
                      "TRWGT16",
                      "TRWGT17",
                      "TRWGT18",
                      "TRWGT19",
                      "TRWGT20",
                      "TRWGT21",
                      "TRWGT22",
                      "TRWGT23",
                      "TRWGT24",
                      "TRWGT25",
                      "TRWGT26",
                      "TRWGT27",
                      "TRWGT28",
                      "TRWGT29",
                      "TRWGT30",
                      "TRWGT31",
                      "TRWGT32",
                      "TRWGT33",
                      "TRWGT34",
                      "TRWGT35",
                      "TRWGT36",
                      "TRWGT37",
                      "TRWGT38",
                      "TRWGT39",
                      "TRWGT40",
                      "TRWGT41",
                      "TRWGT42",
                      "TRWGT43",
                      "TRWGT44",
                      "TRWGT45",
                      "TRWGT46",
                      "TRWGT47",
                      "TRWGT48",
                      "TRWGT49",
                      "TRWGT50",
                      "TRWGT51",
                      "TRWGT52",
                      "TRWGT53",
                      "TRWGT54",
                      "TRWGT55",
                      "TRWGT56",
                      "TRWGT57",
                      "TRWGT58",
                      "TRWGT59",
                      "TRWGT60",
                      "TRWGT61",
                      "TRWGT62",
                      "TRWGT63",
                      "TRWGT64",
                      "TRWGT65",
                      "TRWGT66",
                      "TRWGT67",
                      "TRWGT68",
                      "TRWGT69",
                      "TRWGT70",
                      "TRWGT71",
                      "TRWGT72",
                      "TRWGT73",
                      "TRWGT74",
                      "TRWGT75",
                      "TRWGT76",
                      "TRWGT77",
                      "TRWGT78",
                      "TRWGT79",
                      "TRWGT80",
                      "TRWGT81",
                      "TRWGT82",
                      "TRWGT83",
                      "TRWGT84",
                      "TRWGT85",
                      "TRWGT86",
                      "TRWGT87",
                      "TRWGT88",
                      "TRWGT89",
                      "TRWGT90",
                      "TRWGT91",
                      "TRWGT92",
                      "TRWGT93",
                      "TRWGT94",
                      "TRWGT95",
                      "TRWGT96",
                      "TRWGT97",
                      "TRWGT98",
                      "TRWGT99",
                      "TRWGT100")




# Checking/inspecting the data
View(data)
names(data)
head(data)
dim(data)
str(data)
summary(data)
psych::describe(data)





# Import level-2 data
library(haven)
datal2 <- read_sav("BCGUSAT3.sav")




# Checking/inspecting the data
View(datal2)
names(datal2)
head(datal2)
dim(datal2)
str(datal2)
summary(datal2)
psych::describe(datal2)





# Merge necessary level-2 data into level-1 data
datal2_subset <- datal2[c("IDSCHOOL","TC3G12")]
merged_data <- merge(data,datal2_subset, by = "IDSCHOOL")
data <- merged_data





# Checking/inspecting the data
View(data)
names(data)
head(data)
dim(data)
str(data)
summary(data)
psych::describe(data)





# Filter dataset
data <- dplyr::filter(data, TT3G12 == 2 & TT3G36 == 2)


        


# Re-code subject area variable
table(data$TT3G37)
library(car)
data$TT3G37 <- recode(data$TT3G37,"1=1; 2=2; 3=3; 4=4; 5=5; 6=5; 7=5; 8=5; 9=5; 10=5; 11=5; 12=5; NA=NA") 
table(data$TT3G37)




# Re-code highest level of education variable
table(data$TT3G03)
library(car)
data$TT3G03 <- recode(data$TT3G03,"2=1; 4=1; 5=1; 6=2; 7=2; NA=NA") 
table(data$TT3G03)





# Re-code content emphases variables
table(data$TT3G06A1)
library(car)
data$TT3G06A1 <- recode(data$TT3G06A1,"1=1; 2=0; NA=NA") 
data$TT3G06B1 <- recode(data$TT3G06B1,"1=1; 2=0; NA=NA")
data$TT3G06C1 <- recode(data$TT3G06C1,"1=1; 2=0; NA=NA") 
data$TT3G06D1 <- recode(data$TT3G06D1,"1=1; 2=0; NA=NA") 
data$TT3G06E1 <- recode(data$TT3G06E1,"1=1; 2=0; NA=NA")
data$TT3G06F1 <- recode(data$TT3G06F1,"1=1; 2=0; NA=NA") 
data$TT3G06G1 <- recode(data$TT3G06G1,"1=1; 2=0; NA=NA") 
data$TT3G06H1 <- recode(data$TT3G06H1,"1=1; 2=0; NA=NA")
data$TT3G06I1 <- recode(data$TT3G06I1,"1=1; 2=0; NA=NA") 
data$TT3G06J1 <- recode(data$TT3G06J1,"1=1; 2=0; NA=NA") 
table(data$TT3G06A1)





# Re-express teaching experience as year
table(data$TT3G11B)
data$year <- 2018-data$TT3G11B
table(data$year)





# Filter to include 1990 or later only
table(data$year)
data <- subset(data, year > 1989)
table(data$year)





# Generate histogram for year of completion of PSTE
hist(data$year, 
     main = "", 
     xlab = "Year of Completion of Pre-Service Teacher Education", 
     ylab = "Frequency", 
     col = "blue", 
     border = "black",
     xlim = c(1990, 2018),
     breaks = seq(1990, 2018, by = 1), # Set bar width to one year
     xaxt = 'n') # Suppress x-axis labels

# Add custom x-axis labels from 1990 to 2018
axis(1, at = seq(1990, 2018, by = 1))






# Normalize weights
library(psych)
describe(data$TCHWGT)
data$TCHWGT <- data$TCHWGT/427.76
describe(data$TCHWGT)
                           
                           



# Select variables needed for MCAR test
dataMCAR <- dplyr::select(data,
                      "IDSCHOOL",
                      "TCHWGT",
                      "TT3G37",
                      "TT3G06A1",
                      "TT3G06A2",
                      "TT3G06B1",
                      "TT3G06B2",
                      "TT3G06C1",
                      "TT3G06C2",
                      "TT3G06D1",
                      "TT3G06D2",
                      "TT3G06E1",
                      "TT3G06E2",
                      "TT3G06F1",
                      "TT3G06F2",
                      "TT3G06G1",
                      "TT3G06G2",
                      "TT3G06H1",
                      "TT3G06H2",
                      "TT3G06I1",
                      "TT3G06I2",
                      "TT3G06J1",
                      "TT3G06J2",
                      "year",
                      "TT3G03",
                      "TC3G12")




# Checking/inspecting the data
View(dataMCAR)
names(dataMCAR)
head(dataMCAR)
dim(dataMCAR)
str(dataMCAR)
summary(dataMCAR)
psych::describe(dataMCAR)





# Little's MCAR test
library(naniar)
library(dplyr)
mcar_test<-mcar_test(dataMCAR)
mcar_test





# Examine missing data patterns
library(mice)
md.pattern(data)





# Specify variables as factors
data$TT3G03 <- factor(data$TT3G03)
data$TT3G37 <- factor(data$TT3G37)
data$TC3G12 <- factor(data$TC3G12)

data$TT3G06A2 <- factor(data$TT3G06A2)
data$TT3G06B2 <- factor(data$TT3G06B2)
data$TT3G06C2 <- factor(data$TT3G06C2)
data$TT3G06D2 <- factor(data$TT3G06D2)
data$TT3G06E2 <- factor(data$TT3G06E2)
data$TT3G06F2 <- factor(data$TT3G06F2)
data$TT3G06G2 <- factor(data$TT3G06G2)
data$TT3G06H2 <- factor(data$TT3G06H2)
data$TT3G06I2 <- factor(data$TT3G06I2)
data$TT3G06J2 <- factor(data$TT3G06J2)





# Isolate replication weights
repweight <- dplyr::select(data,
                      "TRWGT1",
                      "TRWGT2",
                      "TRWGT3",
                      "TRWGT4",
                      "TRWGT5",
                      "TRWGT6",
                      "TRWGT7",
                      "TRWGT8",
                      "TRWGT9",
                      "TRWGT10",
                      "TRWGT11",
                      "TRWGT12",
                      "TRWGT13",
                      "TRWGT14",
                      "TRWGT15",
                      "TRWGT16",
                      "TRWGT17",
                      "TRWGT18",
                      "TRWGT19",
                      "TRWGT20",
                      "TRWGT21",
                      "TRWGT22",
                      "TRWGT23",
                      "TRWGT24",
                      "TRWGT25",
                      "TRWGT26",
                      "TRWGT27",
                      "TRWGT28",
                      "TRWGT29",
                      "TRWGT30",
                      "TRWGT31",
                      "TRWGT32",
                      "TRWGT33",
                      "TRWGT34",
                      "TRWGT35",
                      "TRWGT36",
                      "TRWGT37",
                      "TRWGT38",
                      "TRWGT39",
                      "TRWGT40",
                      "TRWGT41",
                      "TRWGT42",
                      "TRWGT43",
                      "TRWGT44",
                      "TRWGT45",
                      "TRWGT46",
                      "TRWGT47",
                      "TRWGT48",
                      "TRWGT49",
                      "TRWGT50",
                      "TRWGT51",
                      "TRWGT52",
                      "TRWGT53",
                      "TRWGT54",
                      "TRWGT55",
                      "TRWGT56",
                      "TRWGT57",
                      "TRWGT58",
                      "TRWGT59",
                      "TRWGT60",
                      "TRWGT61",
                      "TRWGT62",
                      "TRWGT63",
                      "TRWGT64",
                      "TRWGT65",
                      "TRWGT66",
                      "TRWGT67",
                      "TRWGT68",
                      "TRWGT69",
                      "TRWGT70",
                      "TRWGT71",
                      "TRWGT72",
                      "TRWGT73",
                      "TRWGT74",
                      "TRWGT75",
                      "TRWGT76",
                      "TRWGT77",
                      "TRWGT78",
                      "TRWGT79",
                      "TRWGT80",
                      "TRWGT81",
                      "TRWGT82",
                      "TRWGT83",
                      "TRWGT84",
                      "TRWGT85",
                      "TRWGT86",
                      "TRWGT87",
                      "TRWGT88",
                      "TRWGT89",
                      "TRWGT90",
                      "TRWGT91",
                      "TRWGT92",
                      "TRWGT93",
                      "TRWGT94",
                      "TRWGT95",
                      "TRWGT96",
                      "TRWGT97",
                      "TRWGT98",
                      "TRWGT99",
                      "TRWGT100")





# Checking/inspecting the data
View(repweight)
names(repweight)
head(repweight)
dim(repweight)
str(repweight)
summary(repweight)
psych::describe(repweight)






# Select variables for multiple imputation
data <- dplyr::select(data,
                      "IDSCHOOL",
                      "TCHWGT",
                      "TT3G37",
                      "TT3G06A1",
                      "TT3G06A2",
                      "TT3G06B1",
                      "TT3G06B2",
                      "TT3G06C1",
                      "TT3G06C2",
                      "TT3G06D1",
                      "TT3G06D2",
                      "TT3G06E1",
                      "TT3G06E2",
                      "TT3G06F1",
                      "TT3G06F2",
                      "TT3G06G1",
                      "TT3G06G2",
                      "TT3G06H1",
                      "TT3G06H2",
                      "TT3G06I1",
                      "TT3G06I2",
                      "TT3G06J1",
                      "TT3G06J2",
                      "year",
                      "TT3G03",
                      "TC3G12")





# Checking/inspecting the data
View(data)
names(data)
head(data)
dim(data)
str(data)
summary(data)
psych::describe(data)





# Multiply impute missing values using a random starting value
library(mice)
library(dplyr)
imputed_data <- mice(data, m=5, seed=100, remove.collinear = TRUE)





# Define the multiply-imputed data sets
imputed_data1 <- complete(imputed_data, action = 1)
imputed_data2 <- complete(imputed_data, action = 2)
imputed_data3 <- complete(imputed_data, action = 3)
imputed_data4 <- complete(imputed_data, action = 4)
imputed_data5 <- complete(imputed_data, action = 5)





# Examine descriptive statistics for the multiply-imputed data sets
summary(imputed_data1)
summary(imputed_data2)
summary(imputed_data3)
summary(imputed_data4)
summary(imputed_data5)





# Check to make sure imputation worked
md.pattern(imputed_data1)
md.pattern(imputed_data2)
md.pattern(imputed_data3)
md.pattern(imputed_data4)
md.pattern(imputed_data5)





# Merge in replication weights
imputed_data1<-cbind(imputed_data1, repweight)
imputed_data2<-cbind(imputed_data2, repweight)
imputed_data3<-cbind(imputed_data3, repweight)
imputed_data4<-cbind(imputed_data4, repweight)
imputed_data5<-cbind(imputed_data5, repweight)





# Define list of data frames
library(mitools)
implist <- imputationList(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5))





# Define survey design object
library(survey)
dstrat <- svrepdesign(ids = ~IDSCHOOL, weights = ~TCHWGT, repweights="TRWGT[0-9]+", type= "Fay", rho=.5, data = implist)
dstrat
summary(dstrat)





# Compute descriptive statistics
MIcombine(with(dstrat, svymean(~TT3G37+TT3G03+TC3G12)))


MIcombine(with(dstrat, svymean(~year)))

MIcombine(with(dstrat, svymean(~TT3G06A1)))
MIcombine(with(dstrat, svymean(~TT3G06B1)))
MIcombine(with(dstrat, svymean(~TT3G06C1)))
MIcombine(with(dstrat, svymean(~TT3G06D1)))
MIcombine(with(dstrat, svymean(~TT3G06E1)))
MIcombine(with(dstrat, svymean(~TT3G06F1)))
MIcombine(with(dstrat, svymean(~TT3G06G1)))
MIcombine(with(dstrat, svymean(~TT3G06H1)))
MIcombine(with(dstrat, svymean(~TT3G06I1)))
MIcombine(with(dstrat, svymean(~TT3G06J1)))

MIcombine(with(dstrat, svymean(~TT3G06A2)))
MIcombine(with(dstrat, svymean(~TT3G06B2)))
MIcombine(with(dstrat, svymean(~TT3G06C2)))
MIcombine(with(dstrat, svymean(~TT3G06D2)))
MIcombine(with(dstrat, svymean(~TT3G06E2)))
MIcombine(with(dstrat, svymean(~TT3G06F2)))
MIcombine(with(dstrat, svymean(~TT3G06G2)))
MIcombine(with(dstrat, svymean(~TT3G06H2)))
MIcombine(with(dstrat, svymean(~TT3G06I2)))
MIcombine(with(dstrat, svymean(~TT3G06J2)))

MIcombine(with(dstrat, svyquantile(~TT3G06A2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06B2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06C2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06D2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06E2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06F2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06G2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06H2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06I2, quantile=0.5, interval.type="quantile")))
MIcombine(with(dstrat, svyquantile(~TT3G06J2, quantile=0.5, interval.type="quantile")))

MIcombine(with(dstrat, svyvar(~year)))





# Logistic regression analysis (must change dependent variable)
results <-with(dstrat, svyglm(TT3G06A1~year + TT3G03 + TT3G37 + TC3G12, family=quasibinomial()))
MIcombine(results)
table<-MIcombine(results)
coefficients<-table$coefficients
coefficients<-as.data.frame(coefficients)





# Ordinal regression analysis (must change dependent variable)
results <-with(dstrat, svyolr(TT3G06A2~year + TT3G03 + TT3G37 + TC3G12))
MIcombine(results)
table<-MIcombine(results)
coefficients<-table$coefficients
coefficients<-as.data.frame(coefficients)





# Testing the proportional odds assumption (must change dependent variable)
library(MASS)
library(brant)

# List of data file names
data_files <- c("imputed_data1", "imputed_data2", "imputed_data3", "imputed_data4", "imputed_data5")

# Function to perform the analysis
perform_analysis <- function(data_file) {
  # Fit the model
  model <- polr(TT3G06A2 ~ year + TT3G03 + TT3G37 + TC3G12, data = get(data_file), Hess = TRUE)
  
  # Print the summary of the model
  print(summary(model))
  
  # Perform Brant's test
  brant_test <- brant(model)
  
  # Print the Brant test results
  print(brant_test)
}

# Loop through each data file and perform the analysis
for (data_file in data_files) {
  print(paste("Analysis for", data_file))
  perform_analysis(data_file)
  cat("\n\n")
}