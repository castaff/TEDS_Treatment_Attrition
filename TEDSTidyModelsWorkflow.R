#************************************
# Treatment Dropout Model Development
#************************************

# Summary of changes in code revision: (highlighted in code using "#NEW LINES OF CODE")
# 1. Organization of data, results, and figures in sub-directories
# 2. Addition of 2019 TEDS-D
# 3. Addition of LOS (length of stay) and LIVARAG (living arangement at admission), and removal of HLTHINS (health insurance) from models
# 4. Removal of treatment episodes that were terminated due to transfers (REASON==4)
# 5. Missing data analysis
# 6. Revised cross-validation including CART, bagged trees, and boosting
# 7. Addition of new "unknown" level in categorical variables with high missingness (>20%)
# 8. Analysis by LOS category (categories not used as predictors)
# 9. Analysis by treatment setting (categories not used as predictors)
# 10. Revised analysis by year
# 11. Revised variable importance plots and partial dependence plots

# Setup -------------------------------------------------------------------

rm(list = ls()[!(ls() %in% c())]) #Removing all variables #NEW LINES OF CODE

#Selecting home directory #NEW LINES OF CODE
if(Sys.info()["sysname"]=="Linux"){ #selecting path by computer name
  home_dir = file.path("~/Documents/Treatment Dropout/R")
  data_dir = file.path("~/Documents/Treatment Dropout/R/Data")
  results_dir = file.path("~/Documents/Treatment Dropout/R/Results")
  cv_dir = file.path(results_dir,"/CV Analysis")
  fold_dir = file.path(cv_dir,"/Folds")
  fig_dir = file.path("~/Documents/Treatment Dropout/R/Figures")
  setwd(home_dir)
}else{
  home_dir = file.path(Sys.getenv("USERPROFILE"),"/Dropbox (Dartmouth College)/Treatment dropout/Codes and analysis")
  data_dir = file.path(Sys.getenv("USERPROFILE"),"/Dropbox (Dartmouth College)/Treatment dropout/Codes and analysis/Data")
  results_dir = file.path(Sys.getenv("USERPROFILE"),"/Dropbox (Dartmouth College)/Treatment dropout/Codes and analysis/Results")
  cv_dir = file.path(results_dir,"/CV Analysis")
  fold_dir = file.path(cv_dir,"/Folds")
  fig_dir = file.path(Sys.getenv("USERPROFILE"),"/Dropbox (Dartmouth College)/Treatment dropout/Codes and analysis/Figures")
  setwd(home_dir)
}

# Installing and loading necessary packages
if(!("dplyr" %in% installed.packages()[,"Package"])) install.packages("dplyr"); library(dplyr)
if(!("tidyverse" %in% installed.packages()[,"Package"])) install.packages("tidyverse"); library(tidyverse)
if(!("tidymodels" %in% installed.packages()[,"Package"])) install.packages("tidymodels"); library(tidymodels)
if(!("magrittr" %in% installed.packages()[,"Package"])) install.packages("magrittr"); library(magrittr)
if(!("pROC" %in% installed.packages()[,"Package"])) install.packages("pROC"); library(pROC)
if(!("missForest" %in% installed.packages()[,"Package"])) install.packages("missForest"); library(missForest)
if(!("vip" %in% installed.packages()[,"Package"])) install.packages("vip"); library(vip)
if(!("broom" %in% installed.packages()[,"Package"])) install.packages("broom"); library(broom)
if(!("pdp" %in% installed.packages()[,"Package"])) install.packages("pdp"); library(pdp)
if(!("doParallel" %in% installed.packages()[,"Package"])) install.packages("doParallel"); library(doParallel)
if(!("patchwork" %in% installed.packages()[,"Package"])) install.packages("patchwork"); library(patchwork)
if(!("janitor" %in% installed.packages()[,"Package"])) install.packages("janitor"); library(janitor)
if(!("data.table" %in% installed.packages()[,"Package"])) install.packages("data.table"); library(data.table)
if(!("gridExtra" %in% installed.packages()[,"Package"])) install.packages("gridExtra"); library(gridExtra)
if(!("cowplot" %in% installed.packages()[,"Package"])) install.packages("cowplot"); library(cowplot)
if(!("rpart" %in% installed.packages()[,"Package"])) install.packages("rpart"); library(rpart)
if(!("baguette" %in% installed.packages()[,"Package"])) install.packages("baguette"); library(baguette)
if(!("ranger" %in% installed.packages()[,"Package"])) install.packages("ranger"); library(ranger)
if(!("xgboost" %in% installed.packages()[,"Package"])) install.packages("xgboost"); library(xgboost)

# Establishing overall parameters
outer_v = 10; inner_v = 5 # number of folds in outer and inner CV
sz = 5 # size of grid to tune the models
fm = DROPOUT ~ . # formula for models' recipe - dropout as a function of all predictors
miss_th = 0.2 # high percentage of missingness threshold (20% chosen following Reviewer 2's recommendation)
imp_th = 0.01 # importance threhold for variable importance plots by categories and partial dependence plots (1% chosen in conversation with MJ on 4/7/2022 - based on variable importance plot of main analysis on imputed data

# Define theme for all plots
my_theme = theme(
  axis.text = element_text(size = 9, color = "black"),
  panel.grid.major.y =  element_blank(),
  plot.title = element_text(face = "bold")
)

# Establishing number of cores #NEW LINES OF CODE
if(Sys.info()["sysname"]=="Linux"){ #selecting path by computer name
  cores = 20
}else{
  cores = detectCores()-1
}

# # Load Data ---------------------------------------------------------------
# #Load each of the 5 years of included TEDS-D data - these can be found here:
# #https://www.datafiles.samhsa.gov/study-series/treatment-episode-data-set-discharges-teds-d-nid13520
# 
# setwd(data_dir)
# assign('teds2015', get(load('TEDSD2015.Rdata')))
# assign('teds2016', get(load('TEDSD2016.Rdata')))
# assign('teds2017', get(load('TEDSD2017.Rdata')))
# assign('teds2018', get(load('TEDSD2018.Rdata')))
# assign('teds2019', get(load('TEDSD2019.Rdata'))) #NEW LINES OF CODE
# rm("PUF", "tedsd_2015_puf","tedsd_2016_puf", "tedsd_puf_2017")
# 
# # Converting data frames to data tables
# setDT(teds2015); setDT(teds2016); setDT(teds2017); setDT(teds2018); setDT(teds2019)
# 
# # #Subsetting the data (for debugging purposes only)
# # teds2015 = teds2015[1:1000,]; teds2016 = teds2016[1:1000,]; teds2017 = teds2017[1:1000,]; teds2018 = teds2018[1:1000,]; teds2019 = teds2019[1:1000,]
# 
# # Process variables so all years are consistent and combine ---------------------------
# 
# # Joining 2017-2019 in a list for data processing
# teds1719 = list(teds2017,teds2018,teds2019)
# rm(teds2017,teds2018,teds2019); gc()
# 
# #Create a count variable for number of substances in years where this is not included - 2017-2019 #NEW LINES OF CODE
# years = 2017:2019
# for (y in seq(years)){
#   teds1719[[y]]<- teds1719[[y]] %>%
#     mutate( SUB1SUB = case_when(
#       SUB1 > 1  ~ 1,
#       TRUE  ~ 0),
#       SUB2SUB = case_when(
#         SUB2 > 1  ~ 1,
#         TRUE  ~ 0),
#       SUB3SUB = case_when(
#         SUB3 > 1  ~ 1,
#         TRUE  ~ 0
#       ), NUMSUBS = (SUB1SUB + SUB2SUB + SUB3SUB),
#       YEAR = years[y],
#       CBSA = CBSA2010
#     )
#   # Remove extra created variables
#   teds1719[[y]] <- subset(teds1719[[y]], select = -c(SUB1SUB, SUB2SUB, SUB3SUB, CBSA2010))
# }
# 
# #Combine files together - 2015-2019 #NEW LINES OF CODE
# teds1519 <- append(list(teds2015,teds2016),teds1719); rm(teds2015,teds2016,teds1719); gc()
# teds1519 <- rbindlist(teds1519, use.names = T); gc()
# 
# 
# #Get rid of variables that are collected only at discharge,
# #keep only records where opioid use is indicated, replace -9s with NA
# teds1519 <- teds1519 %>%
#             select( -ARRESTS_D, -DETNLF_D, -EMPLOY_D, -FREQ_ATND_SELF_HELP, -FREQ_ATND_SELF_HELP_D,
#                     -FREQ1_D, -FREQ2_D, -FREQ3_D, -LIVARAG_D, -SERVICES_D, -SUB1_D, -SUB2_D, -SUB3_D) %>%
#             filter(OPSYNFLG ==1|HERFLG==1 | METHFLG ==1) %>%
#             mutate_all(list(~na_if(., -9)))
# 
# #Select only the variables we need for processing and analysis #NEW LINES OF CODE (LOS WAS ADDED AND HLTHINS WAS REMOVED)
# teds1519 <- teds1519 %>%
#   select(FRSTUSE1, FRSTUSE2, FRSTUSE3, FREQ1, FREQ2, FREQ3, ROUTE1, ROUTE2, ROUTE3, STIMFLG, AMPHFLG, MTHAMFLG, COKEFLG, TRNQFLG, BENZFLG,
#          SEDHPFLG, BARBFLG, HALLFLG, PCPFLG, OTHERFLG, REASON, LOS, AGE, SUB1, SUB2, SUB3, ALCFLG, ARRESTS, DAYWAIT, EDUC, EMPLOY, ETHNIC,
#          GENDER, INHFLG, LIVARAG, MARFLG, MARSTAT, METHUSE, NOPRIOR, NUMSUBS, PRIMINC, PRIMPAY, PSOURCE, PSYPROB,
#          RACE, REGION, SERVICES, VET, YEAR) # HLTHINS, YEAR
# 
# #Removing treatment episodes that were terminated due to transfers #NEW LINES OF CODE
# teds1519 <- teds1519[REASON!=4,]

# # Variable Recodes --------------------------------------------------------
# #Feature engineering and recodes, selection of variables included in the analysis
# teds1519 <- teds1519 %>%
#   mutate(  #Create variable for age of first substance use
#     FRSTSUBUSE = pmin(FRSTUSE1, FRSTUSE2, FRSTUSE3, na.rm = TRUE),
#     #Create variable for maximum frequency of use of any opioid
#     FREQOH = case_when(
#       SUB1 == 5  ~ FREQ1,
#       SUB2 == 5  ~ FREQ2,
#       SUB3 == 5  ~ FREQ3,
#       TRUE  ~ NA_real_),
#     FREQOM = case_when(
#       SUB1 == 6  ~ FREQ1,
#       SUB2 == 6  ~ FREQ2,
#       SUB3 == 6  ~ FREQ3,
#       TRUE  ~ NA_real_),
#     FREQORx = case_when(
#       SUB1 == 7  ~ FREQ1,
#       SUB2 == 7  ~ FREQ2,
#       SUB3 == 7  ~ FREQ3,
#       TRUE  ~ NA_real_),
#     FREQMAX = pmax(FREQOH, FREQOM, FREQORx, na.rm = TRUE),
#     #Create Needle use flag variable indicating any injection drug use (included variable, IDU, only
#     #indicates injection as route of administration for primary substance of use (SUB1))
#     ROUTE11 = case_when(
#       is.na(ROUTE1)~ NA_real_,
#       ROUTE1 == 4 ~ 1,
#       TRUE  ~ 0),
#     ROUTE22 = case_when(
#       is.na(ROUTE2)~ NA_real_,
#       ROUTE2 == 4 ~ 1,
#       TRUE  ~ 0),
#     ROUTE33 = case_when(
#       is.na(ROUTE3)~ NA_real_,
#       ROUTE3 == 4 ~ 1,
#       TRUE  ~ 0),
#     NEEDLEUSE = pmax(ROUTE11, ROUTE22, ROUTE33, na.rm = TRUE),
#     #Create flag variable for any stimulant use
#     STIMFLAG = case_when(
#       STIMFLG == 1 ~ 1,
#       AMPHFLG == 1 ~ 1,
#       MTHAMFLG == 1 ~ 1,
#       COKEFLG == 1 ~ 1,
#       TRUE  ~ 0),
#     #create flag variable for any tranquilizer use
#     TRNQFLAG = case_when(
#       TRNQFLG == 1 ~ 1,
#       BENZFLG == 1 ~ 1,
#       TRUE  ~ 0),
#     #create flag variable for any sedative use
#     SEDFLAG = case_when(
#       SEDHPFLG == 1 ~ 1,
#       BARBFLG == 1 ~ 1,
#       TRUE  ~ 0),
#     #create flag variable for any hallucinogen use
#     HALFLAG = case_when(
#       HALLFLG == 1 ~ 1,
#       PCPFLG == 1 ~ 1,
#       OTHERFLG == 1 ~ 1,
#       TRUE  ~ 0),
#     #Create binary flag variable for outcome
#     DROPOUT = case_when(
#       REASON == 2 ~ 1,
#       TRUE  ~ 0),
#     #Collapsing age category into less buckets so as not to bias the random forest
#     #towards variables with a relatively high number of levels
#     AGECAT = case_when(
#       AGE %in% c("1")  ~ 1,
#       AGE %in% c("2","3", "4")  ~ 2,
#       AGE %in% c("5", "6")~ 3,
#       AGE %in% c("7", "8") ~ 4,
#       AGE %in% c("9", "10") ~ 5,
#       AGE == 11 ~ 6,
#       AGE == 12 ~ 7,
#       TRUE  ~ 0),
#     #Heroin use indicator
#     HEROIN = case_when(
#       SUB1 == 5  ~ 1,
#       SUB2 == 5  ~ 1,
#       SUB3 == 5  ~ 1,
#       TRUE  ~ 0)
#   ) %>%
# select(AGECAT, ALCFLG, ARRESTS, DAYWAIT, DROPOUT, EDUC, EMPLOY, ETHNIC, FREQMAX,
#        FRSTSUBUSE, GENDER, HALFLAG, HEROIN,INHFLG, LIVARAG, MARFLG, MARSTAT,
#        METHUSE, NEEDLEUSE, NOPRIOR, NUMSUBS, PRIMINC, PRIMPAY, PSOURCE, PSYPROB,
#        RACE, REGION, SEDFLAG, SERVICES, STIMFLAG, TRNQFLAG, VET, YEAR, LOS) # HLTHINS,
# 
# 
# 
# #convert numeric variables to ordered and unordered factors
# colsfactor <- c("REGION", "PRIMPAY", "DROPOUT", "ETHNIC", "LIVARAG", "MARSTAT",
#                 "PRIMINC", "PSOURCE", "RACE", "SERVICES", "ALCFLG", "GENDER", "METHUSE", "PSYPROB", "VET",
#                 "NEEDLEUSE", "HALFLAG", "INHFLG", "MARFLG", "STIMFLAG", "TRNQFLAG", "SEDFLAG", "HEROIN") # "HLTHINS",
# 
# teds1519 %<>% mutate_at(colsfactor, funs(factor(.)))
# 
# colsordered <- c("AGECAT", "ARRESTS", "EDUC", "EMPLOY", "NOPRIOR", "FREQMAX", "FRSTSUBUSE","DAYWAIT",
#                  "NUMSUBS" )
# 
# teds1519 %<>% mutate_at(colsordered, funs(ordered(.))); gc()
# 
# #Calculating the proportion of episodes at different LOS duration #NEW LINES OF CODE
# ##Generating LOS categories
# teds1519[,los_cat:=LOS] # creating variable for LOS categories
# teds1519[,los_cat:=ifelse(LOS==1,"1",los_cat)] # LOS of 1 day
# teds1519[,los_cat:=ifelse(2<=LOS & LOS<=7,"2-7",los_cat)] # LOS of 2-7 days
# teds1519[,los_cat:=ifelse(8<=LOS & LOS<=30,"8-30",los_cat)] # LOS of 2-7 days
# teds1519[,los_cat:=ifelse(LOS==31|LOS==32,"31-60",los_cat)] # LOS of 31-60 days
# teds1519[,los_cat:=ifelse(LOS==33|LOS==34,"61-120",los_cat)] # LOS of 61-120 days
# teds1519[,los_cat:=ifelse(LOS==35|LOS==36,"121-365",los_cat)] # LOS of 121-365 days
# teds1519[,los_cat:=ifelse(LOS==37,">365",los_cat)] # LOS of more than 365 days
# teds1519[,los_cat:=factor(los_cat, levels = c("1", "2-7", "8-30", "31-60", "61-120", "121-365", ">365"))] # converting column to factor
# teds1519[,LOS:=NULL] # removing original LOS variable
# 
# ##Calculating totals and proportions
# sum_los = teds1519[,.(N=.N, prop=round(.N/nrow(teds1519),2)),by=los_cat] # by LOS categories
# sum_los = sum_los[order(los_cat)] # sorting output
# 
# sum_los_year = teds1519[,{tot = .N
# .SD[,.(N=.N, prop=round(.N/tot,2)),by=los_cat]},
# by=YEAR] # by LOS categories and years
# sum_los_year = sum_los_year[order(YEAR,los_cat)] # sorting output
# 
# #Calculating the proportion of episodes at different treatment settings #NEW LINES OF CODE
# ##Generating treatment setting categories
# teds1519[,tx_set_cat:=SERVICES] # creating variable for treatment setting categories
# teds1519[,tx_set_cat:=ifelse(SERVICES==1|SERVICES==2,"Detox",tx_set_cat)] # Detox treatment settings
# teds1519[,tx_set_cat:=ifelse(SERVICES==3|SERVICES==4|SERVICES==5,"Rehab",tx_set_cat)] # Rehab treatment settings
# teds1519[,tx_set_cat:=ifelse(SERVICES==6|SERVICES==7|SERVICES==8,"Ambulatory",tx_set_cat)] # Ambulatory treatment settings
# teds1519[,tx_set_cat:=factor(tx_set_cat)] # converting column to factor
# 
# ##Renaming the levels of the SERVICES variable
# levels(teds1519$SERVICES) = c("Detox (inpatient)", "Detox (residential)",
#                               "Rehab (hospital)", "Rehab (at most 30 days)",
#                               "Rehab (more than 30 days)",
#                               "Ambulatory (intensive outpatient)",
#                               "Ambulatory (non-intensive outpatient)",
#                               "Ambulatory (detoxification)")
# 
# ##By treatment setting
# sum_ts = teds1519[,.(N=.N, perct=round(.N/nrow(teds1519)*100,2)),by=SERVICES]
# sum_ts = sum_ts[order(SERVICES)] # sorting output
# 
# ##By treatment setting and year
# sum_ts_year = teds1519[,{tot = .N
# .SD[,.(N=.N, perct=round(.N/tot*100,2)),by=SERVICES]},
# by=YEAR]
# sum_ts_year = sum_ts_year[order(YEAR,SERVICES)] # sorting output
# 
# #Removing year, length of stay, and treatment setting categories from main dataset
# #(additional variables kept  to divide data for analyses by year, LOS category, and treatment setting)
# year = teds1519[,YEAR] # extracting years
# los_cat = teds1519[,los_cat] # extracting LOS categories
# tx_set = teds1519[,tx_set_cat] # extracting treatment setting categories
# teds1519[,c("YEAR","los_cat", "tx_set_cat"):=NULL] # removing treatment setting categories from main data table
# 
# # Saving combined data
# setwd(data_dir)
# save(year, los_cat, tx_set, teds1519, file="combined_teds.rda")

# # Missing Data Analysis #NEW LINES OF CODE --------------------------------
# 
# #Loading data
# setwd(data_dir)
# load("combined_teds - full data.rda"); var_meaning = read.csv("Variable Meaning.csv")
# teds1519[,HEROIN:=factor(HEROIN)] # making sure the indicator for heroin is a factor
# 
# #Calculating number and percentage of missingness in each variable
# tmp = sapply(teds1519, function(x) sum(is.na(x)))
# miss_tbl = data.frame(N=tmp,p=round(tmp/nrow(teds1519),4)); rm(tmp)
# miss_tbl = miss_tbl[order(miss_tbl$N, decreasing = T),]
# 
# #Identifying variables with high percentage of missingness
# miss_vars = row.names(miss_tbl)[which(miss_tbl$p>miss_th)] # idetifying variables using their names
# 
# #Testing the assumption of variables missing completely at random (MCAR) using logistic regression (https://stats.stackexchange.com/questions/221289/how-to-check-missing-data-is-missing-at-random-or-not)
# #We did not use Little's Missing Completely at Random (MCAR) Test because the test is not suitable for categorical variables (see https://rdrr.io/cran/misty/man/na.test.html)
# ##Conclusion: Since all the outcomes have significant predictors (could be because of the large sample size), we conclude that there is not evidence to claim that the assumption of MCAR is true
# lr = list() # creating list to store output of logistic regressions
# for(m in seq(miss_vars)){
#   teds1519[,ind:=ifelse(is.na(teds1519[,miss_vars[m],with=FALSE]),1,0)] # adding indicator for missingness in the appropriate variable
#   fm_m = as.formula(paste0("ind~",paste0(setdiff(names(teds1519),c(miss_vars[m],"ind")),collapse = "+"))) # formula for logistic regression
#   lr[[m]] = glm(fm_m, data = teds1519, family = "binomial") # logistic regression
# }
# names(lr) = miss_vars # identifying each element of the list with the names of the variables with high missingness
# 
# #Evaluating the influence of dropout in the missingness of predictors (https://measuringu.com/missing-data/)
# ##Conclusion: All proportions are statistically different (could be because of the large sample size) which may be an indication that the data is MNAR.
# #However, the differences in the proportions are smaller than 10% in a large dataset making them not practically significant.
# #There is not enough evidence to believe that the data is not MAR. (There is no way to test MAR - Ch 1 of Ender's book.)
# pt = list() # creating list to store the output of the hypothesis tests
# bonf = 0.05/length(miss_vars) # Boferroni correction threshold to account for multiple testing
# for(m in seq(miss_vars)){
#   teds1519[,ind:=ifelse(is.na(teds1519[,miss_vars[m],with=FALSE]),1,0)] # adding indicator for missingness in the appropriate variable
#   successes = c(length(which(teds1519[DROPOUT==0,ind]==1)),length(which(teds1519[DROPOUT==1,ind]==1))) # counting number of times the variable was missing when DROPOUT==0 or DROPOUT==1
#   counts = c(length(teds1519[DROPOUT==0,ind]==1),length(teds1519[DROPOUT==1,ind]==1)) # counting number of cases considered (missing or non-missing) when DROPOUT==0 or DROPOUT==1
#   pt[[m]] = prop.test(successes, counts, correct = F, conf.level = bonf) # conducting tests for 2 proportions without Yate's correction
# }
# names(pt) = miss_vars # identifying each element of the list with the names of the varables with high missingness
# 
# ## Calculating difference in proportion missingness
# round(c(diff(pt$PRIMPAY$estimate), diff(pt$DAYWAIT$estimate), diff(pt$PRIMINC$estimate), diff(pt$MARSTAT$estimate)),2)
# 
# ##Counting number of significant predictors
# c(length(which(summary(lr$PRIMPAY)$coefficients[,4]<0.05)), length(which(summary(lr$DAYWAIT)$coefficients[,4]<0.05)), 
#   length(which(summary(lr$PRIMINC)$coefficients[,4]<0.05)), length(which(summary(lr$MARSTAT)$coefficients[,4]<0.05)))
# 
# #Summarizing missingness results in table (Supplemental Table 3)
# ## Adding indicator for ordered factor
# tmp = t(data.frame(sapply(teds1519,class)))
# miss_tbl["Ordered factor"] = ifelse(tmp[,1]=="ordered","TRUE","FALSE"); rm(tmp)
# 
# ## Adding number of levels in each factor
# miss_tbl["Levels"] = sapply(teds1519, function(x) length(levels(x)))
# 
# ## Adding variable filtered labels
# miss_tbl["variable"] = as.character(row.names(miss_tbl)) # converting row names into a column
# row.names(miss_tbl) = NULL # removing row names
# miss_tbl = merge(miss_tbl, var_meaning[,c("variable", "filtered_label")], by="variable", all.x = TRUE) # adding labels
# 
# ## Modifying table
# miss_tbl = miss_tbl[, c("variable", "filtered_label", "N", "p", "Ordered factor", "Levels")] # changing order of columns
# colnames(miss_tbl) = c("Variable label", "Description", "Incomplete data points", "Incomplete percentage", "Ordered factor", "Levels") # changing column names
# 
# # ##Saving table
# # setwd(results_dir)
# # write.csv(miss_tbl, file = "Missing data summary.csv")
# 

# # Removing Records with Missing Data #NEW LINES OF CODE -------------------
# 
# # Loading data
# setwd(data_dir)
# load("combined_teds.rda")
# 
# # Calculating number and percentage of missingness in each variable (also in Missing Data Analysis section)
# tmp = sapply(teds1519, function(x) sum(is.na(x)))
# miss_tbl = data.frame(N=tmp,p=round(tmp/nrow(teds1519),4))
# miss_tbl = miss_tbl[order(miss_tbl$N, decreasing = T),]; rm(tmp)
# 
# # Identifying variables with high percentage of missingness  (also in Missing Data Analysis section)
# miss_vars = row.names(miss_tbl)[which(miss_tbl$p>miss_th)] # idetifying variables using their names
# 
# # Adding an "unknown" category to variables with high level of missingness
# for(i in seq(miss_vars)){
#   teds1519[which(is.na(teds1519[,miss_vars[i],with=FALSE])),miss_vars[i]] = "Unknown"
# }
# 
# # Excluding incomplete records
# tedsnonmissing = na.omit(teds1519)
# 
# # Saving dataset with complete records
# setwd(data_dir)
# save(tedsnonmissing, file = "tedsnonmissing.rda")

# # Tuning and Evaluation of Models using CV - TIDYMODELS #NEW LINES OF CODE -------------
# 
# # Loading data
# setwd(data_dir)
# load("tedsnonmissing - full data.rda")
# 
# #Splitting complete data into testing and training set
# set.seed(123)
# data_split <- initial_split(tedsnonmissing, prop = .75, strata = DROPOUT)
# data_train <- training(data_split)
# data_test <- testing(data_split)
# 
# # Dividing data into 10 folds (for model evaluation) and each of the folds into 5 sub-folds (for parameter tuning)
# set.seed(773) # setting seed for pseudo-random numbers
# data_nested_cv = nested_cv(data_train, outside=vfold_cv(v=outer_v, strata = DROPOUT), inside=vfold_cv(v=inner_v, strata = DROPOUT)) # nested CV
# 
# print(paste("Data Split Done",Sys.time()))
# 
# # Function to tune and evaluate models
# tune_eval = function(fm,spec,params,sz){
# 
#   ## Creating grid using the max entropy method
#   if(!is.null(params)){ # no tuning in bagging
#     grid =
#       dials::grid_max_entropy(
#         params,
#         size = sz
#       )
#   }
# 
#   ## Hyperparameter tuning and finding the best parameters per outer fold
#   auc_roc = list() # lists to store results
#   for(v in seq(outer_v)){ # looping through each outer CV iteration
# 
# 
#     # Creating cluster of parallel workers
#     cl = makeCluster(cores) #, outfile=""
#     registerDoParallel(cl)
#     print(paste("Cluster Done",Sys.time()))
# 
#     # Declaring training and testing sets from outer CV
#     train_data = training(data_nested_cv$splits[[v]]) # identifying training data in current CV iteration
#     test_data = testing(data_nested_cv$splits[[v]]) # identifying testing data (holdout) in current CV iteration
# 
#     # # Differentiating between CART and CART bagging (not used - bag_tree takes too long)
#     # if(spec$engine=="rpart"){ # CART or bagging
#     #   if(length(spec$eng_args)>0){ # bagging
#     #     model = "bagging"
#     #   }else{ # CART
#     #     model = "CART"
#     #   }
#     #   print(paste("Running fold",v,"for",model,Sys.time()))
#     # }else{ # other models
#     #   model = spec$engine
#     #   print(paste("Running fold",v,"for",model,Sys.time()))
#     # }
# 
#     # Differentiating between CART bagging and random forest
#     if(spec$engine=="ranger"){ # bagging or random forest
#       if(spec$args$mtry[[2]]==31){ # bagging
#         model = "bagging"
#       }else{ # RF
#         model = "RF"
#       }
#       print(paste("Running fold",v,"for",model,Sys.time()))
#     }else{ # other models
#       model = spec$engine
#       print(paste("Running fold",v,"for",model,Sys.time()))
#     }
# 
#     ## Establishing models' recipe
#     if(spec$engine=="xgboost"){ # pre-processing data for XGBoost (one-hot encoding factors)
#       rec = recipe(fm, data = train_data) %>%
#         step_dummy(all_nominal_predictors(), one_hot = TRUE)
#     }else{
#       rec = recipe(fm, data = train_data)
#     }
# 
#     # Workflow
#     wf = workflow() %>%
#       add_recipe(rec) %>%
#       add_model(spec)
# 
#     # Tuning model parameters
#     if(is.null(params)){ # no tunning in bagging
# 
#       # Using the initial workflow as final
#       final_wf = wf
# 
#     }else{ #tuning the rest of the models
#       set.seed(v) # setting seed for pseudo-random numbers
#       tuned = tune::tune_grid(
#         object = wf,
#         resamples = data_nested_cv$inner_resamples[[v]],
#         grid = grid,
#         metrics = yardstick::metric_set(roc_auc),
#         control = tune::control_grid(verbose = TRUE, parallel_over = "everything")
#       )
# 
#       # Identifying the best parameters for the model
#       best_params = tuned %>%
#         tune::select_best("roc_auc")
# 
#       # Using the best parameters in the model
#       tuned_model = spec %>%
#         finalize_model(best_params)
# 
#       # Finalizing workflow
#       final_wf = wf %>%
#         finalize_workflow(select_best(tuned, metric = "roc_auc"))
#     }
# 
#     ## Reverse engineering an rsplit object
#     combined = bind_rows(train_data, test_data)
#     ind = list(analysis = seq(nrow(train_data)), assessment = nrow(train_data) + seq(nrow(test_data)))
#     splits = make_splits(ind, combined)
# 
#     ## Fitting tuned model in training set and evaluating in testing set
#     set.seed(v) # setting seed for pseudo-random numbers
#     final_fit = final_wf %>%
#       last_fit(splits, metrics=yardstick::metric_set(roc_auc))
# 
#     # Extracting ROC AUC from final fit
#     auc_roc[[v]] = final_fit$.metrics[[1]]$.estimate
# 
#     # Stopping the cluster
#     stopCluster(cl)
# 
#     print(paste("Prediction Done",Sys.time()))
# 
#     setwd(fold_dir)
#     v_results = auc_roc[[v]]
#     save(v_results,file = paste("ROC AUC results for ", model," at fold ",v,".RData",sep = ""))
#     print(paste("Saving Done",Sys.time()))
#   }
#   return(unlist(auc_roc))
# }
# 
# # Classification tree specification
# ## Specification
# spec = decision_tree(cost_complexity = tune()) %>%
#   set_mode("classification") %>%
#   set_engine("rpart")
# 
# ## Grid specification
# params =
#   dials::parameters(
#     cost_complexity()
#   ) # identifying parameters to tune
# 
# ## Tuning and evaluation
# cart_auc = tune_eval(fm,spec,params,sz)
# 
# ## Saving preliminary results
# setwd(results_dir)
# save(cart_auc, file = "CART CV.rda"); gc()
# print(paste("CART Done",Sys.time()))
# 
# #Bagged trees specification
# ##Specification
# # spec = bag_tree() %>%
# #   set_mode("classification") %>%
# #   set_engine("rpart", times=250) # takes a very long time
# spec = rand_forest(trees = 250, mtry=31) %>% # more memory consuming but faster than bag_tree
#   set_mode("classification") %>%
#   set_engine("ranger")
# 
# ## Grid specification
# params = NULL # no tuning
# 
# ## Tuning and evaluation
# bag_auc = tune_eval(fm,spec,params,sz)
# 
# ## Saving preliminary results
# setwd(results_dir)
# save(bag_auc, file = "Bagging CV.rda"); gc()
# print(paste("Bagging Done",Sys.time()))
# 
# #Random forest
# ##Specification
# spec = rand_forest(trees = 250, mtry=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("ranger")
# 
# ## Grid specification
# params =
#   dials::parameters(
#     mtry()) %>% # identifying parameters to tune
#   finalize(x = data_nested_cv$splits[[1]]$data %>% select(-DROPOUT)) # providing upper limit for mtry
# 
# ## Tuning and evaluation
# rf_auc = tune_eval(fm,spec,params,sz)
# 
# ## Saving preliminary results
# setwd(results_dir)
# save(rf_auc, file = "RF CV.rda"); gc()
# print(paste("RF Done",Sys.time()))
# 
# # Boosted trees specification
# ## Specification
# spec = boost_tree(trees = 250, learn_rate = tune(), loss_reduction = tune()) %>%
#   set_mode("classification") %>%
#   set_engine("xgboost", objective = "binary:logistic", eval_metric = 'logloss')
# 
# ## Grid specification
# params =
#   dials::parameters(
#     learn_rate(),
#     loss_reduction()
#   )
# 
# ## Tuning and evaluation
# boost_auc = tune_eval(fm,spec,params,sz)
# 
# ## Saving preliminary results
# setwd(results_dir)
# save(boost_auc, file = "Boosting CV.rda"); gc()
# print(paste("Boosting Done",Sys.time()))

# # Merging CV Results ----------------------------------------------------
# 
# # Identifying files per model in fold directory
# setwd(fold_dir) # setting working directory
# fnames = list.files() # listing files in directory
# mod_names = c("rpart", "CART", "bagging", "RF", "xgboost") # listing potential names of models (CART was used when the bagging models are based on rpart)
# files_model = sapply(mod_names, function(x) fnames[grep(x, fnames)]) # files per model
# files_model = files_model[unlist(sapply(files_model,function(x) length(x)>0))] # extracting models with files
# 
# # Loading and merging files per model
# auc_list = list()
# for (m in seq(files_model)){
#   tmp = lapply(files_model[[m]],function(x){load(x);v_results})
#   auc_list[[m]] = mapply(c,tmp)
# }
# auc_df = as.data.frame(do.call(cbind, auc_list))
# colnames(auc_df) = names(files_model); rm(auc_list)
# 
# # Incorporating results of uninterrumpted model runs
# setwd(cv_dir)
# fnames = setdiff(list.files(), list.dirs(recursive = FALSE, full.names = FALSE))
# mod_names = list("cart_auc"="CART", "bag_auc"="bagging", "rf_auc"="RF")
# for(f in fnames){
#   var = load(f); load(f) # loading file name and then the actual file
#   id = which(var==names(mod_names))
#   auc_df = cbind(auc_df, get(names(mod_names)[id]))
#   colnames(auc_df)[ncol(auc_df)] = mod_names[id]
# }
# auc_df = auc_df[,c("CART","bagging","RF","xgboost")] # ordering columns of data frame
# mean_auc = colMeans(auc_df)
# 
# # Saving merged results
# setwd(cv_dir)
# train_id = data_split$in_id # saving indices of training set for future data splits
# save(auc_df, mean_auc, train_id, file = "CVResults.rda")
# 
# 
# # Summary of CV Analysis --------------------------------------------------
# 
# #Loading results
# setwd(cv_dir)
# load("CVResults.rda")
# colnames(auc_df) = c("CART", "Bagged CART", "Random Forest", "Boosted CART")
# 
# #Boxplot of ROC AUC
# setwd(fig_dir)
# df = melt(auc_df)
# names(df) = c("Model","AUC")
# svg("ROC AUC Boxplot.svg", width = 5, height = 3.5)
# ggplot(data=df, aes(x=reorder(Model,AUC,mean), y=AUC)) +
#   geom_boxplot(fill = "gray85",outlier.shape=8,outlier.size=1)+
#   stat_summary(fun=mean, geom="point", shape=13, size=2) +
#   labs(x = "",y = "ROC AUC")+
#   scale_x_discrete(breaks=unique(df$Model))+
#   scale_y_continuous(breaks = seq(0.7,0.74,0.01),labels = seq(0.7,0.74,0.01),limits = c(0.7,0.74)) +
#   theme_light()+
#   my_theme
# dev.off()
# 
# #Bonferroni Correction
# alpha = 1-(1-0.05)^(1/(ncol(auc_df)))
# 
# #Mann-Whitney test with Bonferroni Correction (MSE)
# ##Gives warning when testing a variable against itself
# options(warn = -1)
# p_w_auc = matrix(NA,ncol(auc_df),ncol(auc_df))
# for (i in seq(ncol(auc_df))){
#   for (j in seq(ncol(auc_df))){
#     p_w_auc[i,j] = round(wilcox.test(auc_df[,i],auc_df[,j], paired=FALSE,
#                                var.equal = FALSE,conf.level = 1-alpha)$p.value,5)
#   }
# }
# options(warn = 0)
# p_w_auc = data.frame(p_w_auc)
# names(p_w_auc) = colnames(auc_df)
# row.names(p_w_auc) = colnames(auc_df)
# p_w_auc[lower.tri(p_w_auc)] = ""
# print(p_w_auc)
# 
# # Executing Best-Performing Model on Complete Data #NEW LINES OF CODE --------
# 
# print(paste("Executing Best-Performing Model on Complete Data",Sys.time()))
# 
# # Loading data and results
# setwd(data_dir); load("tedsnonmissing.rda")
# setwd(cv_dir); load("CVResults.rda")
# 
# # Dividing data into training and testing set (using previous split)
# data_train = tedsnonmissing[train_id,]
# data_test = tedsnonmissing[-train_id,]
# 
# print(paste("Loading Done",Sys.time()))
# 
# # Creating cluster of parallel workers
# cl = makeCluster(cores)
# registerDoParallel(cl)
# 
# print(paste("Clustering Done",Sys.time()))
# 
# # Fitting best-performing model in complete training set and evaluating in testing set
# ## Specification
# spec = rand_forest(trees = 250, mtry=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("ranger", importance = "permutation")
# 
# ## Grid specification
# params =
#   dials::parameters(
#     mtry()
#   ) %>% # identifying parameters to tune
#   finalize(x = data_train %>% select(-DROPOUT)) # providing upper limit for mtry
# 
# ## Creating grid using the max entropy method
# grid =
#   dials::grid_max_entropy(
#     params,
#     size = sz
#   )
# 
# ## Dividing training set into 5-folds for tuning
# set.seed(154) # setting seed for pseudo-random numbers
# final_cv = vfold_cv(data = data_train,v=inner_v, strata = DROPOUT)
# 
# ## Establishing models' recipe
# rec = recipe(fm, data = data_train)
# 
# # Workflow
# wf = workflow() %>%
#   add_recipe(rec) %>%
#   add_model(spec)
# 
# ## Tuning model parameters
# set.seed(100) # setting seed for pseudo-random numbers
# tuned = tune::tune_grid(
#   object = wf,
#   resamples = final_cv,
#   grid = grid,
#   metrics = yardstick::metric_set(roc_auc),
#   control = tune::control_grid(verbose = TRUE, parallel_over = "everything")
# )
# 
# ## Identifying the best parameters for the model
# best_params = tuned %>%
#   tune::select_best("roc_auc")
# 
# ## Using the best parameters in the model
# tuned_model = spec %>%
#   finalize_model(best_params)
# 
# ## Finalizing workflow
# final_wf = wf %>%
#   finalize_workflow(select_best(tuned, metric = "roc_auc"))
# 
# print(paste("Tuning Done",Sys.time()))
# 
# ## Reverse engineering an rsplit object
# combined = bind_rows(data_train, data_test)
# ind = list(analysis = seq(nrow(data_train)), assessment = nrow(data_train) + seq(nrow(data_test)))
# splits = make_splits(ind, combined)
# 
# ## Fitting tuned model in training set and evaluating in testing set
# set.seed(150) # setting seed for pseudo-random numbers
# final_fit = final_wf %>%
#   last_fit(splits, metrics=yardstick::metric_set(accuracy, roc_auc, pr_auc))
# 
# # Extracting model information
# model_extract = final_fit %>%
#   pluck(".workflow", 1) %>%
#   extract_fit_parsnip()
# 
# print(paste("Prediction Done",Sys.time()))
# 
# # Stopping the cluster
# stopCluster(cl)
# 
# # Saving results
# setwd(results_dir)
# save(final_fit, model_extract, file = "Main results on complete data.rda")
# 
# print(paste("Saving Done",Sys.time()))

# # Variable Importance - Supplementary Figure 3 #NEW LINES OF CODE -----------------------
# 
# # # Loading data and results
# # setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
# # setwd(results_dir); load("Main results on complete data.rda")
# # 
# # # Saving overall results
# # print(final_fit$.metrics)
# # acc = final_fit$.metrics[[1]]$.estimate[1]
# # auc_roc = final_fit$.metrics[[1]]$.estimate[2]
# # auc_pr = final_fit$.metrics[[1]]$.estimate[3]
# # setwd(results_dir)
# # save(acc, auc_roc, auc_pr, file = "Performace summary on complete records.rda")
# # 
# # # Converting variable importance values into a data frame
# # varimpvalues = model_extract$fit$variable.importance # extracting variable importance
# # importance_df <- data.frame(values=varimpvalues) # converting to data frame
# # varimp = data.frame(variable = as.character(row.names(importance_df)),
# #                     mda = as.numeric(importance_df$values), row.names=NULL) # formatting data frame
# # varimp = merge(varimp, var_meaning, by="variable") # adding variable labels
# # 
# # ## Saving filtered variable importance results
# # setwd(results_dir)
# # write.csv(varimp, file = "Variable Importance - Main Analysis on Complete Records.csv")
# 
# # Loading filtered results
# setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
# setwd(results_dir); varimp = read.csv("Variable Importance - Main Analysis on Complete Records.csv")
# varimp = merge(varimp[,c("variable", "mda")], var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels
# 
# # Making plot
# setwd(fig_dir)
# svg("Variable Importance Plot - Complete Data.svg", width = 6.5, height = 7)
# varimp %>%
#   ggplot(aes(x = mda, y = reorder(filtered_label, mda))) +
#   geom_bar(stat = "identity", fill = "gray50") +
#   labs(title = NULL,
#        x = "Percentage decrease in model accuracy\nwith exclusion of a given variable", y = NULL)+
#   coord_cartesian(xlim=c(0,.06))+
#   theme_light()+
#   my_theme +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1))
# dev.off()
# 
# # Partial Dependence Plots - Response Letter Figure 1 #NEW LINES OF CODE ------------------
# 
# # Loading data and filtered variable importance results
# setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv") # loading variable labels
# load("tedsnonmissing.rda") # loading complete records
# setwd(cv_dir); load("CVResults.rda") # loading previous data split
# data_train = tedsnonmissing[train_id,] # identifying training set
# setwd(results_dir); varimp = read.csv("Variable Importance - Main Analysis on Complete Records.csv") # loading summary of results in complete records
# varimp = merge(varimp, var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels
# varimp = varimp[order(-varimp$mda),] # ordering data frame in order of importance
# # vars = varimp$variable[which(varimp$mda>imp_th)] # identifying the most influential variables (based on importance threshold)
# vars = head(varimp$variable, 10) # identifying the top 10 most influential variables (conversation with MJ on 4/12/2022)
# meaning = c("Service\nsetting", "Geographic\nregion", "Days waited to\nenter treatment", "Primary\nincome source", "Primary source\nof payment", 
#             "Referral\nsource", "Marriage\nstatus", "Medication-assisted\nopioid therapy", "Maximum frequency\nof use", "Number of\nsubstances used")
# 
# # Lists to store partial dependence calculations and plots
# pdp = pdp_plots = list()
# 
# # # Loading complete results of analysis on complete records (comment if partial dependence has been pre-calculated)
# # setwd(results_dir); load("Main results on complete data.rda")
# 
# # Loading pre-calculated partial dependence (comment pdp calculation and level change in loop below)
# setwd(results_dir); load("Partial Dependence Results - Complete Records.rda")
# 
# # Making PDP plots
# for(p in c(3,7)){ #seq(vars)
#   # pdp[[p]] = pdp::partial(model_extract$fit, train = data_train,
#   #                         type = "classification", pred.var = vars[p], plot = FALSE) # estimating partial dependence
#   # 
#   # colnames(pdp[[p]]) = c("variable", "probability") # changing column names
#   # 
#   # # Changing levels of variables with high importance (p==1 is SERVICES [Treatment setting], which was modified previously in the data manipulation process)
#   # if(vars[p]=="REGION"){ # Census region
#   #   levels(pdp[[p]]$variable) = c("U.S. territories", "Northeast", "Midwest", "South", "West")
#   #   
#   # }else if(vars[p]=="PRIMPAY"){ # primary source of payment
#   #   levels(pdp[[p]]$variable) = c("Self-pay", "Private insurance", "Medicare", "Medicaid", "Other government payments", "No charge (free, charity, special research, teaching)", "Other", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="PSOURCE"){ # Referral source
#   #   levels(pdp[[p]]$variable) = c("Individual (includes self-referral)", "Alcohol/drug use care provider", "Other health care provider", "School (educational)",
#   #                                 "Employer/EAP", "Other community referral", "Court/criminal justice referral/DUI/DWI") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if (vars[p]=="PRIMINC"){ # Source of income
#   #   levels(pdp[[p]]$variable) = c("Wages/salary", "Public assistance", "Retirement/pension, disability", "Other", "None", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="METHUSE"){ # use of MOUD
#   #   levels(pdp[[p]]$variable) = c("Yes","No")
#   #   
#   # }else if(vars[p]=="MARSTAT"){ # marital status
#   #   levels(pdp[[p]]$variable) = c("Never married", "Now married", "Separated", "Divorced, widowed", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="FREQMAX"){ # maximum frequency of opioid use
#   #   levels(pdp[[p]]$variable) = c("No use in the past month", "Some use", "Daily use") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="DAYWAIT"){ # days waited to enter treatment
#   #   levels(pdp[[p]]$variable) = c("0", "1-7", "8-14", "15-30", "31 or more", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="NUMSUBS"){ # number of substances used
#   #   levels(pdp[[p]]$variable) = c("1", "2", "3")
#   # }
#   
#   pdp_plots[[p]] <- pdp[[p]] %>%
#     ggplot(aes(x = probability, y = reorder(variable, probability))) +
#     geom_bar(stat = "identity", fill = "gray50") +
#     ylab(meaning[p])+
#     coord_cartesian(xlim=c(0,0.6))+
#     theme_light()+
#     my_theme +
#     theme(axis.title = element_text(size = 8), axis.text = element_text(size = 6.5))+
#     scale_x_continuous(labels = scales::percent_format(accuracy = 1)) # saving PDP plot
#   
#   if(p==length(vars)){
#     pdp_plots[[p]] = pdp_plots[[p]] + xlab("Probability of premature exit")
#     
#   }else{
#     pdp_plots[[p]] = pdp_plots[[p]] + xlab("")
#     
#   }
# }
# 
# # # Saving results
# # setwd(results_dir)
# # save(pdp, file = "Partial Dependence Results - Complete Records.rda")
# 
# # Combining plots
# setwd(fig_dir)
# svg("Partial Dependence Plots - Complete Records.svg", width = 4, height = 2.5)
# plot_grid(plotlist=pdp_plots[c(3,7)], align = "v", nrow = length(pdp[c(3,7)]))
# dev.off()

# # Missing Data Imputation -------------------------------------------------
# 
# # Loading data
# setwd(data_dir)
# load("combined_teds.rda")
# 
# #Splitting data with missing values into testing and training set
# set.seed(123)
# data_split <- initial_split(teds1519, prop = .75, strata = DROPOUT)
# data_train <- training(data_split)
# data_test <- testing(data_split)
# 
# # Creating cluster of parallel workers
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# #Imputation of missing data on the training set
# data_train <- as.data.frame(data_train)
# imputedtrain <- missForest(xmis = data_train, maxiter = 5, ntree = 3, variablewise = FALSE, decreasing = FALSE, verbose = FALSE,
#                         mtry = floor(sqrt(ncol(data_train))), replace = TRUE,
#                         classwt = NULL, cutoff = NULL, strata = NULL,
#                         sampsize = NULL, nodesize = NULL, maxnodes = NULL,
#                         xtrue = NA, parallelize = "variables")
# imputedtraindf<- imputedtrain$ximp
# 
# #How did our missing data imputation perform?
# #outofbagerror <- imputedtrain$OOBerror
# 
# 
# #Imputation of missing data on the test set - ALONE
# data_test <- as.data.frame(data_test)
# imputedtest <- missForest(xmis = data_test, maxiter = 5, ntree = 3, variablewise = FALSE, decreasing = FALSE, verbose = FALSE,
#                            mtry = floor(sqrt(ncol(data_test))), replace = TRUE,
#                            classwt = NULL, cutoff = NULL, strata = NULL,
#                            sampsize = NULL, nodesize = NULL, maxnodes = NULL,
#                            xtrue = NA, parallelize = "variables")
# imputedtestdf <- imputedtest$ximp
# 
# #How did our missing data imputation perform?
# #outofbagerror <- imputedtest$OOBerror
# 
# # Saving imputed datasets
# setwd(data_dir)
# train_id = data_split$in_id
# save(imputedtraindf, imputedtestdf, train_id, file = "imputed_sets.rda")
# 
# # Stopping the cluster
# stopCluster(cl)
# 
# # Data Summary (Supplemental Table 4) -------------------------------------
# 
# ## Note: the order of the levels of PSYPROB (Diagnosed psychological problem), METHUSE (Medication-assisted opioid therapy), and VET (veteran status)
# ## were exchanged manually in the paper's table (No goes first in the rest of the factors)
# 
# # Loading data
# setwd(data_dir)
# load("imputed_sets - full data.rda"); var_meaning = read.csv("Variable Meaning.csv"); level_meaning = read.csv("Level Meaning.csv")
# setDT(imputedtraindf); setDT(imputedtestdf) # converting data frames to data tables
# teds_imp = rbindlist(list(imputedtraindf,imputedtestdf))
# names(teds_imp)[match("AGECAT",names(teds_imp))] = "AGE" # changing name of age column to match Variable Meaning spreadsheet
# levels(teds_imp$SERVICES) = as.character(1:8) # reverting treatment setting back to original levels (for merging)
# teds_imp[,HEROIN:=factor(HEROIN)] # making sure the indicator for heroin is a factor
# level_meaning$meaning[match(c("17-Dec","14-Dec", "7-Jan", "14-Aug"),level_meaning$meaning)] = c("12-17", "12-14", "1-7", "8-14") # correcting errors in level labels (Excel converted them to dates)
# 
# # Summarizing categorical data
# cats = sapply(teds_imp, is.factor) #Identifying categorical columns
# lvs = sapply(teds_imp[,..cats],levels) #Identifying levels in categorical variables
# names(lvs)[1]="AGE" # changing name of age column to match Variable Meaning spreadsheet
# 
# # Sorting variables according to order in Table S4 of the paper
# p_order = c('Age', 'Sex', 'Race', 'Ethnicity', 'Arrests in 30 days prior to treatment',
#             'Diagnosed psychological problem', 'Living situation', 'Employment status',
#             'Marriage status', 'Primary income source', 'Education', 'Veteran status', 'Geographic region',
#             'Maximum frequency of use', 'Injection use',
#             'Age of first substance use', 'Number of substances used',
#             'Alcohol use', 'Inhalant use', 'Marijuana use', 'Sedative use',
#             'Tranquilizer use', 'Hallucinogen use', 'Stimulant use',
#             'Heroin use', 'Days waited to enter treatment', 'Medication-assisted opioid therapy',
#             'Prior treatment episodes', 'Service setting', 'Referral source',
#             'Primary source of payment', 'Premature treatment exit') # order of predictors in Table S4 of the paper
# m_order = match(p_order, var_meaning$filtered_label) # column number matching the order in the paper
# var_order = var_meaning$variable[m_order] # getting variable names from column numbers
# lvs = lvs[var_order] # sorting variables in list of levels
# 
# #Summary of categorical variables (number and proportion)
# cat_sum = c()
# for (d in levels(teds_imp$DROPOUT)){
#   n_dropout = length(which(teds_imp$DROPOUT==d))
#   temp = teds_imp[teds_imp$DROPOUT==d,]
#   for (c in seq(lvs)){
#     for(l in seq(lvs[[c]])){
#       cat_var = names(lvs[c])
#       var_lv = lvs[[c]][l]
#       col = match(names(lvs[c]),names(temp))
#       n = length(which(temp[,..col]==lvs[[c]][l]))
#       
#       cat_sum = rbind(cat_sum,c(d,cat_var,var_lv,n,paste0(round(n/n_dropout*100,1),"%")))
#     }
#   }
# }
# 
# # Formatting results
# cat_sum = data.frame(cat_sum) # converting results to data frame
# colnames(cat_sum) = c("DROPOUT", "Variable", "Level", "N", "p") # renaming columns
# counts = prettyNum(cat_sum$N[cat_sum$Variable=="DROPOUT"][c(1,4)], big.mark=",") # storing counts of not premature exit and premature exit
# cat_sum$N = prettyNum(cat_sum$N, big.mark=",") # formatting counts
# cat_sum$sum = paste0(cat_sum$N," (", cat_sum$p,")") # concatenating counts and percentages
# cat_sum = cat_sum[,setdiff(names(cat_sum),c("N","p"))] # removing unnecesary columns
# 
# # "Unmelting" data frame (putting results in different columns side to side)
# cat_sum0 = cat_sum[cat_sum$DROPOUT==0,]
# cat_sum1 = cat_sum[cat_sum$DROPOUT==1,]
# cat_sum = cbind(cat_sum0[,c("Variable","Level","sum")],cat_sum1[,"sum"])
# colnames(cat_sum) = c("variable", "Level", "No", "Yes") # renaming columns
# 
# # Adding labels and count of not premature exit and premature exit at the beginning of data frame
# ## Level labels
# level_meaning$key = paste0(level_meaning$variable,level_meaning$level) # adding key containing variable names and level names
# cat_sum$key = paste0(cat_sum$variable,cat_sum$Level) # adding key containing variable names and level names
# cat_sum = merge(cat_sum, level_meaning[,c("key", "meaning")], by="key", sort = F, all.x = TRUE) # adding labels
# 
# ## Variable labels
# cat_sum = merge(cat_sum, var_meaning[,c("variable", "filtered_label")], by="variable", sort = F, all.x = TRUE) # adding labels
# 
# ## Formatting table and adding counts
# cat_sum = cat_sum[,c("filtered_label", "meaning", "No", "Yes")] # removing variable and level names
# colnames(cat_sum) = c("Variable", "Level", "No", "Yes") # renaming columns
# cat_sum$Level = paste0("'",cat_sum$Level) # adding character to avoid Excel from converting values into dates
# cat_sum = rbind(c("","",counts),cat_sum) # adding counts
# 
# # Saving table
# setwd(results_dir)
# write.csv(cat_sum, file = "Data Summary - Imputed.csv", row.names=F)
# 
# # Descriptive results for paper
# ## Race
# c("White Race",length(which(teds_imp$RACE==5)), round(length(which(teds_imp$RACE==5))/nrow(teds_imp), 3)) # count and proportion of white people
# 
# ## Substance use
# c("Co-occurring  stimulant use",length(which(teds_imp$STIMFLAG==1)), round(length(which(teds_imp$STIMFLAG==1))/nrow(teds_imp), 3)) # count and proportion of co-occurring  stimulant use
# c("Co-occurring  marijuana use",length(which(teds_imp$MARFLG==1)), round(length(which(teds_imp$MARFLG==1))/nrow(teds_imp), 3)) # count and proportion of co-occurring  marijuana use
# c("Co-occurring  alcohol use",length(which(teds_imp$ALCFLG==1)), round(length(which(teds_imp$ALCFLG==1))/nrow(teds_imp), 3)) # count and proportion of co-occurring  alcohol use
# # c(length(which(teds_imp$TRNQFLAG==1)), round(length(which(teds_imp$TRNQFLAG==1))/nrow(teds_imp), 3)) # count and proportion of co-occurring  tranquilizer use
# # c(length(which(teds_imp$INHFLG==1)), round(length(which(teds_imp$INHFLG==1))/nrow(teds_imp), 3)) # count and proportion of co-occurring  inhalant use
# # c(length(which(teds_imp$SEDFLAG==1)), round(length(which(teds_imp$SEDFLAG==1))/nrow(teds_imp), 3)) # count and proportion of co-occurring  sedative use
# 
# ## Dropout
# c("Premature treatment exit",length(which(teds_imp$DROPOUT==1)), round(length(which(teds_imp$DROPOUT==1))/nrow(teds_imp), 3)) # count and proportion of dropout
# 
# # ## Age groups
# # for (a in levels(teds_imp$AGE)){
# #   print(c(a, length(which(teds_imp$AGE==a)), round(length(which(teds_imp$AGE==a))/nrow(teds_imp), 3))) # count and proportion of people in each age group
# # }
# c("25-34 age group", length(which(teds_imp$AGE==3)), round(length(which(teds_imp$AGE==3))/nrow(teds_imp), 3)) # count and proportion of people in 25-34 age group

# # Executing Best-Performing Model on Imputed Data #NEW LINES OF CODE --------
# 
# print(paste("Executing Best-Performing Model on Imputed Data",Sys.time()))
# 
# # Loading data
# setwd(data_dir)
# load("imputed_sets.rda")
# 
# print(paste("Loading Done",Sys.time()))
# 
# # Creating cluster of parallel workers
# cl = makeCluster(cores)
# registerDoParallel(cl)
# 
# print(paste("Clustering Done",Sys.time()))
# 
# # Fitting best-performing model in imputed training set and evaluating in imputed testing set
# ## Specification
# spec = rand_forest(trees = 250, mtry=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("ranger", importance = "permutation")
# 
# ## Grid specification
# params =
#   dials::parameters(
#     mtry()
#   ) %>% # identifying parameters to tune
#   finalize(x = imputedtraindf %>% select(-DROPOUT)) # providing upper limit for mtry
# 
# ## Creating grid using the max entropy method
# grid =
#   dials::grid_max_entropy(
#     params,
#     size = sz
#   )
# 
# ## Dividing training set into 5-folds for tuning
# set.seed(154) # setting seed for pseudo-random numbers
# final_cv = vfold_cv(data = imputedtraindf,v=inner_v, strata = DROPOUT)
# 
# ## Establishing models' recipe
# rec = recipe(fm, data = imputedtraindf)
# 
# # Workflow
# wf = workflow() %>%
#   add_recipe(rec) %>%
#   add_model(spec)
# 
# ## Tuning model parameters
# set.seed(100) # setting seed for pseudo-random numbers
# tuned = tune::tune_grid(
#   object = wf,
#   resamples = final_cv,
#   grid = grid,
#   metrics = yardstick::metric_set(roc_auc),
#   control = tune::control_grid(verbose = TRUE, parallel_over = "everything")
# )
# 
# ## Identifying the best parameters for the model
# best_params = tuned %>%
#   tune::select_best("roc_auc")
# 
# ## Using the best parameters in the model
# tuned_model = spec %>%
#   finalize_model(best_params)
# 
# ## Finalizing workflow
# final_wf = wf %>%
#   finalize_workflow(select_best(tuned, metric = "roc_auc"))
# 
# print(paste("Tuning Done",Sys.time()))
# 
# ## Reverse engineering an rsplit object
# combined = bind_rows(imputedtraindf, imputedtestdf)
# ind = list(analysis = seq(nrow(imputedtraindf)), assessment = nrow(imputedtraindf) + seq(nrow(imputedtestdf)))
# splits = make_splits(ind, combined)
# 
# ## Fitting tuned model in training set and evaluating in testing set
# set.seed(150) # setting seed for pseudo-random numbers
# final_fit = final_wf %>%
#   last_fit(splits, metrics=yardstick::metric_set(accuracy, roc_auc, pr_auc))
# 
# # Extracting model information
# model_extract = final_fit %>%
#   pluck(".workflow", 1) %>%
#   extract_fit_parsnip()
# 
# print(paste("Prediction Done",Sys.time()))
# 
# # Stopping the cluster
# stopCluster(cl)
# 
# # Saving results
# setwd(results_dir)
# save(final_fit, model_extract, file = "Main results on imputed data.rda")
# 
# print(paste("Saving Done",Sys.time()))
# 
# Variable Importance - Figure 2 #NEW LINES OF CODE -----------------------

# # Loading data and results
# setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
# setwd(results_dir); load("Main results on imputed data.rda")
#
# # Saving overall results
# print(final_fit$.metrics)
# acc = final_fit$.metrics[[1]]$.estimate[1]
# auc_roc = final_fit$.metrics[[1]]$.estimate[2]
# auc_pr = final_fit$.metrics[[1]]$.estimate[3]
# setwd(results_dir)
# save(acc, auc_roc, auc_pr, file = "Performace summary on imputed data.rda")
#
# # Converting variable importance values into a data frame
# varimpvalues = model_extract$fit$variable.importance # extracting variable importance
# importance_df <- data.frame(values=varimpvalues) # converting to data frame
# varimp = data.frame(variable = as.character(row.names(importance_df)),
#                     mda = as.numeric(importance_df$values), row.names=NULL) # formatting data frame
# varimp = merge(varimp, var_meaning, by="variable") # adding variable labels
#
# ## Saving filtered variable importance results
# setwd(results_dir)
# write.csv(varimp, file = "Variable Importance - Main Analysis on Imputed Data.csv")

# Loading filtered results
setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
setwd(results_dir); varimp = read.csv("Variable Importance - Main Analysis on Imputed Data.csv")
varimp = merge(varimp[,c("variable", "mda")], var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels

# Making plot
setwd(fig_dir)
svg("Variable Importance Plot - Imputed Data.svg", width = 6.5, height = 7)
varimp %>%
  ggplot(aes(x = mda, y = reorder(filtered_label, mda))) +
  geom_bar(stat = "identity", fill = "gray50") +
  labs(title = NULL,
       x = "Percentage decrease in model accuracy\nwith exclusion of a given variable", y = NULL)+
  coord_cartesian(xlim=c(0,.06))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

# Partial Dependence Plots - Figure 3 #NEW LINES OF CODE ------------------

# Loading data and filtered variable importance results
setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv") # loading variable labels
load("imputed_sets.rda") # loading imputed data sets
setwd(results_dir); varimp = read.csv("Variable Importance - Main Analysis on Imputed Data.csv") # loading summary of results in imputed data
varimp = merge(varimp, var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels
varimp = varimp[order(-varimp$mda),] # ordering data frame in order of importance
# vars = varimp$variable[which(varimp$mda>imp_th)] # identifying the most influential variables (based on importance threshold)
vars = head(varimp$variable, 10) # identifying the top 10 most influential variables (conversation with MJ on 4/12/2022)
meaning = c("Service\nsetting", "Geographic\nregion", "Primary source\nof payment", "Referral\nsource", "Primary\nincome source",
            "Planned use\nof MOUD", "Employment\nstatus", "Maximum frequency\nof use", "Days waited to\nenter treatment", "Number of\nsubstances used")

# Lists to store partial dependence calculations and plots
pdp = pdp_plots = list()

# # Loading complete results of analysis on imputed data (comment if partial dependence has been pre-calculated)
# setwd(results_dir); load("Main results on imputed data.rda")

# Loading pre-calculated partial dependence (comment pdp calculation and level change in loop below)
setwd(results_dir); load("Partial Dependence Results - Imputed Data.rda")

# Making PDP plots
for(p in seq(vars)){
  # pdp[[p]] = pdp::partial(model_extract$fit, train = imputedtraindf,
  #                               type = "classification", pred.var = vars[p], plot = FALSE) # estimating partial dependence
  #
  # colnames(pdp[[p]]) = c("variable", "probability") # changing column names
  #
  # # Changing levels of variables with high importance (p==1 is SERVICES [Treatment setting], which was modified previously in the data manipulation process)
  # if(vars[p]=="REGION"){ # Census region
  #   levels(pdp[[p]]$variable) = c("U.S. territories", "Northeast", "Midwest", "South", "West")
  #
  # }else if(vars[p]=="PRIMPAY"){ # primary source of payment
  #   levels(pdp[[p]]$variable) = c("Self-pay", "Private insurance", "Medicare", "Medicaid", "Other government payments", "No charge (free, charity, special research, teaching)", "Other") #, "Missing/unknown/not collected/invalid"
  #
  # }else if(vars[p]=="PSOURCE"){ # Referral source
  #   levels(pdp[[p]]$variable) = c("Individual (includes self-referral)", "Alcohol/drug use care provider", "Other health care provider", "School (educational)",
  #                                 "Employer/EAP", "Other community referral", "Court/criminal justice referral/DUI/DWI") #, "Missing/unknown/not collected/invalid"
  #
  # }else if (vars[p]=="PRIMINC"){ # Source of income
  #   levels(pdp[[p]]$variable) = c("Wages/salary", "Public assistance", "Retirement/pension, disability", "Other", "None") #, "Missing/unknown/not collected/invalid"
  #
  # }else if(vars[p]=="METHUSE"){
  #   levels(pdp[[p]]$variable) = c("Yes","No")
  #
  # }else if(vars[p]=="EMPLOY"){
  #   levels(pdp[[p]]$variable) = c("Full-time", "Part-time", "Unemployed", "Not in labor force") #, "Missing/unknown/not collected/invalid"
  #
  # }else if(vars[p]=="FREQMAX"){
  #   levels(pdp[[p]]$variable) = c("No use in the past month", "Some use", "Daily use") #, "Missing/unknown/not collected/invalid"
  #
  # }else if(vars[p]=="DAYWAIT"){
  #   levels(pdp[[p]]$variable) = c("0", "1-7", "8-14", "15-30", "31 or more") #, "Missing/unknown/not collected/invalid"
  #
  # }

  pdp_plots[[p]] <- pdp[[p]] %>%
    ggplot(aes(x = probability, y = reorder(variable, probability))) +
    geom_bar(stat = "identity", fill = "gray50") +
    ylab(meaning[p])+
    coord_cartesian(xlim=c(0,0.6))+
    theme_light()+
    my_theme +
    theme(axis.title = element_text(size = 8), axis.text = element_text(size = 6.5))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) # saving PDP plot

  if(p==length(vars)){
    pdp_plots[[p]] = pdp_plots[[p]] + xlab("Probability of premature exit")

  }else{
    pdp_plots[[p]] = pdp_plots[[p]] + xlab("")

  }
}

# # Saving results
# setwd(results_dir)
# save(pdp, file = "Partial Dependence Results - Imputed Data.rda")

# Combining plots
setwd(fig_dir)
svg("Partial Dependence Plots.svg", width = 6.5, height = 11)
plot_grid(plotlist=pdp_plots, align = "v", nrow = length(pdp))
dev.off()

# # Analysis by LOS Category #NEW LINES OF CODE -----------------------------
# 
# print(paste("Executing Analysis by LOS Category",Sys.time()))
# 
# # Loading data
# setwd(data_dir)
# load("combined_teds.rda")#; rm(teds1519); gc()
# load("imputed_sets.rda")
# 
# # # Counting the number and percentage of records in each category
# # t(rbind(table(los_cat), table(los_cat)/sum(table(los_cat))))
# 
# # Converting data frames to data tables
# setDT(imputedtraindf); setDT(imputedtestdf)
# 
# # Adding LOS categories based on initial data split
# imputedtraindf[,los_cat:=los_cat[train_id]]
# imputedtestdf[,los_cat:=los_cat[-train_id]]
# 
# # Splitting data tables according to LOS category
# train_los_list = split(imputedtraindf, by="los_cat", sorted = T)
# test_los_list = split(imputedtestdf, by="los_cat", sorted = T)
# 
# # Establishing model's formula (excluding LOS category as a predictor)
# fm_los = as.formula(paste0("DROPOUT~",
#                            paste0(colnames(imputedtraindf)[-which(colnames(imputedtraindf)%in%c("DROPOUT","los_cat"))], collapse = "+")))
# 
# # Creating cluster of parallel workers
# cl = makeCluster(cores)
# registerDoParallel(cl)
# 
# # Fitting best-performing model in subsets of training set and evaluating in subsets of testing set (assuming RF for the moment)
# final_fit = model_extract = list() # lists to store variable importance and performance metrics
# for(los in seq(train_los_list)){
#   ## Specification
#   spec = rand_forest(trees = 250, mtry=tune()) %>%
#     set_mode("classification") %>%
#     set_engine("ranger", importance = "permutation")
# 
#   ## Grid specification
#   params =
#     dials::parameters(
#       mtry()
#     ) %>% # identifying parameters to tune
#     finalize(x = train_los_list[[los]] %>% select(-DROPOUT)) # providing upper limit for mtry
# 
#   ## Creating grid using the max entropy method
#   grid =
#     dials::grid_max_entropy(
#       params,
#       size = sz
#     )
# 
#   ## Dividing training set into 5-folds for tuning
#   set.seed(los) # setting seed for pseudo-random numbers
#   final_cv = vfold_cv(data = train_los_list[[los]],v=5, strata = DROPOUT)
# 
#   ## Establishing models' recipe
#   rec = recipe(fm_los, data = train_los_list[[los]])
# 
#   # Workflow
#   wf = workflow() %>%
#     add_recipe(rec) %>%
#     add_model(spec)
# 
#   ## Tuning model parameters
#   set.seed(los) # setting seed for pseudo-random numbers
#   tuned = tune::tune_grid(
#     object = wf,
#     resamples = final_cv,
#     grid = grid,
#     metrics = yardstick::metric_set(roc_auc, pr_auc),
#     control = tune::control_grid(verbose = TRUE, parallel_over = "everything")
#   )
# 
#   ## Identifying the best parameters for the model
#   best_params = tuned %>%
#     tune::select_best("roc_auc")
# 
#   ## Using the best parameters in the model
#   tuned_model = spec %>%
#     finalize_model(best_params)
# 
#   ## Finalizing workflow
#   final_wf = wf %>%
#     finalize_workflow(select_best(tuned, metric = "roc_auc"))
# 
#   ## Reverse engineering an rsplit object
#   combined = bind_rows(train_los_list[[los]], test_los_list[[los]])
#   ind = list(analysis = seq(nrow(train_los_list[[los]])), assessment = nrow(train_los_list[[los]]) + seq(nrow(test_los_list[[los]])))
#   splits = make_splits(ind, combined)
# 
#   ## Fitting tuned model in training set and evaluating in testing set
#   ## (use final_fit$.metrics[[1]] to extract ROC AUC and PR AUC)
#   set.seed(los) # setting seed for pseudo-random numbers
#   final_fit[[los]] = final_wf %>%
#     last_fit(splits, metrics=yardstick::metric_set(accuracy, roc_auc, pr_auc))
# 
#   # Extracting model information (use model_extract$fit$variable.importance to extract variable importance)
#   model_extract[[los]] = final_fit[[los]] %>%
#     pluck(".workflow", 1) %>%
#     extract_fit_parsnip()
# }
# 
# # Stopping the cluster
# stopCluster(cl)
# 
# # Saving results
# setwd(results_dir)
# save(final_fit, model_extract, file = "Results_LOS.rda")

# Variable Importance - Figure 4 #NEW LINES OF CODE ---------

# # Loading data and results
# setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
# load("combined_teds.rda"); rm(teds1519); gc()
# setwd(results_dir); load("Results_LOS.rda")
#
# # Converting variable importance values into a data frame
# varimpvalues = sapply(model_extract, function(x) x$fit$variable.importance) # extracting variable importance
# importance_df = data.frame(varimpvalues) # converting to data frame
# varimp = data.frame(variable = as.character(row.names(importance_df)),importance_df, row.names=NULL); rm(importance_df) # formatting data frame
# colnames(varimp)[-1] = as.character(sort(unique(los_cat))) # renaming appropriate columns
# varimp = merge(varimp, var_meaning, by="variable") # adding variable labels
# varimp = reshape2::melt(varimp, id.vars = c("variable","label"), variable.name = "LOS", value.name = "mda") # melting data frame
#
# ## Saving filtered variable importance results
# setwd(results_dir)
# write.csv(varimp, file = "Variable Importance - Analysis by LOS Category.csv")

# Loading filtered results
setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
setwd(results_dir); varimp = read.csv("Variable Importance - Analysis by LOS Category.csv")
varimp$LOS = factor(varimp$LOS, levels = c("1", "2-7", "8-30", "31-60", "61-120", "121-365", ">365"))
varimp = merge(varimp[,c("variable", "LOS", "mda")], var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels

# # Including variables with an importance above threshold
# varimp_sub = varimp[which(varimp$mda>imp_th),]
# varimp_sub = droplevels(varimp_sub)
# los_cat = levels(varimp_sub$LOS)

# Including 10 most important variables (Conversation with MJ on 4/12/2022)
los_cat = levels(varimp$LOS)
varimp_sub = list()
for (los in los_cat){
  tmp = varimp[which(varimp$LOS==los),]
  varimp_sub[[los]] = head(tmp[order(tmp$mda,decreasing = T),],10)
}

# Making plots
los_p = list()
for(p in los_cat){
  # los_p[[p]] = varimp_sub[varimp_sub$LOS==p,] %>% # exclusion by importance threshold
  los_p[[p]] = varimp_sub[[p]] %>% # exclusion by number of predictors
    ggplot(aes(x = mda, y = reorder(filtered_label, mda))) +
    geom_bar(stat = "identity", fill = "gray50") +
    scale_x_continuous(breaks = seq(0,0.1,0.02), labels = scales::percent_format(accuracy = 1), limits = c(0,0.101))+
    scale_y_discrete(position = "left")+
    theme_light()+
    my_theme +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=9), axis.text = element_text(size = 7),
          axis.title = element_text(size = 7))

  if(p==los_cat[length(los_cat)]){
    los_p[[p]] = los_p[[p]] + labs(title = paste("Length of Stay:", p,"Days"),
                                   x = "Percentage decrease in model accuracy with exclusion of a given variable", y = NULL)
  }else{
    los_p[[p]] = los_p[[p]] + labs(title = paste("Length of Stay:", p,"Days"),
                                   x = NULL, y = NULL)
  }
}

## Combining plots
setwd(fig_dir)
svg("Variable Importance Plot - Analysis by LOS Category.svg", width = 6.5, height = 10)
plot_grid(plotlist=los_p, align = "v", nrow = length(los_cat)) #, rel_widths = c(0.8, 1, 1, 1, 1, 1, 1, 1)
dev.off()

# # Partial Dependence Plots - Response Letter Figure 1 #NEW LINES OF CODE ------------------
# 
# # Loading data and filtered variable importance results
# setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv") # loading variable labels
# load("tedsnonmissing.rda") # loading complete records
# setwd(cv_dir); load("CVResults.rda") # loading previous data split
# data_train = tedsnonmissing[train_id,] # identifying training set
# setwd(results_dir); varimp = read.csv("Variable Importance - Main Analysis on Complete Records.csv") # loading summary of results in complete records
# varimp = merge(varimp, var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels
# varimp = varimp[order(-varimp$mda),] # ordering data frame in order of importance
# # vars = varimp$variable[which(varimp$mda>imp_th)] # identifying the most influential variables (based on importance threshold)
# vars = head(varimp$variable, 10) # identifying the top 10 most influential variables (conversation with MJ on 4/12/2022)
# meaning = c("Service\nsetting", "Geographic\nregion", "Days waited to\nenter treatment", "Primary\nincome source", 
#             "Primary source\nof payment", "Referral\nsource", "Marriage\nstatus", "Medication-assisted\nopioid therapy", 
#             "Maximum frequency\nof use", "Number of\nsubstances used")
# 
# # Lists to store partial dependence calculations and plots
# pdp = pdp_plots = list()
# 
# # # Loading complete results of analysis on complete records (comment if partial dependence has been pre-calculated)
# # setwd(results_dir); load("Main results on complete data.rda")
# 
# # Loading pre-calculated partial dependence (comment pdp calculation and level change in loop below)
# setwd(results_dir); load("Partial Dependence Results - Complete Records.rda")
# 
# # Making PDP plots
# for(p in c(3,7)){ #seq(vars)
#   # pdp[[p]] = pdp::partial(model_extract$fit, train = data_train,
#   #                         type = "classification", pred.var = vars[p], plot = FALSE) # estimating partial dependence
#   # 
#   # colnames(pdp[[p]]) = c("variable", "probability") # changing column names
#   # 
#   # # Changing levels of variables with high importance (p==1 is SERVICES [Treatment setting], which was modified previously in the data manipulation process)
#   # if(vars[p]=="REGION"){ # Census region
#   #   levels(pdp[[p]]$variable) = c("U.S. territories", "Northeast", "Midwest", "South", "West")
#   #   
#   # }else if(vars[p]=="PRIMPAY"){ # primary source of payment
#   #   levels(pdp[[p]]$variable) = c("Self-pay", "Private insurance", "Medicare", "Medicaid", "Other government payments", "No charge (free, charity, special research, teaching)", "Other", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="PSOURCE"){ # Referral source
#   #   levels(pdp[[p]]$variable) = c("Individual (includes self-referral)", "Alcohol/drug use care provider", "Other health care provider", "School (educational)",
#   #                                 "Employer/EAP", "Other community referral", "Court/criminal justice referral/DUI/DWI") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if (vars[p]=="PRIMINC"){ # Source of income
#   #   levels(pdp[[p]]$variable) = c("Wages/salary", "Public assistance", "Retirement/pension, disability", "Other", "None", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="METHUSE"){ # use of MOUD
#   #   levels(pdp[[p]]$variable) = c("Yes","No")
#   #   
#   # }else if(vars[p]=="MARSTAT"){ # marital status
#   #   levels(pdp[[p]]$variable) = c("Never married", "Now married", "Separated", "Divorced, widowed", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="FREQMAX"){ # maximum frequency of opioid use
#   #   levels(pdp[[p]]$variable) = c("No use in the past month", "Some use", "Daily use") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="DAYWAIT"){ # days waited to enter treatment
#   #   levels(pdp[[p]]$variable) = c("0", "1-7", "8-14", "15-30", "31 or more", "Unknown") #, "Missing/unknown/not collected/invalid"
#   #   
#   # }else if(vars[p]=="NUMSUBS"){ # number of substances used
#   #   levels(pdp[[p]]$variable) = c("1", "2", "3")
#   # }
#   
#   pdp_plots[[p]] <- pdp[[p]] %>%
#     ggplot(aes(x = probability, y = reorder(variable, probability))) +
#     geom_bar(stat = "identity", fill = "gray50") +
#     ylab(meaning[p])+
#     coord_cartesian(xlim=c(0,0.6))+
#     theme_light()+
#     my_theme +
#     theme(axis.title = element_text(size = 9), axis.text = element_text(size = 6.5))+
#     scale_x_continuous(labels = scales::percent_format(accuracy = 1)) # saving PDP plot
#   
#   if(p==length(vars)){
#     pdp_plots[[p]] = pdp_plots[[p]] + xlab("Probability of premature exit")
#     
#   }else{
#     pdp_plots[[p]] = pdp_plots[[p]] + xlab("")
#     
#   }
# }
# 
# # # Saving results
# # setwd(results_dir)
# # save(pdp, file = "Partial Dependence Results - Complete Records.rda")
# 
# # Combining plots
# setwd(fig_dir)
# svg("Partial Dependence Plots - Complete Records.svg", width = 4, height = 2.5)
# plot_grid(plotlist=pdp_plots[c(3,7)], align = "v", nrow = length(pdp[c(3,7)]))
# dev.off()

# # Analysis by Treatment Setting #NEW LINES OF CODE ------------------------
# 
# print(paste("Executing Analysis by Treatment Setting",Sys.time()))
# 
# # Loading data
# setwd(data_dir)
# load("combined_teds.rda"); rm(teds1519); gc()
# load("imputed_sets.rda")
# 
# # Converting data frames to data tables
# setDT(imputedtraindf); setDT(imputedtestdf)
# 
# # Adding treatment setting categories based on initial data split
# imputedtraindf[,setting:=tx_set[train_id]]
# imputedtestdf[,setting:=tx_set[-train_id]]
# 
# # Splitting data tables by treatment setting
# train_ts_list = split(imputedtraindf, by="setting", sorted = T)
# test_ts_list = split(imputedtestdf, by="setting", sorted = T)
# 
# # Establishing model's formula (excluding treatment setting category as a predictor)
# fm_ts = as.formula(paste0("DROPOUT~",
#                            paste0(colnames(imputedtraindf)[-which(colnames(imputedtraindf)%in%c("DROPOUT","setting"))], collapse = "+")))
# 
# # Creating cluster of parallel workers
# cl = makeCluster(cores)
# registerDoParallel(cl)
# 
# # Fitting best-performing model in subsets of training set and evaluating in subsets of testing set (assuming RF for the moment)
# final_fit = model_extract = list() # lists to store variable importance and performance metrics
# for(ts in seq(train_ts_list)){
#   ## Specification
#   spec = rand_forest(trees = 250, mtry=tune()) %>%
#     set_mode("classification") %>%
#     set_engine("ranger", importance = "permutation")
# 
#   ## Grid specification
#   params =
#     dials::parameters(
#       mtry()
#     ) %>% # identifying parameters to tune
#     finalize(x = train_ts_list[[ts]] %>% select(-DROPOUT)) # providing upper limit for mtry
# 
#   ## Creating grid using the max entropy method
#   grid =
#     dials::grid_max_entropy(
#       params,
#       size = sz
#     )
# 
#   ## Dividing training set into 5-folds for tuning
#   set.seed(ts) # setting seed for pseudo-random numbers
#   final_cv = vfold_cv(data = train_ts_list[[ts]],v=5, strata = DROPOUT)
# 
#   ## Establishing models' recipe
#   rec = recipe(fm_ts, data = train_ts_list[[ts]])
# 
#   # Workflow
#   wf = workflow() %>%
#     add_recipe(rec) %>%
#     add_model(spec)
# 
#   ## Tuning model parameters
#   set.seed(ts) # setting seed for pseudo-random numbers
#   tuned = tune::tune_grid(
#     object = wf,
#     resamples = final_cv,
#     grid = grid,
#     metrics = yardstick::metric_set(roc_auc, pr_auc),
#     control = tune::control_grid(verbose = TRUE, parallel_over = "everything")
#   )
# 
#   ## Identifying the best parameters for the model
#   best_params = tuned %>%
#     tune::select_best("roc_auc")
# 
#   ## Using the best parameters in the model
#   tuned_model = spec %>%
#     finalize_model(best_params)
# 
#   ## Finalizing workflow
#   final_wf = wf %>%
#     finalize_workflow(select_best(tuned, metric = "roc_auc"))
# 
#   ## Reverse engineering an rsplit object
#   combined = bind_rows(train_ts_list[[ts]], test_ts_list[[ts]])
#   ind = list(analysis = seq(nrow(train_ts_list[[ts]])), assessment = nrow(train_ts_list[[ts]]) + seq(nrow(test_ts_list[[ts]])))
#   splits = make_splits(ind, combined)
# 
#   ## Fitting tuned model in training set and evaluating in testing set
#   ## (use final_fit$.metrics[[1]] to extract ROC AUC and PR AUC)
#   set.seed(ts) # setting seed for pseudo-random numbers
#   final_fit[[ts]] = final_wf %>%
#     last_fit(splits, metrics=yardstick::metric_set(accuracy, roc_auc, pr_auc))
# 
#   # Extracting model information (use model_extract$fit$variable.importance to extract variable importance)
#   model_extract[[ts]] = final_fit[[ts]] %>%
#     pluck(".workflow", 1) %>%
#     extract_fit_parsnip()
# }
# 
# # Stopping the cluster
# stopCluster(cl)
# 
# # Saving results
# setwd(results_dir)
# save(final_fit, model_extract, file = "Results_TS.rda")
# 
# Variable Importance - Figure 5 #NEW LINES OF CODE ---------

# # Loading data and results
# setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
# load("combined_teds.rda"); rm(teds1519); gc()
# setwd(results_dir); load("Results_TS.rda")
#
# # Converting variable importance values into a data frame
# varimpvalues = sapply(model_extract, function(x) x$fit$variable.importance) # extracting variable importance
# importance_df = data.frame(varimpvalues) # converting to data frame
# varimp = data.frame(variable = as.character(row.names(importance_df)),importance_df, row.names=NULL); rm(importance_df) # formatting data frame
# colnames(varimp)[-1] = as.character(sort(unique(tx_set))) # renaming appropriate columns
# varimp = merge(varimp, var_meaning, by="variable") # adding variable labels
# varimp = reshape2::melt(varimp, id.vars = c("variable","label"), variable.name = "TS", value.name = "mda") # melting data frame
#
# ## Saving filtered variable importance results
# setwd(results_dir)
# write.csv(varimp, file = "Variable Importance - Analysis by Treatment Setting.csv")

# Loading filtered results
setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
setwd(results_dir); varimp = read.csv("Variable Importance - Analysis by Treatment Setting.csv")
varimp$TS = factor(varimp$TS, levels = c("Ambulatory", "Detox", "Rehab"))
varimp = merge(varimp[,c("variable", "TS", "mda")], var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels

# Renaming "Service setting" variable to appropriate setting (Conversation with MJ on 4/12/2022)
varimp$variableTS = paste0(varimp$variable,varimp$TS) # concatenating variable name with treatment type category
varimp$filtered_label[which(varimp$variableTS=="SERVICESAmbulatory")] = "Ambulatory service type" # Ambulatory treatment setting
varimp$filtered_label[which(varimp$variableTS=="SERVICESDetox")] = "Detox service type" # Detox treatment setting
varimp$filtered_label[which(varimp$variableTS=="SERVICESRehab")] = "Rehab service type" # Rehab treatment setting

# # Including variables with an importance above threshold
# varimp_sub = varimp[which(varimp$mda>imp_th),]
# varimp_sub = droplevels(varimp_sub)
# ts_cat = levels(varimp_sub$TS)

# Including 10 most important variables (Conversation with MJ on 4/12/2022)
ts_cat = levels(varimp$TS)
varimp_sub = list()
for (ts in ts_cat){
  tmp = varimp[which(varimp$TS==ts),]
  varimp_sub[[ts]] = head(tmp[order(tmp$mda,decreasing = T),],10)
}

# Making plots
ts_p = list()
for(p in ts_cat){
  # ts_p[[p]] = varimp_sub[varimp_sub$TS==p,] %>% # exclusion by importance threshold
  ts_p[[p]] = varimp_sub[[p]] %>% # exclusion by number of predictors
    ggplot(aes(x = mda, y = reorder(filtered_label, mda))) +
    geom_bar(stat = "identity", fill = "gray50") +
    scale_x_continuous(breaks = seq(0,0.1,0.02), labels = scales::percent_format(accuracy = 1), limits = c(0,0.101))+
    scale_y_discrete(position = "left")+
    theme_light()+
    my_theme +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=9), axis.text = element_text(size = 7),
          axis.title = element_text(size = 7))

  if(p==ts_cat[length(ts_cat)]){
    ts_p[[p]] = ts_p[[p]] + labs(title = paste("Service Setting:", p),
                                   x = "Percentage decrease in model accuracy with exclusion of a given variable", y = NULL)
  }else{
    ts_p[[p]] = ts_p[[p]] + labs(title = paste("Service Setting:", p),
                                   x = NULL, y = NULL)
  }
}

## Combining plots
setwd(fig_dir)
svg("Variable Importance Plot - Analysis by Treatment Setting.svg", width = 6.5, height = 10)
plot_grid(plotlist=ts_p, align = "v", nrow = length(ts_cat)) #, rel_widths = c(0.8, 1, 1, 1, 1, 1, 1, 1)
dev.off()

# # Analysis by Year #NEW LINES OF CODE -------------------------------------
# 
# print(paste("Executing Analysis by Year",Sys.time()))
# 
# # Loading data
# setwd(data_dir)
# load("combined_teds.rda"); rm(teds1519); gc()
# load("imputed_sets.rda")
# 
# # Converting data frames to data tables
# setDT(imputedtraindf); setDT(imputedtestdf)
# 
# # Adding years based on initial data split
# imputedtraindf[,year:=year[train_id]]
# imputedtestdf[,year:=year[-train_id]]
# 
# # Splitting data tables by year
# train_yr_list = split(imputedtraindf, by="year", sorted = T)
# test_yr_list = split(imputedtestdf, by="year", sorted = T)
# 
# # Establishing model's formula (excluding year as a predictor)
# fm_yr = as.formula(paste0("DROPOUT~",
#                           paste0(colnames(imputedtraindf)[-which(colnames(imputedtraindf)%in%c("DROPOUT","year"))], collapse = "+")))
# 
# # Fitting best-performing model in subsets of training set and evaluating in subsets of testing set (assuming RF for the moment)
# final_fit = model_extract = list() # list to store variable importance and AUCs
# for(y in seq(train_yr_list)){
#   # Creating cluster of parallel workers
#   cl = makeCluster(cores)
#   registerDoParallel(cl)
#   
#   ## Specification
#   spec = rand_forest(trees = 250, mtry=tune()) %>%
#     set_mode("classification") %>%
#     set_engine("ranger", importance = "permutation")
# 
#   ## Grid specification
#   params =
#     dials::parameters(
#       mtry()
#     ) %>% # identifying parameters to tune
#     finalize(x = train_yr_list[[y]] %>% select(-DROPOUT)) # providing upper limit for mtry
# 
#   ## Creating grid using the max entropy method
#   grid =
#     dials::grid_max_entropy(
#       params,
#       size = sz
#     )
# 
#   ## Dividing training set into 5-folds for tuning
#   set.seed(y) # setting seed for pseudo-random numbers
#   final_cv = vfold_cv(data = train_yr_list[[y]],v=5, strata = DROPOUT)
# 
#   ## Establishing models' recipe
#   rec = recipe(fm_yr, data = train_yr_list[[y]])
# 
#   # Workflow
#   wf = workflow() %>%
#     add_recipe(rec) %>%
#     add_model(spec)
# 
#   ## Tuning model parameters
#   set.seed(y) # setting seed for pseudo-random numbers
#   tuned = tune::tune_grid(
#     object = wf,
#     resamples = final_cv,
#     grid = grid,
#     metrics = yardstick::metric_set(roc_auc, pr_auc),
#     control = tune::control_grid(verbose = TRUE, parallel_over = "everything")
#   )
# 
#   ## Identifying the best parameters for the model
#   best_params = tuned %>%
#     tune::select_best("roc_auc")
# 
#   ## Using the best parameters in the model
#   tuned_model = spec %>%
#     finalize_model(best_params)
# 
#   ## Finalizing workflow
#   final_wf = wf %>%
#     finalize_workflow(select_best(tuned, metric = "roc_auc"))
# 
#   ## Reverse engineering an rsplit object
#   combined = bind_rows(train_yr_list[[y]], test_yr_list[[y]])
#   ind = list(analysis = seq(nrow(train_yr_list[[y]])), assessment = nrow(train_yr_list[[y]]) + seq(nrow(test_yr_list[[y]])))
#   splits = make_splits(ind, combined)
# 
#   ## Fitting tuned model in training set and evaluating in testing set
#   ## (use final_fit$.metrics[[1]] to extract ROC AUC and PR AUC)
#   set.seed(y) # setting seed for pseudo-random numbers
#   final_fit[[y]] = final_wf %>%
#     last_fit(splits, metrics=yardstick::metric_set(accuracy, roc_auc, pr_auc))
# 
#   # Extracting model information (use model_extract$fit$variable.importance to extract variable importance)
#   model_extract[[y]] = final_fit[[y]] %>%
#     pluck(".workflow", 1) %>%
#     extract_fit_parsnip()
#   
#   # Stopping the cluster
#   stopCluster(cl)
# }
# 
# # Saving results
# setwd(results_dir)
# save(final_fit, model_extract, file = "Results_YEAR.rda")
# 
# # Variable Importance - Supplementary Figure 2 #NEW LINES OF CODE ---------
# 
# # # Loading data and results
# # setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
# # setwd(results_dir); load("Results_YEAR.rda")
# #
# # # Converting variable importance values into a data frame
# # varimpvalues = sapply(model_extract, function(x) x$fit$variable.importance) # extracting variable importance
# # importance_df = data.frame(varimpvalues) # converting to data frame
# # varimp = data.frame(variable = as.character(row.names(importance_df)),importance_df, row.names=NULL); rm(importance_df) # formatting data frame
# # colnames(varimp)[2:6] = 2015:2019 # renaming appropriate columns
# # varimp = merge(varimp, var_meaning, by="variable") # adding variable labels
# # varimp = reshape2::melt(varimp, id.vars = c("variable","label"), variable.name = "Year", value.name = "mda") # melting data frame
# #
# # ## Saving filtered variable importance results
# # setwd(results_dir)
# # write.csv(varimp, file = "Variable Importance - Analysis by Year.csv")
# #
# # Loading filtered results
# setwd(data_dir); var_meaning = read.csv("Variable Meaning.csv")
# setwd(results_dir); varimp = read.csv("Variable Importance - Analysis by Year.csv")
# varimp$Year = factor(varimp$Year, levels = as.character(2015:2019))
# varimp = merge(varimp[,c("variable", "Year", "mda")], var_meaning[,c("variable", "filtered_label")], by="variable") # adding filtered variable labels
# 
# # # Including variables with an importance above threshold
# # varimp_sub = varimp[which(varimp$mda>imp_th),]
# # varimp_sub = droplevels(varimp_sub)
# # yr_cat = levels(varimp$Year)
# 
# # Including 10 most important variables (Conversation with MJ on 4/12/2022)
# yr_cat = levels(varimp$Year)
# varimp_sub = list()
# for (yr in yr_cat){
#   tmp = varimp[which(varimp$Year==yr),]
#   varimp_sub[[yr]] = head(tmp[order(tmp$mda,decreasing = T),],10)
# }
# 
# # Making plot
# setwd(fig_dir)
# svg("Variable Importance Plot - Analysis by Year.svg", width = 6.5, height = 7)
# varimp %>%
#   ggplot(aes(fill=Year, x = mda, y = reorder(filtered_label, mda))) +
#   geom_bar(position="dodge", stat="identity") +
#   labs(title = NULL,
#        x = "Percentage decrease in model accuracy\nwith exclusion of a given variable", y = NULL)+
#   coord_cartesian(xlim=c(0,.06))+
#   theme_light()+
#   my_theme +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1))
# dev.off()
