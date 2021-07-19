
# Load Necessary Packages -------------------------------------------------
library(dplyr) #For data manipulation
library(tidyverse) # for data wrangling
library(magrittr)
require(pROC)
library(tidymodels)
library(vip)
library(broom)
library(pdp)
library(ranger)
library(parallel)
library(doParallel)
library(patchwork)
library(janitor)


# Load Data ---------------------------------------------------------------
#Load each of the 4 years of included TEDS-D data - these can be found here: 
#https://www.datafiles.samhsa.gov/study-series/treatment-episode-data-set-discharges-teds-d-nid13520

assign('teds2015', get(load('TEDSD2015.Rdata')))
assign('teds2016', get(load('TEDSD2016.Rdata')))
assign('teds2017', get(load('TEDSD2017.Rdata')))
assign('teds2018', get(load('TEDSD2018.Rdata')))
rm("PUF", "tedsd_2015_puf","tedsd_2016_puf", "tedsd_puf_2017")


# Process variables so all years are consistent and combine ---------------------------

#Create a count variable for number of substances in years where this is not included - 2017
teds2017<- teds2017 %>%
  mutate( SUB1SUB = case_when(
    SUB1 > 1  ~ 1,
    TRUE  ~ 0),
    SUB2SUB = case_when(
      SUB2 > 1  ~ 1,
      TRUE  ~ 0),
    SUB3SUB = case_when(
      SUB3 > 1  ~ 1,
      TRUE  ~ 0
    ), NUMSUBS = (SUB1SUB + SUB2SUB + SUB3SUB),
    YEAR = 2017,
    CBSA = CBSA2010
  )
# Remove extra created variables
teds2017 <- subset(teds2017, select = -c(SUB1SUB, SUB2SUB, SUB3SUB, CBSA2010))
#Create a count variable for number of substances in years where this is not included - 2018
teds2018<- teds2018 %>%
  mutate( SUB1SUB = case_when(
    SUB1 > 1  ~ 1,
    TRUE  ~ 0),
    SUB2SUB = case_when(
      SUB2 > 1  ~ 1,
      TRUE  ~ 0),
    SUB3SUB = case_when(
      SUB3 > 1  ~ 1,
      TRUE  ~ 0
    ), NUMSUBS = (SUB1SUB + SUB2SUB + SUB3SUB),
    YEAR = 2018,
    CBSA = CBSA2010
  )
# Remove extra created variables
teds2018 <- subset(teds2018, select = -c(SUB1SUB, SUB2SUB, SUB3SUB, CBSA2010))

#Combine files together - 2015-2018
TEDS1518 <- rbind(teds2015, teds2016, teds2017, teds2018)


#Get rid of variables that are collected only at discharge,
#keep only records where opioid use is indicated, replace -9s with NA
TEDS1518 <- TEDS1518 %>% 
            select( -ARRESTS_D, -DETNLF_D, -EMPLOY_D, -FREQ_ATND_SELF_HELP, -FREQ_ATND_SELF_HELP_D, 
                    -FREQ1_D, -FREQ2_D, -FREQ3_D, -LIVARAG_D, -SERVICES_D, -SUB1_D, -SUB2_D, -SUB3_D) %>% 
            filter(OPSYNFLG ==1|HERFLG==1 | METHFLG ==1) %>% 
            mutate_all(list(~na_if(., -9)))

#Select only the variables we need for processing and analysis
TEDS1518 <- TEDS1518 %>%
  select(FRSTUSE1, FRSTUSE2, FRSTUSE3, FREQ1, FREQ2, FREQ3, ROUTE1, ROUTE2, ROUTE3, STIMFLG, AMPHFLG, MTHAMFLG, COKEFLG, TRNQFLG, BENZFLG,
         SEDHPFLG, BARBFLG, HALLFLG, PCPFLG, OTHERFLG,REASON, AGE, SUB1, SUB2, SUB3, ALCFLG, ARRESTS, DAYWAIT, EDUC, EMPLOY, ETHNIC,
         GENDER, HLTHINS, INHFLG, LIVARAG, MARFLG, MARSTAT,METHUSE, NOPRIOR, NUMSUBS, PRIMINC, PRIMPAY, PSOURCE, PSYPROB, 
         RACE, REGION, SERVICES, VET, YEAR)



# Variable Recodes --------------------------------------------------------
#Feature engineering and recodes, selection of variables included in the analysis
TEDS1518 <- TEDS1518 %>%
  mutate(  #Create variable for age of first substance use
    FRSTSUBUSE = pmin(FRSTUSE1, FRSTUSE2, FRSTUSE3, na.rm = TRUE),
    #Create variable for maximum frequency of use of any opioid     
    FREQOH = case_when(
      SUB1 == 5  ~ FREQ1,
      SUB2 == 5  ~ FREQ2,
      SUB3 == 5  ~ FREQ3,
      TRUE  ~ NA_real_),
    FREQOM = case_when(
      SUB1 == 6  ~ FREQ1,
      SUB2 == 6  ~ FREQ2,
      SUB3 == 6  ~ FREQ3,
      TRUE  ~ NA_real_),
    FREQORx = case_when(
      SUB1 == 7  ~ FREQ1,
      SUB2 == 7  ~ FREQ2,
      SUB3 == 7  ~ FREQ3,
      TRUE  ~ NA_real_),
    FREQMAX = pmax(FREQOH, FREQOM,FREQORx, na.rm = TRUE),
    #Create Needle use flag variable indicating any injection drug use (included variable, IDU, only 
    #indicates injection as route of administration for primary substance of use (SUB1))
    ROUTE11 = case_when(
      is.na(ROUTE1)~ NA_real_,
      ROUTE1 == 4 ~ 1,
      TRUE  ~ 0),
    ROUTE22 = case_when(
      is.na(ROUTE2)~ NA_real_,
      ROUTE2 == 4 ~ 1,
      TRUE  ~ 0),
    ROUTE33 = case_when(
      is.na(ROUTE3)~ NA_real_,
      ROUTE3 == 4 ~ 1,
      TRUE  ~ 0),
    NEEDLEUSE = pmax(ROUTE11, ROUTE22, ROUTE33, na.rm = TRUE),
    #Create flag variable for any stimulant use
    STIMFLAG = case_when(
      STIMFLG == 1 ~ 1,
      AMPHFLG == 1 ~ 1,
      MTHAMFLG == 1 ~ 1,
      COKEFLG == 1 ~ 1,
      TRUE  ~ 0),
    #create flag variable for any tranquilizer use 
    TRNQFLAG = case_when(
      TRNQFLG == 1 ~ 1,
      BENZFLG == 1 ~ 1,
      TRUE  ~ 0),
    #create flag variable for any sedative use
    SEDFLAG = case_when(
      SEDHPFLG == 1 ~ 1,
      BARBFLG == 1 ~ 1,
      TRUE  ~ 0),
    #create flag variable for any hallucinogen use
    HALFLAG = case_when(
      HALLFLG == 1 ~ 1,
      PCPFLG == 1 ~ 1,
      OTHERFLG == 1 ~ 1,
      TRUE  ~ 0),
    #Create binary flag variable for outcome
    DROPOUT = case_when(
      REASON == 2 ~ 1,
      TRUE  ~ 0),
    #Collapsing age category into less buckets so as not to bias the random forest 
    #towards variables with a relatively high number of levels
    AGECAT = case_when(
      AGE %in% c("1")  ~ 1,
      AGE %in% c("2","3", "4")  ~ 2,
      AGE %in% c("5", "6")~ 3,
      AGE %in% c("7", "8") ~ 4,
      AGE %in% c("9", "10") ~ 5,
      AGE == 11 ~ 6,
      AGE == 12 ~ 7,
      TRUE  ~ 0),
    #Heroin use indicator
    HEROIN = case_when(
      SUB1 == 5  ~ 1,
      SUB2 == 5  ~ 1,
      SUB3 == 5  ~ 1, 
      TRUE  ~ 0)
  ) %>%
select(AGECAT, ALCFLG, ARRESTS, DAYWAIT, DROPOUT, EDUC, EMPLOY, ETHNIC, FREQMAX,
       FRSTSUBUSE, GENDER, HALFLAG, HEROIN,HLTHINS, INHFLG, LIVARAG, MARFLG, MARSTAT, 
       METHUSE, NEEDLEUSE, NOPRIOR, NUMSUBS, PRIMINC, PRIMPAY, PSOURCE, PSYPROB, 
       RACE, REGION, SEDFLAG, SERVICES, STIMFLAG, TRNQFLAG, VET, YEAR)
rm("teds2015","teds2016", "teds2017", "teds2018")



#convert numeric variables to ordered and unordered factors

colsfactor <- c("HLTHINS", "REGION", "PRIMPAY", "DROPOUT", "ETHNIC", "LIVARAG", "MARSTAT", 
                "PRIMINC", "PSOURCE", "RACE", "SERVICES", "ALCFLG", "GENDER", "METHUSE", "PSYPROB", "VET", 
                "NEEDLEUSE", "HALFLAG", "INHFLG", "MARFLG", "STIMFLAG", "TRNQFLAG", "SEDFLAG")

TEDS1518 %<>% mutate_at(colsfactor, funs(factor(.)))

colsordered <- c("AGECAT", "ARRESTS", "EDUC", "EMPLOY", "NOPRIOR", "FREQMAX", "FRSTSUBUSE","DAYWAIT",
                 "NUMSUBS" )

TEDS1518 %<>% mutate_at(colsordered, funs(ordered(.)))

#Create file of only complete/nonmissing records for supplementary analysis
# Uncomment as needed
#tedsnonmissing <- na.omit(TEDS1518)
#save(tedsnonmissing, file = "tedsnonmissing.rda")
#rm("colsfactor", "colsordered", "tedsnonmissing")
#load("tedsnonmissing.rda")


# Data Spending/Training and Test Sets ------------------------------------

set.seed(123)
#For COMPLETE record analysis run this
#data_split <- initial_split(tedsnonmissing, prop = .75, strata = DROPOUT)

#For all records (with imputation) run this
data_split <- initial_split(TEDS1518, prop = .75, strata = DROPOUT)

data_train <- training(data_split)
data_test <- testing(data_split)


# Missing Data Imputation -------------------------------------------------
library(missForest)
library(doParallel)
registerDoParallel(cores = 17)

detach("package:tidyverse", unload=TRUE)


#Imputation of missing data on the training set
data_train <- as.data.frame(data_train)
imputedtrain <- missForest(xmis = data_train, maxiter = 5, ntree = 3, variablewise = FALSE, decreasing = FALSE, verbose = FALSE, 
                        mtry = floor(sqrt(ncol(data_train))), replace = TRUE,
                        classwt = NULL, cutoff = NULL, strata = NULL,
                        sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                        xtrue = NA, parallelize = "variables")

imputedtraindf<- imputedtrain$ximp
#How did our missing data imputation perform?
#outofbagerror <- imputedtrain$OOBerror

save(imputedtraindf, file = "imputedtrainingset.rda")

#load(file = "imputedtrainingset.rda")

#Imputation of missing data on the test set - ALONE
data_test <- as.data.frame(data_test)
imputedtest <- missForest(xmis = data_test, maxiter = 5, ntree = 3, variablewise = FALSE, decreasing = FALSE, verbose = FALSE, 
                           mtry = floor(sqrt(ncol(data_test))), replace = TRUE,
                           classwt = NULL, cutoff = NULL, strata = NULL,
                           sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                           xtrue = NA, parallelize = "variables")

imputedtestdf<- imputedtest$ximp
#How did our missing data imputation perform?
#outofbagerror <- imputedtest$OOBerror

save(imputedtestdf, file = "imputedtestingset.rda")

#load(file = "imputedtestingset.rda")


#Drop year from both test/training set - keep only for individual year analyses

imputedtraindf <- imputedtraindf %>% select(-YEAR)
imputedtestdf <- imputedtestdf %>% select(-YEAR)


# Resampling - 10 fold ------------------------------------------------------

#10fold cross-validation resamples --> set of 10 training/testing sets
set.seed(713)
#For complete record analysis:
#data_cv <- vfold_cv(data_train, strata = DROPOUT)
#For imputed data analysis
data_cv <- vfold_cv(imputedtraindf, strata = DROPOUT)


# Model Specifications ----------------------------------------------------
#recipe - dropout as a function of all predictors
rf_rec <- recipe(DROPOUT ~ ., data = imputedtraindf)


#rf specification
rf_spec <- rand_forest(trees = 200) %>% 
  set_mode("classification") %>% 
  set_engine("ranger",importance = "permutation" )


# RF Work Flow for CV ------------------------------------------------------------
rf_wf <- workflow() %>% 
  add_recipe(rf_rec) %>% 
  add_model(rf_spec)

doParallel::registerDoParallel()


# # Cross Validation - Fit resamples ----------------------------------------

get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

ctrl <- control_resamples(save_pred = TRUE, extract = get_model)

set.seed(773)
#Actually fit the thing
rf_rs <- rf_wf %>%
 # add_model(rf_spec) %>%
  fit_resamples(
    resamples = data_cv,
    control = ctrl
  )

#Save the results for later use
save(rf_rs, file = "RFCVResults.rda")

#load("RFCVResults.rda")

#Stop the cluster
#stopCluster(cl)

#all_coef <- map_dfr(rf_rs$.extracts, ~ .x[[1]][[1]])
# Show the replicates for a single predictor:
#filter(all_coef, term == "AGECAT")

 
# #evaluate modeling performance on resamples
# #summarize = false gives us the metrics for each individual resample rather than the average across CV samples
collect_metrics(rf_rs, summarize = FALSE)

collect_metrics(rf_rs)


# Confusion matrix/AUC Curve ----------------------------------------------

#Confusion matrix on resamples --> only works when save_pred = true
 rf_rs %>%
  conf_mat_resampled(tidy = FALSE)

#AUC curve visualization --> only works when save_pred = true

AUCplot <- rf_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(DROPOUT, .pred_0) %>%
  autoplot()


#Reverse engineer an rsplit object
combined <- bind_rows(imputedtraindf, imputedtestdf)
ind <- list(analysis = seq(nrow(imputedtraindf)), assessment = nrow(imputedtraindf) + seq(nrow(imputedtestdf)))
splits <- make_splits(ind, combined)
splits



#Look at performance on the validation set
final_fitted <- last_fit(rf_wf, splits)
#Save for later!
#save(final_fitted, file = "RFResults.rda")
#Load the results later if you need to
#load("RFResults.rda")

# metrics on the *testing* set
collect_metrics(final_fitted) 

#Confusion matrix on the test set
collect_predictions(final_fitted) %>%
  conf_mat(DROPOUT, .pred_0) %>%
  autoplot()


# Variable Importance - Figure 2-----------------------------------------------------
#Variable Importance Plot
final_fitted %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(geom = "point", num_features = 10)
# can specify number of features with num_features = inside of VIP


#https://www.tidymodels.org/start/case-study/

importance <- final_fitted %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit()

#Actual values of importance measures  
varimpvalues <- importance$fit$variable.importance

#I need to get better at this/dedicate the time but right now I saved the 
#above vector in a csv and then imported - elegant solutions and assistance are welcome
varimp <- read.csv("variableimportance.csv")
varimp <- varimp %>% clean_names()
varimp <- varimp %>% rename(variable = i_name, mda = permutation_decrease_in_accuracy_of_model)


#Define theme for all plots
my_theme = theme(
  axis.text = element_text(size = 9, color = "black"),
  panel.grid.major.y =  element_blank(),
  plot.title = element_text(face = "bold")
)

figure2 <- varimp %>%
  ggplot(aes(x = mda, y = reorder(variable, mda))) +
  geom_bar(stat = "identity", fill = rgb(0,.19,.54)) +
  labs(title = "Relative importance of variables in predicting premature treatment exit", 
       x = "Percentage decrease in model accuracy with exclusion of a given variable", y = NULL)+
  coord_cartesian(xlim=c(0,.045))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))


# Partial Dependence Plots - Figure 3 -------------------------------------
# This is just for Agecat as the predictor variable - make a loop to create a graph for each variable
SERVICESplot <- pdp::partial(importance$fit, train = imputedtraindf, 
                      type = "classification", pred.var = "SERVICES", plot = FALSE)

REGIONplot <- pdp::partial(importance$fit, train = imputedtraindf, 
                             type = "classification", pred.var = "REGION", plot = FALSE)
PSOURCEplot <- pdp::partial(importance$fit, train = imputedtraindf, 
                             type = "classification", pred.var = "PSOURCE", plot = FALSE)
PRIMPAYplot <- pdp::partial(importance$fit, train = imputedtraindf, 
                             type = "classification", pred.var = "PRIMPAY", plot = FALSE)
HLTHINSplot <- pdp::partial(importance$fit, train = imputedtraindf, 
                             type = "classification", pred.var = "HLTHINS", plot = FALSE)

#Re the above, saved these as csv's. Will add a more elegant solution at some point
SERVICESplot <- read.csv("SERVICESplot.csv")

REGIONplot <- read.csv("REGIONplot.csv")

PRIMPAYplot <- read.csv("PRIMPAYplot.csv")

HLTHINSplot <- read.csv("HLTHINSplot.csv")

PSOURCEplot <- read.csv("PSOURCEplot.csv")

SERVICESplot <- SERVICESplot %>% clean_names()
SERVICESplot <- SERVICESplot %>% rename(SERVICES = i_services)


REGIONplot <- REGIONplot %>% clean_names()
REGIONplot <- REGIONplot %>% rename(REGION = i_region)

PSOURCEplot <- PSOURCEplot %>% clean_names()
PSOURCEplot <- PSOURCEplot %>% rename(PSOURCE = i_psource)

PRIMPAYplot <- PRIMPAYplot %>% clean_names()
PRIMPAYplot <- PRIMPAYplot %>% rename(PRIMPAY = i_primpay)

HLTHINSplot <- HLTHINSplot %>% clean_names()
HLTHINSplot <- HLTHINSplot %>% rename(HLTHINS = i_health_insurance_status)




p1 <- SERVICESplot %>%
  ggplot(aes(x = probability, y = reorder(SERVICES, probability))) +
  geom_bar(stat = "identity", fill = rgb(0,.19,.54)) +
  xlab("") + ylab("Service setting")+
  coord_cartesian(xlim=c(0,.7))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

p2 <- REGIONplot %>%
  ggplot(aes(x = probability, y = reorder(REGION, probability))) +
  geom_bar(stat = "identity", fill = rgb(0,.19,.54)) +
  xlab("") + ylab("Region")+
  coord_cartesian(xlim=c(0,.7))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

p4 <- PSOURCEplot %>%
  ggplot(aes(x = probability, y = reorder(PSOURCE, probability))) +
  geom_bar(stat = "identity", fill = rgb(0,.19,.54)) +
  xlab("") + ylab("Referral Source")+
  coord_cartesian(xlim=c(0,.7))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

p5 <- HLTHINSplot %>%
  ggplot(aes(x = probability, y = reorder(HLTHINS, probability))) +
  geom_bar(stat = "identity", fill = rgb(0,.19,.54)) +
  xlab("Probability of premature exit") + ylab("Health Insurance")+
  coord_cartesian(xlim=c(0,.7))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

p3 <- PRIMPAYplot %>%
  ggplot(aes(x = probability, y = reorder(PRIMPAY, probability))) +
  geom_bar(stat = "identity", fill = rgb(0,.19,.54)) +
  xlab("") + ylab("Primary payment source")+
  coord_cartesian(xlim=c(0,.7))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))



#Using patchwork combine individual plots into one
figure3 <- p1/p2/p3/p4/p5
figure3title <- figure3 + plot_annotation(
  title = 'Marginal effect of factor levels on treatment attrition',
  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
#save with size 800x850


#Figure S2
#Ran analyses on individual years of data, saved varimp values all in one csv
#Again, will add a more elegant solution at some point
varimpyearly <- read.csv("variableimportanceyearly.csv")
varimpyearly <- varimpyearly %>% clean_names()
varimpyearly <- varimpyearly %>% rename(name = i_variable, mda = permutation_decrease_in_accuracy_of_model)


figures2 <- varimpyearly %>%
  ggplot(aes( mda, factor(reorder(name, mda)),  fill = year)) +
  geom_bar(stat = "identity",  position = "dodge") +
  labs(title = "Relative importance of variables in predicting premature treatment exit", 
       x = "Percentage decrease in model accuracy with exclusion of a given variable", y = NULL)+
  coord_cartesian(xlim=c(0,.045))+
  theme_light()+
  my_theme +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))