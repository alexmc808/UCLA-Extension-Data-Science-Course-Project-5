#A.
library(tidycensus)
library(ggplot2)
library(ggrepel)


#Only need to run the following three lines one time (each person will have their own key). 
#census_api_key("bba651673f601ccdf83ed14027327f1c4404d6f8", install = TRUE) # overwrite = TRUE.
#readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_API_KEY")

#Load ACS data.
acs_var = load_variables(2021, "acs5", cache = TRUE)

#Get data for number of residents (age > 25) with a bachelor's degree by county in the U.S. Note: using year = 2021.
num_bachelors = get_acs(geography = "county", variables = "B15003_022", year = 2021, survey = "acs5", geometry = FALSE)
str(num_bachelors)

#Get data for number of total adults by county in the U.S. Note: using year = 2021.
tot_adults = get_acs(geography = "county", variables = "B15003_001", year = 2021, survey = "acs5", geometry = FALSE)


#Get the median household income by county (B19013_001). Note: using year = 2021.
median_income = get_acs(geography = "county", variables = "B19013_001", year = 2021, survey = "acs5", geometry = FALSE)
str(median_income)

#Merge num_bachelors, tot_adults, and median_income by geographic ID: GEOID
acs_data = merge(num_bachelors, tot_adults, by = "GEOID", all.x = TRUE)
acs_data = merge(acs_data, median_income, by = "GEOID", all.x = TRUE)

#Calculate percentage of college degree/total adult population by county and add it as a new variable to the data set.
acs_data$perc_college = acs_data$estimate.x / acs_data$estimate.y

#Plot correlation between college degree percentage and the log median household income.
plot = ggplot(acs_data, aes(perc_college * 100, log(estimate))) + geom_point() + labs(x = "% of College Graduates by County",
y = "Log Median Household Income by County", title = "Correlation Plot") + theme_bw() + geom_smooth(method = "lm", col = "red")
plot

#Run simple linear regression using x = college degree % and y = log median household income.
lm.fit = lm(log(acs_data$estimate) ~ acs_data$perc_college, data = acs_data)
summary(lm.fit) #Adjusted R^2 is 0.314.

#Based on the summary of the simple linear regression, the percentage of college graduates is a significant predictor of the log 
#of the median household income by county. The simple regression explains that as the percentage of college graduates increases, 
#the median household income by county also increases. This makes sense as it is more likely for people with higher education degrees
#to get more high paying jobs, which increases their median household income. The adjusted R^2 for this model is 0.314.

#What are the abnormal observations in the correlation chart?
acs_puerto = subset(acs_data, grepl("Puerto Rico", acs_data$NAME.x))
acs_puerto$estimate = log(acs_puerto$estimate)
plot1 = plot + geom_text_repel(data = acs_puerto, aes(acs_puerto$perc_college, acs_puerto$estimate), label = acs_puerto$NAME.x, max.overlaps = 100)
plot1

plot2 = plot + geom_point(data = acs_puerto, color = "red")
plot2

#As seen in plot1 via labeling the counties of the abnormal points, although it is very messy, the abnormal observations correspond to counties in Puerto Rico, 
#which is a territory of the United States but is not part of the 50 states. As seen in plot 2, the red points represent the Puerto Rico observations, which make
#up a majority of the abnormal data.

#B.
setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 5")

library(dplyr)
library(corrplot)
library(ggplot2)
library(cowplot)
library(caret)
library(pROC)

#Import creditcard data set.
cc = read.csv("creditcard.csv")
str(cc)
summary(cc)

#Data management:
missing_data = cc %>% summarise_all(funs(sum(is.na(.))/n())) #No NA values.
colnames(cc)[7] = "PAY_1" #Rename PAY_0 as PAY_1 for better readability. 
colnames(cc)[25] = "default" #Rename default.payment.next.month as default
table(cc$SEX)
table(cc$EDUCATION)
cc$EDUCATION = ifelse(cc$EDUCATION %in% c(0, 5, 6), 4, cc$EDUCATION) #Consolidate education levels (4 = others).
table(cc$MARRIAGE)
cc$MARRIAGE = ifelse(cc$MARRIAGE %in% 0, 3, cc$MARRIAGE) #Consolidate marriage levels (3 = others).


#Convert some variables to factor variables.
cc$SEX = as.factor(cc$SEX)
cc$EDUCATION = as.factor(cc$EDUCATION)
cc$MARRIAGE = as.factor(cc$MARRIAGE)
cc = cc %>% mutate_at(vars(PAY_1, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6), factor)
cc$default = as.factor(cc$default)
str(cc)

#Exploratory Data Analysis:
cor = cor(cc[,-c(1, 3, 4, 5, 7, 8, 9, 10, 11, 12, 25)])
corrplot(cor, type = "lower", method = "number") #Bill amounts are highly correlated.

#Default Percent: 22.1% of people defaulted in October 2005.
cc %>% group_by(default) %>% summarise(count = n()) %>% mutate(percent = prop.table(count) * 100) %>%
ggplot(aes(reorder(default, -percent), percent), fill = default) + geom_col(fill = c("purple", "green")) +
geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = 0.2, vjust = 2, size = 5) + theme_bw() +  
xlab("Default") + ylab("Percent") + ggtitle("Default Percent in October 2005")

#Default against categorical variables.
plot_grid(ggplot(cc, aes(x = SEX, fill = default)) + geom_bar(position = "fill") + theme_bw(), 
          ggplot(cc, aes(x = EDUCATION, fill = default)) + geom_bar(position = "fill") + theme_bw(),
          ggplot(cc, aes(x = MARRIAGE, fill = default))+ geom_bar(position = "fill") + theme_bw())
#Males default slightly more than females do. People with higher education levels tend to default less. 
#Married and others default slightly more than single people do.

plot_grid(ggplot(cc, aes(x = PAY_1, fill = default)) + geom_bar(position = "fill") + theme_bw(), 
          ggplot(cc, aes(x = PAY_2, fill = default)) + geom_bar(position = "fill") + theme_bw(),
          ggplot(cc, aes(x = PAY_3, fill = default))+ geom_bar(position = "fill") + theme_bw(),
          ggplot(cc, aes(x = PAY_4, fill = default)) + geom_bar(position = "fill") + theme_bw(), 
          ggplot(cc, aes(x = PAY_5, fill = default)) + geom_bar(position = "fill") + theme_bw(),
          ggplot(cc, aes(x = PAY_6, fill = default))+ geom_bar(position = "fill") + theme_bw())
#General trend: For each month, the earlier the payment is made, the less likely a person will default the payment.

ggplot(cc, aes(x = AGE, fill = default)) + geom_bar() + facet_wrap(~SEX) + theme_bw() + xlab("Age") + 
ggtitle("Default by Age and Sex")
#Females in their 20-30s tend to default more than males in any age bracket.

ggplot(cc, aes(x = AGE, fill = default)) + geom_bar() + facet_wrap(~MARRIAGE) + theme_bw() + xlab("Age") + 
  ggtitle("Default by Age and Marriage")
#Married people in their 20s-30s and unmarried people in their 20-30s tend to default the most. It appears that
#the tendency for unmarried people in their 20s-30s to default is higher than that of married people in the same age bracket.

#Default against numeric variables.
ggplot(cc, aes(x = AGE, fill = default)) + geom_histogram(bins = 6, color = "black") + theme_bw() +
xlab("Age") + ggtitle("Default by Age")
#People in their 20-30s default more than people 40+. In general, the older a person is, the less likely they are to default.

ggplot(cc, aes(x = default, y = LIMIT_BAL, fill = default)) + geom_violin() + theme_bw() + xlab("Default") + ylab("Limit Balance") + ggtitle("Default by Limit Balance")
#There are more defaulters when limit balance is lower. 

plot_grid(ggplot(cc, aes(x = PAY_AMT1, y = BILL_AMT1)) + geom_point(color = "red") + theme_bw(), 
          ggplot(cc, aes(x = PAY_AMT2, y = BILL_AMT2)) + geom_point(color = "green") + theme_bw(), 
          ggplot(cc, aes(x = PAY_AMT3, y = BILL_AMT3)) + geom_point(color = "blue") + theme_bw(), 
          ggplot(cc, aes(x = PAY_AMT4, y = BILL_AMT4)) + geom_point(color = "orange") + theme_bw(), 
          ggplot(cc, aes(x = PAY_AMT5, y = BILL_AMT5)) + geom_point(color = "purple") + theme_bw(), 
          ggplot(cc, aes(x = PAY_AMT6, y = BILL_AMT6)) + geom_point(color = "pink") + theme_bw())
#Negative correlation between amount paid and amount billed. There are a lot of people with high bill amounts but low pay amounts.

#Logistic regression model (supervised learning).
#Split data into train and test sets. 
set.seed(999)
indices = createDataPartition(cc$default, p = 0.7, list=FALSE)
trainset = cc[indices,][-1]
testset = cc[-indices,][-1]
  
#Run model.
cc1 = cc[-1] #Remove ID variable because it does not affect mode.
control = trainControl(method = "cv", number = 10) #Establish settings for resampling method that will be used to evaluate performance of the model
#during training. Using 10-fold cross validation.
metric = "Accuracy" #Error metric being used.

glm.fit = train(default ~., data = cc1, method = "glm", family = binomial(link = "logit"), metric = metric, trControl = control)
print(glm.fit) #Accuracy: 0.82; Kappa: 0.37. 

#Use model on test set to measure prediction accuracy. 
glm.pred = predict(glm.fit, testset)
glm.pred
confusionMatrix(glm.pred, testset$default) #Sensitivity (true positive rate) is 0.95, which is very good. The accuracy and kappa values for the model
#are 0.82 and 0.36.

glm.pred.o = ordered(glm.pred, levels = c(0, 1))
glm.roc = roc(testset$default, ordered(glm.pred.o))
plot(glm.roc, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
coords(glm.roc, "best", "threshold") #Best probability threshold is 0.5 (but I don't know if this is accurate because I had to change glm.pred to an ordered
#factor in order for the roc function to work).

#Although my best threshold value came out to be 0.5 based on the ROC curve, I do not think 0.5 is necessarily a good threshold point. This is because false 
#negatives (predicting that a person won't default when they actually do) are much more costly than false positives in this context. Thus, a probability 
#threshold value such as 0.2 seems more appropriate because, although there will be a higher number of false positives, this is still more beneficial because 
#it will ensure that the false negative rate decreases, which credit card companies are more interested in.

#Principal component analysis model (unsupervised learning).
#Remove ID and default variables from data. 
cc2 = cc[-c(1, 25)]
cc2 = cc2 %>% mutate_at(vars(SEX, EDUCATION, MARRIAGE, PAY_1, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6), as.numeric) #Change factor variables to numeric variables.
str(cc2)

#Run model.
pr.out = prcomp(cc2, scale = T)
summary(pr.out)
pr.out$rotation[,1:5] #Loadings for each variable in first 5 PCs.

#Proportion of variance explained by each principal component. 
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')

#Clustering model(unsupervised learning).
set.seed(8)
km.out = kmeans(cc2, 4, nstart = 20)
km.out$cluster #Cluster assignments of each observation.

#Plot LIMIT_BAL and BILL_AMT1 and display cluster assignments with different colors.
options(scipen = 1000)
plot3 = plot(cc2$LIMIT_BAL, cc2$BILL_AMT1, col = (km.out$cluster + 1), main = "K-Means Clustering", 
xlab = "Amount of Given Credit (NT Dollars)", ylab = "Amount of Bill Statement in September 2005 (NT Dollars)")

#The dark blue and green clusters represent people with a high income because they have high credit amounts. However,
#their spending habits differ because the dark blue cluster has a high bill statement, which indicates high spending
#whereas the green cluster has a low bill statement (low spending). The light blue cluster represents people who have a
#lower income and do not spend a lot because they have a low amount of credit and a small bill statement amount. The red
#cluster has a medium income level and also a medium spending level.

#C. 
setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 5/Health in America Covid19 Variation/Health in America Covid19 Variation")

library(readxl)  
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(tidycensus)
library(tidyverse)
library(viridis)
library(mapview)
library(corrplot)
library(broom)
options(tigris_use_cache = TRUE)

#Use death rate data from 12/31/22 by county as dependent variable in Model 1 from BA01a_crossSection. 

# 2019 5 year ACS the list of variables: https://api.census.gov/data/2019/acs/acs5/profile/variables.html
acs19_5y = get_acs(geography = "county",
                   variables = c(sparent_m = "DP02_0007PE", sparent_f = "DP02_0011PE", hhage65a = "DP02_0013PE", hhsize = "DP02_0016E", 
                                 college_higher = "DP02_0068PE", ed_below9 = "DP02_0060PE", ed_g912 = "DP02_0061PE",ed_hs = "DP02_0062PE",
                                 ed_scollege = "DP02_0063PE",ed_associate = "DP02_0064PE",ed_bachelor = "DP02_0065PE",ed_higher = "DP02_0066PE",
                                 disable = "DP02_0072PE", disable65a = "DP02_0078PE", 
                                 fborn = "DP02_0093PE",fb_nonc = "DP02_0096PE", computer = "DP02_0152PE", broadband = "DP02_0153PE", 
                                 lcp = "DP03_0003PE", ur = "DP03_0009PE",
                                 commute_p = "DP03_0021PE", wfh = "DP03_0024PE", commute_ms = "DP03_0025E",
                                 eea_hhs = "DP03_0042PE", eea_lh = "DP03_0043PE", mincome = "DP03_0062E", 
                                 fstamp = "DP03_0074PE", 
                                 hi_pub = "DP03_0098PE", hi_no = "DP03_0099PE", hi_no_neea ="DP03_0113PE", hi_no_nlc ="DP03_0118PE",
                                 poverty ="DP03_0119PE",
                                 rentalu = "DP04_0047PE", rental_size = "DP04_0049E", mrent = "DP04_0134E", mhomeprice = "DP04_0089E", 
                                 pop = "DP05_0001E", mage ="DP05_0018E", a85a ="DP05_0017PE", a7584="DP05_0016PE",
                                 a6574 = "DP05_0015PE", a6064 ="DP05_0014PE", a5559 ="DP05_0013PE",a4554 ="DP05_0012PE",
                                 a3544 ="DP05_0011PE",a2534 ="DP05_0010PE",a2024 ="DP05_0009PE",a1519 ="DP05_0008PE",
                                 black ="DP05_0038PE", asian = "DP05_0044PE", aindian = "DP05_0039PE", latino = "DP05_0071PE"
                   ),                                        
                   survey = "acs5",
                   output = "wide",
                   year = 2019)   

acs19 = acs19_5y %>% select(GEOID, NAME, sparent_m, sparent_f, hhage65a, hhsize, college_higher, disable, disable65a, 
                            fborn,fb_nonc, computer, broadband, lcp, ur,commute_p, commute_ms, wfh, eea_hhs, eea_lh, mincome, 
                            fstamp, hi_pub, hi_no, hi_no_neea, hi_no_nlc, poverty, rentalu, rental_size, mrent, mhomeprice, 
                            pop, mage, a85a, a7584, a6574, a6064, a5559, a4554,a3544,a2534, a2024,a1519,black, asian, aindian, latino) 

chci = acs19_5y %>% select(GEOID, NAME, ed_below9, ed_g912,ed_hs, ed_scollege ,ed_associate,ed_bachelor,ed_higher)
chci = chci %>% mutate(chci=(1/100)*(50*ed_below9+100*ed_g912+120*ed_hs+130*ed_scollege+140*ed_associate+190*ed_bachelor+230*ed_higher))
acs19 = left_join(acs19, chci[,c("GEOID","chci")], by="GEOID")

acs19 = acs19 %>% mutate(sparent = sparent_m + sparent_f, a75a=a7584+a85a, a65a=a6574+a75a, a60a=a65a+a6064, a5564=a5559+a6064,
                         a3554=a3544+a4554, a2034=a2024+a2534, afford = mincome/mhomeprice) %>% rename(county=GEOID)

#Linear Regression Analysis
#Data source: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
covid_case = read.csv("CovidCases.csv") 
covid_death = read.csv("CovidDeaths.csv") 
population = read.csv("population.csv") 
usland =  read_xlsx("US_land_area.xlsx") 

covidc_123122 = covid_case[,c(1:4, 1079)]
names(covidc_123122)=c("county", "name","statename","stateid","case")
covidd_123122 = covid_death[,c(1, 1079)]
names(covidd_123122)=c("county", "death")
covidc_53120 = covid_case[,c("countyFIPS","X2020.05.31")]
names(covidc_53120)=c("county", "case0520")
covidd_53120 = covid_death[,c("countyFIPS","X5.31.2020")]
names(covidd_53120)=c("county", "death0520")

pop_123122 = population[,c(1, 4)]
names(pop_123122) = c("county","population")
us_land = usland[,c("STCOU", "LND110190D")]
names(us_land)=c("county", "area")

covidc_123122 = covidc_123122 %>% filter(county!=0)
covidd_123122 = covidd_123122 %>% filter(county!=0)
covidc_53120 = covidc_53120 %>% filter(county!=0)
covidd_53120 = covidd_53120 %>% filter(county!=0)

pop_123122 = pop_123122 %>% filter(county!=0)

us_land = us_land %>% mutate(county=as.numeric(county))

acs19 = acs19 %>% mutate(county=as.numeric(county))

new = left_join(covidc_123122, covidd_123122, by="county") %>% left_join(covidc_53120, by="county") %>% left_join(covidd_53120, by="county") %>%
  left_join(pop_123122, by="county") %>% left_join(us_land, by="county") %>% left_join(acs19, by="county")

#Calculating population density,
new = new %>% mutate(pdensity = population/area) %>% mutate(pdensity=gsub("Inf", NA, pdensity)) %>% mutate(pdensity=as.numeric(pdensity))
new = new %>% mutate(casep = 100*case/population, deathp = 1000000*death/population, casep520 = 100*case0520/population, 
                     deathp520 = 1000000*death0520/population) 

str(new)

#sparent_m, sparent_f, hhage65a, hhsize, college_higher, disable, disable65a, 
#fborn, fb_nonc, computer, broadband, lcp, ur,commute_p, commute_ms, wfh, eea_hhs, eea_lh, mincome, 
#fstamp, hi_pub, hi_no, hi_no_neea, hi_no_nlc, poverty, rentalu, rental_size, mrent, mhomeprice, 
#pop, mage, a85a, a7584, a6574, a6064, black, asian, aindian, latino) 
#a75a=a7584+a85a, a65a=a6574+a75a, a60a=a65a+a6064.

## QCEW NAICS Data; https://www.bls.gov/cew/downloadable-data-files.htm
nic621111 = read.csv("nic621111.csv") # Offices of physicians
doctor = nic621111 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, doctor = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic713940 = read.csv("nic713940.csv") # Gym
gym = nic713940 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, gym = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic445110 = read.csv("nic445110.csv") # Grocery stores
grocery = nic445110 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, grocery = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic623110 = read.csv("nic623110.csv") # Nursing care facilities
nursehome = nic623110 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, nursehome = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic311612 = read.csv("nic311612.csv") # Meat packing factories
meatpack = nic311612 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, meatpack = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic445310 = read.csv("nic445310.csv") # Liquor stores
liquor = nic445310 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, liquor = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic481111 = read.csv("nic481111.csv") # Airport
airport = nic481111 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, airport = annual_avg_emplvl) %>% mutate(county=as.numeric(county))
nic7211 = read.csv("nic7211.csv") # Travel
travel = nic7211 %>% filter(own_code ==5) %>% select(area_fips, annual_avg_emplvl) %>% 
  rename(county=area_fips, travel = annual_avg_emplvl) %>% mutate(county=as.numeric(county))

## Election data
election = read.csv("countypres_2000-2016.csv") 
election1 = election %>% filter(year == 2016, party == "democrat") %>% select(FIPS, candidatevotes, totalvotes) %>%
  mutate(demv = 100*candidatevotes/totalvotes) %>% rename(county = FIPS)

new1 = left_join(new, election1, by = "county") %>% left_join(doctor, by = "county") %>% left_join(gym, by = "county") %>%
  left_join(grocery, by = "county") %>% left_join(nursehome, by = "county")  %>% left_join(meatpack, by = "county") %>%
  left_join(liquor, by = "county")  %>% left_join(airport, by = "county") %>% left_join(travel, by = "county")

# Replace missing values with 0s.
new1[, 75:81][is.na(new1[, 75:81])] = 0

new1 = new1 %>% mutate(cfr = deathp/casep, p_doctor = 100 * doctor/population, p_gym = 100 * gym/population, p_grocery = 100 * grocery/population,
                       p_nursehome = 100 * nursehome/population, p_meatpack = 100 * meatpack/population, p_liquor = 100 * liquor/population,
                       p_airport = 100 * airport/population, p_travel = 100 * travel/population, hi_no_eea = 100 - hi_no_neea - hi_no_nlc)

## Health Data: 
# https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation
health = read_xlsx("2020 County Health Rankings Data.xlsx") 

colnames(health)
str(health)
health = health %>% mutate(county = as.numeric(county))

new2 = left_join(new1, health, by = "county") 
colnames(new2)

eq01 = lm(deathp ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq01) #Adjusted R^2 is 0.49.

eq02 = lm(casep ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq02) #Adjusted R^2 is 0.19.

eq03 = lm(cfr ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq03) #Adjusted R^2 is 0.31.

eq04 = lm(deathp ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq04) #Adjusted R^2 is 0.49.

eq05 = lm(deathp ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw + statename, data=new2)
summary(eq05) #Adjusted R^2 is 0.66.

eq06 = lm(deathp ~ casep + a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq06) #Adjusted R^2 is 0.50.

eq07 = lm(deathp ~ deathp520 + a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + deathp520 +
            commute_p + wfh + computer + p_nursehome + p_liquor + drinking + prematured + lowbirthw, data=new2)
summary(eq07) #Adjusted R^2 is 0.52.

stat01 = tidy(eq01); stat02 = tidy(eq02); stat03 = tidy(eq03) 
stat04 = tidy(eq04); stat05 = tidy(eq05); stat06 = tidy(eq06); stat07 = tidy(eq07)

stats_all = rbind(stat01, NA, stat02, NA, stat03, NA, stat04, NA, stat05, NA, stat06, NA, stat07)
write.csv(stats_all, "stats_all.csv")

#The results of using the 12/31/22 Covid data for the model show that being aged 65 and up and especially ages 75 and up are significant
#The other significant predictors of Covid deaths include being latino, single parent households, CHCI, nursing home exposure, excess drinking,
#people with public insurance,democratic voting percentage in 2016, public commuting, working from home, computer access, premature death, and low birth weight
#These findings are comparable to the findings found in the report from 2021.