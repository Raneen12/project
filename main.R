# import libraries
library(tidyverse)
library(lubridate)
library(caTools)
library(e1071)


# read dataset
data <-  read_csv("dataset.csv")

# show first five rows of the dataset
head(data)


# show the strcuture of the data
str(data)



# modify column names
names(data)<-  c('patient_id','appointment_id','gender','schedule_day','appointment_day','age','neighborhood', 'scholarship','hypertension','diabetes','alcoholism','handicap','sms_received','no_show')


# see if missing values exist
sapply(data,function(x)sum(is.na(x)))


# change data type of some atrributes to categorical to be able to perform analysis
data <- mutate_at(data, vars('gender','neighborhood','scholarship','hypertension','diabetes','alcoholism','handicap','sms_received'), as.factor)



# show data types of attributes
lapply(data, class)


# show descrptive summary statistics
summary(data)


# remove outliers from Age attribute
data <-data[!(data$age<= 0),]


##########################################
# Exploratory Data Analysis (EDA)
#  Explore the relationship between Age attribute and No Show
#########################################

# plot age attribute vesus the class label attribute
ggplot(data,aes(x=age)) + 
  geom_histogram(data=subset(data,no_show == 'No'),fill = '#00BFC4', alpha = 0.8, bins = 40) +
  geom_histogram(data=subset(data,no_show == 'Yes'),fill = '#F8766D', alpha = 0.8, bins = 40) +
  ggtitle('Age vs No Show Histogram')+
  theme(plot.title = element_text(hjust = 0.5))



# change class labesl from yes/no to not showed up/ showed up to be more descriptive
data$no_show[data$no_show == 'No'] <- 'Showed up'
data$no_show[data$no_show == 'Yes'] <- 'not showed up'

# convert class label attribute to categorical
data$no_show <- as.factor(data$no_show)


# Box plot of the Age attribute
ggplot(data, aes(x = no_show, y = age, fill = no_show))+
  geom_boxplot()+ 
  ggtitle("Age versus No Show Boxplot")+
  theme(plot.title = element_text(hjust = 0.5))


# show statistical summary of Age attribute
select(data, age, no_show) %>% 
  group_by(no_show) %>% 
  summarise(age_mean = mean(age))


# Perform T test of age attribute

t.test(data$age ~ data$no_show)

#---------------------------------------------------

# Explore the relationship between Gender attribute and No Show
####################################################
levels(data$gender)[levels(data$gender)=="M"] <- "Male"
levels(data$gender)[levels(data$gender)=="F"] <- "Female"


# plot distribution of gender in the dataset
ggplot(data = data)+
  geom_bar(aes(x = gender,fill = gender))+
  ggtitle("Gender Distribution")+
  theme(plot.title = element_text(hjust = 0.5))


# show gender distribution in the dataset
table(data$gender, data$no_show)

# Perform chi-squared test to test of the gender attribute

chisq.test(table(data$gender,data$no_show))

#-------------------------------------------------------

# Model Building
df <- select(data, age, gender, scholarship, hypertension, diabetes, alcoholism, handicap,sms_received, no_show)

# Divide the dataset into two  70% train and 30% test sets 
set.seed(100)
split = sample.split(df$no_show, SplitRatio = 0.70)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

# Logistic Regression
model1 <- glm(formula = no_show ~ . ,  data = train, family =binomial(link = 'logit') )

# Model Evaluation

predicted<- predict(model1, newdata=test,type='response')
pred_test <-  ifelse(predicted>0.5,1,0)
tab <- table(predicted = pred_test, actual = test$no_show)
confusion_matrix <- tab/nrow(test)*100

