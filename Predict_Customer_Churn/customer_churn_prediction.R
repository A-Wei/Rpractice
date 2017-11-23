# This script is a walk throught of this artical in r-blogger for predition models
# https://www.r-bloggers.com/predict-customer-churn-logistic-regression-decision-tree-and-random-forest/

# Set wd to file folder and load packages
setwd("/Users/wei/Documents/R/Predict_Customer_Churn")
require("pacman")
p_load(gglot2,plyr,corrplot,ggplot2,gridExtra,ggthemes,caret,MASS,randomForest,party,rvest)

# Read dataset from website
ibm_data <- read_html("https://www.ibm.com/communities/analytics/watson-analytics-blog/guide-to-sample-datasets/")
churn <- ibm_data %>% 
        html_nodes("p:nth-child(17) a") %>% # nodes are based on number instead of text, number may change.
        html_attr("href") %>%  # Get the link
        read.csv(.,header=T,na.strings=c("")) # Load dataset

sapply(churn,function(x) sum(is.na(x))) # count how many missing values in each column
churn <- churn[complete.cases(churn),] # Complete.cases on rows select rows with no missing values.

# Change 'No internet service' to 'No' for 6 columsn they are
# 1.“OnlineSecurity”, 2.“OnlineBackup”, 3.“DeviceProtection”, 4.“TechSupport”, 5.“streamingTV”, 6.“streamingMovies”
cols_recodel <- c(10:15)
for (i in 1:ncol(churn[,cols_recodel])) {
        churn[,cols_recodel][,i] <- as.factor(mapvalues(churn[,cols_recodel][,i], from = c('No internet service'), to = c('No')))
}

# Belos is also working but perhapes overfitting the case, and not good thinking behind it? not sure
#for (i in 10:15){
#        churn[,i] <- as.factor(mapvalues(churn[,i],from = 'No internet service', to = 'No' ))
#}

# Change 'No phone service' to 'No' for column 'MultipleLines'
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

# Since the minimum tenure is 1 month and maximum tenure is 72 months, we can group them into five tenure groups: “0–12 Month”, “12–24 Month”, “24–48 Months”, “48–60 Month”, “> 60 Month”
churn$tenure_group <- NA
churn[churn$tenure >= 0 & churn$tenure < 13,]$tenure_group <- "0-12 Month"
churn[churn$tenure >= 13 & churn$tenure < 25,]$tenure_group <- "12-24 Month"
churn[churn$tenure >= 25 & churn$tenure < 49,]$tenure_group <- "24-48 Month"
churn[churn$tenure >= 49 & churn$tenure < 61,]$tenure_group <- "48-60 Month"
churn[churn$tenure >= 61,]$tenure_group <- ">60 Month"
churn$tenure_group <- as.factor(churn$tenure_group)

# Change the values in column “SeniorCitizen” from 0 or 1 to “No” or “Yes”.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

#Remove the columns we do not need for the analysis.
churn$customerID <- NULL
churn$tenure <- NULL

# Exploratory data analysis and feature selection
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# The Monthly Charges and Total Charges are correlated. So one of them will be removed from the model. We remove Total Charges.
# Q:Why remove one column? Does that mean in prediction model only 1 of correlated variables can be selected? Is it because coule potentialy "weight"/"double count" the varialbe?
churn$TotalCharges <- NULL

# Bar plot of categorical variables
p1 <- ggplot(churn,aes(x = gender)) + 
        ggtitle("Gender") + 
        xlab("Gender") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
        ylab("Percentage") + 
        coord_flip() + 
        theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen))+
        ggtitle("Senior Citizen")+
        xlab("Senior Citizen")+
        geom_bar(aes(y = 100*(..count../sum(..count..))),width = 0.5)+
        ylab("Percentage")+
        coord_flip()+
        theme_minimal()
p3 <- ggplot(churn, aes(x=Partner))+
        ggtitle("Partner")+
        xlab("Partner")+
        geom_bar(aes(y = 100*(..count../sum(..count..))),width = 0.5)+
        ylab("Percentage")+
        coord_flip()+
        theme_minimal()
p4 <- ggplot(churn,aes(x = Dependents))+
        ggtitle("Dependents")+
        xlab("Dependents")+
        geom_bar(aes(y = 100*(..count../sum(..count..))),width = 0.5)+
        ylab("Percentage")+
        coord_flip()+
        theme_minimal()

grid.arrange(p1,p2,p3,p4, ncol=2)
p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)

#First, we split the data into training and testing sets

intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]

#Fitting the Logistic Regression Model
#This part throw an error "invalid type (list) for variable 'churn'", that because Churn (capital C) is a column in training dataset but churn is a dataframe
LogModel <- glm(Churn~.,family = binomial(link = "logit"), data = training) 
print(summary(LogModel))

#The top three most-relevant features include Contract, tenure_group and PaperlessBilling.
anova(LogModel, test="Chisq")
