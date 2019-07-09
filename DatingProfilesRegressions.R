setwd("C:/Users/maddy/Documents/IoC Work/profiles.csv")
getwd()

data <- read.csv("profiles.csv")


##Create Gender variable by altering sex variable inputs to integers
data$Gender <- NA
data$Gender[data$sex == "m"] <- 1
data$Gender[data$sex == "f"] <- 2

##Create StatusNum variable by altering relationship status variable inputs to integers
data$StatusNum <- NA
data$StatusNum[data$status == 0] <- NA
data$StatusNum[data$status == "single"] <- 1
data$StatusNum[data$status == "available"] <- 2
data$StatusNum[data$status == "seeing someone"] <- 3
data$StatusNum[data$status == "married"] <- 4


##Create StarSign variable by cleaning all inputs of sign variable to only represent string of each star sign
data$StarSign <- NA
data$StarSign[data$sign == "aquarius" | data$sign == "aquarius and it matters a lot" |
                data$sign == "aquarius and it&rsquo;s fun to think about" |
                data$sign == "aquarius but it doesn&rsquo;t matter"] <- "aquarius"
data$StarSign[data$sign == "aries" | data$sign == "aries and it matters a lot" |
                data$sign == "aries and it&rsquo;s fun to think about" |
                data$sign == "aries but it doesn&rsquo;t matter"] <- "aries"
data$StarSign[data$sign == "cancer" | data$sign == "cancer and it matters a lot" |
                data$sign == "cancer and it&rsquo;s fun to think about" |
                data$sign == "cancer but it doesn&rsquo;t matter"] <- "cancer"
data$StarSign[data$sign == "capricorn" | data$sign == "capricorn and it matters a lot" |
                data$sign == "capricorn and it&rsquo;s fun to think about" |
                data$sign == "capricorn but it doesn&rsquo;t matter"] <- "capricorn"
data$StarSign[data$sign == "gemini" | data$sign == "gemini and it matters a lot" |
                data$sign == "gemini and it&rsquo;s fun to think about" |
                data$sign == "gemini but it doesn&rsquo;t matter"] <- "gemini"
data$StarSign[data$sign == "leo" | data$sign == "leo and it matters a lot" |
                data$sign == "leo and it&rsquo;s fun to think about" |
                data$sign == "leo but it doesn&rsquo;t matter"] <- "leo"
data$StarSign[data$sign == "libra" | data$sign == "libra and it matters a lot" |
                data$sign == "libra and it&rsquo;s fun to think about" |
                data$sign == "libra but it doesn&rsquo;t matter"] <- "libra"
data$StarSign[data$sign == "pisces" | data$sign == "pisces and it matters a lot" |
                data$sign == "pisces and it&rsquo;s fun to think about" |
                data$sign == "pisces but it doesn&rsquo;t matter"] <- "pisces"
data$StarSign[data$sign == "sagittarius" | data$sign == "sagittarius and it matters a lot" |
                data$sign == "sagittarius and it&rsquo;s fun to think about" |
                data$sign == "sagittarius but it doesn&rsquo;t matter"] <- "sagittarius"
data$StarSign[data$sign == "scorpio" | data$sign == "scorpio and it matters a lot" |
                data$sign == "scorpio and it&rsquo;s fun to think about" |
                data$sign == "scorpio but it doesn&rsquo;t matter"] <- "scorpio"
data$StarSign[data$sign == "taurus" | data$sign == "taurus and it matters a lot" |
                data$sign == "taurus and it&rsquo;s fun to think about" |
                data$sign == "taurus but it doesn&rsquo;t matter"] <- "taurus"
data$StarSign[data$sign == "virgo" | data$sign == "virgo and it matters a lot" |
                data$sign == "virgo and it&rsquo;s fun to think about" |
                data$sign == "virgo but it doesn&rsquo;t matter"] <- "virgo"

##Make StarSign variable factor
data$StarSign <- as.factor(data$StarSign)
data$status <- as.factor(data$status)

##Create Drink variable by altering drinks variable inputs to integers
data$Drink <- NA
data$Drink[data$drinks == "not at all"] <- 0
data$Drink[data$drinks == "rarely"] <- 1
data$Drink[data$drinks == "socially"] <- 2
data$Drink[data$drinks == "often"] <- 3
data$Drink[data$drinks == "very often"] <- 4
data$Drink[data$drinks == "desperately"] <- 5

##Create Drug variable by altering drugs variable inputs to integers
data$Drug <- NA
data$Drug[data$drugs == "never"] <- 0
data$Drug[data$drugs == "sometimes"] <- 1
data$Drug[data$drugs == "often"] <- 2

##Create Smoke variable by altering smokes variable inputs to integers
data$Smoke <- NA
data$Smoke[data$smokes == "no"] <- 0
data$Smoke[data$smokes == "trying to quit"] <- 1
data$Smoke[data$smokes == "sometimes"] <- 2
data$Smoke[data$smokes == "when drinking"] <- 3
data$Smoke[data$smokes == "yes"] <- 4


##Regression analysis of different variables within the data to see if there are any
##interesting or significant results that may be related or explain one another.

##Regression of age explained by Star Sign
reg <- lm(data$age ~ data$StarSign)
summary(reg)
      ##Significant negative relationship between age and cancer, capricorn, and scorpio.
      ##Very significant negative relationship between age and libra
      ##Extremely significant negative relationship between age and leo/virgo.


##Regression of drinking explained by Star Sign
reg2 <- lm(data$Drink ~ data$StarSign)
summary(reg2)
      ##Significant slightly positive relationship between drinking and pisces.
reg20 <- lm(data$Drink ~ data$StarSign + data$age)
summary(reg20)
      ##Significant slightly positive relationship between drinking and pisces, controlled for age.
reg21 <- lm(data$Drink ~ data$StarSign + data$age + data$Gender)
summary(reg21)
      ##Significant slightly positive relationship between drinking and pisces, controlled for age and gender.

install.packages("stargazer")
library(stargazer)
## OLS models
linear.1 <- lm(Drink ~ StarSign + age + Gender,data=data)

## create an indicator dependent variable, and run a probit model

table <- stargazer(linear.1, title="Regression Results", align=TRUE)
table


stargazer(linear.1, type="text", title="Regression Results")




##Regression of drug use explained by Star Sign
reg3 <- lm(data$Drug ~ data$StarSign)
summary(reg3)
      ##No significant relationships between drug use and star sign.


##Regression of smoking explained by Star Sign
reg4 <- lm(data$Smoke ~ data$StarSign)
summary(reg4)
      ##No significant relationships between smoking and star sign.


##Regression of drug use explained by drinking
reg5 <- lm(data$Drug ~ data$Drink)
summary(reg5)
      ##Extremely significant relationship between drug use and drinking


##Regression of smoking explained by drinking
reg6 <- lm(data$Smoke ~ data$Drink)
summary(reg6)
      ##Extremely significant relationship between smoking and drinking


##Regression of relationship status explained by Star Sign
reg7 <- lm(data$StatusNum ~ data$StarSign)
summary(reg7)
      ##No significant relationship between relationship status and star sign


#Regression of relationship status explained by drinking
reg8 <- lm(data$StatusNum ~ data$Drink)
summary(reg8)
      ##No significant relationship between relationship status and drinking


#Regression of relationship status explained by drugs
reg9 <- lm(data$StatusNum ~ data$Drug)
summary(reg9)
      ##Extremely significant relationship between relationship status and drugs
reg91 <- lm(data$StatusNum ~ data$Drug + data$age)
summary(reg91)
reg92 <- lm(data$StatusNum ~ data$Drug + data$age + data$Gender)
summary(reg92)


#Regression of relationship status explained by drugs and gender
reg10 <- lm(data$StatusNum ~ data$Drug * factor(data$Gender))
summary(reg10)
      ##Extremely significant relationship between relationship status being more serious if both genders are doing
      ##drugs


#Regression of relationship status explained by smoking
reg11 <- lm(data$StatusNum ~ data$Smoke)
summary(reg11)
      ##Significant relationship between relationship status and smoking


##Regression of relationship status explained by age
reg12 <- lm(data$StatusNum ~ data$age)
summary(reg12)
    ##Extremely significant relationship between relationship status and age