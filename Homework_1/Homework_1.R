library(RCurl)

data <- read.csv("https://raw.githubusercontent.com/jhamski/DATA621-Homework/master/Homework_1/moneyball-training-data.csv?token=AOktEMRsQhHYy-5Se8VDAS3-c9q-YevKks5XZWRfwA%3D%3D", header=TRUE, stringsAsFactors = FALSE, sep = ",")
data_before <- data

#Data appears to be normally distributed, so I beleive that the NA's can be replaced by a sample of normally distributed values.
#It keeps the same shape with the data and doesn't add any outliers.

plot(TARGET_WINS~TEAM_BATTING_SO, data=data)
hist(TEAM_BATTING_SO)
summary(lm(TARGET_WINS~TEAM_BATTING_SO, data=data))
boxplot(data$TEAM_BATTING_SO)

values <- round(sample(rnorm(n= 500, mean= mean(data$TEAM_BATTING_SO, na.rm = TRUE), sd = sd(data$TEAM_BATTING_SO, na.rm = TRUE)), 200),0)
values[which(values > 0)]

i <- 1
j <- 1
for (i in 1:NROW(data$TEAM_BATTING_SO))
  {
     if(is.na(data$TEAM_BATTING_SO[i]))
    {
     data$TEAM_BATTING_SO[i] <- values[j]
     j < - j + 1
    }
}

#Data is heavily skewed to the right. A random sample of normal data points will not work to replace the NA's because the data is not normal.
#The regression line appears to be a better choice. The values of the regression equation are very significant. The R value may be low, but
#the components of the line seem very significant. A regression line may be the better way to fill in the missing values.

plot(TARGET_WINS~TEAM_BASERUN_SB, data=data)
hist(data$TEAM_BASERUN_SB)
summary(lm(TARGET_WINS~TEAM_BASERUN_SB, data=data))

i <- 1
for (i in 1:NROW(data$TEAM_BASERUN_SB))
{
  if(is.na(data$TEAM_BASERUN_SB[i]))
  {
    data$TEAM_BASERUN_SB[i] <- .02273*data$TARGET_WINS[i] + 78.00909
  }
}

#The data is heavily skewed to the right. Also, the data by itself doe not really appear to have any correlation to the number of wins that 
#the team gets throught the years. It is not very often that a player get caught stealing in a game. Steals are somewhat of a rareity in the
#game of baseball and outof those who do steal. Very few of them get thrown out. I would personally not use this data value at all. It is a 
#rather rare occasion in a baseball game, and there really is no correaltion to wins at all. Due to the fact that the data is very skewed, using
#the mean would not be advised. There are a lot outliers, so that will make you mean higher then it is supposed to be. The median will be a better 
#choice.

plot(TARGET_WINS~TEAM_BASERUN_CS, data=data)
hist(data$TEAM_BASERUN_CS)
summary(lm(TARGET_WINS~TEAM_BASERUN_CS, data=data))
boxplot(data$TEAM_BASERUN_CS)

i <- 1
for (i in 1:NROW(data$TEAM_BASERUN_CS))
{
  if(is.na(data$TEAM_BASERUN_CS[i]))
  {
    data$TEAM_BASERUN_CS[i] <- median(data$TEAM_BASERUN_CS, na.rm = TRUE)
  }
}

#Drop the HBP stat
data <- data[,-11]

#This data is very skewed to the right, so much that you can't real'y even see the smaller points becase there are a lot of outliers. The R value
#of the best fit line is rather small, but the intercept and the slope are highly significant. als the p-value is very small, way below .05. 
#Also, the residual standard error is only 15, which is pretty good in my opinion.

plot(TARGET_WINS~TEAM_PITCHING_SO, data=data)
hist(data$TEAM_PITCHING_SO)
summary(lm(TARGET_WINS~TEAM_PITCHING_SO, data=data))
boxplot(data$TEAM_PITCHING_SO)

i <- 1
for (i in 1:NROW(data$TEAM_PITCHING_SO))
{
  if(is.na(data$TEAM_PITCHING_SO[i]))
  {
    data$TEAM_PITCHING_SO[i] <- -0.0022085*data$TARGET_WINS[i] + 82.5704787
  }
}

#This data apears to be pretty normal. That means that I am going to keep the same shape of the data and create a random sample of numbers
#with the same mean and standard deviation of the original data. that should keep the data in the same shape and not introduce anymore outliers.

plot(TARGET_WINS~TEAM_FIELDING_DP, data=data)
hist(data$TEAM_FIELDING_DP)
summary(lm(TARGET_WINS~TEAM_FIELDING_DP, data=data))
boxplot(data$TEAM_FIELDING_DP)

values <- round(sample(rnorm(n= 500, mean= mean(data$TEAM_FIELDING_DP, na.rm = TRUE), sd = sd(data$TEAM_FIELDING_DP, na.rm = TRUE)), 200),0)
values[which(values > 0)]

i <- 1
j <- 1
for (i in 1:NROW(data$TEAM_FIELDING_DP))
{
  if(is.na(data$TEAM_FIELDING_DP[i]))
  {
    data$TEAM_FIELDING_DP[i] <- values[j]
    j < - j + 1
  }
}
