df_seeds <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WNCAATourneySeeds.csv")
df_tour <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WNCAATourneyCompactResults.csv")
df_reg <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WRegularSeasonCompactResults.csv")
teams <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WTeams.csv")

#WGameCities <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WGameCities.csv")

####################### Data ####################### 

scores_t <- df_tour$WScore
wins_t <- c(rep("Tournament",length(scores_t)))
scores_r <- df_reg$WScore
wins_r <- c(rep("Regular",length(scores_r)))

my_t <- cbind(scores_t,wins_t)
my_r <- cbind(scores_r,wins_r)
my_df <- rbind(my_t,my_r)

my_df <- as.data.frame(my_df)
colnames(my_df) <- c("Score","Status")
my_df$Score <- as.numeric(as.character(my_df$Score))

ggplot(my_df, aes(Score)) +
  geom_density(aes(fill=factor(Status)), alpha=0.8) + 
  labs(title="Distribution of Winning Scores", 
       x="Score",
       y = "Density",
       fill="Status")

scores_t <- df_tour$LScore
loss_t <- c(rep("Tournament",length(scores_t)))
scores_r <- df_reg$LScore
loss_r <- c(rep("Regular",length(scores_r)))

my_t <- cbind(scores_t,loss_t)
my_r <- cbind(scores_r,loss_r)
my_df <- rbind(my_t,my_r)

my_df <- as.data.frame(my_df)
colnames(my_df) <- c("Score","Status")
my_df$Score <- as.numeric(as.character(my_df$Score))

ggplot(my_df, aes(Score)) +
  geom_density(aes(fill=factor(Status)), alpha=0.8) + 
  labs(title="Distribution of Lossing Scores", 
       x="Score",
       y = "Density",
       fill="Status")

ggplot(df_tour, aes(WScore)) + 
  geom_histogram(binwidth = 5, 
                 fill = "mediumturquoise",
                 colour = "gray92",
                 size=1) +  # change binwidth
  labs(title="Histogram of Winning Scores",
       x = "Score",
       y = "Frequency") 

ggplot(df_tour, aes(LScore)) + 
  geom_histogram(binwidth = 5, 
                 fill = "mediumturquoise",
                 colour = "gray92",
                 size=1) +  # change binwidth
  labs(title="Histogram of Loosing Scores",
       x = "Score",
       y = "Frequency")

scores_w <- df_tour$WScore
wins <- c(rep("Winner",length(scores_w)))
scores_l <- df_tour$LScore
losses <- c(rep("Loser",length(scores_l)))

my_win <- cbind(scores_w,wins)
my_loss <- cbind(scores_l,losses)
my_df <- rbind(my_win,my_loss)

my_df <- as.data.frame(my_df)
colnames(my_df) <- c("Score","Status")
my_df$Score <- as.numeric(as.character(my_df$Score))

ggplot(my_df, aes(Status, Score)) + 
  geom_boxplot(varwidth=T, fill="plum") + 
  labs(x="Winner/Loser",
       y="Score")



####################### Investigation ####################### 
df_seeds$seed_int <- substr(df_seeds$Seed, 2, 3)
df_seeds$seed_int <- as.numeric(df_seeds$seed_int)

df_seeds <- df_seeds[c("Season","TeamID","seed_int")]
df_tour <- df_tour[c("Season","WTeamID","LTeamID")]

df_winseeds <- df_seeds
colnames(df_winseeds) <- c("Season","WTeamID","WSeed")

df_lossseeds <- df_seeds
colnames(df_lossseeds) <- c("Season","LTeamID","LSeed")

df_temp <- merge(df_tour,df_winseeds, by = c("Season","WTeamID"))
df_concat <- merge(df_temp, df_lossseeds, by = c("Season","LTeamID"))
df_concat$seed_diff <- df_concat$WSeed - df_concat$LSeed
head(df_concat)

################ for Data ###############
ggplot(df_concat, aes(seed_diff)) + 
  geom_histogram(binwidth = 5, 
                 fill = "mediumturquoise",
                 colour = "gray92",
                 size=1) +  # change binwidth
  labs(title="Histogram of Seed Differences",
       x = "Seed Difference",
       y = "Frequency") 

#########################################

# Create training data set
w <- df_concat$seed_diff
r <- c(rep(1, length(w)))
df_wins <- as.data.frame(cbind(w,r))
colnames(df_wins) <- c("SeedDiff","Result")

l <- -df_concat$seed_diff
r <- c(rep(0,length(l)))
df_losses <- as.data.frame(cbind(l,r))
colnames(df_losses) <- c("SeedDiff","Result")

df_predictions <- as.data.frame(rbind(df_wins,df_losses))
head(df_predictions)

reshape <- list()
index <- 1
for(p in df_predictions$SeedDiff){
  reshape[[index]] <- p
  index <- index + 1
}

X_train <- reshape
Y_train <- df_predictions$Result

train <- as.data.frame(cbind(X_train,Y_train))
train <- train[sample(nrow(train)),]

trainingData <- data.frame(unlist(train$X_train),unlist(train$Y_train))
colnames(trainingData) <- c("X_TRAIN","Y_TRAIN")


logitMod <- glm(Y_TRAIN ~ X_TRAIN, 
                data=trainingData, family=binomial(link="logit"))

summary(logitMod)

ggplot(trainingData, aes(x=X_TRAIN, y=Y_TRAIN)) + 
  geom_point() + 
  #geom_abline(intercept = -3.335e-16, slope = -2.625e-01, color = "red") +
  stat_smooth(method = glm) +
  labs(y="Results", 
       x="Seed Diff", 
       title="Logistic Regression")

ncaa.res = resid(logitMod)
trainingData$residuals <- ncaa.res
trainingData$index <- c(1:length(trainingData$X_TRAIN))

#transformed residual plot
ggplot(trainingData, aes(x=index, y=residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") +  
  labs(y="Residual", 
       x="Index", 
       title="Residual Plot")

#histogram of transformed residuals
ggplot(trainingData, aes(residuals)) + 
  geom_histogram(binwidth = 1, 
                 fill = "mediumturquoise",
                 colour = "gray92",
                 size=1) +  # change binwidth
  labs(title="Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")  

#qqplot of transformed residuals
ggplot(trainingData, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")


#predicted <- predict(logitMod, testData, type="response")


#################### Team Rank Logistic Regression ########################

df_tour <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WNCAATourneyCompactResults.csv")

df_temp <- merge(df_tour,df_winseeds, by = c("Season","WTeamID"))
df_concat <- merge(df_temp, df_lossseeds, by = c("Season","LTeamID"))

#only tournament wins/losses

scores_w <- df_tour$WScore
teams_w <- df_tour$WTeamID
wins <- c(rep("Winner",length(scores_w)))
scores_l <- df_tour$LScore
teams_l <- df_tour$LTeamID
losses <- c(rep("Loser",length(scores_l)))

my_win <- cbind(scores_w,teams_w,wins)
my_loss <- cbind(scores_l,teams_l,losses)
my_df <- rbind(my_win,my_loss)

my_df <- as.data.frame(my_df)
colnames(my_df) <- c("Score","TeamID","Status")
my_df$Score <- as.numeric(as.character(my_df$Score))

df_predictions <- merge(my_df, teams, by = c("TeamID"))
df_predictions$Bstatus <- ifelse(df_predictions$Status=="Winner",1,0)
head(df_predictions)

team_logit <- glm(Bstatus ~ Score + TeamName, data=df_predictions, 
                  family=binomial(link = "logit"))
summary(team_logit)

plot(team_logit)

ggplot(df_predictions, aes(x=Score, y=Bstatus)) + 
  geom_point() + 
  #geom_abline(intercept = -3.335e-16, slope = -2.625e-01, color = "red") +
  stat_smooth(method = glm) +
  labs(y="Results", 
       x="Score", 
       title="Logistic Regression")

team.res = resid(team_logit)
df_predictions$residuals <- team.res
df_predictions$index <- c(1:length(df_predictions$Bstatus))
head(df_predictions)

#transformed residual plot
ggplot(df_predictions, aes(x=index, y=residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") +  
  labs(y="Residual", 
       x="Index", 
       title="Residual Plot")

#histogram of transformed residuals
ggplot(df_predictions, aes(residuals)) + 
  geom_histogram(binwidth = 1, 
                 fill = "mediumturquoise",
                 colour = "gray92",
                 size=1) +  # change binwidth
  labs(title="Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")  

#qqplot of transformed residuals
ggplot(df_predictions, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")


#### for data portion ###############################
ggplot(df_predictions, aes(x=TeamID, y=Score)) + 
  geom_point(aes(col=Status)) + 
  #xlim(c(0, 0.1)) + 
  #ylim(c(0, 500000)) + 
  theme_light() +
  theme(panel.grid = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  labs(y="Score", 
       x="Team ID", 
       title="Scores per Team")
#####################################################

head(df_seeds)


head(df_tour)

all_teams <- sort(unique(c(df_tour$WTeamID,df_tour$LTeamID)))

team_wins <- list()
index <- 1

for(team in all_teams){
  temp <- df_tour[which(df_tour$WTeamID==team),]
  num_wins <- nrow(temp)
  team_wins[[index]] <- data.frame(teamID = team,
                                   wins = num_wins)
  index <- index + 1
}

my_df <- team_wins[[1]]
for(i in 2:length(team_wins)){
  my_df <- rbind(my_df,team_wins[[i]])
}

my_df

my_df[which(my_df$wins > 3),]
