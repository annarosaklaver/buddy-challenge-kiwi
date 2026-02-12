library(lme4)
library(lmerTest)
library(plyr)
library(dplyr)
library(emmeans)
library(ggplot2)


##############Experiment data1
### Load data

data<-AGI_study_1_data
#Exclude trial exclusions
data<-subset(data,Include=="y")

#change condition to Factor
data$condition<-as.factor(data$Condition)
#change Locations and Letters to numeric
data$letters<-as.numeric(data$Letters)
data$locations<-as.numeric(data$Locations)


####Speech Variables: Does each speech variable vary by condition?
words<-lmer(wordcount~condition+(condition|Participant)+(condition|Trial),data=data)
summary(words)
lsmeansLT(words)
difflsmeans(words)


time<-lmer(Seconds~condition+(condition|Participant)+(condition|Trial),data=data)
summary(time)
lsmeansLT(time)
difflsmeans(time)

##singular fit, drop slopes
time<-lmer(Seconds~condition+(1|Participant)+(1|Trial),data=data)
summary(time)
lsmeansLT(time)
difflsmeans(time)

fluency<-lmer(Disfluenices~condition+(condition|Participant)+(condition|Trial),data=data)
summary(fluency)
lsmeansLT(fluency)
difflsmeans(fluency)

##singular.fit, drop slopes
fluency<-lmer(Disfluenices~condition+(1|Participant)+(1|Trial),data=data)
summary(fluency)
lsmeansLT(fluency)
difflsmeans(fluency)

index.speech<-glmer(index.speech~Condition+(Condition|Participant)+(Condition|Trial),family=binomial,data=data)
summary(index.speech)


##singular fit, drop slopes
index.speech<-glmer(index.speech~Condition+(1|Participant)+(1|Trial),family=binomial,data=data)
summary(index.speech)
# Use the aggregate function to calculate the proportion of index.speech in each level of Condition
prop_index_speech <- aggregate(index.speech ~ Condition, data = data, FUN = function(x) mean(x))
prop_index_speech

###Speech variables: Does each speech variable predict memory on secondary task?
##Letters
wordcount.letters<-lmer(letters~wordcount+(1|Participant)+(1|Trial),data=data)
summary(wordcount.letters)
time.letters<-lmer(letters~Seconds+(1|Participant)+(1|Trial),data=data)
summary(time.letters)
fluency.letters<-lmer(letters~Disfluenices+(1|Participant)+(1|Trial),data=data)
summary(fluency.letters)
index.letters<-lmer(letters~index.speech+(1|Participant)+(1|Trial),data=data)
summary(index.letters)

##Locations
wordcount.locations<-lmer(locations~wordcount+(1|Participant)+(1|Trial),data=data)
summary(wordcount.locations)
time.locations<-lmer(locations~Seconds+(1|Participant)+(1|Trial),data=data)
summary(time.locations)
fluency.locations<-lmer(locations~Disfluenices+(1|Participant)+(1|Trial),data=data)
summary(fluency.locations)
index.locations<-lmer(locations~index.speech+(1|Participant)+(1|Trial),data=data)
summary(index.locations)
avg_locations <- aggregate(locations ~ index.speech, data = data,
                           FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x))))

# Print the average locations and standard errors in each level of index.speech
avg_locations


###############model comparison for Experiment 1
##Letters

# Fit the full model with all four speech variables as control variables, drop slopes due to non-convergence
full.model <- lmer(letters ~ Disfluenices + Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data)


##Compute models as a list
models<-list(full.model, lmer(letters ~ Disfluenices + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ Seconds + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ wordcount + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ index.speech + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ Disfluenices + Seconds + Condition + (1|Participant) + (1|Trial), data),
             lmer(letters ~ Disfluenices + wordcount + Condition + (1|Participant) + (1|Trial), data=data),
             lmer(letters ~ Disfluenices + index.speech + Condition + (1|Participant) + (1|Trial), data =data),
             lmer(letters ~ Seconds + wordcount + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ Seconds + index.speech + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ Disfluenices+ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ Seconds + Disfluenices + wordcount + Condition + (1|Participant) + (1|Trial), data = data),
             lmer(letters ~ Seconds + Disfluenices + index.speech + Condition + (1|Participant) + (1|Trial), data = data))


# Compute AIC scores
AIC_scores <- sapply(models, function(m) AIC(m))

# Identify the best model
best_model <- which.min(AIC_scores)
summary(models[[best_model]])

##Summary of the best model
best.letters<-lmer(letters ~ Disfluenices + condition + (1|Participant) + (1|Trial), data = data)
lsmeansLT(best.letters)
difflsmeans(best.letters)


###Locations
##Best model for locations
full.model.locs <- lmer(locations ~ Disfluenices + Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data)

# Compute models as a list
models.locs <- list(full.model.locs,
                    lmer(locations ~ Disfluenices + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Seconds + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ wordcount + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ index.speech + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Disfluenices + Seconds + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Disfluenices + wordcount + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Disfluenices + index.speech + Condition + (1|Participant) + (1|Trial), data =data),
                    lmer(locations ~ Seconds + wordcount + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Seconds + index.speech + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data), 
                    lmer(locations ~ Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Disfluenices+ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Seconds + Disfluenices + wordcount + Condition + (1|Participant) + (1|Trial), data = data),
                    lmer(locations ~ Seconds + Disfluenices + index.speech + Condition + (1|Participant) + (1|Trial), data = data))


# Compute AIC scores
AIC_scores.locs <- sapply(models.locs, function(m) AIC(m))

# Identify the best model
best_model.locs <- which.min(AIC_scores.locs)
summary(models.locs[[best_model.locs]])

##Summary of the best model
best.locations<-lmer(locations ~ Disfluenices + index.speech+condition + (1|Participant) + (1|Trial), data = data)
lsmeansLT(best.locations)
difflsmeans(best.locations)

##Figure
# Aggregate the "letters" variable by averaging across trials for each participant
data_agg_letters <- aggregate(data$letters, by = list(data$Participant, data$condition), mean)
colnames(data_agg_letters) <- c("participant", "condition", "letters")

# Create a grouped violin plot to show the distribution of scores across participants
# for each of the three conditions
ggplot(data_agg_letters, aes(x = condition, y = letters)) +
  geom_violin(scale = "width", trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.5) +
  geom_jitter(size = 1, width = 0.2, height = 0, color="black") +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9), alpha = 0) +
  labs(x = "", y = "Average Letters Recalled", fill = "Condition", color = "Condition") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = c("Gesture", "Make", "Hands Still")) +
  ylim(0,5)

# Save the plot as a JPG file
ggsave("Exp1Letters.jpg", dpi = 300, width = 8, height = 6)



# Aggregate the "locations" variable by averaging across trials for each participant
data_agg_locations <- aggregate(data$locations, by = list(data$Participant, data$condition), mean)
colnames(data_agg_locations) <- c("participant", "condition", "letters")

# Create a grouped violin plot to show the distribution of scores across participants
# for each of the three conditions
ggplot(data_agg_locations, aes(x = condition, y = letters)) +
  geom_violin(scale = "width", trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.5) +
  geom_jitter(size = 1, width = 0.2, height = 0, color="black") +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9), alpha = 0) +
  labs(x = "", y = "Average Locations Recalled", fill = "Condition", color = "Condition") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = c("Gesture", "Make", "Hands Still")) +
  ylim(0,5)

# Save the plot as a JPG file
ggsave("Exp1Locations.jpg", dpi = 300, width = 8, height = 6)

##################################################################################Experiment 2
### Load data
data2<-read.csv("AGI study 2 data.csv")
data2<-subset(data2,Include=="y")
data2$Condition<-as.factor(data2$condition)


####Speech Variables: Does each speech variable vary by condition (E2)?
words2<-lmer(wordcount~Condition+(Condition|Participant)+(Condition|Trial),data=data2)
summary(words2)
lsmeansLT(words2)
difflsmeans(words2)

#singularfit, drop slopes
words2<-lmer(wordcount~Condition+(1|Participant)+(1|Trial),data=data2)
summary(words2)
lsmeansLT(words2)
difflsmeans(words2)


time2<-lmer(Seconds~Condition+(Condition|Participant)+(Condition|Trial),data=data2)
summary(time2)
lsmeansLT(time2)
difflsmeans(time2)

#singular fit, drop slopes
time2<-lmer(Seconds~Condition+(1|Participant)+(1|Trial),data=data2)
summary(time2)
lsmeansLT(time2)
difflsmeans(time2)

fluency2<-lmer(Disfluencies~Condition+(Condition|Participant)+(Condition|Trial),data=data2)
#failed to converge--remove random slopes
fluency2<-lmer(Disfluencies~Condition+(1|Participant)+(1|Trial),data=data2)
summary(fluency2)
lsmeansLT(fluency2)
difflsmeans(fluency2)

data2$index.speech<-as.factor(data2$index.speech)
index.speech2<-glmer(index.speech~Condition+(Condition|Participant)+(Condition|Trial),family=binomial,data=data2)
summary(index.speech)


##singular fit, drop slopes
index.speech2<-glmer(index.speech~Condition+(1|Participant)+(1|Trial),family=binomial,data=data2)
summary(index.speech2)

# Use ifelse() function to replace "y" with 1 and "n" with 0
data2$index.speech <- ifelse(data2$index.speech == "y", 1, 0)

# Use the aggregate function to calculate the proportion of index.speech in each level of Condition
prop_index_speech2 <- aggregate(index.speech ~ Condition, data = data2, FUN = function(x) mean(x))
prop_index_speech2


###Speech variables: Does each speech variable predict memory on secondary task?(E2)
##Letters(E2)
wordcount.letters2<-lmer(Letters~wordcount+(1|Participant)+(1|Trial),data=data2)
summary(wordcount.letters2)
time.letters2<-lmer(Letters~Seconds+(1|Participant)+(1|Trial),data=data2)
summary(time.letters2)
fluency.letters2<-lmer(Letters~Disfluencies+(1|Participant)+(1|Trial),data=data2)
summary(fluency.letters2)
index.letters2<-lmer(Letters~index.speech+(1|Participant)+(1|Trial),data=data2)
summary(index.letters2)

##Locations(E2)
wordcount.locations2<-lmer(Locations~wordcount+(1|Participant)+(1|Trial),data=data2)
summary(wordcount.locations2)
time.locations2<-lmer(Locations~Seconds+(1|Participant)+(1|Trial),data=data2)
summary(time.locations2)
fluency.locations2<-lmer(Locations~Disfluencies+(1|Participant)+(1|Trial),data=data2)
summary(fluency.locations2)
index.locations2<-lmer(Locations~index.speech+(1|Participant)+(1|Trial),data=data2)
summary(index.locations2)

#######################Experiment 2 Model comparison


##E2Letters
data2$letters<-data2$Letters
# Fit the full model with all four speech variables as control variables
full.model.2 <- lmer(letters ~ Disfluencies + Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2)

# Compute models as a list
models2 <- list(full.model.2,
               lmer(letters ~ Disfluencies + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Seconds + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Disfluencies + Seconds + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Disfluencies + wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Disfluencies + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Seconds + wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Seconds + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Disfluencies+ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Seconds + Disfluencies + wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
               lmer(letters ~ Seconds + Disfluencies + index.speech + Condition + (1|Participant) + (1|Trial), data = data2))


# Compute AIC scores
AIC_scores2 <- sapply(models2, function(m) AIC(m))

# Identify the best model
best_model2 <- which.min(AIC_scores2)
summary(models2[[best_model2]])

##Summary for best model
best_model2letters<-lmer(letters ~ Seconds + Condition + (1|Participant) + (1|Trial), data = data2)
summary(best_model2letters)
lsmeansLT(best_model2letters)
difflsmeans(best_model2letters)

##################################E2 Locations Best model fit
data2$locations<-data2$Locations
# Fit the full model with all four speech variables as control variables
full.model.2locs <- lmer(locations ~ Disfluencies + Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2)

# Compute models as a list
models2locs <- list(
  full.model.2locs,
  lmer(locations ~ Disfluencies + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Seconds + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Disfluencies + Seconds + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Disfluencies + wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Disfluencies + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Seconds + wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Seconds + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Seconds + wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Disfluencies+ wordcount + index.speech + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Seconds + Disfluencies + wordcount + Condition + (1|Participant) + (1|Trial), data = data2),
  lmer(locations ~ Seconds + Disfluencies + index.speech + Condition + (1|Participant) + (1|Trial), data = data2))

# Compute AIC scores
AIC_scores2locs <- sapply(models2locs, function(m) AIC(m))

# Identify the best model
best_model2locs <- which.min(AIC_scores2locs)

# Print summary of the best model
summary(models2locs[[best_model2locs]])

##Summary for best model
best_model2locations<-lmer(locations ~ index.speech + Condition + (1|Participant) + (1|Trial), data = data2)
summary(best_model2locations)
lsmeansLT(best_model2locations)
difflsmeans(best_model2locations)

##Figure
# Aggregate the "letters" variable by averaging across trials for each participant
data_agg_letters2 <- aggregate(data2$letters, by = list(data2$Participant, data2$condition), mean)
colnames(data_agg_letters2) <- c("participant", "condition", "letters")

# Create a grouped violin plot to show the distribution of scores across participants
# for each of the three conditions
ggplot(data_agg_letters2, aes(x = condition, y = letters)) +
  geom_violin(scale = "width", trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.5) +
  geom_jitter(size = 1, width = 0.2, height = 0, color="black") +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9), alpha = 0) +
  labs(x = "", y = "Average Letters Recalled", fill = "Condition", color = "Condition") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = c("Gesture", "Make", "Hands Still")) +
  ylim(0,5)

# Save the plot as a JPG file
ggsave("Exp2Letters.jpg", dpi = 300, width = 8, height = 6)


##Combine datasets
View(AGI_Data_Both_Studies)
data3<-AGI_Data_Both_Studies
data3<-subset(data3,Include=="y")
attach(data3)
#change condition to Factor
condition<-as.factor(Condition)
#change Locations and Letters to numeric
letters<-as.numeric(Letters)
locations<-as.numeric(Locations)
study<-as.factor(Study)

###Does condition predict letters recalled?
model.letters<-lmer(letters~condition*Study+(condition|PpID)+(Trial|PpID))
#failed to converge, drop slopes
model.letters<-lmer(letters~relevel(condition,"NG")*Study+(1|PpID)+(1|Trial))
summary(model.letters)
lsmeansLT(model.letters)
difflsmeans(model.letters)














