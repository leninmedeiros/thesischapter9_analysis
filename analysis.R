results_3daysfull <- read.table("Final_Full.csv", header=TRUE, sep=",")
### Hypothesis H1: Participants who interacted with the proposed chatbot, i.e. the individuals allocated 
###     in Groups B and H, have their levels of arousal reduced after such interactions.
### Hypothesis H2: Participants who interacted with the proposed chatbot, i.e. the individuals allocated 
###     in Groups B and H, have their levels of valence increased after such interactions.
### Hypothesis H3: Arousal level reduction is larger for participants in Group H 
###     than in Group B and it is larger in Group B than in Control Group.
### Hypothesis H4: Valence level increase is larger for participants in Group H 
###     than in Group B and it is larger in Group B than in Control Group.
### Randomisation Check Starts Here ###
### Preparing data for randomisation check (arousal) starts here. ###
bot_arousal_before1st_full = subset(results_3daysfull$a_b_d1, results_3daysfull$group == "bot")
human_arousal_before1st_full = subset(results_3daysfull$a_b_d1, results_3daysfull$group == "human")
control_arousal_before1st_full = subset(results_3daysfull$a_b_d1, results_3daysfull$group == "control")
data_arousal_before1st_full <- data.frame(
  Y=c(bot_arousal_before1st_full, human_arousal_before1st_full, control_arousal_before1st_full),
  Group =factor(rep(c("bot", "human", "control"), 
                    times=c(length(bot_arousal_before1st_full), length(human_arousal_before1st_full), length(control_arousal_before1st_full))))
)
summary(bot_arousal_before1st_full)
sd(bot_arousal_before1st_full)
summary(human_arousal_before1st_full)
sd(human_arousal_before1st_full)
summary(control_arousal_before1st_full)
sd(control_arousal_before1st_full)
analysis_of_variance_arousal_before1st_full <- aov(Y~Group, data=data_arousal_before1st_full)
### Preparing data for randomisation check (arousal) ends here. ###
### Preparing data for randomisation check (valence) starts here. ###
bot_valence_before1st_full = subset(results_3daysfull$v_b_d1, results_3daysfull$group == "bot")
human_valence_before1st_full = subset(results_3daysfull$v_b_d1, results_3daysfull$group == "human")
control_valence_before1st_full = subset(results_3daysfull$v_b_d1, results_3daysfull$group == "control")
data_valence_before1st_full <- data.frame(
  Y=c(bot_valence_before1st_full, human_valence_before1st_full, control_valence_before1st_full),
  Group =factor(rep(c("bot", "human", "control"), 
                    times=c(length(bot_valence_before1st_full), length(human_valence_before1st_full), length(control_valence_before1st_full))))
)
summary(bot_valence_before1st_full)
sd(bot_valence_before1st_full)
summary(human_valence_before1st_full)
sd(human_valence_before1st_full)
summary(control_valence_before1st_full)
sd(control_valence_before1st_full)
analysis_of_variance_valence_before1st_full <- aov(Y~Group, data=data_valence_before1st_full)
### Preparing data for randomisation check (valence) ends here. ###
anova(analysis_of_variance_arousal_before1st_full) ### Randomisation check for arousal.
anova(analysis_of_variance_valence_before1st_full) ### Randomisation check for valence.
### Randomisation Check Ends Here ###
### Testing H1 & H2 Starts Here ###
### Preparing data for testing H1 starts here. ###
bot_arousal_b_d1 <- subset(results_3daysfull, group == "bot", select=c(id, a_b_d1))
bot_arousal_b_d2 <- subset(results_3daysfull, group == "bot", select=c(id, a_b_d2))
bot_arousal_b_d3 <- subset(results_3daysfull, group == "bot", select=c(id, a_b_d3))
bot_arousal_b <- subset(merge(bot_arousal_b_d1, merge(bot_arousal_b_d2, bot_arousal_b_d3, by="id"), by="id"),select=-id)
human_arousal_b_d1 <- subset(results_3daysfull, group == "human", select=c(id, a_b_d1))
human_arousal_b_d2 <- subset(results_3daysfull, group == "human", select=c(id, a_b_d2))
human_arousal_b_d3 <- subset(results_3daysfull, group == "human", select=c(id, a_b_d3))
human_arousal_b <- subset(merge(human_arousal_b_d1, merge(human_arousal_b_d2, human_arousal_b_d3, by="id"), by="id"),select=-id)
chatbot_arousal_b <- rowMeans(rbind(bot_arousal_b,human_arousal_b))
bot_arousal_a_d1 <- subset(results_3daysfull, group == "bot", select=c(id, a_a_d1))
bot_arousal_a_d2 <- subset(results_3daysfull, group == "bot", select=c(id, a_a_d2))
bot_arousal_a_d3 <- subset(results_3daysfull, group == "bot", select=c(id, a_a_d3))
bot_arousal_a <- subset(merge(bot_arousal_a_d1, merge(bot_arousal_a_d2, bot_arousal_a_d3, by="id"), by="id"),select=-id)
human_arousal_a_d1 <- subset(results_3daysfull, group == "human", select=c(id, a_a_d1))
human_arousal_a_d2 <- subset(results_3daysfull, group == "human", select=c(id, a_a_d2))
human_arousal_a_d3 <- subset(results_3daysfull, group == "human", select=c(id, a_a_d3))
human_arousal_a <- subset(merge(human_arousal_a_d1, merge(human_arousal_a_d2, human_arousal_a_d3, by="id"), by="id"),select=-id)
chatbot_arousal_a <- rowMeans(rbind(bot_arousal_a,human_arousal_a))
### Preparing data for testing H1 ends here. ###
### Preparing data for testing H2 starts here. ###
bot_valence_b_d1 <- subset(results_3daysfull, group == "bot", select=c(id, v_b_d1))
bot_valence_b_d2 <- subset(results_3daysfull, group == "bot", select=c(id, v_b_d2))
bot_valence_b_d3 <- subset(results_3daysfull, group == "bot", select=c(id, v_b_d3))
bot_valence_b <- subset(merge(bot_valence_b_d1, merge(bot_valence_b_d2, bot_valence_b_d3, by="id"), by="id"),select=-id)
human_valence_b_d1 <- subset(results_3daysfull, group == "human", select=c(id, v_b_d1))
human_valence_b_d2 <- subset(results_3daysfull, group == "human", select=c(id, v_b_d2))
human_valence_b_d3 <- subset(results_3daysfull, group == "human", select=c(id, v_b_d3))
human_valence_b <- subset(merge(human_valence_b_d1, merge(human_valence_b_d2, human_valence_b_d3, by="id"), by="id"),select=-id)
chatbot_valence_b <- rowMeans(rbind(bot_valence_b,human_valence_b))
bot_valence_a_d1 <- subset(results_3daysfull, group == "bot", select=c(id, v_a_d1))
bot_valence_a_d2 <- subset(results_3daysfull, group == "bot", select=c(id, v_a_d2))
bot_valence_a_d3 <- subset(results_3daysfull, group == "bot", select=c(id, v_a_d3))
bot_valence_a <- subset(merge(bot_valence_a_d1, merge(bot_valence_a_d2, bot_valence_a_d3, by="id"), by="id"),select=-id)
human_valence_a_d1 <- subset(results_3daysfull, group == "human", select=c(id, v_a_d1))
human_valence_a_d2 <- subset(results_3daysfull, group == "human", select=c(id, v_a_d2))
human_valence_a_d3 <- subset(results_3daysfull, group == "human", select=c(id, v_a_d3))
human_valence_a <- subset(merge(human_valence_a_d1, merge(human_valence_a_d2, human_valence_a_d3, by="id"), by="id"),select=-id)
chatbot_valence_a <- rowMeans(rbind(bot_valence_a,human_valence_a))
### Preparing data for testing H2 ends here. ###
t.test(chatbot_arousal_b, chatbot_arousal_a, alternative = "g", paired = TRUE) ### T-test for H1.
t.test(chatbot_valence_b, chatbot_valence_a, alternative = "l", paired = TRUE) ### T-test for H2.
### Testing H1 & H2 Ends Here ###
### Testing H3 & H4 Starts Here ###
### Preparing data for testing H3 via one-way ANOVA starts here. ###
bot_arousal <- subset(results_3daysfull$mean_arousal, results_3daysfull$group == "bot")
human_arousal <- subset(results_3daysfull$mean_arousal, results_3daysfull$group == "human")
control_arousal <- subset(results_3daysfull$mean_arousal, results_3daysfull$group == "control")
data_arousal <- data.frame(
  Y=c(bot_arousal, human_arousal, control_arousal),
  Group =factor(rep(c("bot", "human", "control"), 
                    times=c(length(bot_arousal), length(human_arousal), length(control_arousal))))
)
summary(bot_arousal)
sd(bot_arousal)
summary(human_arousal)
sd(human_arousal)
summary(control_arousal)
sd(control_arousal)
analysis_of_variance_arousal <- aov(Y~Group, data=data_arousal)
### Preparing data for testing H3 via one-way ANOVA ends here. ###
### Preparing data for testing H4 via one-way ANOVA starts here. ###
bot_valence <- subset(results_3daysfull$mean_valence, results_3daysfull$group == "bot")
human_valence <- subset(results_3daysfull$mean_valence, results_3daysfull$group == "human")
control_valence <- subset(results_3daysfull$mean_valence, results_3daysfull$group == "control")
data_valence <- data.frame(
  Y=c(bot_valence, human_valence, control_valence),
  Group =factor(rep(c("bot", "human", "control"), 
                    times=c(length(bot_valence), length(human_valence), length(control_valence))))
)
summary(bot_valence)
sd(bot_valence)
summary(human_valence)
sd(human_valence)
summary(control_valence)
sd(control_valence)
analysis_of_variance_valence <- aov(Y~Group, data=data_valence)
### Preparing data for testing H4 via one-way ANOVA ends here. ###
anova(analysis_of_variance_arousal) ### One-way ANOVA for H3.
anova(analysis_of_variance_valence) ### One-way ANOVA for H4.
### Aditional tests for H4 start here. ###
t.test(human_valence, control_valence, alternative = "g") ### Welch's T-Test: H x Control - Is H higher?
# Control x B - Is Control higher?
t.test(control_valence, bot_valence, alternative = "g") ### Welch's T-Test: Control x B - Is Control higher?
### Aditional tests for H4 end here. ###
### Testing H3 & H4 Ends Here ###