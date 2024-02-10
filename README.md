# TraitPerception
Examining perceived trustworthiness in male and female voices with vary pitch.

## load libraries 
library("cocor")
library("performance")
library("see")
library("pwr")
library("tidyverse")
library("broom")

## import selected data files
participant_dem <- read.csv("ParticipantDemographics.csv")
voice_acoustics <- read.csv("VoiceAcoustics.csv")
voice_dem <- read.csv("VoiceDemographics.csv")
voice_ratings <- read.csv("VoiceRatingsData.csv")

## data wrangling 
participant_f_dem <- filter(participant_dem, Gender == "F")
participant_m_dem <- filter(participant_dem, Gender == "M")

summarise(participant_f_dem, mean_age = mean(Age), sd(Age), sum = n())
summarise(participant_m_dem, mean_age = mean(Age), sd(Age), sum = n())

voice_f_dem <- filter(voice_dem, Voice_Sex == "Female")
voice_m_dem <- filter(voice_dem, Voice_Sex == "Male")
summarise(voice_f_dem, mean_age = mean(Age_At_Recording), sd(Age_At_Recording))
summarise(voice_m_dem, mean_age = mean(Age_At_Recording), sd(Age_At_Recording))

## descriptive statistics
trust_ratings <- filter(voice_ratings, Trait == "Trustworthiness") %>%
  select(-Condition, -Block, -Trial, -ReactionTime, -Timestamp) %>%
  filter(!VoiceCode %in% c("gm_F08", "gm_F15", "gm_F23", "gm_M07", "gm_M08", "gm_M10", "gm_M12", "gm_M17", "gm_M22", "gm_M23")) %>%
  group_by(ParticipantID)
 mean_pitch_ratings <- voice_acoustics %>%
  group_by(VoiceCode, Voice_Sex) %>%
  summarise(total_pitch_mean = mean(pitch_mean), total_sd = sd(pitch_mean)) %>%
  filter(!VoiceCode %in% c("gm_F08", "gm_F15", "gm_F23", "gm_M07", "gm_M08", "gm_M10", "gm_M12", "gm_M17", "gm_M22", "gm_M23"))

  joint_ratings <- full_join(mean_pitch_ratings, trust_ratings) %>%
  group_by(RatingCount, ParticipantID, Voice_Sex, VoiceCode) %>%
  summarise(mean_response = mean(ResponseGiven), mean_pitch = mean(total_pitch_mean))

## filtered stats based on rating 
pitch_ratings1 <- joint_ratings %>%
  filter(RatingCount != "Rating2")

pitch_ratings2 <- joint_ratings %>%
  filter(RatingCount != "Rating1")

## data visualizations
ggplot(pitch_ratings1, aes(x = mean_pitch, y = mean_response, color = Voice_Sex)) +
  geom_point() +
  labs(x = "Mean Pitch (Hz)", y = "Mean Trustworthiness Response", title = "Relationship Between Trustworthiness and Pitch in First Ratings") 

ggplot(pitch_ratings2, aes(x = mean_pitch, y = mean_response, color = Voice_Sex)) +
  geom_point() +
  labs(x = "Mean Pitch (Hz)", y = "Mean Trustworthiness Response", title = "Relationship Between Trustworthiness and Pitch in Second Ratings") 

ggplot(pitch_ratings1, aes(x = mean_pitch, y = mean_response, fill = Voice_Sex)) +
  geom_violin(trim = FALSE, show.legend = FALSE, alpha = .4) +
  geom_boxplot(width = .2, show.legend = FALSE, alpha = .7)+
  scale_x_discrete(name = "Mean Pitch") +
  scale_y_continuous(name = "Mean Trustworthiness Response") +
  facet_wrap(~Voice_Sex) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(x = "Mean Pitch (Hz)", y = "Mean Trustworthiness Response", title = "Relationship Between Trustworthiness and Pitch in First Ratings") 

ggplot(pitch_ratings2, aes(x = mean_pitch, y = mean_response, fill = Voice_Sex)) +
  geom_violin(trim = FALSE, show.legend = FALSE, alpha = .4) +
  geom_boxplot(width = .2, show.legend = FALSE, alpha = .7)+
  scale_x_discrete(name = "Mean Pitch") +
  scale_y_continuous(name = "Mean Trustworthiness Response") +
  facet_wrap(~Voice_Sex) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(x = "Mean Pitch (Hz)", y = "Mean Trustworthiness Response", title = "Relationship Between Trustworthiness and Pitch in Second Ratings") 

## model assumptions 
mod1 <- lm(mean_response ~ mean_pitch, pitch_ratings1)
mod1_summary <- summary(mod1)
check_model(mod1)
check_normality(mod1)
check_heteroscedasticity(mod1)

mod2 <- lm(mean_response ~ mean_pitch, pitch_ratings2)
mod2_summary <- summary(mod2)
check_model(mod2)
check_normality(mod2)
check_heteroscedasticity(mod2)

## standardization of scores
mutated_ratings1 <- pitch_ratings1 %>%
  mutate(z = (mean_response - mean(mean_response))/sd(mean_response)) 
final_ratings1 <- mutated_ratings1 %>%
  filter(z <= 2.5 & z >= -2.5)
mutated_ratings2 <- pitch_ratings2 %>%
  mutate(z = (mean_response - mean(mean_response))/sd(mean_response)) 
final_ratings2 <- mutated_ratings2 %>%
  filter(z <= 2.5 & z >= -2.5)

## inferential stats 
cor.test(final_ratings1$mean_pitch, final_ratings1$mean_response, method = "spearman", alternative = "two.sided") %>%
  broom::tidy()

cor.test(final_ratings2$mean_pitch, final_ratings2$mean_response, method = "spearman", alternative = "two.sided") %>%
  broom::tidy() 


female_ratings <- filter(joint_ratings, Voice_Sex == "F") %>%
  mutate(z = (mean_response - mean(mean_response))/sd(mean_response)) 
final_female <- female_ratings %>%
  filter(z <= 2.5 & z >= -2.5)
male_ratings <- filter(joint_ratings, Voice_Sex == "M") %>%
  mutate(z = (mean_response - mean(mean_response))/sd(mean_response)) 
final_male <- male_ratings %>%
  filter(z <= 2.5 & z >= -2.5)


cor.test(final_female$mean_pitch, final_female$mean_response, method = "spearman", alternative = "two.sided") %>%
  broom::tidy() 

cor.test(final_male$mean_pitch, final_male$mean_response, method = "spearman", alternative = "two.sided") %>%
  broom::tidy() 

cocor.indep.groups(r1.jk = .10, 
                   r2.hm = .12, 
                   n1 = 66, 
                   n2 = 66)

cocor.indep.groups(r1.jk = .13, 
                   r2.hm = .03, 
                   n1 = 32, 
                   n2 = 34)

 pwr.t.test(n = 66,
           power = .8,
           sig.level = .025,
           alternative = "two.sided")                


  
