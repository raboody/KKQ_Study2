# Analyzing KKQ Pilot data


library(tidyverse)
library(plyr)
library(patchwork)
library(boot)

# Bootstrap functions for obtaining bootstrap over the mean
GetMean = function(myData, selectedIDs){
  tempdata = myData[selectedIDs]
  return(mean(tempdata))
}

GetMeanBootstrap = function(myData){
  Bootstrapsamples = boot(myData,GetMean,R=10000)
  return(boot.ci(Bootstrapsamples, type = "basic"))
}

setwd("/Users/michaelokeeffe/Documents/CoCoDev Fall 2023/KKQ Study 2") # set directory to folder containing your data csv's

pilot_data <- ldply(list.files(), read.csv, header=TRUE) # combine all files in the folder above using "plyr" package
length(unique(pilot_data$child_hashed_id)) # counting the number of subjects we have to double-check we combined correctly


setwd("/Users/michaelokeeffe/Documents/CoCoDev Fall 2023") # set directory to folder containing the rest


# ------------- DEIDENTIFIED (MISHA & CECILIA) START ---------------
consent_info = read.csv("Consent_info_deidentified.csv") %>%
  as_tibble() %>%
  select(c(response__video_privacy, response__databrary, child__hashed_id)) %>%
  # some of these look like 2 underscores but it's actually a special character. I want to get rid of it. 
  plyr::rename(c("response__databrary" = "databrary_consent", "response__video_privacy" = "video_privacy",
                 "child__hashed_id" = "child_hashed_id"))


demo_info = read.csv("DOB_info_deidentified.csv") %>% as_tibble() %>%
  plyr::rename(c("child__hashed_id" = "child_hashed_id", "child__global_id" = "child_global_id", 
                 "child__age_in_days" = "child_age_days")) %>%
  # change age in days into a more useful form
  mutate(age_continuous = child_age_days/365.25, age_years = floor(age_continuous))
  



# --------------NOT deidentified (ROSIE) START: -------------------
# upload data sheet with consent info
consent_info = read.csv("Consent_info.csv") %>% 
  as_tibble() %>% 
  select(c(response__video_privacy, response__databrary, child__hashed_id)) %>%
  # some of these look like 2 underscores but it's actually a special character. I want to get rid of it. 
  plyr::rename(c("response__databrary" = "databrary_consent", "response__video_privacy" = "video_privacy",
           "child__hashed_id" = "child_hashed_id"))

# upload data sheet with age & global id
demo_info = read.csv("DOB_info.csv") %>% as_tibble() %>%
  select(c(child__global_id, child__hashed_id, child__age_in_days, excluded, version)) %>%
  # same as above
  plyr::rename(c("child__global_id" = "child_global_id", "child__hashed_id" = "child_hashed_id", 
           "child__age_in_days" = "child_age_days")) %>%
  # change age in days into a more useful form
  mutate(age_continuous = child_age_days/365.25, age_years = floor(age_continuous))

# --------------NOT deidentified (ROSIE) END -------------------




# Joining all 3 data frames:
pilot_data <- rename(pilot_data,c('child__hashed_id'='child_hashed_id'))
pilot_data_raw = pilot_data %>% full_join(consent_info, by = c("child_hashed_id")) %>%
  full_join(demo_info, by = c("child_hashed_id"))

pilot_data_excluded = pilot_data_raw %>% filter(excluded != "yes")

# DO LATER:  will need to check out parent survey later 


# KKQ

# first figuring out counterbalance:
KKQ_counterbalance= 
  pilot_data_excluded %>%
  filter((str_detect(frame_id, "KKQ-3") & (key == "videoShown"))) %>%
  mutate(who_was_accepted = ifelse(str_detect(value, "CB_1"), "left", "right")) %>% 
  select(c(child_global_id, who_was_accepted))



# PILOT1
KKQ_attn_correct_pilot1 = 
  pilot_data_excluded %>%
  filter(version == "Pilot1") %>%
  filter(str_detect(frame_id, "KKQ-8|KKQ-9") & (key == "selectedImage")) %>%
  separate(frame_id, c("order", "temp", "temp2", "temp3", "temp4", "question"), "-", extra = "merge") %>% # remove the order number so I can pivot_wider
  select(c(child_global_id, age_continuous, age_years, question, value)) %>%
  mutate(correct = ifelse(question == "Michelle-green" & value == "right", 1, 
                   ifelse(question == "Erin-blue" & value == "left", 1, 0))) %>%
  mutate(question = ifelse(question == "Michelle-green", "Attn_1", "Attn_2")) %>%
  select(-value) %>%
  pivot_wider(names_from = question, values_from = correct) %>% 
  mutate(Attn_correct = ifelse(Attn_1 == 1 & Attn_2 == 1, 1, 0)) %>%
  select(-c(Attn_1, Attn_2))


# PILOT2
KKQ_attn_correct_pilot2 = 
  pilot_data_excluded %>%
  filter(version == "Pilot2") %>%
  filter(str_detect(frame_id, "KKQ-attention-check-1|KKQ-attention-check-2") & ((key == "selectedImage") |(key == "audioPlayed"))) %>%
  pivot_wider(names_from = key, values_from = value) %>% 
  mutate(correct_answer = ifelse(str_detect(audioPlayed, "KKQ-9-CB-1|KKQ-10-CB-2"), "left", "right"),
         correct = ifelse(correct_answer==selectedImage, 1, 0)) %>% 
  separate(frame_id, c("temp", "temp2", "temp3", "question"), "-", extra = "merge") %>% # remove the order number so I can pivot_wider
  select(c(child_global_id, age_continuous, age_years, question, correct)) %>%
  pivot_wider(names_from = question, values_from = correct) %>% 
  mutate(Attn_correct = ifelse(`check-1` == 1 & `check-2` == 1, 1, 0)) %>%
  select(-c(`check-1`, `check-2`))


# Combining the coding for both pilot versions:
KKQ_attn_correct_all = rbind(KKQ_attn_correct_pilot1, KKQ_attn_correct_pilot2)


# filtering for the data, joining with the CB and manipulating to be in the right form
KKQ_pilot_data = 
  pilot_data_excluded %>%
  filter((str_detect(frame_id, "KKQ-6-test|KKQ-test-2") & ((key == "selectedImage")))) %>%
  mutate(test_question = ifelse(str_detect(frame_id, "test-2"), "test_2", "test_1")) %>%
  select(c(child_global_id, age_continuous, age_years, test_question, version, value)) %>%
  rename(c("value" = "choice")) %>%
  full_join(KKQ_counterbalance, by = "child_global_id") %>%
  mutate(correct = ifelse(choice == who_was_accepted, 1, 0)) %>%
  full_join(KKQ_attn_correct_all, by = c("child_global_id", "age_continuous", "age_years"))


# summarize considering whether kids got the attn check correct
KKQ_pilot_data %>% 
  group_by(age_years, version, test_question, Attn_correct) %>%
  dplyr::summarize(N = n(), Correct=sum(correct)) %>% # correct is a factor for plotting so we have to do this to fix it for summarizing
  mutate(percent_correct = Correct/N*100) %>%
  arrange(version, Attn_correct)

# summarize with  no attn check
KKQ_pilot_data %>% 
  group_by(age_years,  version, test_question) %>%
  dplyr::summarize(N = n(), Correct=sum(correct)) %>% # correct is a factor for plotting so we have to do this to fix it for summarizing
  mutate(percent_correct = Correct/N*100) %>% arrange(version)


# summarize grouping 5 & 6yo's, filtering out people who failed the attention check
KKQ_pilot_data %>% 
  filter(age_years != 4, Attn_correct == 1) %>%
  group_by(version, test_question) %>%
  dplyr::summarize(N = n(), Correct=sum(correct)) %>% # correct is a factor for plotting so we have to do this to fix it for summarizing
  mutate(percent_correct = Correct/N*100)



# getting bootstrapped CI's:
GetMeanBootstrap(filter(KKQ_pilot_data, version == "Pilot1", age_years != 4)$correct) # 0.4828,  0.8276 
GetMeanBootstrap(filter(KKQ_pilot_data, version == "Pilot2", test_question == "test_1", age_years != 4)$correct) # 0.3, 0.9
GetMeanBootstrap(filter(KKQ_pilot_data, version == "Pilot2", test_question == "test_2", age_years != 4)$correct) # 0, 0.6

                 

# PLOTTING
# comparing performance across versions, collapsing 5 & 6yo's:
KKQ_pilot_data %>% 
  filter(age_years != 4, Attn_correct == 1) %>%
  group_by(version, test_question) %>%
  dplyr::summarize(N = n(), Correct=sum(correct)) %>% # correct is a factor for plotting so we have to do this to fix it for summarizing
  mutate(percent_correct = Correct/N*100,
         Lower = c(48.28, 30, 0), Upper = c(82.76, 90, 60)) %>% 
  ggplot(aes(x = test_question, y = percent_correct, ymin = Lower, ymax = Upper)) +
  geom_bar(stat = "identity", color = "black", fill = "gray") +
  geom_errorbar(width = 0.2) +
  facet_wrap(~version) +
  theme_classic() +
  labs(x = "Test Question", y = "Percent Correct") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 100)) +
  theme(text = element_text(color = "black", size = 24), axis.text = element_text(color = "black", size = 24), 
        axis.ticks = element_line(color = "black"), plot.title = element_text(hjust = 0.5))

# this will save in your current working directory. Use "getwd()" to see your current working directory
ggsave("bar plot.pdf", width = 5.5, height = 6)

# plotting continuously by age, including only 5-6yo's and people who passed the attention check
KKQ_pilot_data %>%
  filter(age_years != 4, Attn_correct == 1) %>%
  ggplot(aes(x = age_continuous, y = correct)) +
  geom_jitter(aes(color = factor(correct)), width = 0, height = 0.1, alpha = 0.65, size=3) +
  geom_smooth(color = "black", method = "glm", 
              method.args = list(family = "binomial")) +
  facet_wrap(version~ test_question, ncol = 1, scales = "free") +
  theme_classic() +
  scale_x_continuous(limits = c(5.0, 7.0)) +
  ggtitle("Continuous") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = "Age", y = "",  color = "Correct") +
  scale_color_manual(values = c("darksalmon", "darkseagreen3")) +
  theme(text = element_text(color = "black", size = 24), axis.text = element_text(color = "black", size = 24), 
        axis.ticks = element_line(color = "black"), plot.title = element_text(hjust = 0.5))

ggsave("scatter plot.pdf", width = 8, height = 16)







# Misha's Plot(s)
KKQ_pilot_data %>%
  filter(age_years != 4, Attn_correct == 1, version == "Pilot2") %>%
  ggplot(aes(x = age_continuous, y = correct)) +
  theme_classic() +
  geom_smooth(method = glm, color = "black") +
  geom_jitter(aes(color = factor(correct)), width = 0, height = 0.2, alpha = 0.65, size=3) +
  scale_x_continuous(limits = c(5.0, 7.0)) +
  ggtitle("Continuous") +
  facet_wrap(version~ test_question, ncol = 1, scales = "free") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = "Age", y = "",  color = "Correct") +
  scale_color_manual(values = c("darkred", "navyblue")) +
  theme(text = element_text(color = "black", size = 24), axis.text = element_text(color = "black", size = 24), 
        axis.ticks = element_line(color = "black"), plot.title = element_text(hjust = 0.5))

ggsave("pilot 2 misha scatter plot.png")

pilot_2_data <- KKQ_pilot_data %>%
  filter(version == "Pilot2")

pilot_1_data <- KKQ_pilot_data %>%
  filter(test_question == "test_1")

pilot_2_data_summary <- pilot_2_data %>%
  filter(age_years != 4, Attn_correct == 1) %>%
  group_by(version) %>%
  dplyr::summarize(N = n(), Correct = sum(correct)) %>%
  mutate(percent_correct = Correct/N * 100,
         margin_of_error = qnorm(0.975) * sqrt(percent_correct * (100 - percent_correct) / N))

pilot_1_data_summary <- pilot_1_data %>%
  filter(age_years != 4, Attn_correct == 1) %>%
  group_by(version) %>%
  dplyr::summarize(N = n(), Correct = sum(correct)) %>%
  mutate(percent_correct = Correct/N * 100,
         margin_of_error = qnorm(0.975) * sqrt(percent_correct * (100 - percent_correct) / N))

pilot_2_data %>%
  filter(age_years != 4, Attn_correct == 1) %>%
  group_by(version, test_question) %>%
  dplyr::summarize(N = n(), Correct = sum(correct)) %>% 
  mutate(percent_correct = Correct / N * 100,
         Lower = c(0),
         Upper = c(100)) %>%
  ggplot(aes(x = test_question, y = percent_correct, ymin = Lower, ymax = Upper, fill = factor("correct"))) +
  geom_bar(position = "stack", stat = "identity") +
  geom_errorbar(width = 0.2) +
  facet_wrap(~version) +
  theme_classic() +
  labs(x = "Test Question", y = "Percent Correct") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  scale_fill_manual(values = c("1" = "darkseagreen3", "0" = "darksalmon")) +  # Adjust fill colors
  theme(
    text = element_text(color = "black", size = 24),
    axis.text = element_text(color = "black", size = 24),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

pilot_2_data %>%
  filter(age_years != 4, Attn_correct == 1) %>%
  ggplot(aes(x = test_question, fill = factor(correct))) +
  geom_bar(stat = "count", position = "stack") +
  theme_classic() +
  labs(y = "Percent Choosing") +
  scale_fill_manual(values = c("1" = "darkseagreen3", "0" = "darksalmon")) +
  theme(
    text = element_text(color = "black", size = 24),
    axis.text = element_text(color = "black", size = 24),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

pilot_2_data %>%
  filter(age_years != 4, Attn_correct == 1, test_question == "test_1") %>%
  ggplot(aes(x = test_question, fill = factor(correct))) +
  geom_bar(stat = "count", position = "stack") +
  theme_classic() +
  labs(y = "Percent Choosing") +
  scale_fill_manual(values = c("1" = "darkseagreen3", "0" = "darksalmon")) +
  theme(
    text = element_text(color = "black", size = 24),
    axis.text = element_text(color = "black", size = 24),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

pilot_1_data %>%
  filter(age_years != 4, Attn_correct == 1) %>%
  group_by(version, test_question) %>%
  dplyr::summarize(N = n(), Correct = sum(correct)) %>% 
  mutate(percent_correct = Correct / N * 100,
         Lower = c(0),
         Upper = c(100)) %>%
  ggplot(aes(x = test_question, y = percent_correct, ymin = Lower, ymax = Upper, fill = factor("correct"))) +
  geom_bar(position = "stack", stat = "identity") +
  geom_errorbar(width = 0.2) +
  facet_wrap(~version) +
  theme_classic() +
  labs(x = "Test Question", y = "Percent Correct") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  scale_fill_manual(values = c("1" = "darkseagreen3", "0" = "darksalmon")) +  # Adjust fill colors
  theme(
    text = element_text(color = "black", size = 24),
    axis.text = element_text(color = "black", size = 24),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )