library(dplyr)
library(tidyr)
library(matrixStats)
library(lme4)
library(lmerTest)  
library(ggplot2)   
library(ggeffects) 

data <- experiment1_data

score_cols <- grep("^(long|short|bad_filler)", names(data), value = TRUE)

data[score_cols] <- lapply(data[score_cols], function(x) as.numeric(as.character(x)))
data[score_cols] <- lapply(data[score_cols], function(x) ifelse(x %in% 1:7, x, NA))

var_meta <- tibble(
  var_name = score_cols,
  question_type = ifelse(grepl("filler", var_name), "filler", "target"),
  VP_length = case_when(
    grepl("^long", var_name) ~ "long",
    grepl("^short", var_name) ~ "short",
    TRUE ~ NA_character_
  ),
  form = case_when(
    grepl("full", var_name) ~ "full",
    grepl("ellipsis", var_name) ~ "ellipsis",
    TRUE ~ NA_character_
  ),
  question_number = seq_along(score_cols)  # 给每个题目标号
)


data_long <- data %>%
  mutate(participant_id = row_number()) %>%
  pivot_longer(
    cols = all_of(score_cols),   # 精准指定列
    names_to = "var_name",
    values_to = "score"
  ) %>%
  left_join(var_meta, by = "var_name") %>%
  select(-var_name)


target_data <- data_long %>%
  filter(question_type == "target") %>%
  select(score, VP_length, form, participant_id, question_number) %>%
  na.omit()  
target_data$VP_length <- factor(target_data$VP_length, levels = c("long", "short"))
target_data$form <- factor(target_data$form, levels = c("ellipsis", "full"))


model <- lmer(score ~ VP_length * form + (1 | participant_id) + (1 | question_number),
              data = target_data)

model1 <- lmer(score ~ VP_length * form + (1 | participant_id),
               data = target_data)
summary(model1)
model2 <- lmer(score ~ VP_length + form + (1 | participant_id),
               data = target_data)
summary(model2)
anova(model, model1)  
anova(model1, model2) 

