### Statistical analysis of the data generated from the user engagement questionnaire for the 'Show Me the GIFference! Using data-GIFs as Educational Tools' work ###

# Reference: https://jakec007.github.io/2021-06-23-R-likert/

# Loading the libraries
library("readxl")
library("tidyverse")
library("likert")
library("plyr")
library("MASS")
library("brant")
library("car")
library("Hmisc")

# Loading the data
data = read_excel('gifference_data_engagement.xlsx')
data = as.data.frame(data)

# Preparing the data
data$"1" = as.factor(data$"1")
data$"2" = as.factor(data$"2")
data$"3" = as.factor(data$"3")
data$"4" = as.factor(data$"4")
data$"5" = as.factor(data$"5")
data$"6" = as.factor(data$"6")
data$"7" = as.factor(data$"7")
data$"8" = as.factor(data$"8")
data$"9" = as.factor(data$"9")
data$"10" = as.factor(data$"10")
data$"11" = as.factor(data$"11")
data$"12" = as.factor(data$"12")
data$"13" = as.factor(data$"13")
data$"14" = as.factor(data$"14")
data$"15" = as.factor(data$"15")
data$"16" = as.factor(data$"16")
data$"17" = as.factor(data$"17")
data$"18" = as.factor(data$"18")
data$"19" = as.factor(data$"19")
data$"20" = as.factor(data$"20")
data$"21" = as.factor(data$"21")
data$"22" = as.factor(data$"22")
data$"23" = as.factor(data$"23")
data$"24" = as.factor(data$"24")
data$"25" = as.factor(data$"25")
data$"26" = as.factor(data$"26")
data$"27" = as.factor(data$"27")
data$"28" = as.factor(data$"28")
data$variant = as.factor(data$variant)
data$gender = as.factor(data$gender)
data$vl_level = as.factor(data$vl_level)
data$cb = as.factor(data$cb)

# Plotting the data
plotting_responses <- function(data) {
  
# Renaming the items so that the question statement becomes the name
items = data[1:28]
names(items) = c(
  "During viewing, I reflected on the content of the visualizations/GIFs/videos.",
  "During viewing, I hardly thought about the visualizations/GIFs/videos' content.",
  "I vividly remember some parts of the visualizations/GIFs/videos.",
  "I learned something new.",
  "At points, I had a hard time making sense of what was going on in the visualizations/GIFs/videos.",
  "I found the content of the visualizations/GIFs/videos easy to understand.",
  "My understanding of the facts is unclear.",
  "I enjoyed viewing the visualizations/GIFs/videos.",
  "The visualizations/GIFs/videos were entertaining.",
  "I found the visualizations/GIFs/videos boring.",
  "I’d recommend the viewing of the visualizations/GIFs/videos to my friends.",
  "The content of the visualizations/GIFs/videos appealed to me.",
  "I felt tired.",
  "I felt involved.",
  "I found my mind wandering while the visualizations/GIFs/videos were being played.",
  "I had a hard time keeping my mind on the visualizations/GIFs/videos.",
  "I forgot about my immediate surroundings while watching the visualizations/GIFs/videos.",
  "While watching the visualizations/GIFs/videos, I found myself concentrating on specific aspects of them.",
  "While watching the visualizations/GIFs/videos, I had to pay attention to multiple things at the same time.",
  "While watching the visualizations/GIFs/videos, time seemed to pass quickly.",
  "I feel that I could construct a story about the facts presented in the visualizations/GIFs/videos.",
  "The content or message of the visualizations/GIFs/videos was interesting to me.",
  "While watching the visualizations/GIFs/videos, I enjoyed and accepted any challenges they presented.",
  "While watching the visualizations/GIFs/videos, I had to think carefully, deeply, or reflectively.",
  "I would want to watch the visualizations/GIFs/videos if I saw them somewhere else and was not required or encouraged to watch them.",
  "The visualizations/GIFs/videos were aesthetically appealing.",
  "The visualizations/GIFs/videos were visually pleasing.",
  "I found the graphics in the visualizations/GIFs/videos distracting."
  )

# Recoding numerical responses into ordered factors
likert_recode <- function(x) {
  y = ifelse(x == 1, "Absolutely disagree",
                     ifelse(x == 2, "Strongly disagree",
                            ifelse(x == 3, "Disagree",
                                   ifelse(x == 4, "Neutral",
                                          ifelse(x == 5, "Agree",
                                                 ifelse(x == 6, "Strongly agree", "Absolutely agree"))))))
  
  y = factor(y, levels = c("Absolutely disagree", "Strongly disagree", "Disagree",
                            "Neutral", "Agree", "Strongly agree", "Absolutely agree"))
  
  return(y)
}

# Transforming the items into factors and save the data set as a likert object
items_likert = items %>%
  mutate_all(likert_recode) %>%
  likert()

# Creating a stacked bar chart for the user engagement test results
plot(items_likert,
     group.order = names(items),
     centered = TRUE,
     col=c("#8c510a","#d8b365","#f6e8c3","#f5f5f5","#c7eae5","#5ab4ac","#01665e")) +
     theme(text=element_text(size=12))
}

# Plotting the results related to each variant
plotting_responses(data[ which(data$variant==1),] )
plotting_responses(data[ which(data$variant==2),] )
plotting_responses(data[ which(data$variant==3),] )
plotting_responses(data[ which(data$variant==4),] )

# Performing an Ordinal Logistic Regression for each statement
# Setting the GIF with smooth transition as the reference variant and the medium VL_level as the reference vl_level
data = within(data, variant <- relevel(variant, ref = 3))
data = within(data, vl_level <- relevel(vl_level, ref = 2))

# Statement 1
model1 = polr(data$"1" ~ variant + vl_level + gender, data = data)
(ctable1 <- coef(summary(model1)))
p1 = pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
(ctable1 = cbind(ctable1, "p value" = p1))
(ci1 = confint(model1)) 
exp(cbind(OR = coef(model1), ci1))
# During viewing, I reflected on the content of the visualizations/GIFs/videos.

vif(model1)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model1) 
# The proportional odds assumption holds at significance level alpha = 0.01.

# 1/0.1243342 = 8.04
# For participants who watched the GIFs with smooth transitions, the odds of being more likely to reflect on the content of the GIF during viewing are 8.04 times that
# of participants who viewed the static visualizations, holding constant all other variables.

# There is no evidence that the odds of being more likely to reflect on the content of the GIF during viewing are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# 1/0.2065053 = 4.84
# For participants who watched the GIFs with smooth transitions, the odds of being more likely to reflect on the content of the GIF during viewing are 4.84 times that
# of participants who watched the data-videos, holding constant all other variables.

# 1/0.2138393 = 4.68
# For participants with a low VL level, the odds of being more likely to reflect on the content of the GIF during viewing are 4.68 times that
# of participants with a medium VL level, holding constant all other variables.

# There is no evidence that the odds of being more likely to reflect on the content of the GIF during viewing are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to reflect on the content of the GIF during viewing are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"1")
popower(c(1/48,  1/48, 0,  6/48,  4/48, 22/48, 14/48), 0.1243342, 48, alpha=0.05)
popower(c(1/48,  1/48, 0,  6/48,  4/48, 22/48, 14/48), 0.2829525, 48, alpha=0.05)
popower(c(1/48,  1/48, 0,  6/48,  4/48, 22/48, 14/48), 0.2065053, 48, alpha=0.05)
popower(c(1/48,  1/48, 0,  6/48,  4/48, 22/48, 14/48), 0.2138393, 48, alpha=0.05)
popower(c(1/48,  1/48, 0,  6/48,  4/48, 22/48, 14/48), 0.5253506, 48, alpha=0.05)
popower(c(1/48,  1/48, 0,  6/48,  4/48, 22/48, 14/48), 0.3545784, 48, alpha=0.05)


# Statement 2
model2 = polr(data$"2" ~ variant + vl_level + gender, data = data)
(ctable2 <- coef(summary(model2)))
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
(ctable2 <- cbind(ctable2, "p value" = p2))
(ci2 <- confint(model2))
exp(cbind(OR = coef(model2), ci2)) 
# During viewing, I hardly thought about the visualizations/GIFs/videos' content.

vif(model2)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model2)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to hardly think about the visualizations/GIFs/videos' content during viewing are different for participants who viewed the static visualizations
# and participants who watched the GIFs with smooth transitions.

# For participants who watched the GIFs with interchangeability, the odds of being more likely to hardly think about the visualizations/GIFs/videos' content during viewing are 8.55 times that
# of participants who watched the GIFs with smooth transitions, holding constant all other variables.

# There is no evidence that the odds of being more likely to hardly think about the visualizations/GIFs/videos' content during viewing are different for participants who watched the data-videos
# and participants who watched the GIFs with smooth transitions.

# For participants with a low VL level, the odds of being more likely to hardly think about the visualizations/GIFs/videos' content during viewing are 6.74 times that
# of participants with a medium VL level, holding constant all other variables.

# For participants with a high VL level, the odds of being more likely to hardly think about the visualizations/GIFs/videos' content during viewing are 3.99 times that
# of participants with a medium VL level, holding constant all other variables.

# There is no evidence that the odds of being more likely to hardly think about the visualizations/GIFs/videos' content during viewing are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"2")
popower(c(9/48,  16/48, 7/48, 3/48,  6/48, 5/48, 2/48), 3.840951, 48, alpha=0.05)
popower(c(9/48,  16/48, 7/48, 3/48,  6/48, 5/48, 2/48), 8.545770, 48, alpha=0.05)
popower(c(9/48,  16/48, 7/48, 3/48,  6/48, 5/48, 2/48), 1.472217, 48, alpha=0.05)
popower(c(9/48,  16/48, 7/48, 3/48,  6/48, 5/48, 2/48), 6.741286, 48, alpha=0.05)
popower(c(9/48,  16/48, 7/48, 3/48,  6/48, 5/48, 2/48), 3.991695, 48, alpha=0.05)
popower(c(9/48,  16/48, 7/48, 3/48,  6/48, 5/48, 2/48), 1.426443, 48, alpha=0.05)


# Statement 3
model3 = polr(data$"3" ~ variant + vl_level + gender, data = data)
(ctable3 <- coef(summary(model3)))
p3 <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2
(ctable3 <- cbind(ctable3, "p value" = p3))
(ci3 <- confint(model3))
exp(cbind(OR = coef(model3), ci3))
# I vividly remember some parts of the visualizations/GIFs/videos.

vif(model3)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model3)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to remember some parts of the visualizations/GIFs/videos are different for participants who viewed the static visualizations
# and participants who watched the GIFs with smooth transitions.

# There is no evidence that the odds of being more likely to remember some parts of the visualizations/GIFs/videos are different for participants who watched the GIFs with interchangeability
# and participants who watched the GIFs with smooth transitions.

# There is no evidence that the odds of being more likely to remember some parts of the visualizations/GIFs/videos are different for participants who watched the data-videos
# and participants who watched the GIFs with smooth transitions.

# There is no evidence that the odds of being more likely to remember some parts of the visualizations/GIFs/videos are different for participants with a low VL level and
# and participants with a medium VL level.

# There is no evidence that the odds of being more likely to remember some parts of the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a high VL level.

# 1/0.2551909 = 3.92
# For male participants, the odds of being more likely to remember some parts of the visualizations/GIFs/videos are 3.92 times that
# of female participants, holding constant all other variables.

# Post-hoc power analysis
table(data$"3")
popower(c(0,  1/48, 0, 3/48,  10/48, 12/48, 22/48), 1.1161876, 48, alpha=0.05)
popower(c(0,  1/48, 0, 3/48,  10/48, 12/48, 22/48), 0.3786692, 48, alpha=0.05)
popower(c(0,  1/48, 0, 3/48,  10/48, 12/48, 22/48), 0.2645203, 48, alpha=0.05)
popower(c(0,  1/48, 0, 3/48,  10/48, 12/48, 22/48), 0.2895019, 48, alpha=0.05)
popower(c(0,  1/48, 0, 3/48,  10/48, 12/48, 22/48), 0.6432641, 48, alpha=0.05)
popower(c(0,  1/48, 0, 3/48,  10/48, 12/48, 22/48), 0.2551909, 48, alpha=0.05)


# Statement 4
model4 = polr(data$"4" ~ variant + vl_level + gender, data = data)
(ctable4 <- coef(summary(model4)))
p4 <- pnorm(abs(ctable4[, "t value"]), lower.tail = FALSE) * 2
(ctable4 <- cbind(ctable4, "p value" = p4))
(ci4 <- confint(model4))
exp(cbind(OR = coef(model4), ci4))
# I learned something new.

vif(model4)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model4)
# The proportional odds assumption holds at significance level alpha = 0.01.

# 1/0.05650479 = 17.70
# For participants who watched the GIFs with smooth transitions, the odds of being more likely to learn something new are 17.70 times that
# of participants who viewed the static visualizations, holding constant all other variables.

# There is no evidence that the odds of being more likely to learn something new are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to learn something new are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to learn something new are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to learn something new are different for participants with a medium VL level and
# and participants with a high VL level.

# 1/0.35217459 = 2.84
# For male participants, the odds of being more likely to learn something new are 2.84 times those of female participants and
# and female participants.

# Post-hoc power analysis
table(data$"4")
popower(c(4/48,  6/48, 3/48, 5/48,  10/48, 12/48, 8/48), 0.05650479, 48, alpha=0.05)
popower(c(4/48,  6/48, 3/48, 5/48,  10/48, 12/48, 8/48), 0.46166244, 48, alpha=0.05)
popower(c(4/48,  6/48, 3/48, 5/48,  10/48, 12/48, 8/48), 0.48597286, 48, alpha=0.05)
popower(c(4/48,  6/48, 3/48, 5/48,  10/48, 12/48, 8/48), 1.76132285, 48, alpha=0.05)
popower(c(4/48,  6/48, 3/48, 5/48,  10/48, 12/48, 8/48), 0.79311297, 48, alpha=0.05)
popower(c(4/48,  6/48, 3/48, 5/48,  10/48, 12/48, 8/48), 0.35217459, 48, alpha=0.05)


# Statement 5
model5 = polr(data$"5" ~ variant + vl_level + gender, data = data)
(ctable5 <- coef(summary(model5)))
p5 <- pnorm(abs(ctable5[, "t value"]), lower.tail = FALSE) * 2
(ctable5 <- cbind(ctable5, "p value" = p5))
(ci5 <- confint(model5))
exp(cbind(OR = coef(model5), ci5))
# At points, I had a hard time making sense of what is going on in the visualizations/GIFs/videos.

vif(model5)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model5)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to have hard time making sense of what is going on in the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the GIFs with smooth transitions.

# There is no evidence that the odds of being more likely to have hard time making sense of what is going on in the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to have hard time making sense of what is going on in the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to have hard time making sense of what is going on in the visualizations/GIFs/videos are different
# for participants with a low VL level and participants with medium VL level.

# For participants with a high VL level, the odds of being more likely to have hard time making sense of what is going on in the visualizations/GIFs/videos are 3.85 times that
# of participants with a medium VL level, holding constant all other variables.

# There is no evidence that the odds of being more likely to have hard time making sense of what is going on in the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"5")
popower(c(9/48,  17/48, 7/48, 6/48,  6/48, 1/48, 2/48), 2.4672837, 48, alpha=0.05)
popower(c(9/48,  17/48, 7/48, 6/48,  6/48, 1/48, 2/48), 2.1175405, 48, alpha=0.05)
popower(c(9/48,  17/48, 7/48, 6/48,  6/48, 1/48, 2/48), 0.5379547, 48, alpha=0.05)
popower(c(9/48,  17/48, 7/48, 6/48,  6/48, 1/48, 2/48), 2.6363315, 48, alpha=0.05)
popower(c(9/48,  17/48, 7/48, 6/48,  6/48, 1/48, 2/48), 3.8471158, 48, alpha=0.05)
popower(c(9/48,  17/48, 7/48, 6/48,  6/48, 1/48, 2/48), 2.3976048, 48, alpha=0.05)


# Statement 6
model6 = polr(data$"6" ~ variant + vl_level + gender, data = data)
(ctable6 <- coef(summary(model6)))
p6 <- pnorm(abs(ctable6[, "t value"]), lower.tail = FALSE) * 2
(ctable6 <- cbind(ctable6, "p value" = p6))
(ci6 <- confint(model6))
exp(cbind(OR = coef(model6), ci6))
# I found the content of the visualizations/GIFs/videos easy to understand.

vif(model6)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model6)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos easy to understand are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos easy to understand are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos easy to understand are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos easy to understand are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos easy to understand are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos easy to understand are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"6")
popower(c(0,  2/48, 0, 4/48,  9/48, 19/48, 14/48), 0.5843014, 48, alpha=0.05)
popower(c(0,  2/48, 0, 4/48,  9/48, 19/48, 14/48), 0.7952955, 48, alpha=0.05)
popower(c(0,  2/48, 0, 4/48,  9/48, 19/48, 14/48), 0.5301590, 48, alpha=0.05)
popower(c(0,  2/48, 0, 4/48,  9/48, 19/48, 14/48), 0.7865949, 48, alpha=0.05)
popower(c(0,  2/48, 0, 4/48,  9/48, 19/48, 14/48), 0.6565314, 48, alpha=0.05)
popower(c(0,  2/48, 0, 4/48,  9/48, 19/48, 14/48), 0.8750008, 48, alpha=0.05)


# Statement 7
model7 = polr(data$"7" ~ variant + vl_level + gender, data = data)
(ctable7 <- coef(summary(model7)))
p7 <- pnorm(abs(ctable7[, "t value"]), lower.tail = FALSE) * 2
(ctable7 <- cbind(ctable7, "p value" = p7))
(ci7 <- confint(model7))
exp(cbind(OR = coef(model7), ci7))
# My understanding of the facts is unclear.

vif(model7)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model7)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to understand the facts more clearly are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to understand the facts more clearly are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to understand the facts more clearly are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos

# There is no evidence that the odds of being more likely to understand the facts more clearly are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to understand the facts more clearly are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to understand the facts more clearly are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"7")
popower(c(20/48, 16/48, 3/48, 4/48,  2/48, 2/48, 1/48), 0.7407229, 48, alpha=0.05)
popower(c(20/48, 16/48, 3/48, 4/48,  2/48, 2/48, 1/48), 0.7562137, 48, alpha=0.05)
popower(c(20/48, 16/48, 3/48, 4/48,  2/48, 2/48, 1/48), 0.4000188, 48, alpha=0.05)
popower(c(20/48, 16/48, 3/48, 4/48,  2/48, 2/48, 1/48), 3.1945555, 48, alpha=0.05)
popower(c(20/48, 16/48, 3/48, 4/48,  2/48, 2/48, 1/48), 1.3852115, 48, alpha=0.05)
popower(c(20/48, 16/48, 3/48, 4/48,  2/48, 2/48, 1/48), 1.3456101, 48, alpha=0.05)


# Statement 8
model8 = polr(data$"8" ~ variant + vl_level + gender, data = data)
(ctable8 <- coef(summary(model8)))
p8 <- pnorm(abs(ctable8[, "t value"]), lower.tail = FALSE) * 2
(ctable8 <- cbind(ctable8, "p value" = p8))
(ci8 <- confint(model8))
exp(cbind(OR = coef(model8), ci8))
# I enjoyed viewing the visualizations/GIFs/videos.

vif(model8)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model8)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to enjoy viewing the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to enjoy viewing the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to enjoy viewing the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to enjoy viewing the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to enjoy viewing the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to enjoy viewing the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"8")
popower(c(0, 1/48, 3/48, 5/48,  9/48, 17/48, 13/48), 0.3338325, 48, alpha=0.05)
popower(c(0, 1/48, 3/48, 5/48,  9/48, 17/48, 13/48), 0.5391103, 48, alpha=0.05)
popower(c(0, 1/48, 3/48, 5/48,  9/48, 17/48, 13/48), 1.4134649, 48, alpha=0.05)
popower(c(0, 1/48, 3/48, 5/48,  9/48, 17/48, 13/48), 2.1289390, 48, alpha=0.05)
popower(c(0, 1/48, 3/48, 5/48,  9/48, 17/48, 13/48), 2.8039636, 48, alpha=0.05)
popower(c(0, 1/48, 3/48, 5/48,  9/48, 17/48, 13/48), 1.1650838, 48, alpha=0.05)


# Statement 9
model9 = polr(data$"9" ~ variant + vl_level + gender, data = data)
(ctable9 <- coef(summary(model9)))
p9 <- pnorm(abs(ctable9[, "t value"]), lower.tail = FALSE) * 2
(ctable9 <- cbind(ctable9, "p value" = p9))
(ci9 <- confint(model9))
exp(cbind(OR = coef(model9), ci9))
# The visualizations/GIFs/videos were entertaining.

vif(model9)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model9)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos entertaining are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos entertaining are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos entertaining are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos entertaining are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos entertaining are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos entertaining are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"9")
popower(c(1/48, 2/48, 3/48, 9/48,  13/48, 12/48, 8/48), 0.5850192, 48, alpha=0.05)
popower(c(1/48, 2/48, 3/48, 9/48,  13/48, 12/48, 8/48), 0.6434837, 48, alpha=0.05)
popower(c(1/48, 2/48, 3/48, 9/48,  13/48, 12/48, 8/48), 1.0545929, 48, alpha=0.05)
popower(c(1/48, 2/48, 3/48, 9/48,  13/48, 12/48, 8/48), 2.5432667, 48, alpha=0.05)
popower(c(1/48, 2/48, 3/48, 9/48,  13/48, 12/48, 8/48), 1.1517820, 48, alpha=0.05)
popower(c(1/48, 2/48, 3/48, 9/48,  13/48, 12/48, 8/48), 1.0715403, 48, alpha=0.05)


# Statement 10
model10 = polr(data$"10" ~ variant + vl_level + gender, data = data)
(ctable10 <- coef(summary(model10)))
p10 <- pnorm(abs(ctable10[, "t value"]), lower.tail = FALSE) * 2
(ctable10 <- cbind(ctable10, "p value" = p10))
(ci10 <- confint(model10))
exp(cbind(OR = coef(model10), ci10))
# I found the visualizations/GIFs/videos boring.

vif(model10)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model10)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos boring are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos boring are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos boring are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos boring are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos boring are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos boring are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"10")
popower(c(13/48, 14/48, 9/48, 6/48, 5/48, 1/48, 0), 2.6360699, 48, alpha=0.05)
popower(c(13/48, 14/48, 9/48, 6/48, 5/48, 1/48, 0), 0.5679895, 48, alpha=0.05)
popower(c(13/48, 14/48, 9/48, 6/48, 5/48, 1/48, 0), 0.4413666, 48, alpha=0.05)
popower(c(13/48, 14/48, 9/48, 6/48, 5/48, 1/48, 0), 0.7715999, 48, alpha=0.05)
popower(c(13/48, 14/48, 9/48, 6/48, 5/48, 1/48, 0), 1.6084528, 48, alpha=0.05)
popower(c(13/48, 14/48, 9/48, 6/48, 5/48, 1/48, 0), 0.6999336, 48, alpha=0.05)


# Statement 11
model11 = polr(data$"11" ~ variant + vl_level + gender, data = data)
(ctable11 <- coef(summary(model11)))
p11 <- pnorm(abs(ctable11[, "t value"]), lower.tail = FALSE) * 2
(ctable11 <- cbind(ctable11, "p value" = p11))
(ci11 <- confint(model11)) 
exp(cbind(OR = coef(model11), ci11))
# I’d recommend the viewing of the visualizations/GIFs/videos to my friends.

vif(model11)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model11)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to recommend the viewing of the visualizations/GIFs/videos to my friends are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to recommend the viewing of the visualizations/GIFs/videos to my friends are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to recommend the viewing of the visualizations/GIFs/videos to my friends are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to recommend the viewing of the visualizations/GIFs/videos to my friends are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to recommend the viewing of the visualizations/GIFs/videos to my friends are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to recommend the viewing of the visualizations/GIFs/videos to my friends are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"11")
popower(c(4/48, 4/48, 2/48, 6/48, 10/48, 11/48, 11/48), 0.4410379, 48, alpha=0.05)
popower(c(4/48, 4/48, 2/48, 6/48, 10/48, 11/48, 11/48), 0.7446737, 48, alpha=0.05)
popower(c(4/48, 4/48, 2/48, 6/48, 10/48, 11/48, 11/48), 1.0151464, 48, alpha=0.05)
popower(c(4/48, 4/48, 2/48, 6/48, 10/48, 11/48, 11/48), 2.6210776, 48, alpha=0.05)
popower(c(4/48, 4/48, 2/48, 6/48, 10/48, 11/48, 11/48), 1.1707663, 48, alpha=0.05)
popower(c(4/48, 4/48, 2/48, 6/48, 10/48, 11/48, 11/48), 0.8675822, 48, alpha=0.05)


# Statement 12
model12 = polr(data$"12" ~ variant + vl_level + gender, data = data)
(ctable12 <- coef(summary(model12)))
p12 <- pnorm(abs(ctable12[, "t value"]), lower.tail = FALSE) * 2
(ctable12 <- cbind(ctable12, "p value" = p12))
(ci12 <- confint(model12))
exp(cbind(OR = coef(model12), ci12))
# The content of the visualizations/GIFs/videos appealed to me.

vif(model12)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model12)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos appealing are different for participants who watched the static visualizations
# and participants who watched the GIFs with smooth transitions.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos appealing are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos appealing are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos appealing are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos appealing are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find the content of the visualizations/GIFs/videos appealing are different for female participants and
# and male participants.


# Post-hoc power analysis
table(data$"12")
popower(c(0, 2/48, 5/48, 9/48, 15/48, 10/48, 7/48), 0.2347289, 48, alpha=0.05)
popower(c(0, 2/48, 5/48, 9/48, 15/48, 10/48, 7/48), 0.3361385, 48, alpha=0.05)
popower(c(0, 2/48, 5/48, 9/48, 15/48, 10/48, 7/48), 0.4025460, 48, alpha=0.05)
popower(c(0, 2/48, 5/48, 9/48, 15/48, 10/48, 7/48), 0.7441223, 48, alpha=0.05)
popower(c(0, 2/48, 5/48, 9/48, 15/48, 10/48, 7/48), 1.9019841, 48, alpha=0.05)
popower(c(0, 2/48, 5/48, 9/48, 15/48, 10/48, 7/48), 0.4663792, 48, alpha=0.05)

# Statement 13
model13 = polr(data$"13" ~ variant + vl_level + gender, data = data)
(ctable13 <- coef(summary(model13)))
p13 <- pnorm(abs(ctable13[, "t value"]), lower.tail = FALSE) * 2
(ctable13 <- cbind(ctable13, "p value" = p13))
(ci13 <- confint(model13))
exp(cbind(OR = coef(model13), ci13))
# I felt tired.

vif(model13)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model13)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to feel tired are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to feel tired are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to feel tired are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to feel tired are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to feel tired are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to feel tired are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"13")
popower(c(22/48, 16/48, 1/48, 3/48, 4/48, 2/48, 0), 1.6456863, 48, alpha=0.05)
popower(c(22/48, 16/48, 1/48, 3/48, 4/48, 2/48, 0), 0.8192693, 48, alpha=0.05)
popower(c(22/48, 16/48, 1/48, 3/48, 4/48, 2/48, 0), 1.3600959, 48, alpha=0.05)
popower(c(22/48, 16/48, 1/48, 3/48, 4/48, 2/48, 0), 0.8929934, 48, alpha=0.05)
popower(c(22/48, 16/48, 1/48, 3/48, 4/48, 2/48, 0), 0.7921851, 48, alpha=0.05)
popower(c(22/48, 16/48, 1/48, 3/48, 4/48, 2/48, 0), 2.2862369, 48, alpha=0.05)


# Statement 14
model14 = polr(data$"14" ~ variant + vl_level + gender, data = data)
(ctable14 <- coef(summary(model14)))
p14 <- pnorm(abs(ctable14[, "t value"]), lower.tail = FALSE) * 2
(ctable14 <- cbind(ctable14, "p value" = p14))
(ci14 <- confint(model14))
exp(cbind(OR = coef(model14), ci14))
# I felt involved.

vif(model14)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model14)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to feel involved are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# 1/0.09833555 = 10.17
# For participants who watched the GIFs with smooth transitions, the odds of being more likely to feel involved are 10.17 times that
# of participants who watched the GIFs with interchangeability, holding constant all other variables.

# 1/0.15939594 = 6.27
# For participants who watched the GIFs with smooth transitions, the odds of being more likely to feel involved are 6.27 times that
# of participants who watched the data-videos, holding constant all other variables.

# For participants with a low VL level, the odds of being more likely to feel involved are 4.40 times that
# of participants with a medium VL level, holding constant all other variables.

# There is no evidence that the odds of being more likely to feel involved are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to feel involved are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"14")
popower(c(1/48, 3/48, 2/48, 5/48, 11/48, 12/48, 14/48), 0.22038087, 48, alpha=0.05)
popower(c(1/48, 3/48, 2/48, 5/48, 11/48, 12/48, 14/48), 0.09833555, 48, alpha=0.05)
popower(c(1/48, 3/48, 2/48, 5/48, 11/48, 12/48, 14/48), 0.15939594, 48, alpha=0.05)
popower(c(1/48, 3/48, 2/48, 5/48, 11/48, 12/48, 14/48), 4.40232455, 48, alpha=0.05)
popower(c(1/48, 3/48, 2/48, 5/48, 11/48, 12/48, 14/48), 1.13403385, 48, alpha=0.05)
popower(c(1/48, 3/48, 2/48, 5/48, 11/48, 12/48, 14/48), 0.90528273, 48, alpha=0.05)


# Statement 15
model15 = polr(data$"15" ~ variant + vl_level + gender, data = data)
(ctable15 <- coef(summary(model15)))
p15 <- pnorm(abs(ctable15[, "t value"]), lower.tail = FALSE) * 2
(ctable15 <- cbind(ctable15, "p value" = p15))
(ci15 <- confint(model15))
exp(cbind(OR = coef(model15), ci15))
# I found my mind wandering while the visualizations/GIFs/videos were being played.

vif(model15)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model15)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find their mind wandering while the visualizations/GIFs/videos were being played are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find their mind wandering while the visualizations/GIFs/videos were being played are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find their mind wandering while the visualizations/GIFs/videos were being played are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find their mind wandering while the visualizations/GIFs/videos were being played are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find their mind wandering while the visualizations/GIFs/videos were being played are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find their mind wandering while the visualizations/GIFs/videos were being played are different for female participants and
# and male participants.

# Post-hoc ower analysis
table(data$"15")
popower(c(12/48, 17/48, 4/48, 2/48, 8/48, 2/48, 3/48), 1.2670332, 48, alpha=0.05)
popower(c(12/48, 17/48, 4/48, 2/48, 8/48, 2/48, 3/48), 0.6387224, 48, alpha=0.05)
popower(c(12/48, 17/48, 4/48, 2/48, 8/48, 2/48, 3/48), 0.3416312, 48, alpha=0.05)
popower(c(12/48, 17/48, 4/48, 2/48, 8/48, 2/48, 3/48), 0.4034453, 48, alpha=0.05)
popower(c(12/48, 17/48, 4/48, 2/48, 8/48, 2/48, 3/48), 0.6386040, 48, alpha=0.05)
popower(c(12/48, 17/48, 4/48, 2/48, 8/48, 2/48, 3/48), 0.9070398, 48, alpha=0.05)


# Statement 16
# This is necessary to fix the error 'initial value in 'vmmin' is not finite'
model16_start = polr(data$"16" ~ variant, data = data)
myGuess <- c(model16_start$coefficients, 0, 0, 0, model16_start$zeta)

model16 = polr(data$"16" ~ variant + vl_level + gender, data = data, start = myGuess)
(ctable16 <- coef(summary(model16)))
p16 <- pnorm(abs(ctable16[, "t value"]), lower.tail = FALSE) * 2
(ctable16 <- cbind(ctable16, "p value" = p16))
(ci16 <- confint(model16)) # I forgot about my immediate surroundings while watching the visualizations/GIFs/videos.
exp(cbind(OR = coef(model16), ci16))
# I had a hard time keeping my mind on the visualizations/GIFs/videos.

vif(model16)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model16)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to have a hard time keeping their mind on the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# For participants who watched the GIFs with interchangeability, the odds of being more likely to have a hard time keeping their mind on the visualizations/GIFs/videos are 5.29 times those of 
# participants who watched the GIFs with smooth transitions.

# There is no evidence that the odds of being more likely to have a hard time keeping their mind on the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to have a hard time keeping their mind on the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a low VL level.

# 1/0.2099489 = 4.76
# For participants with a medium VL level, the odds of being more likely to have a hard time keeping their mind on the visualizations/GIFs/videos are 4.76 times those of
# participants with a high VL level.

# There is no evidence that the odds of being more likely to have a hard time keeping their mind on the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"16")
popower(c(19/48, 20/48, 3/48, 4/48, 1/48, 1/48, 0), 3.9445113, 48, alpha=0.05)
popower(c(19/48, 20/48, 3/48, 4/48, 1/48, 1/48, 0), 5.2864844, 48, alpha=0.05)
popower(c(19/48, 20/48, 3/48, 4/48, 1/48, 1/48, 0), 1.9699490, 48, alpha=0.05)
popower(c(19/48, 20/48, 3/48, 4/48, 1/48, 1/48, 0), 0.6847983, 48, alpha=0.05)
popower(c(19/48, 20/48, 3/48, 4/48, 1/48, 1/48, 0), 0.2099489, 48, alpha=0.05)
popower(c(19/48, 20/48, 3/48, 4/48, 1/48, 1/48, 0), 2.4257199, 48, alpha=0.05)


# Statement 17
model17 = polr(data$"17" ~ variant + vl_level + gender, data = data)
(ctable17 <- coef(summary(model17)))
p17 <- pnorm(abs(ctable17[, "t value"]), lower.tail = FALSE) * 2
(ctable17 <- cbind(ctable17, "p value" = p17))
(ci17 <- confint(model17))
exp(cbind(OR = coef(model17), ci17))
# I forgot about my immediate surroundings while watching the visualizations/GIFs/videos.

vif(model7)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model17)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to forget about their immediate surroundings while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to forget about their immediate surroundings while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to forget about their immediate surroundings while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to forget about their immediate surroundings while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to forget about their immediate surroundings while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to forget about their immediate surroundings while watching the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"17")
popower(c(2/48, 5/48, 7/48, 8/48, 9/48, 12/48, 5/48), 0.4269379, 48, alpha=0.05)
popower(c(2/48, 5/48, 7/48, 8/48, 9/48, 12/48, 5/48), 0.5964870, 48, alpha=0.05)
popower(c(2/48, 5/48, 7/48, 8/48, 9/48, 12/48, 5/48), 1.9807714, 48, alpha=0.05)
popower(c(2/48, 5/48, 7/48, 8/48, 9/48, 12/48, 5/48), 1.2904311, 48, alpha=0.05)
popower(c(2/48, 5/48, 7/48, 8/48, 9/48, 12/48, 5/48), 0.8892081, 48, alpha=0.05)
popower(c(2/48, 5/48, 7/48, 8/48, 9/48, 12/48, 5/48), 1.1308347, 48, alpha=0.05)


# Statement 18
model18 = polr(data$"18" ~ variant + vl_level + gender, data = data)
(ctable18 <- coef(summary(model18)))
p18 <- pnorm(abs(ctable18[, "t value"]), lower.tail = FALSE) * 2
(ctable18 <- cbind(ctable18, "p value" = p18))
(ci18 <- confint(model18))
exp(cbind(OR = coef(model18), ci18))
# While watching the visualizations/GIFs/videos, I found myself concentrating on specific aspects of them.

vif(model18)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model18)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find themselves concentrating on specific aspects of the visualizations/GIFs/videos while watching them are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find themselves concentrating on specific aspects of the visualizations/GIFs/videos while watching them are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find themselves concentrating on specific aspects of the visualizations/GIFs/videos while watching them are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find themselves concentrating on specific aspects of the visualizations/GIFs/videos while watching them are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find themselves concentrating on specific aspects of the visualizations/GIFs/videos while watching them are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find themselves concentrating on specific aspects of the visualizations/GIFs/videos while watching them are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"18")
popower(c(1/48, 1/48, 1/48, 2/48, 11/48, 13/48, 19/48), 0.2289654, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 2/48, 11/48, 13/48, 19/48), 0.3589465, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 2/48, 11/48, 13/48, 19/48), 0.5842413, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 2/48, 11/48, 13/48, 19/48), 0.3913452, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 2/48, 11/48, 13/48, 19/48), 1.2930236, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 2/48, 11/48, 13/48, 19/48), 1.8942446, 48, alpha=0.05)


# Statement 19
model19 = polr(data$"19" ~ variant + vl_level + gender, data = data)
(ctable19 <- coef(summary(model19)))
p19 <- pnorm(abs(ctable19[, "t value"]), lower.tail = FALSE) * 2
(ctable19 <- cbind(ctable19, "p value" = p19))
(ci19 <- confint(model19))
exp(cbind(OR = coef(model19), ci19))
# While watching the visualizations/GIFs/videos, I had to pay attention to multiple things at the same time.

vif(model19)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model19)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to have to pay attention to multiple things at the same time while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to have to pay attention to multiple things at the same time while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to have to pay attention to multiple things at the same time while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to have to pay attention to multiple things at the same time while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to have to pay attention to multiple things at the same time while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to have to pay attention to multiple things at the same time while watching the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"19")
popower(c(1/48, 3/48, 1/48, 5/48, 12/48, 17/48, 9/48), 0.8175416, 48, alpha=0.05)
popower(c(1/48, 3/48, 1/48, 5/48, 12/48, 17/48, 9/48), 0.8414119, 48, alpha=0.05)
popower(c(1/48, 3/48, 1/48, 5/48, 12/48, 17/48, 9/48), 0.3996359, 48, alpha=0.05)
popower(c(1/48, 3/48, 1/48, 5/48, 12/48, 17/48, 9/48), 1.1360757, 48, alpha=0.05)
popower(c(1/48, 3/48, 1/48, 5/48, 12/48, 17/48, 9/48), 0.9454354, 48, alpha=0.05)
popower(c(1/48, 3/48, 1/48, 5/48, 12/48, 17/48, 9/48), 2.6388605, 48, alpha=0.05)


# Statement 20
model20 = polr(data$"20" ~ variant + vl_level + gender, data = data)
(ctable20 <- coef(summary(model20)))
p20 <- pnorm(abs(ctable20[, "t value"]), lower.tail = FALSE) * 2
(ctable20 <- cbind(ctable20, "p value" = p20))
(ci20 <- confint(model20))
exp(cbind(OR = coef(model20), ci20))
# While watching the visualizations/GIFs/videos, time seemed to pass quickly.

vif(model20)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model20)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to think that the time seemed to pass quickly while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to think that the time seemed to pass quickly while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to think that the time seemed to pass quickly while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to think that the time seemed to pass quickly while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to think that the time seemed to pass quickly while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to think that the time seemed to pass quickly while watching the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"20")
popower(c(0, 3/48, 4/48, 17/48, 4/48, 12/48, 8/48), 1.5529680, 48, alpha=0.05)
popower(c(0, 3/48, 4/48, 17/48, 4/48, 12/48, 8/48), 1.5042801, 48, alpha=0.05)
popower(c(0, 3/48, 4/48, 17/48, 4/48, 12/48, 8/48), 1.6764072, 48, alpha=0.05)
popower(c(0, 3/48, 4/48, 17/48, 4/48, 12/48, 8/48), 2.8536570, 48, alpha=0.05)
popower(c(0, 3/48, 4/48, 17/48, 4/48, 12/48, 8/48), 2.1889839, 48, alpha=0.05)
popower(c(0, 3/48, 4/48, 17/48, 4/48, 12/48, 8/48), 0.5522399, 48, alpha=0.05)


# Statement 21
model21 = polr(data$"21" ~ variant + vl_level + gender, data = data)
(ctable21 <- coef(summary(model21)))
p21 <- pnorm(abs(ctable21[, "t value"]), lower.tail = FALSE) * 2
(ctable21 <- cbind(ctable21, "p value" = p21))
(ci21 <- confint(model21))
exp(cbind(OR = coef(model21), ci21))
# I feel that I could construct a story about the facts presented in the visualizations/GIFs/videos.

vif(model21)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model21)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to feel that they could construct a story about the facts presented in the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to feel that they could construct a story about the facts presented in the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to feel that they could construct a story about the facts presented in the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# 1/0.2419614 = 4.13
# For participants with a medium VL level, the odds of being more likely to feel that they could construct a story about the facts presented in the visualizations/GIFs/videos are 4.13 times that
# of participants with a low VL level, holding constant all other variables.

# There is no evidence that the odds of being more likely to feel that they could construct a story about the facts presented in the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to feel that they could construct a story about the facts presented in the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"21")
popower(c(3/48, 4/48, 3/48, 7/48, 12/48, 10/48, 9/48), 0.6440652, 48, alpha=0.05)
popower(c(3/48, 4/48, 3/48, 7/48, 12/48, 10/48, 9/48), 0.9594939, 48, alpha=0.05)
popower(c(3/48, 4/48, 3/48, 7/48, 12/48, 10/48, 9/48), 0.8026259, 48, alpha=0.05)
popower(c(3/48, 4/48, 3/48, 7/48, 12/48, 10/48, 9/48), 0.2419614, 48, alpha=0.05)
popower(c(3/48, 4/48, 3/48, 7/48, 12/48, 10/48, 9/48), 0.7431975, 48, alpha=0.05)
popower(c(3/48, 4/48, 3/48, 7/48, 12/48, 10/48, 9/48), 2.8090704, 48, alpha=0.05)


# Statement 22
model22 = polr(data$"22" ~ variant + vl_level + gender, data = data)
(ctable22 <- coef(summary(model22)))
p22 <- pnorm(abs(ctable22[, "t value"]), lower.tail = FALSE) * 2
(ctable22 <- cbind(ctable22, "p value" = p22))
(ci22 <- confint(model22))
exp(cbind(OR = coef(model22), ci22))
# The content or message of the visualizations/GIFs/videos was interesting to me.

vif(model22)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model22)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the content or message of the visualizations/GIFs/videos interesting are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find the content or message of the visualizations/GIFs/videos interesting are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the content or message of the visualizations/GIFs/videos interesting are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find the content or message of the visualizations/GIFs/videos interesting are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the content or message of the visualizations/GIFs/videos interesting are different for participants with a medium VL level and
# and participants with a high VL level.

# 1/0.3840719 = 2.60
# For the male participants, the odds of being more likely to find the content or message of the visualizations/GIFs/videos interesting are 2.60 times those of female participants.

# Post-hoc power analysis
table(data$"22")
popower(c(1/48, 1/48, 3/48, 8/48, 10/48, 17/48, 8/48), 0.3846449, 48, alpha=0.05)
popower(c(1/48, 1/48, 3/48, 8/48, 10/48, 17/48, 8/48), 1.3540363, 48, alpha=0.05)
popower(c(1/48, 1/48, 3/48, 8/48, 10/48, 17/48, 8/48), 0.6604726, 48, alpha=0.05)
popower(c(1/48, 1/48, 3/48, 8/48, 10/48, 17/48, 8/48), 1.2035268, 48, alpha=0.05)
popower(c(1/48, 1/48, 3/48, 8/48, 10/48, 17/48, 8/48), 1.6826776, 48, alpha=0.05)
popower(c(1/48, 1/48, 3/48, 8/48, 10/48, 17/48, 8/48), 0.3840719, 48, alpha=0.05)

# Statement 23
model23 = polr(data$"23" ~ variant + vl_level + gender, data = data)
(ctable23 <- coef(summary(model23)))
p23 <- pnorm(abs(ctable23[, "t value"]), lower.tail = FALSE) * 2
(ctable23 <- cbind(ctable23, "p value" = p23))
(ci23 <- confint(model23))
exp(cbind(OR = coef(model23), ci23))
# While watching the visualizations/GIFs/videos, I enjoyed and accepted any challenges they presented.

vif(model23)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model23)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to enjoy and accept any challenges the visualizations/GIFs/videos present are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to enjoy and accept any challenges the visualizations/GIFs/videos present are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to enjoy and accept any challenges the visualizations/GIFs/videos present are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to enjoy and accept any challenges the visualizations/GIFs/videos present are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to enjoy and accept any challenges the visualizations/GIFs/videos present are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to enjoy and accept any challenges the visualizations/GIFs/videos present are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"23")
popower(c(1/48, 1/48, 1/48, 7/48, 10/48, 15/48, 13/48), 0.3649166, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 7/48, 10/48, 15/48, 13/48), 1.3734887, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 7/48, 10/48, 15/48, 13/48), 1.2274220, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 7/48, 10/48, 15/48, 13/48), 1.5170503, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 7/48, 10/48, 15/48, 13/48), 1.1550321, 48, alpha=0.05)
popower(c(1/48, 1/48, 1/48, 7/48, 10/48, 15/48, 13/48), 0.6672195, 48, alpha=0.05)


# Statement 24
model24 = polr(data$"24" ~ variant + vl_level + gender, data = data)
(ctable24 <- coef(summary(model24)))
p24<- pnorm(abs(ctable24[, "t value"]), lower.tail = FALSE) * 2
(ctable24 <- cbind(ctable24, "p value" = p24))
(ci24 <- confint(model24)) 
exp(cbind(OR = coef(model24), ci24))
# While watching the visualizations/GIFs/videos, I had to think carefully, deeply, or reflectively.

vif(model24)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model24)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to have to think carefully, deeply, or reflectively while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to have to think carefully, deeply, or reflectively while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to have to think carefully, deeply, or reflectively while watching the visualizations/GIFs/videos are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to have to think carefully, deeply, or reflectively while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to have to think carefully, deeply, or reflectively while watching the visualizations/GIFs/videos are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to have to think carefully, deeply, or reflectively while watching the visualizations/GIFs/videos are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"24")
popower(c(2/48, 2/48, 4/48, 6/48, 9/48, 17/48, 8/48), 0.7258379, 48, alpha=0.05)
popower(c(2/48, 2/48, 4/48, 6/48, 9/48, 17/48, 8/48), 1.0849196, 48, alpha=0.05)
popower(c(2/48, 2/48, 4/48, 6/48, 9/48, 17/48, 8/48), 1.0162636, 48, alpha=0.05)
popower(c(2/48, 2/48, 4/48, 6/48, 9/48, 17/48, 8/48), 1.5372758, 48, alpha=0.05)
popower(c(2/48, 2/48, 4/48, 6/48, 9/48, 17/48, 8/48), 1.5983909, 48, alpha=0.05)
popower(c(2/48, 2/48, 4/48, 6/48, 9/48, 17/48, 8/48), 1.3143355, 48, alpha=0.05)


# Statement 25
model25 = polr(data$"25" ~ variant + vl_level + gender, data = data)
(ctable25 <- coef(summary(model25)))
p25 <- pnorm(abs(ctable25[, "t value"]), lower.tail = FALSE) * 2
(ctable25 <- cbind(ctable25, "p value" = p25))
(ci25 <- confint(model25))
exp(cbind(OR = coef(model25), ci25))
# I would want to watch the visualizations/GIFs/videos if I saw them somewhere else and was not required or encouraged to watch them.

vif(model25)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model25)
# The proportional odds assumption holds at significance level alpha = 0.01.

# 1/0.2073060 = 4.82
# For participants who watched the GIFs with smooth transitions, the odds of being more likely to watch the visualizations/GIFs/videos if they were not required or encouraged to watch them is 4.82 times that
# of participants who viewed the static visualizations, holding constant all other variables.

# There is no evidence that the odds of being more likely to watch the visualizations/GIFs/videos if they were not required or encouraged to watch them are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to watch the visualizations/GIFs/videos if they were not required or encouraged to watch them are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to watch the visualizations/GIFs/videos if they were not required or encouraged to watch them are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to watch the visualizations/GIFs/videos if they were not required or encouraged to watch them are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to watch the visualizations/GIFs/videos if they were not required or encouraged to watch them are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"25")
popower(c(0, 5/48, 7/48, 8/48, 7/48, 12/48, 9/48), 0.2073060, 48, alpha=0.05)
popower(c(0, 5/48, 7/48, 8/48, 7/48, 12/48, 9/48), 2.2599994, 48, alpha=0.05)
popower(c(0, 5/48, 7/48, 8/48, 7/48, 12/48, 9/48), 0.5829800, 48, alpha=0.05)
popower(c(0, 5/48, 7/48, 8/48, 7/48, 12/48, 9/48), 0.8378774, 48, alpha=0.05)
popower(c(0, 5/48, 7/48, 8/48, 7/48, 12/48, 9/48), 2.1059483, 48, alpha=0.05)
popower(c(0, 5/48, 7/48, 8/48, 7/48, 12/48, 9/48), 0.6712649, 48, alpha=0.05)


# Statement 26
model26 = polr(data$"26" ~ variant + vl_level + gender, data = data)
(ctable26 <- coef(summary(model26)))
p26 <- pnorm(abs(ctable26[, "t value"]), lower.tail = FALSE) * 2
(ctable26 <- cbind(ctable26, "p value" = p26))
(ci26 <- confint(model26)) # The visualizations/GIFs/videos were visually pleasing.
exp(cbind(OR = coef(model26), ci26))
# The visualizations/GIFs/videos were aesthetically appealing.

vif(model26)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model26)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were aesthetically appealing are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were aesthetically appealing are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were aesthetically appealing are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were aesthetically appealing are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were aesthetically appealing are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were aesthetically appealing are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"26")
popower(c(1/48, 6/48, 2/48, 6/48, 16/48, 8/48, 9/48), 0.3726740, 48, alpha=0.05)
popower(c(1/48, 6/48, 2/48, 6/48, 16/48, 8/48, 9/48), 0.6211419, 48, alpha=0.05)
popower(c(1/48, 6/48, 2/48, 6/48, 16/48, 8/48, 9/48), 2.3819033, 48, alpha=0.05)
popower(c(1/48, 6/48, 2/48, 6/48, 16/48, 8/48, 9/48), 1.8525982, 48, alpha=0.05)
popower(c(1/48, 6/48, 2/48, 6/48, 16/48, 8/48, 9/48), 0.7992850, 48, alpha=0.05)
popower(c(1/48, 6/48, 2/48, 6/48, 16/48, 8/48, 9/48), 1.1544654, 48, alpha=0.05)


# Statement 27
model27 = polr(data$"27" ~ variant + vl_level + gender, data = data)
(ctable27 <- coef(summary(model27)))
p27 <- pnorm(abs(ctable27[, "t value"]), lower.tail = FALSE) * 2
(ctable27 <- cbind(ctable27, "p value" = p27))
(ci27 <- confint(model27))
exp(cbind(OR = coef(model27), ci27))
# The visualizations/GIFs/videos were visually pleasing.

vif(model27)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model27)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were visually pleasing are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were visually pleasing are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were visually pleasing are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were visually pleasing are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were visually pleasing are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find the visualizations/GIFs/videos were visually pleasing are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"27")
popower(c(0, 5/48, 2/48, 8/48, 13/48, 12/48, 8/48), 0.3695387, 48, alpha=0.05)
popower(c(0, 5/48, 2/48, 8/48, 13/48, 12/48, 8/48), 0.6048604, 48, alpha=0.05)
popower(c(0, 5/48, 2/48, 8/48, 13/48, 12/48, 8/48), 3.1055108, 48, alpha=0.05)
popower(c(0, 5/48, 2/48, 8/48, 13/48, 12/48, 8/48), 1.4448392, 48, alpha=0.05)
popower(c(0, 5/48, 2/48, 8/48, 13/48, 12/48, 8/48), 0.7261087, 48, alpha=0.05)
popower(c(0, 5/48, 2/48, 8/48, 13/48, 12/48, 8/48), 0.5728858, 48, alpha=0.05)


# Statement 28
model28 = polr(data$"28" ~ variant + vl_level + gender, data = data)
(ctable28 <- coef(summary(model28)))
p28 <- pnorm(abs(ctable28[, "t value"]), lower.tail = FALSE) * 2
(ctable28 <- cbind(ctable28, "p value" = p28))
(ci28 <- confint(model28))
exp(cbind(OR = coef(model28), ci28))
# I found the graphics in the visualizations/GIFs/videos distracting.

vif(model28)
# All VIFs are around 1 which means that there is no multicollinearity.

brant(model28)
# The proportional odds assumption holds at significance level alpha = 0.01.

# There is no evidence that the odds of being more likely to find the graphics in the visualizations/GIFs/videos distracting are different for participants who watched the GIFs with smooth transitions
# and participants who viewed the static visualizations.

# There is no evidence that the odds of being more likely to find the graphics in the visualizations/GIFs/videos distracting are different for participants who watched the GIFs with smooth transitions
# and participants who watched the GIFs with interchangeability.

# There is no evidence that the odds of being more likely to find the graphics in the visualizations/GIFs/videos distracting are different for participants who watched the GIFs with smooth transitions
# and participants who watched the data-videos.

# There is no evidence that the odds of being more likely to find the graphics in the visualizations/GIFs/videos distracting are different for participants with a medium VL level and
# and participants with a low VL level.

# There is no evidence that the odds of being more likely to find the graphics in the visualizations/GIFs/videos distracting are different for participants with a medium VL level and
# and participants with a high VL level.

# There is no evidence that the odds of being more likely to find the graphics in the visualizations/GIFs/videos distracting are different for female participants and
# and male participants.

# Post-hoc power analysis
table(data$"28")
popower(c(12/48, 20/48, 4/48, 9/48, 3/48, 0, 0), 0.8618464, 48, alpha=0.05)
popower(c(12/48, 20/48, 4/48, 9/48, 3/48, 0, 0), 0.4241439, 48, alpha=0.05)
popower(c(12/48, 20/48, 4/48, 9/48, 3/48, 0, 0), 0.4211531, 48, alpha=0.05)
popower(c(12/48, 20/48, 4/48, 9/48, 3/48, 0, 0), 0.4073941, 48, alpha=0.05)
popower(c(12/48, 20/48, 4/48, 9/48, 3/48, 0, 0), 0.4083733, 48, alpha=0.05)
popower(c(12/48, 20/48, 4/48, 9/48, 3/48, 0, 0), 3.2449630, 48, alpha=0.05)


# Colorblind analysis
model_cb1 = polr(data$"1" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb1 <- coef(summary(model_cb1)))
p_cb1 <- pnorm(abs(ctable_cb1[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb1 <- cbind(ctable_cb1, "p value" = p_cb1))
(ci_cb1 <- confint(model_cb1))
exp(cbind(OR = coef(model_cb1), ci_cb1))

model_cb2 = polr(data$"2" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb2 <- coef(summary(model_cb2)))
p_cb2 <- pnorm(abs(ctable_cb2[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb2 <- cbind(ctable_cb2, "p value" = p_cb2))
(ci_cb2 <- confint(model_cb2))
exp(cbind(OR = coef(model_cb2), ci_cb2))

model_cb3 = polr(data$"3" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb3 <- coef(summary(model_cb3)))
p_cb3 <- pnorm(abs(ctable_cb3[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb3 <- cbind(ctable_cb3, "p value" = p_cb3))
(ci_cb3 <- confint(model_cb3))
exp(cbind(OR = coef(model_cb3), ci_cb3))

model_cb4 = polr(data$"4" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb4 <- coef(summary(model_cb4)))
p_cb4 <- pnorm(abs(ctable_cb4[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb4 <- cbind(ctable_cb4, "p value" = p_cb4))
(ci_cb4 <- confint(model_cb4))
exp(cbind(OR = coef(model_cb4), ci_cb4))

model_cb5 = polr(data$"5" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb5 <- coef(summary(model_cb5)))
p_cb5 <- pnorm(abs(ctable_cb5[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb5 <- cbind(ctable_cb5, "p value" = p_cb5))
(ci_cb5 <- confint(model_cb5))
exp(cbind(OR = coef(model_cb5), ci_cb5))
# For colorblind viewers, the odds of being more likely to have hard time making sense of what is going on in the GIFs
# are 95.31 times higher those of non-colorblind viewers.

model_cb6 = polr(data$"6" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb6 <- coef(summary(model_cb6)))
p_cb6 <- pnorm(abs(ctable_cb6[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb6 <- cbind(ctable_cb6, "p value" = p_cb6))
(ci_cb6 <- confint(model_cb6))
exp(cbind(OR = coef(model_cb6), ci_cb6))

model_cb7 = polr(data$"7" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb7 <- coef(summary(model_cb7)))
p_cb7 <- pnorm(abs(ctable_cb7[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb7 <- cbind(ctable_cb7, "p value" = p_cb7))
(ci_cb7 <- confint(model_cb7))
exp(cbind(OR = coef(model_cb7), ci_cb7))

model_cb8 = polr(data$"8" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb8 <- coef(summary(model_cb8)))
p_cb8 <- pnorm(abs(ctable_cb8[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb8 <- cbind(ctable_cb8, "p value" = p_cb8))
(ci_cb8 <- confint(model_cb8))
exp(cbind(OR = coef(model_cb8), ci_cb8))

model_cb9 = polr(data$"9" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb9 <- coef(summary(model_cb9)))
p_cb9 <- pnorm(abs(ctable_cb9[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb9 <- cbind(ctable_cb9, "p value" = p_cb9))
(ci_cb9 <- confint(model_cb9))
exp(cbind(OR = coef(model_cb9), ci_cb9))

model_cb10 = polr(data$"10" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb10 <- coef(summary(model_cb10)))
p_cb10 <- pnorm(abs(ctable_cb10[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb10 <- cbind(ctable, "p value" = p_cb10))
(ci_cb10 <- confint(model_cb10))
exp(cbind(OR = coef(model_cb10), ci_cb10))

model_cb11 = polr(data$"11" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb11 <- coef(summary(model_cb11)))
p_cb11 <- pnorm(abs(ctable_cb11[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb11 <- cbind(ctable_cb11, "p value" = p_cb11))
(ci_cb11 <- confint(model_cb11))
exp(cbind(OR = coef(model_cb11), ci_cb11))

model_cb12 = polr(data$"12" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb12 <- coef(summary(model_cb12)))
p_cb12 <- pnorm(abs(ctable_cb12[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb12 <- cbind(ctable_cb12, "p value" = p_cb12))
(ci_cb12 <- confint(model_cb12))
exp(cbind(OR = coef(model_cb12), ci_cb12))

model_cb13 = polr(data$"13" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb13 <- coef(summary(model_cb13)))
p_cb13 <- pnorm(abs(ctable_cb13[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb13 <- cbind(ctable_cb13, "p value" = p_cb13))
(ci_cb13 <- confint(model_cb13))
exp(cbind(OR = coef(model_cb13), ci_cb13))

model_cb14= polr(data$"14" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb14 <- coef(summary(model_cb14)))
p_cb14 <- pnorm(abs(ctable_cb14[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb14 <- cbind(ctable_cb14, "p value" = p_cb14))
(ci_cb14 <- confint(model_cb14))
exp(cbind(OR = coef(model_cb14), ci_cb14))

model_cb15 = polr(data$"15" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb15 <- coef(summary(model_cb15)))
p_cb15 <- pnorm(abs(ctable_cb15[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb15 <- cbind(ctable_cb15, "p value" = p_cb15))
(ci_cb15 <- confint(model_cb15))
exp(cbind(OR = coef(model_cb15), ci_cb15))

model_cb16 = polr(data$"16" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb16 <- coef(summary(model_cb16)))
p_cb16 <- pnorm(abs(ctable_cb16[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb16 <- cbind(ctable_cb16, "p value" = p_cb16))
(ci_cb16 <- confint(model_cb16))
exp(cbind(OR = coef(model_cb16), ci_cb16))

model_cb17 = polr(data$"17" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb17 <- coef(summary(model_cb17)))
p_cb17 <- pnorm(abs(ctable_cb17[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb17 <- cbind(ctable_cb17, "p value" = p_cb17))
(ci_cb17 <- confint(model_cb17))
exp(cbind(OR = coef(model_cb17), ci_cb17))
# For colorblind viewers, the odds of being more likely to forget about their immediate surroundings 
# while watching the GIFs are 15.70 times higher those of non-colorblind viewers.

model_cb18 = polr(data$"18" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb18 <- coef(summary(model_cb18)))
p_cb18 <- pnorm(abs(ctable_cb18[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb18 <- cbind(ctable_cb18, "p value" = p_cb18))
(ci_cb18 <- confint(model_cb18))
exp(cbind(OR = coef(model_cb18), ci_cb18))

model_cb19 = polr(data$"19" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb19 <- coef(summary(model_cb19)))
p_cb19 <- pnorm(abs(ctable_cb19[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb19 <- cbind(ctable_cb19, "p value" = p_cb19))
(ci_cb19 <- confint(model_cb19))
exp(cbind(OR = coef(model_cb19), ci_cb19))

model_cb20 = polr(data$"20" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb20 <- coef(summary(model_cb20)))
p_cb20 <- pnorm(abs(ctable_cb20[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb20 <- cbind(ctable_cb20, "p value" = p_cb20))
(ci_cb20 <- confint(model_cb20))
exp(cbind(OR = coef(model_cb20), ci_cb20))

model_cb21 = polr(data$"21" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb21 <- coef(summary(model_cb21)))
p_cb21 <- pnorm(abs(ctable_cb21[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb21 <- cbind(ctable_cb21, "p value" = p_cb21))
(ci_cb21 <- confint(model_cb21))
exp(cbind(OR = coef(model_cb21), ci_cb21))

model_cb22 = polr(data$"22" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb22 <- coef(summary(model_cb22)))
p_cb22 <- pnorm(abs(ctable_cb22[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb22 <- cbind(ctable_cb22, "p value" = p_cb22))
(ci_cb22 <- confint(model_cb22))
exp(cbind(OR = coef(model_cb22), ci_cb22))

model_cb23 = polr(data$"23" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb23 <- coef(summary(model_cb23)))
p_cb23 <- pnorm(abs(ctable_cb23[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb23 <- cbind(ctable_cb23, "p value" = p_cb23))
(ci_cb23 <- confint(model_cb23))
exp(cbind(OR = coef(model_cb23), ci_cb23))

model_cb24 = polr(data$"24" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb24 <- coef(summary(model_cb24)))
p_cb24 <- pnorm(abs(ctable_cb24[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb24 <- cbind(ctable_cb24, "p value" = p_cb24))
(ci_cb24 <- confint(model_cb24))
exp(cbind(OR = coef(model_cb24), ci_cb24))

model_cb25 = polr(data$"25" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb25 <- coef(summary(model_cb25)))
p_cb25 <- pnorm(abs(ctable_cb25[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb25 <- cbind(ctable_cb25, "p value" = p_cb25))
(ci_cb25 <- confint(model_cb25))
exp(cbind(OR = coef(model_cb25), ci_cb25))

model_cb26 = polr(data$"26" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb26 <- coef(summary(model_cb26)))
p_cb26 <- pnorm(abs(ctable_cb26[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb26 <- cbind(ctable_cb26, "p value" = p_cb26))
(ci_cb26 <- confint(model_cb26))
exp(cbind(OR = coef(model_cb26), ci_cb26))

model_cb27 = polr(data$"27" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb27 <- coef(summary(model_cb27)))
p_cb27 <- pnorm(abs(ctable_cb27[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb27 <- cbind(ctable_cb27, "p value" = p_cb27))
(ci_cb27 <- confint(model_cb27))
exp(cbind(OR = coef(model_cb27), ci_cb27))

model_cb28 = polr(data$"28" ~ cb + variant + vl_level + gender, data = data)
(ctable_cb28 <- coef(summary(model_cb28)))
p_cb28 <- pnorm(abs(ctable_cb28[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb28 <- cbind(ctable_cb28, "p value" = p_cb28))
(ci_cb28 <- confint(model_cb28))
exp(cbind(OR = coef(model_cb28), ci_cb28))
