### Statistical analysis of the data generated from the user engagement questionnaire #
# for the 'Show Me The GIFference' work ###

# Reference: https://jakec007.github.io/2021-06-23-R-likert/

# Loading the libraries
library("readxl")
library("tidyverse")
library("likert")
library("plyr")
library("MASS")

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
data$condition = as.factor(data$condition)


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

# Creating a stacked bar chart
plot(items_likert,
     group.order = names(items),
     centered = TRUE,
     col=c("#8c510a","#d8b365","#f6e8c3","#f5f5f5","#c7eae5","#5ab4ac","#01665e")) +
     theme(text=element_text(size=12))





# Performing an Ordinal Linear Regression for each statement
data = within(data, condition <- relevel(condition, ref = 3))

model1 = polr(data$"1" ~ data$condition, data = data)
(ctable1 <- coef(summary(model1)))
p1 = pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
(ctable1 = cbind(ctable1, "p value" = p1))
(ci1 = confint(model1)) # During viewing, I reflected on the content of the visualizations/GIFs/videos.
exp(cbind(OR = coef(model1), ci1))


model2 = polr(data$"2" ~ data$condition, data = data)
(ctable2 <- coef(summary(model2)))
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
(ctable2 <- cbind(ctable2, "p value" = p2))
(ci2 <- confint(model2))
exp(cbind(OR = coef(model2), ci2)) 
# 

model3 = polr(data$"3" ~ data$condition, data = data)
(ctable3 <- coef(summary(model3)))
p3 <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2
(ctable3 <- cbind(ctable3, "p value" = p3))
(ci3 <- confint(model3))
exp(cbind(OR = coef(model3), ci3))

model4 = polr(data$"4" ~ data$condition, data = data)
(ctable4 <- coef(summary(model4)))
p4 <- pnorm(abs(ctable4[, "t value"]), lower.tail = FALSE) * 2
(ctable4 <- cbind(ctable4, "p value" = p4))
(ci4 <- confint(model4)) # I learned something new.
exp(cbind(OR = coef(model4), ci4))

model5 = polr(data$"5" ~ data$condition, data = data)
(ctable5 <- coef(summary(model5)))
p5 <- pnorm(abs(ctable5[, "t value"]), lower.tail = FALSE) * 2
(ctable5 <- cbind(ctable5, "p value" = p5))
(ci5 <- confint(model5)) # At points, I had a hard time making sense of what was going on in the visualizations/GIFs/videos.
exp(cbind(OR = coef(model5), ci5))

model6 = polr(data$"6" ~ data$condition, data = data)
(ctable6 <- coef(summary(model6)))
p6 <- pnorm(abs(ctable6[, "t value"]), lower.tail = FALSE) * 2
(ctable6 <- cbind(ctable6, "p value" = p6))
(ci6 <- confint(model6))
exp(cbind(OR = coef(model6), ci6))

model7 = polr(data$"7" ~ data$condition, data = data)
(ctable7 <- coef(summary(model7)))
p7 <- pnorm(abs(ctable7[, "t value"]), lower.tail = FALSE) * 2
(ctable7 <- cbind(ctable7, "p value" = p7))
(ci7 <- confint(model7))
exp(cbind(OR = coef(model7), ci7))

model8 = polr(data$"8" ~ data$condition, data = data)
(ctable8 <- coef(summary(model8)))
p8 <- pnorm(abs(ctable8[, "t value"]), lower.tail = FALSE) * 2
(ctable8 <- cbind(ctable8, "p value" = p8))
(ci8 <- confint(model8))
exp(cbind(OR = coef(model8), ci8))

model9 = polr(data$"9" ~ data$condition, data = data)
(ctable9 <- coef(summary(model9)))
p9 <- pnorm(abs(ctable9[, "t value"]), lower.tail = FALSE) * 2
(ctable9 <- cbind(ctable9, "p value" = p9))
(ci9 <- confint(model9))
exp(cbind(OR = coef(model9), ci9))

model10 = polr(data$"10" ~ data$condition, data = data)
(ctable10 <- coef(summary(model10)))
p10 <- pnorm(abs(ctable10[, "t value"]), lower.tail = FALSE) * 2
(ctable10 <- cbind(ctable10, "p value" = p10))
(ci10 <- confint(model10)) # I found the visualizations/GIFs/videos boring.
exp(cbind(OR = coef(model10), ci10))

model11 = polr(data$"11" ~ data$condition, data = data)
(ctable11 <- coef(summary(model11)))
p11 <- pnorm(abs(ctable11[, "t value"]), lower.tail = FALSE) * 2
(ctable11 <- cbind(ctable11, "p value" = p11))
(ci11 <- confint(model11)) 
exp(cbind(OR = coef(model11), ci11))

model12 = polr(data$"12" ~ data$condition, data = data)
(ctable12 <- coef(summary(model12)))
p12 <- pnorm(abs(ctable12[, "t value"]), lower.tail = FALSE) * 2
(ctable12 <- cbind(ctable12, "p value" = p12))
(ci12 <- confint(model12))
exp(cbind(OR = coef(model12), ci12))

model13 = polr(data$"13" ~ data$condition, data = data)
(ctable13 <- coef(summary(model13)))
p13 <- pnorm(abs(ctable13[, "t value"]), lower.tail = FALSE) * 2
(ctable13 <- cbind(ctable13, "p value" = p13))
(ci13 <- confint(model13))
exp(cbind(OR = coef(model13), ci13))

model14 = polr(data$"14" ~ data$condition, data = data)
(ctable14 <- coef(summary(model14)))
p14 <- pnorm(abs(ctable14[, "t value"]), lower.tail = FALSE) * 2
(ctable14 <- cbind(ctable14, "p value" = p14))
(ci14 <- confint(model14)) # I felt involved.
exp(cbind(OR = coef(model14), ci14))
#

model15 = polr(data$"15" ~ data$condition, data = data)
(ctable15 <- coef(summary(model15)))
p15 <- pnorm(abs(ctable15[, "t value"]), lower.tail = FALSE) * 2
(ctable15 <- cbind(ctable15, "p value" = p15))
(ci15 <- confint(model15))
exp(cbind(OR = coef(model15), ci15))

model16 = polr(data$"16" ~ data$condition, data = data)
(ctable16 <- coef(summary(model16)))
p16 <- pnorm(abs(ctable16[, "t value"]), lower.tail = FALSE) * 2
(ctable16 <- cbind(ctable16, "p value" = p16))
(ci16 <- confint(model16))
exp(cbind(OR = coef(model16), ci16))

model17 = polr(data$"17" ~ data$condition, data = data)
(ctable17 <- coef(summary(model17)))
p17 <- pnorm(abs(ctable17[, "t value"]), lower.tail = FALSE) * 2
(ctable17 <- cbind(ctable17, "p value" = p17))
(ci17 <- confint(model17)) # I forgot about my immediate surroundings while watching the visualizations/GIFs/videos.
exp(cbind(OR = coef(model17), ci17))

model18 = polr(data$"18" ~ data$condition, data = data)
(ctable18 <- coef(summary(model18)))
p18 <- pnorm(abs(ctable18[, "t value"]), lower.tail = FALSE) * 2
(ctable18 <- cbind(ctable18, "p value" = p18))
(ci18 <- confint(model18))
exp(cbind(OR = coef(model18), ci18))

model19 = polr(data$"19" ~ data$condition, data = data)
(ctable19 <- coef(summary(model19)))
p19 <- pnorm(abs(ctable19[, "t value"]), lower.tail = FALSE) * 2
(ctable19 <- cbind(ctable19, "p value" = p19))
(ci19 <- confint(model19))
exp(cbind(OR = coef(model19), ci19))

model20 = polr(data$"20" ~ data$condition, data = data)
(ctable20 <- coef(summary(model20)))
p20 <- pnorm(abs(ctable20[, "t value"]), lower.tail = FALSE) * 2
(ctable20 <- cbind(ctable20, "p value" = p20))
(ci20 <- confint(model20))
exp(cbind(OR = coef(model20), ci20))

model21 = polr(data$"21" ~ data$condition, data = data)
(ctable21 <- coef(summary(model21)))
p21 <- pnorm(abs(ctable21[, "t value"]), lower.tail = FALSE) * 2
(ctable21 <- cbind(ctable21, "p value" = p21))
(ci21 <- confint(model21))
exp(cbind(OR = coef(model21), ci21))

model22 = polr(data$"22" ~ data$condition, data = data)
(ctable22 <- coef(summary(model22)))
p22 <- pnorm(abs(ctable22[, "t value"]), lower.tail = FALSE) * 2
(ctable22 <- cbind(ctable22, "p value" = p22))
(ci22 <- confint(model22))
exp(cbind(OR = coef(model22), ci22))

model23 = polr(data$"23" ~ data$condition, data = data)
(ctable23 <- coef(summary(model23)))
p23 <- pnorm(abs(ctable23[, "t value"]), lower.tail = FALSE) * 2
(ctable23 <- cbind(ctable23, "p value" = p23))
(ci23 <- confint(model23))
exp(cbind(OR = coef(model23), ci23))

model24 = polr(data$"24" ~ data$condition, data = data)
(ctable24 <- coef(summary(model24)))
p24 <- pnorm(abs(ctable24[, "t value"]), lower.tail = FALSE) * 2
(ctable24 <- cbind(ctable24, "p value" = p24))
(ci24 <- confint(model24))
exp(cbind(OR = coef(model24), ci24))

model25 = polr(data$"25" ~ data$condition, data = data)
(ctable25 <- coef(summary(model25)))
p25 <- pnorm(abs(ctable25[, "t value"]), lower.tail = FALSE) * 2
(ctable25 <- cbind(ctable25, "p value" = p25))
(ci25 <- confint(model25)) # I would want to watch the visualizations/GIFs/videos if I saw them somewhere else and was not required or encouraged to watch them.
exp(cbind(OR = coef(model25), ci25))

model26 = polr(data$"26" ~ data$condition, data = data)
(ctable26 <- coef(summary(model26)))
p26 <- pnorm(abs(ctable26[, "t value"]), lower.tail = FALSE) * 2
(ctable26 <- cbind(ctable26, "p value" = p26))
(ci26 <- confint(model26)) # The visualizations/GIFs/videos were aesthetically appealing.
exp(cbind(OR = coef(model26), ci26))

model27 = polr(data$"27" ~ data$condition, data = data)
(ctable27 <- coef(summary(model27)))
p27 <- pnorm(abs(ctable27[, "t value"]), lower.tail = FALSE) * 2
(ctable27 <- cbind(ctable27, "p value" = p27))
(ci27 <- confint(model27)) # The visualizations/GIFs/videos were visually pleasing.
exp(cbind(OR = coef(model27), ci27))

model28 = polr(data$"28" ~ data$condition, data = data)
(ctable28 <- coef(summary(model28)))
p28 <- pnorm(abs(ctable28[, "t value"]), lower.tail = FALSE) * 2
(ctable28 <- cbind(ctable28, "p value" = p28))
(ci28 <- confint(model28))
exp(cbind(OR = coef(model28), ci))

