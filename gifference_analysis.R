### Statistical analysis of the data generated from the XYZ test for the 'Show Me The GIFference' work ###

# Loading the libraries
library("readxl")
library("ggplot2")
library("ggpubr")
library("tidyverse")
library("rstatix")
library("Kendall")

# Loading the data
data = read_excel('gifference_data.xlsx')
data[data  == 'NA'] = NA # missing data
data[!complete.cases(data),]
data = data %>% 
  group_by(scenario) %>% 
  mutate(time_s = ifelse(is.na(time_s), 
                         median(time_s, na.rm = TRUE), time_s)) %>% as.data.frame()

# Preparing the data
str(data)
data$participant_id = as.factor(data$participant_id)
data$condition = as.factor(data$condition)
data$scenario = as.factor(data$scenario)
data$vl_level = as.factor(data$vl_level)
data$interest_level = as.factor(data$interest_level)
data$application = as.factor(data$application)
data$time_s = as.numeric(data$time_s)
data$x_score = as.numeric(data$x_score)
data$y_score = as.numeric(data$y_score)
data$z_score = as.numeric(data$z_score)
data$total_score = as.numeric(data$total_score)
str(data)


# Descriptive statistics and related plots - Average score
c = data %>%
  group_by(condition) %>%
  get_summary_stats(total_score, type = "mean_sd")

c_plot = ggplot(c, aes(x=condition, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c, aes(x=condition, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Educational tool", y = "Average score") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       legend.position="none")

c_plot$labels$colour = "Educational tool"
c_plot

vl = data %>%
  group_by(vl_level) %>%
  get_summary_stats(total_score, type = "mean_sd")

vl_plot = ggplot(vl, aes(x=vl_level, y=mean)) +
  geom_point(aes(color = c("Low", "Medium", "High"))) +
  geom_crossbar(data=vl, aes(x=vl_level, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Low", "Medium", "High")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) + 
  labs(x = "VL level", y = "Average score") + 
  scale_color_discrete(breaks=c("Low", "Medium", "High")) +
  theme_bw() +   theme(axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       legend.position="none")

vl_plot$labels$colour = "VL level"
vl_plot

s = data %>%
  group_by(scenario) %>%
  get_summary_stats(total_score, type = "mean_sd")

s_plot = ggplot(s, aes(x=scenario, y=mean)) +
  geom_point(aes(color = scenario)) +
  geom_crossbar(data=s, aes(x=scenario, ymin=mean-sd/2, ymax=mean+sd/2, color = scenario), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Scenario", y = "Average score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.position="none") +
  scale_x_discrete(limits=c("6", "2", "1", "4", "7", "3", "5", "8")) +
  scale_colour_discrete(breaks=c("6", "2", "1", "4", "7", "3", "5", "8"), labels = c("1)", "2)", "3)", "4)", "5)", "6)", "7)", "8)"))

s_plot$labels$colour = "Scenario"
s_plot

p = data %>%
  group_by(participant_id) %>%
  get_summary_stats(total_score, type = "mean_sd") %>%
  as.data.frame()

p$vl_level = as.character(rep(c(1, 2, 3), each = 16))
p$index = 1:nrow(p)


p_plot = ggplot(p, aes(x=participant_id, y=mean)) +
  geom_point(aes(color = vl_level)) +
  geom_crossbar(data=p, aes(x=participant_id, ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1) + 
  labs(x = "Participants", y = "Average score") + 
  scale_x_discrete(breaks = seq(0, 48, by = 16)) + theme_bw()

p_plot$labels$colour = "VL level"
p_plot


# Boxplot
ggboxplot(
  data, x = "participant_id", y = "total_score"
) + theme_bw()



# Descriptive statistics and related plots - Consumption time
c_time = data %>%
  group_by(condition) %>%
  get_summary_stats(time_s, type = "mean_sd")

c_time_plot = ggplot(c_time, aes(x=condition, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_time, aes(x=condition, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 100) +
  labs(x = "Educational tool", y = "Time in seconds") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.text=element_text(size=25),
                       legend.title=element_text(size=30),
                       legend.key.height = unit(1.5, "cm"),
                       legend.key.width = unit(1.5, "cm"),
                       legend.position="bottom") + 
  guides(color=guide_legend(nrow=2, byrow=TRUE))

c_time_plot$labels$colour = "Educational tool"
c_time_plot

vl_time = data %>%
  group_by(vl_level) %>%
  get_summary_stats(time_s, type = "mean_sd")

vl_time_plot = ggplot(vl_time, aes(x=vl_level, y=mean)) +
  geom_point(aes(color = c("Low", "Medium", "High"))) +
  geom_crossbar(data=vl_time, aes(x=vl_level, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Low", "Medium", "High")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 100) + 
  labs(x = "VL level", y = "Time in seconds") + 
  scale_color_discrete(breaks=c("Low", "Medium", "High")) +
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.text=element_text(size=25),
                       legend.title=element_text(size=30),
                       legend.key.height = unit(1.5, "cm"),
                       legend.key.width = unit(1.5, "cm"),
                       legend.position="bottom")

vl_time_plot$labels$colour = "VL level"
vl_time_plot

s_time = data %>%
  group_by(scenario) %>%
  get_summary_stats(time_s, type = "mean_sd")

s_time_plot = ggplot(s_time, aes(x=scenario, y=mean)) +
  geom_point(aes(color = scenario)) +
  geom_crossbar(data=s_time, aes(x=scenario, ymin=mean-sd/2, ymax=mean+sd/2, color = scenario), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 100) +
  labs(x = "Scenario", y = "Time in seconds") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.text=element_text(size=25),
                     legend.title=element_text(size=30),
                     legend.key.height = unit(1.5, "cm"),
                     legend.key.width = unit(1.5, "cm"),
                     legend.position="bottom") +
  scale_x_discrete(limits=c("6", "2", "1", "4", "7", "3", "5", "8")) +
  scale_colour_discrete(breaks=c("6", "2", "1", "4", "7", "3", "5", "8"), labels = c("1)", "2)", "3)", "4)", "5)", "6)", "7)", "8)"))

s_time_plot$labels$colour = "Scenario"
s_time_plot

p_time = data %>%
  group_by(participant_id) %>%
  get_summary_stats(time_s, type = "mean_sd") %>%
  as.data.frame()

p_time$vl_level = as.character(rep(c(1, 2, 3), each = 16))
p_time$index = 1:nrow(p_time)


p_time_plot = ggplot(p_time, aes(x=participant_id, y=mean)) +
  geom_point(aes(color = vl_level)) +
  geom_crossbar(data=p_time, aes(x=participant_id, ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 100) + 
  labs(x = "Participants", y = "Time in seconds") + 
  scale_x_discrete(breaks = seq(0, 48, by = 16)) + theme_bw()

p_time_plot$labels$colour = "VL level"
p_time_plot


# Boxplot
ggboxplot(
  data, x = "participant_id", y = "time_s"
) + theme_bw()



# Descriptive statistics and related plots - x_score
c_x = data %>%
  group_by(condition) %>%
  get_summary_stats(x_score, type = "mean_sd")

c_x_plot = ggplot(c_x, aes(x=condition, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_x, aes(x=condition, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Educational tool", y = "X score") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.position="none")

c_x_plot$labels$colour = "Educational tool"
c_x_plot

vl_x = data %>%
  group_by(vl_level) %>%
  get_summary_stats(x_score, type = "mean_sd")

vl_x_plot = ggplot(vl_x, aes(x=vl_level, y=mean)) +
  geom_point(aes(color = c("Low", "Medium", "High"))) +
  geom_crossbar(data=vl_x, aes(x=vl_level, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Low", "Medium", "High")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) + 
  labs(x = "VL level", y = "X score") + 
  scale_color_discrete(breaks=c("Low", "Medium", "High")) +
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.position="none")

vl_x_plot$labels$colour = "VL level"
vl_x_plot

s_x = data %>%
  group_by(scenario) %>%
  get_summary_stats(x_score, type = "mean_sd")

s_x_plot = ggplot(s_x, aes(x=scenario, y=mean)) +
  geom_point(aes(color = scenario)) +
  geom_crossbar(data=s_x, aes(x=scenario, ymin=mean-sd/2, ymax=mean+sd/2, color = scenario), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Scenario", y = "X score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none") +
  scale_x_discrete(limits=c("6", "2", "1", "4", "7", "3", "5", "8")) +
  scale_colour_discrete(breaks=c("6", "2", "1", "4", "7", "3", "5", "8"), labels = c("1)", "2)", "3)", "4)", "5)", "6)", "7)", "8)"))


s_x_plot$labels$colour = "Scenario"
s_x_plot

p_x = data %>%
  group_by(participant_id) %>%
  get_summary_stats(x_score, type = "mean_sd") %>%
  as.data.frame()

p_x$vl_level <- as.character(rep(c(1, 2, 3), each = 16))
p_x$index <- 1:nrow(p_x)


p_x_plot = ggplot(p_x, aes(x=participant_id, y=mean)) +
  geom_point(aes(color = vl_level)) +
  geom_crossbar(data=p_x, aes(x=participant_id, ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) + 
  labs(x = "Participants", y = "X score") + 
  scale_x_discrete(breaks = seq(0, 48, by = 16)) + theme_bw()

p_x_plot$labels$colour = "VL level"
p_x_plot


# Boxplot
ggboxplot(
  data, x = "participant_id", y = "x_score"
) + theme_bw()





# Descriptive statistics and related plots - y_score
c_y = data %>%
  group_by(condition) %>%
  get_summary_stats(y_score, type = "mean_sd")

c_y_plot = ggplot(c_y, aes(x=condition, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_y, aes(x=condition, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Educational tool", y = "Y score") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.position="none")

c_y_plot$labels$colour = "Educational tool"
c_y_plot

vl_y = data %>%
  group_by(vl_level) %>%
  get_summary_stats(y_score, type = "mean_sd")

vl_y_plot = ggplot(vl_y, aes(x=vl_level, y=mean)) +
  geom_point(aes(color = c("Low", "Medium", "High"))) +
  geom_crossbar(data=vl_y, aes(x=vl_level, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Low", "Medium", "High")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) + 
  labs(x = "VL level", y = "Y score") + 
  scale_color_discrete(breaks=c("Low", "Medium", "High")) +
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.position="none")

vl_y_plot$labels$colour = "VL level"
vl_y_plot

s_y = data %>%
  group_by(scenario) %>%
  get_summary_stats(y_score, type = "mean_sd")

s_y_plot = ggplot(s_y, aes(x=scenario, y=mean)) +
  geom_point(aes(color = scenario)) +
  geom_crossbar(data=s_y, aes(x=scenario, ymin=mean-sd/2, ymax=mean+sd/2, color = scenario), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Scenario", y = "Y score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none") +
  scale_x_discrete(limits=c("6", "2", "1", "4", "7", "3", "5", "8")) +
  scale_colour_discrete(breaks=c("6", "2", "1", "4", "7", "3", "5", "8"), labels = c("1)", "2)", "3)", "4)", "5)", "6)", "7)", "8)"))


s_y_plot$labels$colour = "Scenario"
s_y_plot

p_y = data %>%
  group_by(participant_id) %>%
  get_summary_stats(y_score, type = "mean_sd") %>%
  as.data.frame()

p_y$vl_level = as.character(rep(c(1, 2, 3), each = 16))
p_y$index = 1:nrow(p_y)


p_y_plot = ggplot(p_y, aes(x=participant_id, y=mean)) +
  geom_point(aes(color = vl_level)) +
  geom_crossbar(data=p_y, aes(x=participant_id, ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) + 
  labs(x = "Participants", y = "Y score") + 
  scale_x_discrete(breaks = seq(0, 48, by = 16)) + theme_bw()

p_y_plot$labels$colour = "VL level"
p_y_plot


# Boxplot
ggboxplot(
  data, x = "participant_id", y = "y_score"
) + theme_bw()



# Descriptive statistics and related plots - z_score
c_z = data %>%
  group_by(condition) %>%
  get_summary_stats(z_score, type = "mean_sd")

c_z_plot = ggplot(c_z, aes(x=condition, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_z, aes(x=condition, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Educational tool", y = "Z score") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.text=element_text(size=25),
                       legend.title=element_text(size=30),
                       legend.key.height = unit(1.5, "cm"),
                       legend.key.width = unit(1.5, "cm"),
                       legend.position="bottom")

c_z_plot$labels$colour = "Educational tool"
c_z_plot

vl_z = data %>%
  group_by(vl_level) %>%
  get_summary_stats(z_score, type = "mean_sd")

vl_z_plot = ggplot(vl_z, aes(x=vl_level, y=mean)) +
  geom_point(aes(color = c("Low", "Medium", "High"))) +
  geom_crossbar(data=vl_z, aes(x=vl_level, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Low", "Medium", "High")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) + 
  labs(x = "VL level", y = "Z score") + 
  scale_color_discrete(breaks=c("Low", "Medium", "High")) +
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       legend.text=element_text(size=25),
                       legend.title=element_text(size=30),
                       legend.key.height = unit(1.5, "cm"),
                       legend.key.width = unit(1.5, "cm"),
                       legend.position="bottom")

vl_z_plot$labels$colour = "VL level"
vl_z_plot

s_z = data %>%
  group_by(scenario) %>%
  get_summary_stats(z_score, type = "mean_sd")

s_z_plot = ggplot(s_z, aes(x=scenario, y=mean)) +
  geom_point(aes(color = scenario)) +
  geom_crossbar(data=s_z, aes(x=scenario, ymin=mean-sd/2, ymax=mean+sd/2, color = scenario), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Scenario", y = "Z score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.text=element_text(size=25),
                     legend.title=element_text(size=30),
                     legend.key.height = unit(1.5, "cm"),
                     legend.key.width = unit(1.5, "cm"),
                     legend.position="bottom") +
  scale_x_discrete(limits=c("6", "2", "1", "4", "7", "3", "5", "8")) +
  scale_colour_discrete(breaks=c("6", "2", "1", "4", "7", "3", "5", "8"), labels = c("1)", "2)", "3)", "4)", "5)", "6)", "7)", "8)"))


s_z_plot$labels$colour = "Scenario"
s_z_plot

p_z = data %>%
  group_by(participant_id) %>%
  get_summary_stats(z_score, type = "mean_sd") %>%
  as.data.frame()

p_z$vl_level = as.character(rep(c(1, 2, 3), each = 16))
p_z$index = 1:nrow(p_z)


p_z_plot = ggplot(p_z, aes(x=participant_id, y=mean)) +
  geom_point(aes(color = vl_level)) +
  geom_crossbar(data=p_z, aes(x=participant_id, ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) + 
  labs(x = "Participants", y = "Z score") + 
  scale_x_discrete(breaks = seq(0, 48, by = 16)) + theme_bw()

p_z_plot$labels$colour = "VL level"
p_z_plot


# Boxplot
ggboxplot(
  data, x = "participant_id", y = "z_score"
) + theme_bw()







### First investigation ###
# Three-way Mixed ANOVA 
# One dependent variable: total_score
# Two between-subjects factors: condition, vl_level
# One within-subjects factor: scenario


# Descriptive statistics and exploratory analysis
c_vl1 = data %>%
  group_by(condition, vl_level, scenario) %>%
  get_summary_stats(total_score, type = "mean_sd") %>%
  as.data.frame()

c_vl1$condition = as.character(rep(c(1, 2, 3, 4), each = 24))
c_vl1$index = 1:nrow(c_vl1)

c_vl1_plot = ggplot(c_vl1, aes(x=seq(1:nrow(c_vl1)), y=mean, group = condition)) +
  geom_point(aes(color = vl_level)) +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color = "black") +
  geom_crossbar(data=c_vl1, aes(x=seq(1:nrow(c_vl1)), ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1) +
  labs(x = "Group", y = "Average score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       legend.text=element_text(size=25),
                       legend.title=element_text(size=30),
                       legend.key.height = unit(1.5, "cm"),
                       legend.key.width = unit(1.5, "cm"))

c_vl1_plot$labels$colour = "VL level"
c_vl1_plot

ggboxplot(
  data, x = "scenario", y = "total_score",
  color = "vl_level", palette = "jco"
) + facet_wrap(condition ~ .) + theme_bw()


# Outliers detection
mod1 = lm(total_score ~ condition*scenario*vl_level, data=data)
cooksd1 = cooks.distance(mod1)
plot(cooksd1, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/384, col="red")  # add cutoff line
text(x=1:length(cooksd1)+5, y=cooksd1, labels=ifelse(cooksd1>4/384, names(cooksd1),""), col="red")  # add labels
influential1 = na.omit(as.numeric(names(cooksd1)[(cooksd1 > (4/384))]))

# Replacing the outliers with the median average score
data_no_outliers1 = data
data_no_outliers1[which(data$run_id %in% influential1), ]$total_score = median(data$total_score)

# Three-way mixed ANOVA test
aov1.1 <- anova_test(
  data = data, dv = total_score, wid = participant_id,
  between = c(condition, vl_level), within = scenario
)

get_anova_table(aov1.1)



# Three-way mixed ANOVA test (without outliers)
aov1.2 = anova_test(
  data = data_no_outliers1, dv = total_score, wid = participant_id,
  between = c(condition, vl_level), within = scenario
)

get_anova_table(aov1.2)


# Post-hoc tests (without outliers)
two_way1.1 = data_no_outliers1 %>% 
  group_by(scenario) %>%
  anova_test(dv = total_score, wid = participant_id, between = c(condition, vl_level)) %>% View()

two_way1.2 = data_no_outliers1 %>% 
  group_by(condition) %>%
  anova_test(dv = total_score, wid = participant_id, between = c(scenario, vl_level)) %>% View()

vl_level.effect = data_no_outliers1 %>%
  group_by(scenario, condition) %>%
  anova_test(dv = total_score, wid = participant_id, between = vl_level) %>% View()

pairwise.t.test(data_no_outliers1$total_score, data_no_outliers1$condition, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers1$total_score, data_no_outliers1$vl_level, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers1$total_score, data_no_outliers1$scenario, p.adjust.method="bonferroni")

# As the results are not similar, we replaced the outliers with group mean.


# Checking the normality assumption
ggqqplot(data_no_outliers1, "total_score", ggtheme = theme_bw()) +
  facet_grid(scenario ~ vl_level)

# Checking the assumption of the homogeneity of variance
# We have a balanced design and we do not worry about this

# Checking the assumption of the homogeneity of covariances
# We have a balanced design and we do not worry about this




### Second investigation ###
# Three-way Mixed ANOVA 
# One dependent variable: time_s
# Two between-subjects factors: condition, vl_level
# One within-subjects factor: scenario


# Descriptive statistics and exploratory analysis
c_vl2 = data %>%
  group_by(condition, vl_level, scenario) %>%
  get_summary_stats(time_s, type = "mean_sd") %>%
  as.data.frame()

c_vl2$condition = as.character(rep(c(1, 2, 3, 4), each = 24))
c_vl2$index = 1:nrow(c_vl2)

c_vl2_plot = ggplot(c_vl2, aes(x=seq(1:nrow(c_vl2)), y=mean, group = condition)) +
  geom_point(aes(color = vl_level)) +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color = "black") +
  geom_crossbar(data=c_vl2, aes(x=seq(1:nrow(c_vl2)), ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 150) +
 labs(x = "Group", y = "Average score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       legend.text=element_text(size=25),
                       legend.title=element_text(size=30),
                       legend.key.height = unit(1.5, "cm"),
                       legend.key.width = unit(1.5, "cm"))

c_vl2_plot$labels$colour = "VL level"
c_vl2_plot

ggboxplot(
  data, x = "scenario", y = "time_s",
  color = "vl_level", palette = "jco"
) + facet_wrap(condition ~ .) + theme_bw()


# Outliers detection
mod2 = lm(time_s ~ condition*scenario*vl_level, data=data)
cooksd2 = cooks.distance(mod2)
plot(cooksd2, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/384, col="red")  # add cutoff line
text(x=1:length(cooksd2)+5, y=cooksd2, labels=ifelse(cooksd2>4/384, names(cooksd2),""), col="red")  # add labels
influential2 = na.omit(as.numeric(names(cooksd2)[(cooksd2 > (4/384))]))

# Replacing the outliers with the median consumption time
data_no_outliers2 = data
data_no_outliers2[which(data$run_id %in% influential2), ]$time_s = median(data$time_s)


# Three-way mixed ANOVA test
aov2.1 = anova_test(
  data = data, dv = time_s, wid = participant_id,
  between = c(condition, vl_level), within = scenario
)

get_anova_table(aov2.1)


# Three-way mixed ANOVA test (without outliers)
aov2.2 = anova_test(
  data = data_no_outliers2, dv = time_s, wid = participant_id,
  within = scenario, between = c(condition, vl_level)
)

get_anova_table(aov2.2)


# Post-hoc tests (without outliers)
vl_level.effect = data_no_outliers2 %>%
  group_by(scenario, vl_level) %>%
  anova_test(dv = time_s, wid = participant_id, between = condition) %>% View()

pairwise.t.test(data_no_outliers2$time_s, data_no_outliers2$condition, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers2$time_s, data_no_outliers2$vl_level, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers2$time_s, data_no_outliers2$scenario, p.adjust.method="bonferroni")

# As the results are not similar, we remove the outliers from the data.


# Checking the normality assumption
ggqqplot(data_no_outliers2, "time_s", ggtheme = theme_bw()) +
  facet_grid(scenario ~ vl_level)

# Checking the assumption of the homogeneity of variance
# We have a balanced design and we do not worry about this

# Checking the assumption of the homogeneity of covariances
# We have a balanced design and we do not worry about this




### Third investigation ###
# Three-way Mixed MANOVA 
# Three dependent variables: x_score, y_score, z_score
# Two between-subjects factors: condition, vl_level
# One within-subjects factor: scenario

dep_var = cbind(data$x_score, data$y_score, data$z_score)
model = manova(dep_var ~ condition*scenario*vl_level, data = data)
summary(model)


# Checking for outliers
data_no_outliers_manova = data

influential_x = data %>%
  group_by(scenario, condition) %>%
  identify_outliers(x_score)

data_no_outliers_manova[which(data$run_id %in% influential_x$run_id), ]$x_score = median(data$x_score)

influential_y = data %>%
  group_by(scenario, condition) %>%
  identify_outliers(y_score)

data_no_outliers_manova[which(data$run_id %in% influential_y$run_id), ]$y_score = median(data$y_score)

influential_z = data %>%
  group_by(scenario, condition) %>%
  identify_outliers(z_score)

data_no_outliers_manova[which(data$run_id %in% influential_z$run_id), ]$z_score = median(data$z_score)

# Checking for multivariate outliers
influential_m = data %>%
  group_by(scenario) %>%
  mahalanobis_distance(-run_id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

data_no_outliers_manova[which(data$run_id %in% influential_m$run_id), ]$x_score = median(data$x_score)
data_no_outliers_manova[which(data$run_id %in% influential_m$run_id), ]$y_score = median(data$y_score)
data_no_outliers_manova[which(data$run_id %in% influential_m$run_id), ]$z_score = median(data$z_score)



# Redoing the test without outliers
dep_var = cbind(data_no_outliers_manova$x_score, data_no_outliers_manova$y_score, data_no_outliers_manova$z_score)
model = manova(dep_var ~ condition*scenario*vl_level, data = data_no_outliers_manova)
summary(model)

# As the results are not similar, we replace the outliers with the median value

# Checking the normality assumption for condition*vl_level
ggqqplot(data_no_outliers_manova, "x_score", ggtheme = theme_bw()) +
  facet_grid(scenario ~ condition*vl_level)

ggqqplot(data_no_outliers_manova, "y_score", ggtheme = theme_bw()) +
  facet_grid(scenario ~ condition*vl_level)

ggqqplot(data_no_outliers_manova, "z_score", ggtheme = theme_bw()) +
  facet_grid(scenario ~ condition*vl_level)


# Checking the assumption of the homogeneity of variance for condition*vl_level
# We have a balanced design and we do not worry about this

# Checking the assumption of the homogeneity of covariances
# We have a balanced design and we do not worry about this


# Investigating each dependent variable separately #
# x_score #
# Descriptive statistics and exploratory analysis
c_vl3 = data %>%
  group_by(condition, vl_level, scenario) %>%
  get_summary_stats(x_score, type = "mean_sd") %>%
  as.data.frame()

c_vl3$condition = as.character(rep(c(1, 2, 3, 4), each = 24))
c_vl3$index = 1:nrow(c_vl3)

ggplot(c_vl3, aes(x=seq(1:nrow(c_vl3)), y=mean, group = condition)) +
  geom_point(aes(color = vl_level)) +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color = "black") +
  geom_crossbar(data=c_vl3, aes(x=seq(1:nrow(c_vl3)), ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1) + theme_bw()

ggboxplot(
  data, x = "scenario", y = "x_score",
  color = "vl_level", palette = "jco"
) + facet_wrap(condition ~ .) + theme_bw()

# Outliers detection - Cook's distance
mod3 = lm(x_score ~ condition*scenario, data=data)
cooksd3 = cooks.distance(mod3)
plot(cooksd3, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/384, col="red")  # add cutoff line
text(x=1:length(cooksd3)+5, y=cooksd3, labels=ifelse(cooksd3>4/384, names(cooksd3),""), col="red")  # add labels
influential3 = na.omit(as.numeric(names(cooksd3)[(cooksd3 > (4/384))]))

# Creating a reduced sample without outliers
data_no_outliers3 = data
data_no_outliers3[which(data$run_id %in% influential3), ]$x_score = median(data$x_score)

# Three-way mixed ANOVA test
aov3.1 = anova_test(
  data = data, dv = x_score, wid = participant_id,
  between = c(condition, vl_level), within = scenario
)

get_anova_table(aov3.1)


# Three-way mixed ANOVA test (without outliers)
aov3.2 = anova_test(
  data = data_no_outliers3, dv = x_score, wid = participant_id,
  within = scenario, between = c(condition, vl_level)
)

get_anova_table(aov3.2)

# Post-hoc tests (without outliers)
pairwise.t.test(data_no_outliers3$x_score, data_no_outliers3$condition, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers3$x_score, data_no_outliers3$vl_level, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers3$x_score, data_no_outliers3$scenario, p.adjust.method="bonferroni")

# As the results are not similar, we replace the outliers with the median value.

# Checking the normality assumption
ggqqplot(data_no_outliers3, "x_score", ggtheme = theme_bw()) +
  facet_grid(scenario ~ vl_level)

# Checking the assumption of the homogeneity of variance
# We have a balanced design and we do not worry about this

# Checking the assumption of the homogeneity of covariances
# We have a balanced design and we do not worry about this




# y_score #
# Descriptive statistics and exploratory analysis
c_vl4 = data %>%
  group_by(condition, vl_level, scenario) %>%
  get_summary_stats(y_score, type = "mean_sd") %>%
  as.data.frame()

c_vl4$condition = as.character(rep(c(1, 2, 3, 4), each = 24))
c_vl4$index = 1:nrow(c_vl4)

ggplot(c_vl4, aes(x=seq(1:nrow(c_vl4)), y=mean, group = condition)) +
  geom_point(aes(color = vl_level)) +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color = "black") +
  geom_crossbar(data=c_vl4, aes(x=seq(1:nrow(c_vl4)), ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1) + theme_bw()


ggboxplot(
  data, x = "scenario", y = "y_score",
  color = "vl_level", palette = "jco"
) + facet_wrap(condition ~ .) + theme_bw()


# Outliers detection - Cook's distance
mod4 = lm(y_score ~ condition*scenario, data=data)
cooksd4 = cooks.distance(mod4)
plot(cooksd4, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/384, col="red")  # add cutoff line
text(x=1:length(cooksd4)+5, y=cooksd4, labels=ifelse(cooksd4>4/384, names(cooksd4),""), col="red")  # add labels
influential4 = na.omit(as.numeric(names(cooksd4)[(cooksd4 > (4/384))]))

# Creating a reduced sample without outliers
data_no_outliers4 = data
data_no_outliers4[which(data$run_id %in% influential4), ]$y_score = median(data$y_score)


# Three-way mixed ANOVA test
aov4.1 = anova_test(
  data = data, dv = y_score, wid = participant_id,
  between = c(condition, vl_level), within = scenario
)

get_anova_table(aov4.1)


# Three-way mixed ANOVA test (without outliers)
aov4.2 = anova_test(
  data = data_no_outliers4, dv = y_score, wid = participant_id,
  within = scenario, between = c(condition, vl_level)
)

get_anova_table(aov4.2)

# Post-hoc tests (without outliers)
two_way_y = data_no_outliers4 %>% 
  group_by(scenario) %>% #this differs - no scenario
  anova_test(dv = y_score, wid = participant_id, between = c(condition, vl_level)) %>% View()

pairwise.t.test(data_no_outliers4$y_score, data_no_outliers4$condition, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers4$y_score, data_no_outliers4$vl_level, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers4$y_score, data_no_outliers4$scenario, p.adjust.method="bonferroni")

# As the results are not similar, we replace the outliers with the median value.


# Checking the normality assumption
ggqqplot(data_no_outliers4, "y_score", ggtheme = theme_bw()) +
  facet_grid(scenario ~ vl_level)

# Checking the assumption of the homogeneity of variance
# We have a balanced design and we do not worry about this


# Checking the assumption of the homogeneity of covariances
# We have a balanced design and we do not worry about this



# z_score #
# Descriptive statistics and exploratory analysis
c_vl5 = data %>%
  group_by(condition, vl_level, scenario) %>%
  get_summary_stats(z_score, type = "mean_sd") %>%
  as.data.frame()

c_vl5$condition = as.character(rep(c(1, 2, 3, 4), each = 24))
c_vl5$index = 1:nrow(c_vl5)

ggplot(c_vl5, aes(x=seq(1:nrow(c_vl5)), y=mean, group = condition)) +
  geom_point(aes(color = vl_level)) +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color = "black") +
  geom_crossbar(data=c_vl5, aes(x=seq(1:nrow(c_vl5)), ymin=mean-sd/2, ymax=mean+sd/2, color = vl_level), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1) + theme_bw()

ggboxplot(
  data, x = "scenario", y = "z_score",
  color = "vl_level", palette = "jco"
) + facet_wrap(condition ~ .) + theme_bw()

# Outliers detection - Cook's distance
mod5 = lm(z_score ~ condition*scenario, data=data)
cooksd5 = cooks.distance(mod5)
plot(cooksd5, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/384, col="red")  # add cutoff line
text(x=1:length(cooksd5)+5, y=cooksd5, labels=ifelse(cooksd5>4/384, names(cooksd5),""), col="red")  # add labels
influential5 = na.omit(as.numeric(names(cooksd5)[(cooksd5 > (4/384))]))

# Creating a reduced sample without outliers
data_no_outliers5 = data
data_no_outliers5[which(data$run_id %in% influential5), ]$z_score = median(data$z_score)


# Three-way mixed ANOVA test
aov5.1 = anova_test(
  data = data, dv = z_score, wid = participant_id,
  between = c(condition, vl_level), within = scenario
)

get_anova_table(aov5.1)



# Three-way mixed ANOVA test (without outliers)
aov5.2 = anova_test(
  data = data_no_outliers5, dv = z_score, wid = participant_id,
  within = scenario, between = c(condition, vl_level)
)

get_anova_table(aov5.2)

# Post-hoc tests (without outliers)
two_way_z = data_no_outliers4 %>% 
  group_by(scenario) %>% #this differs - no scenario
  anova_test(dv = z_score, wid = participant_id, between = c(condition, vl_level)) %>% View()


pairwise.t.test(data_no_outliers5$z_score, data_no_outliers5$condition, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers5$z_score, data_no_outliers5$vl_level, p.adjust.method="bonferroni")
pairwise.t.test(data_no_outliers5$z_score, data_no_outliers5$scenario, p.adjust.method="bonferroni")

# As the results are not similar, we remove the outliers from the data.


# Checking the normality assumption
ggqqplot(data_no_outliers5, "z_score", ggtheme = theme_bw()) +
  facet_grid(scenario ~ vl_level)

# Checking the assumption of the homogeneity of variance
# We have a balanced design and we do not worry about this

# Checking the assumption of the homogeneity of covariances
# We have a balanced design and we do not worry about this



# Checking for linear trends in the data - Note: we used simulate.p.value = TRUE due to the small cell size
tab1.1 = table(data$total_score, data$vl_level)
chisq_test(data$total_score, data$vl_level, simulate.p.value = TRUE) 

tab1.2 = table(data$time_s, data$vl_level)
chisq_test(data$time_s, data$vl_level, simulate.p.value = TRUE) 

tab1.3 = table(data$x_score, data$vl_level)
chisq_test(data$x_score, data$vl_level, simulate.p.value = TRUE)

tab1.4 = table(data$y_score, data$vl_level)
chisq_test(data$y_score, data$vl_level, simulate.p.value = TRUE)

tab1.5 = table(data$z_score, data$vl_level)
chisq_test(data$z_score, data$vl_level, simulate.p.value = TRUE)
# performance scores and vl level are associated


tab2.1 = table(data$total_score, data$scenario)
chisq_test(data$total_score, data$scenario, simulate.p.value = TRUE)

tab2.2 = table(data$time_s, data$scenario)
chisq_test(data$time_s, data$scenario, simulate.p.value = TRUE)

tab2.3 = table(data$x_score, data$scenario)
chisq_test(data$x_score, data$scenario, simulate.p.value = TRUE)

tab2.4 = table(data$y_score, data$scenario)
chisq_test(data$y_score, data$scenario, simulate.p.value = TRUE)

tab2.5 = table(data$z_score, data$scenario)
chisq_test(data$z_score, data$scenario, simulate.p.value = TRUE)
# consumption time and performance scores are associated with scenario




