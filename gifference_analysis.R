### Statistical analysis of the data generated from the XYZ test for the 'Show Me the GIFference! Using data-GIFs as Educational Tools' work ###

# Loading the libraries
library("readxl")
library("ggplot2")
library("tidyverse")
library("rstatix")
library("ggpubr")
library("effectsize")
library("car")
library("brant")
library("Hmisc")
library("MASS")

# Loading the data
data = read_excel('/Users/lorenzoamabili/Library/Mobile Documents/com~apple~CloudDocs/GIFferences/gifference_data.xlsx')
data[data  == 'NA'] = NA # missing data
data[!complete.cases(data),]
data = data %>% 
  group_by(scenario) %>% 
  mutate(time_s = ifelse(is.na(time_s), 
                         median(time_s, na.rm = TRUE), time_s)) %>% as.data.frame()
data = data %>% 
  group_by(scenario) %>% 
  mutate(loops = ifelse(is.na(loops), 
                         median(loops, na.rm = TRUE), loops)) %>% as.data.frame()

# Preparing the data
str(data)
data$participant_id = as.factor(data$participant_id)
data$variant = as.factor(data$variant)
data$scenario = as.factor(data$scenario)
data$vl_level = as.factor(data$vl_level)
data$gender = as.factor(data$gender)
data$cb = as.factor(data$cb)
data$time_s = as.numeric(data$time_s)
data$loops = as.numeric(data$loops)
data$x_score = as.numeric(data$x_score)
data$y_score = as.numeric(data$y_score)
data$z_score = as.numeric(data$z_score)
data$average_score = as.numeric(data$average_score)
str(data)

# Creating a data set without the observations related to the static visualizations where 
# the ratio consumption time/GIF duration cannot be calculated
data_no_static_viz = data[which(data$variant != 1), ]

# Descriptive statistics and related plots - Average score
c = data %>%
  group_by(variant) %>%
  get_summary_stats(average_score, type = "mean_sd")

c_plot = ggplot(c, aes(x=variant, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c, aes(x=variant, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Variant", y = "Average score") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     legend.position="none")
c_plot$labels$colour = "Variant"
c_plot

vl = data %>%
  group_by(vl_level) %>%
  get_summary_stats(average_score, type = "mean_sd")

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
  get_summary_stats(average_score, type = "mean_sd")

s_plot = ggplot(s, aes(x=scenario, y=mean)) +
  geom_point(aes(color = scenario)) +
  geom_crossbar(data=s, aes(x=scenario, ymin=mean-sd/2, ymax=mean+sd/2, color = scenario), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Scenario", y = "Average score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none")

s_plot$labels$colour = "Scenario"
s_plot


g = data %>%
  group_by(gender) %>%
  get_summary_stats(average_score, type = "mean_sd")

g_plot = ggplot(g, aes(x=gender, y=mean)) +
  geom_point(aes(color = c("Male", "Female"))) +
  geom_crossbar(data=g, aes(x=gender, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Male", "Female")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Gender", y = "Average score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none")

g_plot$labels$colour = "Gender"
g_plot


# Descriptive statistics and related plots - Consumption time
c_time = data %>%
  group_by(variant) %>%
  get_summary_stats(time_s, type = "mean_sd")

c_time_plot = ggplot(c_time, aes(x=variant, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_time, aes(x=variant, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 100) +
  labs(x = "Variant", y = "Time in seconds") + 
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

c_time_plot$labels$colour = "Variant"
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
  scale_colour_discrete(labels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"))

s_time_plot$labels$colour = "Scenario"
s_time_plot


g_time = data %>%
  group_by(gender) %>%
  get_summary_stats(time_s, type = "mean_sd")

g_time_plot = ggplot(g_time, aes(x=gender, y=mean)) +
  geom_point(aes(color = c("Male", "Female"))) +
  geom_crossbar(data=g_time, aes(x=gender, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Male", "Female")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 100) +
  labs(x = "Gender", y = "Time in seconds") + 
  scale_color_discrete(breaks=c("Male", "Female")) +
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.text=element_text(size=25),
                     legend.title=element_text(size=30),
                     legend.key.height = unit(1.5, "cm"),
                     legend.key.width = unit(1.5, "cm"),
                     legend.position="bottom")

g_time_plot$labels$colour = "Gender"
g_time_plot



# Descriptive statistics and related plots - x_score
c_x = data %>%
  group_by(variant) %>%
  get_summary_stats(x_score, type = "mean_sd")

c_x_plot = ggplot(c_x, aes(x=variant, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_x, aes(x=variant, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Variant", y = "X score") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none")

c_x_plot$labels$colour = "Variant"
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
                     legend.position="none")

s_x_plot$labels$colour = "Scenario"
s_x_plot


g_x = data %>%
  group_by(gender) %>%
  get_summary_stats(x_score, type = "mean_sd")

g_x_plot = ggplot(g_x, aes(x=gender, y=mean)) +
  geom_point(aes(color = c("Male", "Female"))) +
  geom_crossbar(data=g_x, aes(x=gender, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Male", "Female")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Gender", y = "X score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none")

g_x_plot$labels$colour = "Gender"
g_x_plot



# Descriptive statistics and related plots - y_score
c_y = data %>%
  group_by(variant) %>%
  get_summary_stats(y_score, type = "mean_sd")

c_y_plot = ggplot(c_y, aes(x=variant, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_y, aes(x=variant, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Variant", y = "Y score") + 
  scale_color_discrete(breaks=c("Static Viz", "GIF Inter", "GIF ST", "Data-video")) + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none")

c_y_plot$labels$colour = "Variant"
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
                     legend.position="none")

s_y_plot$labels$colour = "Scenario"
s_y_plot


g_y = data %>%
  group_by(gender) %>%
  get_summary_stats(y_score, type = "mean_sd")

g_y_plot = ggplot(g_y, aes(x=gender, y=mean)) +
  geom_point(aes(color = c("Male", "Female"))) +
  geom_crossbar(data=g_y, aes(x=gender, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Male", "Female")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Gender", y = "Y score") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none")

g_y_plot$labels$colour = "Gender"
g_y_plot



# Descriptive statistics and related plots - z_score
c_z = data %>%
  group_by(variant) %>%
  get_summary_stats(z_score, type = "mean_sd")

c_z_plot = ggplot(c_z, aes(x=variant, y=mean)) +
  geom_point(aes(color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video"))) +
  geom_crossbar(data=c_z, aes(x=variant, ymin=mean-sd/2, ymax=mean+sd/2 , color = c("Static Viz", "GIF Inter", "GIF ST", "Data-video")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Variant", y = "Z score") + 
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

c_z_plot$labels$colour = "Variant"
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
  scale_colour_discrete(labels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"))


s_z_plot$labels$colour = "Scenario"
s_z_plot


g_z = data %>%
  group_by(gender) %>%
  get_summary_stats(z_score, type = "mean_sd")

g_z_plot = ggplot(g_z, aes(x=gender, y=mean)) +
  geom_point(aes(color = c("Male", "Female"))) +
  geom_crossbar(data=g_z, aes(x=gender, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Male", "Female")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 1.05) +
  labs(x = "Gender", y = "Z score") + 
  scale_color_discrete(breaks=c("Male", "Female")) +
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.text=element_text(size=25),
                     legend.title=element_text(size=30),
                     legend.key.height = unit(1.5, "cm"),
                     legend.key.width = unit(1.5, "cm"),
                     legend.position="bottom")

g_z_plot$labels$colour = "Gender"
g_z_plot



# Descriptive statistics and related plots - Consumption time/GIF duration
c_td = data_no_static_viz %>%
  group_by(variant) %>%
  get_summary_stats(loops, type = "mean_sd")

c_td_plot = ggplot(c_td, aes(x=variant, y=mean)) +
  geom_point(aes(colour = variant)) +
  geom_crossbar(data=c_td, aes(x=variant, colour = variant, ymin=mean-sd/2, ymax=mean+sd/2), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 3) +
  labs(x = "Variant", y = "Time/Duration") + 
  theme_bw() + theme(axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     legend.position="none") + 
  scale_color_manual(breaks = c("2", "3", "4"), values=c("#bdd780", "#80dfe1", "#f69891"))

c_td_plot$labels$colour = "Variant"
c_td_plot

vl_td = data_no_static_viz %>%
  group_by(vl_level) %>%
  get_summary_stats(loops, type = "mean_sd")

vl_td_plot = ggplot(vl_td, aes(x=vl_level, y=mean)) +
  geom_point(aes(color = c("Low", "Medium", "High"))) +
  geom_crossbar(data=vl_td, aes(x=vl_level, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Low", "Medium", "High")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 3) + 
  labs(x = "VL level", y = "Time/Duration") + 
  scale_color_discrete(breaks=c("Low", "Medium", "High")) +
  theme_bw() + theme(axis.ticks.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.text=element_text(size=25),
                       axis.title=element_text(size=30),
                       legend.position="none")

vl_td_plot$labels$colour = "VL level"
vl_td_plot

s_td = data_no_static_viz %>%
  group_by(scenario) %>%
  get_summary_stats(loops, type = "mean_sd")

s_td_plot = ggplot(s_td, aes(x=scenario, y=mean)) +
  geom_point(aes(color = scenario)) +
  geom_crossbar(data=s_td, aes(x=scenario, ymin=mean-sd/2, ymax=mean+sd/2, color = scenario), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 3) +
  labs(x = "Scenario", y = "Time/Duration") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none") + 
  scale_colour_discrete(labels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"))

s_td_plot$labels$colour = "Scenario"
s_td_plot


g_td = data_no_static_viz %>%
  group_by(gender) %>%
  get_summary_stats(loops, type = "mean_sd")

g_td_plot = ggplot(g_td, aes(x=gender, y=mean)) +
  geom_point(aes(color = c("Male", "Female"))) +
  geom_crossbar(data=g_td, aes(x=gender, ymin=mean-sd/2, ymax=mean+sd/2, color = c("Male", "Female")), 
                width=.2, position=position_dodge(0.05)) + ylim(0, 3) +
  labs(x = "Gender", y = "Time/Duration") + 
  theme_bw() + theme(axis.text=element_text(size=25),
                     axis.title=element_text(size=30),
                     axis.ticks.x=element_blank(),
                     axis.text.x=element_blank(),
                     legend.position="none")

g_td_plot$labels$colour = "Gender"
g_td_plot




# Converting the ordinal variables from numeric to factors.
# We used them as numeric for creating the plots.
data$x_score = as.factor(data$x_score)
data$y_score = as.factor(data$y_score)
data$z_score = as.factor(data$z_score)
data$average_score = as.factor(data$average_score)

# Setting the GIF with smooth transition as the reference variant and the medium VL_level as the reference vl_level
data = within(data, variant <- relevel(variant, ref = 3))
data = within(data, vl_level <- relevel(vl_level, ref = 2))


### First investigation ###
# Ordinal Logistic Regression
# One ordinal dependent variable: average_score
# Four independent variables: variant, vl_level, gender, scenario
# Two-way interaction: variant, vl_level
model1 = polr(average_score ~ scenario+gender+variant*vl_level, data = data)
(ctable1 <- coef(summary(model1)))
p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
(ctable1 <- cbind(ctable1, "p value" = p1))
(ci1 <- confint(model1))
exp(cbind(OR = coef(model1), ci1))

# Checking for the multicollinearity
vif(model1)
# All VIFs are low which means that there is no multicollinearity.

# Checking the proportional odds assumption
brant(model1)
# The proportional odds assumption holds at significance level alpha = 0.01.

# Results
# For viewers exposed to scenario A2, the odds of obtaining a higher average score are 2.60 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A4, the odds of obtaining a higher average score are 19.16 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A6, the odds of obtaining a higher average score are 3.54 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A7, the odds of obtaining a higher average score are 7.52 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A8, the odds of obtaining a higher average score are 4.75 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# 1/0.4017378 = 2.49
# For viewers watching GIFs with smooth transitions, the odds of obtaining a higher average score are 2.49 times that
# of viewers watching static visualizations, holding constant all other variables.

# Whatever are the odds of obtaining a higher average score for viewers with a medium level of visualization literacy watching static visualizations and 
# for viewers with a medium level of visualization literacy but watching GIFs with smooth transitions, 
# viewers with a high level of visualization literacy are 7.09 times more likely to obtaining it.

# Whatever are the odds of obtaining a higher average score for viewers with a medium level of visualization literacy watching GIFs with interchangeability and 
# for viewers with a medium level of visualization literacy but watching GIFs with smooth transitions, 
# viewers with a high level of visualization literacy are 4.42 times more likely to obtaining it.

# Whatever are the odds of obtaining a higher average score for viewers with a medium level of visualization literacy watching data-videos and 
# for viewers with a medium level of visualization literacy but watching GIFs with smooth transitions, 
# viewers with a high level of visualization literacy are 4.77 times more likely to obtaining it.


# Post-hoc power analysis
table(data$average_score)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 2.6041538, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.7636005, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 19.1594714, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 1.7566023, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 3.5443858, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 7.5199098, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 4.7543856, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 1.1744963, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.4017378, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.5830543, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.9314050, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.6329000, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.5920492, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.7996940, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 1.3173958, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 0.3216571, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 7.0890309, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 4.4199595, 384, alpha=0.05)
popower(c(1/384,  5/384, 5/384, 6/384,  17/384, 18/384, 21/384, 42/384, 50/384, 40/384, 179/384), 4.7707272, 384, alpha=0.05)

# Anova for testing H3 and H4
data$average_score = as.numeric(data$average_score)
pairwise_t_test(average_score ~ variant, data = data, p.adjust.method = "bonferroni")


### Second investigation ###
# Four-way Mixed ANOVA 
# One dependent variable: time_s
# Three between-subjects factors: variant, vl_level, gender
# One within-subjects factor: scenario

# Outliers detection
mod2 = lm(time_s ~ gender*variant*scenario*vl_level, data=data)
cooksd2 = cooks.distance(mod2)
plot(cooksd2, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/384, col="red")  # add cutoff line
text(x=1:length(cooksd2)+5, y=cooksd2, labels=ifelse(cooksd2>4/384, names(cooksd2),""), col="red")  # add labels
influential2 = na.omit(as.numeric(names(cooksd2)[(cooksd2 > (4/384))]))

# Replacing the outliers with the median consumption time
data_no_outliers2 = data
data_no_outliers2[which(data$run_id %in% influential2), ]$time_s = median(data$time_s)

# Four-way mixed ANOVA test
aov2.1 = aov(time_s ~ gender*variant*vl_level*scenario + Error(participant_id/scenario), data = data)
summary(aov2.1)

# Four-way mixed ANOVA test (after imputation)
aov2.2 = aov(time_s ~ gender*variant*vl_level*scenario + Error(participant_id/scenario), data = data_no_outliers2)
summary(aov2.2)

# As the results are not similar, we replace the outliers with the median.

# Checking the normality assumption
data_no_outliers2 %>%
  group_by(gender, variant, vl_level) %>%
  shapiro_test(time_s)

ggplot(data_no_outliers2, aes(x=time_s)) + geom_density()
# The normality assumption is violated

# Box-cox transformation
bc <- boxcox(time_s ~ gender*variant*scenario*vl_level, data=data_no_outliers2)
(lambda <- bc$x[which.max(bc$y)])

# Checking the normality assumption (after transformation)
data_no_outliers2$time_s = (data_no_outliers2$time_s^lambda-1)/lambda
data_no_outliers2 %>%
  group_by(gender, variant, vl_level) %>%
  shapiro_test(time_s)

ggplot(data_no_outliers2, aes(x=time_s)) + geom_density()
# The situation has improved and we can now consider the response variable as normally distributed

# Checking the assumption of the homogeneity of variance
data_no_outliers2 %>%
  group_by(scenario) %>%
  levene_test(time_s ~ gender*variant*vl_level)
# The assumption of the homogeneity of variance holds

# We check the homogeneity of sphericity in the anova output
anova2.2 = anova_test(dv = time_s, data = data_no_outliers2, wid = participant_id,
                      within = scenario, between = c(variant, vl_level))
anova2.2
# From the results, we can assume the sphericity of the covariance matrix


# Four-way mixed ANOVA test (after imputation and data transformation)
aov2.2 = aov(time_s ~ gender*variant*vl_level*scenario + Error(participant_id/scenario), data = data_no_outliers2)
summary(aov2.2)


# Post-hoc tests
# based on the significant four-way interaction effect gender:variant:vl_level:scenario
data_no_outliers2 %>% 
  group_by(scenario) %>%
  anova_test(dv = time_s, wid = participant_id, between = c(gender, variant, vl_level), type = 2) %>% View()
# scenario A2 and A6

data_no_outliers2 %>% 
  group_by(scenario, gender) %>%
  anova_test(dv = time_s, wid = participant_id, between = c(variant, vl_level), type = 2) %>% View()
# scenario A2, gender 2
# scenario A6, gender 1


# based on the significant three-way interaction effect gender:vl_level:scenario
data_no_outliers2 %>% 
  group_by(scenario) %>%
  anova_test(dv = time_s, wid = participant_id, between = c(vl_level, gender), type = 2) %>% View()


# based on the significant three-way interaction effect gender:variant:vl_level
data_no_outliers2 %>% 
  group_by(variant) %>%
  anova_test(dv = time_s, wid = participant_id, between = c(vl_level, gender), type = 2) %>% View()
# variant 1 and 2

data_no_outliers2 %>% 
  group_by(variant, gender) %>%
  anova_test(dv = time_s, wid = participant_id, between = vl_level, type = 2) %>% View()
# variant 1, gender 1
# variant 2, gender 1 and 2

data_no_outliers2 %>% 
  group_by(variant, gender) %>%
  pairwise_t_test(time_s ~ vl_level, p.adjust.method = "bonferroni") %>% View()
# variant 1, gender 1, vl_level 2 vs 3 (p-adj = 0.039)
# variant 2, gender 1, vl_level 1 vs 2 (p-adj = 0.001)
# variant 2, gender 2, vl_level 2 vs 3 (p-adj = 0.015)

# based on the significant three-way interaction effect gender:variant:scenario
data_no_outliers2 %>% 
  group_by(scenario) %>%
  anova_test(dv = time_s, wid = participant_id, between = c(variant, gender), type = 2) %>% View()
# scenario A6 and A8

data_no_outliers2 %>% 
  group_by(scenario, gender) %>%
  anova_test(dv = time_s, wid = participant_id, between = variant, type = 2) %>% View()
# scenario A6, gender 1 and 2
# scenario A8, gender 2

data_no_outliers2 %>% 
  group_by(scenario, gender) %>%
  pairwise_t_test(time_s ~ variant, p.adjust.method = "bonferroni") %>% View()
# scenario A6 and gender 1, variant 1 vs 4 (p-adj = 0.024)
# scenario A6 and gender 2, variant 2 vs 4 (p-adj = 0.037)
# scenario A8 and gender 2, variant 1 vs 2 (p-adj = 0.013)
# scenario A8 and gender 2, variant 1 vs 4 (p-adj = 0.007)

# based on the significant three-way interaction effect variant:vl_level:scenario
data_no_outliers2 %>% 
  group_by(scenario) %>%
  anova_test(dv = time_s, wid = participant_id, between = c(variant, vl_level)) %>% View()
# scenario A3

data_no_outliers2 %>% 
  group_by(scenario, vl_level) %>%
  anova_test(dv = time_s, wid = participant_id, between = variant) %>% View()
# scenario A3, vl_level 1

data_no_outliers2 %>% 
  group_by(scenario, vl_level) %>%
  pairwise_t_test(time_s ~ variant, p.adjust.method = "bonferroni") %>% View()
# scenario A3 and vl_level 1, variant 1 vs 4 (p-adj = 0.001)
# scenario A3 and vl_level 1, variant 2 vs 4 (p-adj = 0.002)
# scenario A3 and vl_level 1, variant 3 vs 4 (p-adj = 0.020)

data_no_outliers2 %>% 
  group_by(scenario, variant) %>%
  anova_test(dv = time_s, wid = participant_id, between = vl_level) %>% View()
# scenario A3, variant 3 and 4

data_no_outliers2 %>% 
  group_by(scenario, variant) %>%
  pairwise_t_test(time_s ~ vl_level, p.adjust.method = "bonferroni") %>% View()
# scenario A3 and variant 3, vl_level 1 vs 3 (p-adj = 0.037)
# scenario A3 and variant 4, vl_level 1 vs 2 (p-adj = 0.007)


# based on the significant two-way interaction effect vl_level:scenario
data_no_outliers2 %>% 
  group_by(scenario) %>%
  anova_test(dv = time_s, wid = participant_id, between = vl_level) %>% View()
# scenario A2

data_no_outliers2 %>% 
  group_by(scenario) %>%
  pairwise_t_test(time_s ~ vl_level, p.adjust.method = "bonferroni") %>% View()
# scenario A2, vl_level 1 and 2 (p-adj = 0.039) 

data_no_outliers2 %>% 
  group_by(vl_level) %>%
  anova_test(dv = time_s, wid = participant_id, between = scenario) %>% View()
# vl_level 1, 2 and 3

data_no_outliers2 %>% 
  group_by(vl_level) %>%
  pairwise_t_test(time_s ~ scenario, p.adjust.method = "bonferroni", paired = TRUE) %>% View()
# vl_level 1, scenario A1 and A4 (p-adj = 0.001) 
# vl_level 1, scenario A1 and A6 (p-adj = 0.001) 
# vl_level 1, scenario A1 and A7 (p-adj = 0.001) 
# vl_level 1, scenario A1 and A8 (p-adj = 0.001) 
# vl_level 1, scenario A2 and A4 (p-adj = 0.011) 
# vl_level 1, scenario A3 and A7 (p-adj = 0.003) 
# vl_level 1, scenario A3 and A8 (p-adj = 0.039) 
# vl_level 1, scenario A5 and A6 (p-adj = 0.005) 
# vl_level 1, scenario A5 and A7 (p-adj = 0.024) 
# vl_level 1, scenario A5 and A8 (p-adj = 0.008) 
# vl_level 2, scenario A1 and A2 (p-adj = 0.008) 
# vl_level 2, scenario A1 and A4 (p-adj = 0.001) 
# vl_level 2, scenario A1 and A7 (p-adj = 0.001) 
# vl_level 2, scenario A1 and A8 (p-adj = 0.001) 
# vl_level 2, scenario A2 and A3 (p-adj = 0.001) 
# vl_level 2, scenario A2 and A8 (p-adj = 0.044) 
# vl_level 2, scenario A3 and A4 (p-adj = 0.001) 
# vl_level 2, scenario A3 and A5 (p-adj = 0.011) 
# vl_level 2, scenario A3 and A6 (p-adj = 0.009) 
# vl_level 2, scenario A3 and A7 (p-adj = 0.001) 
# vl_level 2, scenario A3 and A8 (p-adj = 0.001) 
# vl_level 2, scenario A4 and A5 (p-adj = 0.004) 
# vl_level 2, scenario A5 and A7 (p-adj = 0.001) 
# vl_level 2, scenario A5 and A8 (p-adj = 0.001) 
# vl_level 3, scenario A1 and A4 (p-adj = 0.001) 
# vl_level 3, scenario A1 and A7 (p-adj = 0.007) 
# vl_level 3, scenario A1 and A8 (p-adj = 0.008) 
# vl_level 3, scenario A3 and A4 (p-adj = 0.002) 
# vl_level 3, scenario A3 and A7 (p-adj = 0.007) 
# vl_level 3, scenario A4 and A5 (p-adj = 0.001) 
# vl_level 3, scenario A5 and A6 (p-adj = 0.006) 
# vl_level 3, scenario A5 and A7 (p-adj = 0.003) 


# based on the significant two-way interaction effect variant:scenario
data_no_outliers2 %>% 
  group_by(scenario) %>%
  anova_test(dv = time_s, wid = participant_id, between = variant) %>% View()
# scenario A2, A5 and A6

data_no_outliers2 %>% 
  group_by(scenario) %>%
  pairwise_t_test(time_s ~ variant, p.adjust.method = "bonferroni") %>% View()
# scenario A2, variant 1 vs 4 (p-adj = 0.009)
# scenario A2, variant 2 vs 4 (p-adj = 0.003)

data_no_outliers2 %>% 
  group_by(variant) %>%
  anova_test(time_s~scenario) %>% View()
# variant 1,2,3 and 4

data_no_outliers2 %>% 
  group_by(variant) %>%
  pairwise_t_test(time_s ~ scenario, paired = TRUE, p.adjust.method = "bonferroni") %>% View()
# variant 1, scenario A1 vs A4 (p-adj = 0.001)
# variant 1, scenario A1 vs A7 (p-adj = 0.001)
# variant 1, scenario A1 vs A8 (p-adj = 0.001)
# variant 1, scenario A5 vs A7 (p-adj = 0.001)
# variant 1, scenario A5 vs A8 (p-adj = 0.001)
# variant 2, scenario A1 vs A4 (p-adj = 0.001)
# variant 2, scenario A1 vs A7 (p-adj = 0.001)
# variant 2, scenario A3 vs A4 (p-adj = 0.001)
# variant 2, scenario A3 vs A7 (p-adj = 0.001)
# variant 2, scenario A3 vs A8 (p-adj = 0.001)
# variant 2, scenario A4 vs A5 (p-adj = 0.001)
# variant 2, scenario A5 vs A7 (p-adj = 0.001)
# variant 2, scenario A5 vs A8 (p-adj = 0.001)
# variant 3, scenario A1 vs A2 (p-adj = 0.001)
# variant 3, scenario A1 vs A4 (p-adj = 0.001)
# variant 3, scenario A1 vs A6 (p-adj = 0.001)
# variant 3, scenario A1 vs A7 (p-adj = 0.001)
# variant 3, scenario A1 vs A8 (p-adj = 0.001)
# variant 3, scenario A2 vs A5 (p-adj = 0.001)
# variant 3, scenario A3 vs A4 (p-adj = 0.001)
# variant 3, scenario A3 vs A7 (p-adj = 0.001)
# variant 3, scenario A3 vs A8 (p-adj = 0.001)
# variant 3, scenario A4 vs A5 (p-adj = 0.001)
# variant 3, scenario A5 vs A8 (p-adj = 0.001)
# variant 4, scenario A1 vs A2 (p-adj = 0.001)
# variant 4, scenario A1 vs A3 (p-adj = 0.001)
# variant 4, scenario A1 vs A4 (p-adj = 0.001)
# variant 4, scenario A1 vs A6 (p-adj = 0.001)
# variant 4, scenario A1 vs A7 (p-adj = 0.001)
# variant 4, scenario A1 vs A8 (p-adj = 0.001)
# variant 4, scenario A2 vs A3 (p-adj = 0.001)
# variant 4, scenario A3 vs A4 (p-adj = 0.001)
# variant 4, scenario A3 vs A5 (p-adj = 0.001)
# variant 4, scenario A3 vs A6 (p-adj = 0.001)
# variant 4, scenario A3 vs A7 (p-adj = 0.001)
# variant 4, scenario A3 vs A8 (p-adj = 0.001)
# variant 4, scenario A4 vs A5 (p-adj = 0.001)
# variant 4, scenario A5 vs A6 (p-adj = 0.001)
# variant 4, scenario A5 vs A8 (p-adj = 0.001)

# based on the significant main effect scenario
data_no_outliers2 %>% 
  pairwise_t_test(time_s ~ scenario, paired = TRUE, p.adjust.method = "bonferroni") %>% View()
# scenario A1 vs A2 (p-adj = 0.001)
# scenario A1 vs A4 (p-adj = 0.001)
# scenario A1 vs A6 (p-adj = 0.001)
# scenario A1 vs A7 (p-adj = 0.001)
# scenario A1 vs A8 (p-adj = 0.001)
# scenario A2 vs A3 (p-adj = 0.001)
# scenario A2 vs A4 (p-adj = 0.002)
# scenario A2 vs A5 (p-adj = 0.002)
# scenario A2 vs A7 (p-adj = 0.030)
# scenario A2 vs A8 (p-adj = 0.001)
# scenario A3 vs A4 (p-adj = 0.001)
# scenario A3 vs A6 (p-adj = 0.001)
# scenario A3 vs A7 (p-adj = 0.001)
# scenario A3 vs A8 (p-adj = 0.001)
# scenario A4 vs A5 (p-adj = 0.001)
# scenario A4 vs A6 (p-adj = 0.012)
# scenario A5 vs A6 (p-adj = 0.003)
# scenario A5 vs A7 (p-adj = 0.001)
# scenario A5 vs A8 (p-adj = 0.001)
# scenario A6 vs A8 (p-adj = 0.007)


# Effect sizes expressed in partial eta squared
eta_squared(aov2.2)
# scenario - es = 0.73
# vl_level:scenario - es = 0.14
# variant:scenario - es = 0.30
# variant:vl_level:scenario - es = 0.28
# gender:variant:scenario - es = 0.21
# gender:variant:vl_level:scenario - es = 0.29


# Anova for testing H3 and H4
data_no_outliers2 %>% 
  pairwise_t_test(time_s ~ variant, p.adjust.method = "bonferroni")

# Anova and linear regression model for understanding the relation between consumption time and average score
summary(aov(time_s~average_score, data = data_no_outliers2))
summary(lm(time_s~average_score, data = data_no_outliers2))


### Third investigation ###
# Ordinal Logistic Regression
# One ordinal dependent variable: x_score
# Four independent variables: variant, vl_level, gender, scenario
model3 = polr(x_score ~ scenario+variant+vl_level+gender, data = data)
(ctable3 <- coef(summary(model3)))
p3 = pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2
(ctable3 = cbind(ctable3, "p value" = p3))
(ci3 = confint(model3)) 
exp(cbind(OR = coef(model3), ci3))

# Checking for the multicollinearity
vif(model3)
# All VIFs are around 1 which means that there is no multicollinearity.

# Checking the proportional odds assumption
brant(model3) 
# The proportional odds assumption holds at significance level alpha = 0.01.

# Results
# For viewers exposed to scenario A2, the odds of obtaining a higher x_score are 8.36 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A4, the odds of obtaining a higher x_score are 100.63 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A6, the odds of obtaining a higher x_score are 10.78 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A7, the odds of obtaining a higher x_score are 10.64 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A8, the odds of obtaining a higher x_score are 7.42 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# 1/0.4154789 = 2.41
# For viewers with a medium VL level, the odds of obtaining a higher x_score are 2.41 times that
# of viewers with a low VL level, holding constant all other variables.


# Post-hoc power analysis
table(data$x_score)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 8.3583326, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.8584003, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 100.6323300, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.4898611, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 10.7767747, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 10.6395231, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 7.4199312, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 0.7634126, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.6828229, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.1021929, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 0.4154789, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.2447051, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 0.9379071, 384, alpha=0.05)



### Fourth investigation ###
# Ordinal Logistic Regression
# One ordinal dependent variable: y_score
# Four independent variables: variant, vl_level, gender, scenario
model4 = polr(y_score ~ scenario+variant+vl_level+gender, data = data)
(ctable4 <- coef(summary(model4)))
p4 = pnorm(abs(ctable4[, "t value"]), lower.tail = FALSE) * 2
(ctable4 = cbind(ctable4, "p value" = p4))
(ci4 = confint(model4)) 
exp(cbind(OR = coef(model4), ci4))

# Checking for the multicollinearity
vif(model4)
# All VIFs are around 1 which means that there is no multicollinearity.

# Checking the proportional odds assumption
brant(model4) 
# The proportional odds assumption holds at significance level alpha = 0.01

# Results
# 1/0.4138966 = 2.42
# For viewers exposed to scenario A1, the odds of obtaining a higher y_score are 2.42 times that
# of viewers exposed to scenario A2, holding constant all other variables.

# 1/0.1680516 = 5.95
# For viewers exposed to scenario A1, the odds of obtaining a higher y_score are 5.95 times that
# of viewers exposed to scenario A3, holding constant all other variables.

# For viewers exposed to scenario A4, the odds of obtaining a higher y_score are 7.62 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A7, the odds of obtaining a higher y_score are 3.35 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# 1/0.4177607 = 2.39
# For viewers watching the GIFs with smooth transitions, the odds of obtaining a higher y_score are 2.39 times that
# of viewers watching the static visualizations, holding constant all other variables.

# 1/0.5006004 = 2
# For viewers watching the GIFs with smooth transitions, the odds of obtaining a higher y_score are 2 times that
# of viewers watching data-videos, holding constant all other variables.

# 1/0.4624913 = 2.16
# For viewers with a medium VL level, the odds of obtaining a higher y_score are 2.16 times that
# of viewers with a low VL level, holding constant all other variables.

# For viewers with a high VL level, the odds of obtaining a higher y_score are 2.19 times that
# of viewers with a medium VL level, holding constant all other variables.

# Post-hoc power analysis
table(data$y_score)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 0.4138966, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 0.1680516, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 7.6226986, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 1.3247602, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 0.8975530, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 3.3471212, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 1.4366344, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 0.4177607, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 0.8760243, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 0.5006004, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 0.4624913, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 2.1946928, 384, alpha=0.05)
popower(c(11/384,  16/384,  40/384, 56/384, 261/384), 1.3446283, 384, alpha=0.05)



### Fifth investigation ###
# Ordinal Logistic Regression
# One ordinal dependent variable: z_score
# Four independent variables: variant, vl_level, gender, scenario
model5 = polr(z_score ~ scenario+variant+vl_level+gender, data = data)
(ctable5 <- coef(summary(model5)))
p5 = pnorm(abs(ctable5[, "t value"]), lower.tail = FALSE) * 2
(ctable5 = cbind(ctable5, "p value" = p5))
(ci5 = confint(model5)) 
exp(cbind(OR = coef(model5), ci5))

# Checking for the multicollinearity
vif(model5)
# All VIFs are around 1 which means that there is no multicollinearity.

# Checking the proportional odds assumption
brant(model5) 
# The proportional odds assumption holds at significance level alpha = 0.01

# Results
# For viewers exposed to scenario A2, the odds of obtaining a higher z_score are 4.41 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A4, the odds of obtaining a higher z_score are 10.22 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A6, the odds of obtaining a higher z_score are 2.59 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A7, the odds of obtaining a higher z_score are 4.41 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# For viewers exposed to scenario A8, the odds of obtaining a higher z_score are 4.97 times that
# of viewers exposed to scenario A1, holding constant all other variables.

# 1/0.5678096 = 1.76
# For viewers with a medium VL level, the odds of obtaining a higher z_score are 1.76 times that
# of viewers with a low VL level, holding constant all other variables.

# For viewers with a high VL level, the odds of obtaining a higher z_score are 1.84 times that
# of viewers with a medium VL level, holding constant all other variables.

# Post-hoc power analysis
table(data$z_score)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 4.4053220, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.5783764, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 10.2183789, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.6605471, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 2.5919467, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 4.4091959, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 4.9698207, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 0.9785568, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 0.9481189, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.2661225, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 0.5678096, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 1.8370071, 384, alpha=0.05)
popower(c(1/384,  8/384,  32/384, 73/384, 270/384), 0.8485990, 384, alpha=0.05)




### Sixth investigation ###
# Four-way Mixed ANOVA 
# One dependent variable: loops
# Three between-subjects factors: variant, vl_level, gender
# One within-subjects factor: scenario

# Outliers detection
mod6 = lm(loops ~ gender*variant*scenario*vl_level, data=data_no_static_viz)
cooksd6 = cooks.distance(mod6)
plot(cooksd6, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/384, col="red")  # add cutoff line
text(x=1:length(cooksd6)+5, y=cooksd6, labels=ifelse(cooksd6>4/384, names(cooksd6),""), col="red")  # add labels
influential6 = na.omit(as.numeric(names(cooksd6)[(cooksd6 > (4/384))]))

# Replacing the outliers with the median consumption time
data_no_outliers6 = data_no_static_viz
data_no_outliers6[which(data_no_static_viz$run_id %in% influential6), ]$loops = median(data_no_static_viz$loops)

# Four-way mixed ANOVA test
aov6.1 = aov(loops ~ gender*variant*vl_level*scenario + Error(participant_id/scenario), data = data_no_static_viz)
summary(aov6.1)

# Four-way mixed ANOVA test (after imputation)
aov6.2 = aov(loops ~ gender*variant*vl_level*scenario + Error(participant_id/scenario), data = data_no_outliers6)
summary(aov6.2)

# As the results are similar, we do not replace the outliers with the median.

# Checking the normality assumption
data_no_static_viz %>%
  shapiro_test(loops)

ggplot(data_no_static_viz, aes(x=loops)) + geom_density()
# The normality assumption is violated

# Box-cox transformation
bc <- boxcox(loops ~ gender*variant*scenario*vl_level, data=data_no_static_viz)
(lambda <- bc$x[which.max(bc$y)])

# Checking the normality assumption (after transformation)
data_no_static_viz$loops = (data_no_static_viz$loops^lambda-1)/lambda
data_no_static_viz %>%
  shapiro_test(loops)

ggplot(data_no_static_viz, aes(x=loops)) + geom_density()
# The situation has not improved and we the normality assumption is still violated



# Pearson's chi-squared test per each XYZ score 
# Note: we used simulate.p.value = TRUE due to the small cell size
chisq_test(data$average_score, data$variant, simulate.p.value = TRUE) 
chisq_test(data$x_score, data$variant, simulate.p.value = TRUE)
chisq_test(data$y_score, data$variant, simulate.p.value = TRUE)
chisq_test(data$z_score, data$variant, simulate.p.value = TRUE)
# performance scores are not associated with variant

chisq_test(data$average_score, data$vl_level, simulate.p.value = TRUE) 
chisq_test(data$x_score, data$vl_level, simulate.p.value = TRUE)
chisq_test(data$y_score, data$vl_level, simulate.p.value = TRUE)
chisq_test(data$z_score, data$vl_level, simulate.p.value = TRUE)
# performance scores are associated with vl_level

chisq_test(data$average_score, data$scenario, simulate.p.value = TRUE)
chisq_test(data$x_score, data$scenario, simulate.p.value = TRUE)
chisq_test(data$y_score, data$scenario, simulate.p.value = TRUE)
chisq_test(data$z_score, data$scenario, simulate.p.value = TRUE)
# performance scores are associated with scenario


chisq_test(data$average_score, data$gender, simulate.p.value = TRUE)
chisq_test(data$x_score, data$gender, simulate.p.value = TRUE)
chisq_test(data$y_score, data$gender, simulate.p.value = TRUE)
chisq_test(data$z_score, data$gender, simulate.p.value = TRUE)
# performance scores are not associated with gender


# Checking the data related to the colorblind individuals
model_cb1 = polr(average_score ~ scenario+variant+gender+vl_level+cb, data = data)
(ctable_cb1 <- coef(summary(model_cb1)))
p_cb1 <- pnorm(abs(ctable_cb1[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb1 <- cbind(ctable_cb1, "p value" = p_cb1))
(ci_cb1 <- confint(model_cb1))
exp(cbind(OR = coef(model_cb1), ci_cb1))

model_cb2 = polr(x_score ~ scenario+variant+gender+vl_level+cb, data = data)
(ctable_cb2 <- coef(summary(model_cb2)))
p_cb2 <- pnorm(abs(ctable_cb2[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb2 <- cbind(ctable_cb2, "p value" = p_cb2))
(ci_cb2 <- confint(model_cb2))
exp(cbind(OR = coef(model_cb2), ci_cb2))

model_cb3 = polr(y_score ~ scenario+variant+gender+vl_level+cb, data = data)
(ctable_cb3 <- coef(summary(model_cb3)))
p_cb3 <- pnorm(abs(ctable_cb3[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb3 <- cbind(ctable_cb3, "p value" = p_cb3))
(ci_cb3 <- confint(model_cb3))
exp(cbind(OR = coef(model_cb3), ci_cb3))

model_cb4 = polr(z_score ~ scenario+variant+gender+vl_level+cb, data = data)
(ctable_cb4 <- coef(summary(model_cb4)))
p_cb4 <- pnorm(abs(ctable_cb4[, "t value"]), lower.tail = FALSE) * 2
(ctable_cb4 <- cbind(ctable_cb4, "p value" = p_cb4))
(ci_cb4 <- confint(model_cb4))
exp(cbind(OR = coef(model_cb4), ci_cb4))

summary(aov(time_s~cb, data = data))
