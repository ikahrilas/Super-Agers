# Super Ager Source Localization Project
# Created by IJK (10.8.18)
# last edited on 3.11.19
# -------------------------------------------------------------------------------------------------------
# load in necessary packages
library(tidyverse)
library(lme4)
library(readxl)
library(psych)
library(lavaan)
library(jtools)
library(cowplot)
# vector of all .mul files in working directory
files <- list.files(pattern = ".mul")
# extract source localization files from files vector
files_sl <- str_subset(files, "source")
# get names for unfiltered/lowfiltered/highfiltered files. This helps with later code.
uf_names <- c(str_subset(files_sl, "unfilter"), str_subset(files_sl, "nofilter"))
lf_names <- str_subset(files_sl, "lowfilter")
hf_names <- str_subset(files_sl, "highfilter")  

##read in mul files, but separate them into unflitered, low filtered, and high filtered lists.
#preallocate space first
#str_detect returns logical true or false output. It searches vector that
#contains a string of interest. In this case, it searches the "files" vector for
#the filter designation and creates a list of that length
unfiltered_files <- vector("list", length = length(uf_names))
lofiltered_files <- vector("list", length = length(lf_names))
hifiltered_files <- vector("list", length = length(hf_names))

#load the files into the preallocated lists for each filtering category
for (i in seq_along(unfiltered_files)) {
  unfiltered_files[[i]] <- read_table2(uf_names[i], skip = 1)
  unfiltered_files[[i]] <- unfiltered_files[[i]] %>%
    mutate(pid = str_extract(uf_names[i], "[0-9]{4,}"),
           pid = as.numeric(substr(as.character(pid), 7, 9)),
           ms = seq(from = -100, to = 1000,
                    by = (1100 + (1100 / nrow(unfiltered_files[[i]]))) / (nrow(unfiltered_files[[i]]))
           ),
           trial = if_else(str_detect(uf_names[i], "nogo"), "no_go", "go")
    )
}

for (i in seq_along(lofiltered_files)) {
  lofiltered_files[[i]] <- read_table2(lf_names[i], skip = 1)
  lofiltered_files[[i]] <- lofiltered_files[[i]] %>%
    mutate(pid = str_extract(lf_names[i], "[0-9]{4,}"),
           pid = as.numeric(substr(as.character(pid), 7, 9)),
           ms = seq(from = -100, to = 1000,
                    by = (1100 + (1100 / nrow(lofiltered_files[[i]]))) / (nrow(lofiltered_files[[i]]))
           ),
           trial = if_else(str_detect(lf_names[i], "nogo"), "no_go", "go")
    )
}

for (i in seq_along(hifiltered_files)) {
  hifiltered_files[[i]] <- read_table2(hf_names[i], skip = 1)
  hifiltered_files[[i]] <- hifiltered_files[[i]] %>%
    mutate(pid = str_extract(hf_names[i], "[0-9]{4,}"),
           pid = as.numeric(substr(as.character(pid), 7, 9)),
           ms = seq(from = -100, to = 1000,
                    by = (1100 + (1100 / nrow(hifiltered_files[[i]]))) / (nrow(hifiltered_files[[i]]))
           ),
           trial = if_else(str_detect(hf_names[i], "nogo"), "no_go", "go")
    )
}

#reduce all the lists into tibbles for analyses and plotting
no_filter <- unfiltered_files %>% reduce(bind_rows)
low_filter <- lofiltered_files %>% reduce(bind_rows)
high_filter <- hifiltered_files %>% reduce(bind_rows)

# delete the X7 column, which just has missing values in it
no_filter <- no_filter %>% select(-X7)
low_filter <- low_filter %>% select(-X7)
high_filter <- high_filter %>% select(-X7)

## load in excel file that has participant information
part_info <- read_csv("120_132 Subject info_forIan.csv", n_max = 49)
classification <- part_info %>%
  select(`Subject Number`, Classification) %>%
  rename(pid = `Subject Number`,
         group = "Classification") %>% 
  mutate(pid = as.numeric(substr(pid, 1, 3)))

#load in behavior data
behavior <- read_excel("120_behavior_for_ian.xlsx")
behavior2 <- read_excel("120_132 Subject info_forIan.xlsx", n_max = 49)

#retain only variables of interest from behavioral data as per Bob's email
behavior <- behavior %>% 
  select(Subject:RAVLT_DEL, GNG_GO_HR:GNG_GO_RT)

#merge the participant data and the source localization data to obtain experimental group
no_filter <- left_join(no_filter, classification, by = "pid") %>% 
  left_join(., behavior, by = c("pid" = "Subject"))
low_filter <- left_join(low_filter, classification, by = "pid") %>%
  left_join(., behavior, by = c("pid" = "Subject"))
high_filter <- left_join(high_filter, classification, by = "pid") %>%
  left_join(., behavior, by = c("pid" = "Subject"))

# modify the tibbles into long form for plotting
nf_long <- no_filter %>% gather("source", "mv", `RS-1-P`:`E6`)
lf_long <- low_filter %>% gather("source", "mv", `RS-1-P`:`E6`)
hf_long <- high_filter %>% gather("source", "mv", `RS-1-P`:`E6`)

##label source variable with appropriate brain regions
#first convert to factor type to facilitate the process
nf_long$source <- as.factor(nf_long$source)
lf_long$source <- as.factor(lf_long$source)
hf_long$source <- as.factor(hf_long$source)
#now rename the levels according to: 
# RS-1- rostral ACC
# RS-2 - pre SMA
# RS-3 - Right inferior occipital gyrus
# RS-4 - Right Precuneous
# RS-5 - Left Fusiform gyrus
levels(nf_long$source) <- c("EXG", "ACC", 
                            "SMA","IOG",
                            "PCU", "FUS")
nf_long$source <- factor(nf_long$source, levels = c("EXG", "IOG", 
                                                    "PCU", "FUS", 
                                                    "SMA", "ACC"))

levels(lf_long$source) <- c("EXG", "Rostral ACC", "Pre-SMA",
                            "Right Inferior Occipital Gyrus",
                            "Right Precuneous", "Left Fusiform Gyrus")

levels(hf_long$source) <- c("EXG", "ACC", 
                            "SMA","IOG",
                            "PCU", "FUS")
hf_long$source <- factor(hf_long$source, levels = c("EXG", "IOG", 
                                                    "PCU", "FUS", 
                                                    "SMA", "ACC"))
# get rid of underscore in group variable - makes it look nicer for plotting
nf_long$group <- sub("_", " ", nf_long$group)
lf_long$group <- sub("_", " ", lf_long$group)
hf_long$group <- sub("_", " ", hf_long$group)

## baseline correction for all participants
# hf
base_corr <- hf_long %>% 
  filter(between(ms, -100, 0)) %>% 
  group_by(pid, source, trial) %>% 
  summarize(correction = mean(mv))
hf_long <- full_join(hf_long, base_corr, by = c("pid", "source", "trial"))
hf_long <- hf_long %>% mutate(mv = mv - correction)

hf_long %>% 
  filter(between(ms, -100, 0)) %>% 
  group_by(pid, source, trial) %>% 
  summarize(mean(mv)) %>% 
  select(`mean(mv)`) %>% 
  pull()

# nf
base_corr <- nf_long %>% 
  filter(between(ms, -100, 0)) %>% 
  group_by(pid, source, trial) %>% 
  summarize(correction = mean(mv))
nf_long <- full_join(nf_long, base_corr, by = c("pid", "source", "trial"))
nf_long <- nf_long %>% mutate(mv = mv - correction)

nf_long %>% 
  filter(between(ms, -100, 0)) %>% 
  group_by(pid, source) %>% 
  summarize(mean(mv)) %>% 
  select(`mean(mv)`) %>% 
  pull()

## plotting with high filtered data, finalized
hf_long %>%
  filter(source != "EXG", ms <= 600) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial, group) %>%
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  ggplot(aes(ms, mv, color = trial)) +
  geom_line(size = 1) +
  facet_grid(group ~ source) +
  labs(x = "Time (ms)",y = expression(paste("Mean Amplitude (nA)"))) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.size = unit(2, "line"),
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_color_manual(name = "Trial", 
                        values = c("gold2", "darkred"),
                        breaks = c("go","no_go"),
                        labels = c("Go", "No Go")
  )

#just significant sources
hf_long %>%
  filter(source %in% c("FUS", "SMA", "ACC"), ms <= 600) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial, group) %>%
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  ggplot(aes(ms, mv, color = trial)) +
  geom_line(size = 1) +
  facet_grid(group ~ source) +
  labs(x = "Time (ms)", y = expression(paste("Mean Amplitude (nA)"))) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.size = unit(2, "line"),
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_color_manual(name = "Trial", 
                     values = c("gold2", "darkred"),
                     breaks = c("go","no_go"),
                     labels = c("Go", "No Go")
  )

# Collapsed across all groups
hf_long %>%
  filter(source != "EXG", ms <= 600) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial) %>%
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  ggplot(aes(ms, mv, color = trial)) +
  geom_line(size = 1) +
  facet_grid(. ~ source) +
  labs(x = "Time (ms)",y = expression(paste("Mean Amplitude (nA)"))) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.size = unit(2, "line"),
        strip.background = element_blank(),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_color_manual(name = "Trial", 
                     values = c("gold2", "darkred"),
                     breaks = c("go","no_go"),
                     labels = c("Go", "No Go")
  )

# Collapsing across age with geom_ribbon to illustrate range
# se 
hf_long %>%
  filter(source != "EXG", ms <= 600) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial) %>%
  summarize(mean = mean(mv, na.rm = TRUE),
            se_max = mean + (sd(mv)/sqrt(n_distinct(hf_long$pid))),
            se_min = mean - (sd(mv)/sqrt(n_distinct(hf_long$pid)))
            ) %>%
  ggplot(aes(ms)) +
  geom_ribbon(aes(ymin = se_min, ymax = se_max, fill = trial), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = mean, color = trial)) +
  facet_grid(. ~ source) +
  labs(x = "Time (ms)", y = "Mean Amplitude (nA)") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.key.size = unit(2, "line"),
        strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_blank()) + 
  scale_fill_manual(values = c("gold2", "darkred")) +
  scale_color_manual(name = "Trial", 
                     values = c("gold2", "darkred"),
                     breaks = c("go","no_go"),
                     labels = c("Go", "No Go")
  )

#95% confidence intervals of the mean
c(mean(x) - 2 * sem, mean(x) + 2 * sem)
hf_long %>%
  filter(source != "EXG", ms <= 600) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial) %>%
  summarize(mean = mean(mv, na.rm = TRUE),
            se_max = mean + 1.96 * (sd(mv)/sqrt(n_distinct(hf_long$pid))),
            se_min = mean - 1.96 * (sd(mv)/sqrt(n_distinct(hf_long$pid)))
  ) %>%
  ggplot(aes(ms)) +
  geom_ribbon(aes(ymin = se_min, ymax = se_max, fill = trial), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = mean, color = trial)) +
  facet_grid(. ~ source) +
  labs(x = "Time (ms)", y = "Amplitude (nA)") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line")) + 
  scale_fill_manual(values = c("gold2", "darkred")) +
  scale_color_manual(name = "Trial", 
                     values = c("gold2", "darkred"),
                     breaks = c("go","no_go"),
                     labels = c("Go", "No Go")
  )

# scatterplot for memory and nogo accuracy
ggplot(behavior, aes(RAVLT_DEL, GNG_NOGO_CR)) +
  geom_jitter(color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "gold2", size = 1.5) +
  labs(x = "Memory (RAVLT Delay)", y = "Cognitive Control (NoGo Accuracy)") +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(0.5, 1.0)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(face = "bold"))

# scatterplot for age and proportion correct, with separate points and lines for go and nogo
behavior %>% 
  mutate(age_group = if_else(AGE > 80, "80 or older", "Younger than 80")) %>%
  filter(!is.na(age_group)) %>% 
ggplot(., aes(x = RAVLT_DEL, y = GNG_NOGO_CR, linetype = age_group)) +
  geom_jitter(color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "gold2", size = 1.5) +
  labs(x = "Memory (RAVLT Delay)", y = "Cognitive Control (NoGo Accuracy)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        legend.key.size = unit(3, "line")) +
  scale_linetype_manual(name = "Age", values = c("solid", "dashed", "solid"))

ggplot(behavior, aes(RAVLT_DEL, GNG_NOGO_CR)) +
  geom_jitter(aes(color = Classification)) +
  geom_smooth(aes(linetype = Classification), method = "lm")

###overlap of rACC and Pre-SMA
hf_long %>%
  filter(source %in% c("Pre-SMA", "Rostral ACC"), ms <= 600) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial, group) %>%
  summarize(mv = mean(mv, na.rm = TRUE)) %>%
  ggplot(aes(ms, mv, color = trial, linetype = source)) +
  geom_line(size = 1) +
  facet_wrap(~ group) +
  labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)"))) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line")) + 
  scale_color_manual(name = "Trial", 
                     values = c("black", "red"),
                     breaks = c("go","no_go"),
                     labels = c("Go", "No Go")
  )



###
#Just plot superagers and rACC/Pre-SMA
hf_long %>% 
  filter(source %in% c("Rostral ACC", "Pre-SMA"),
         group %in% c("SA", "SA Control")) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial, group) %>%
  summarize(mv = mean(mv, na.rm = TRUE)) %>%
  ggplot(aes(ms, mv, color = trial)) +
  geom_line(size = 1) +
  facet_grid(group ~ source) +
  labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)"))) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line")) + 
  scale_color_manual(name = "Trial", 
                     values = c("black", "red"),
                     breaks = c("go","no_go"),
                     labels = c("Go", "No Go")
  )

#with MCI group as well
hf_long %>% 
  filter(source %in% c("Rostral ACC", "Pre-SMA")) %>%
  mutate(ms = round(ms, digits = -0.8)) %>% 
  group_by(ms, source, trial, group) %>%
  summarize(mv = mean(mv, na.rm = TRUE)) %>%
  ggplot(aes(ms, mv, color = trial)) +
  geom_line(size = 1) +
  facet_grid(group ~ source) +
  labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)"))) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line")) + 
  scale_color_manual(name = "Trial", 
                     values = c("black", "red"),
                     breaks = c("go","no_go"),
                     labels = c("Go", "No Go")
  )

# prep categorical variables for analyses
nf_long$group <- as.factor(nf_long$group)
nf_long$group <- relevel(nf_long$group, "SA Control")
nf_long$trial <- as.factor(nf_long$trial)
nf_long$trial <- relevel(nf_long$trial, "no_go")



#Go RT and Accuracy in model*********
##########################
##########MODELS###########
##########################
#window = 250-450 for preSMA
model_presma <- nf_long %>%
  filter(between(ms, 250, 450), source == "SMA") %>% 
  lmer(mv ~ trial * AGE * RAVLT_DEL + (1|pid), data = .)
#summary with 95% CIs and centered continuous predictors
summ(model_presma, digits = 3, center = TRUE)
#visualize 3-way interaction with heat map
interact_plot(model_presma, pred = AGE, modx = trial, mod2 = RAVLT_DEL, y.label = "Mean Amplitude (nA)") + theme_apa()
## visualize 3-way interaction with heat map
# Function to create prediction grid data frame
make_pred_dat <-  function(data = nf_long, nA=20, nB=5) {
  nCat = length(unique(data$trial))
  d = with(data, 
           data.frame(AGE = rep(seq(min(AGE, na.rm = TRUE), max(AGE, na.rm = TRUE), length = nA), nB*2),
                      RAVLT_DEL = rep(rep(seq(min(RAVLT_DEL, na.rm = TRUE), max(RAVLT_DEL, na.rm = TRUE), length = nB), each = nA), nCat),
                      trial = rep(unique(trial), each = nA*nB)))
  
  d$mv = predict(model_presma, newdata = d)
  
  return(d)
}

##main effects and 2-way interactions
main <- nf_long %>%
  filter(between(ms, 250, 450), source == "Pre-SMA") %>% 
  lmer(mv ~ trial * AGE + (1|pid), data = .)
summ(main, center = TRUE)


# window = 400 - 550 for rACC
model_rACC <- nf_long %>% 
  filter(between(ms, 400, 550), source == "ACC") %>%
  lmer(mv ~ trial * AGE * RAVLT_DEL + (1|pid), data = .)
#summary with 95% CIs and centered continuous predictors
summ(model_rACC, digits = 3, center = TRUE)
#visualize interaction
interact_plot(model_rACC, pred = AGE, modx = trial, mod2 = RAVLT_DEL, y.label = "Mean Amplitude (nA)") + 
  theme_apa()

# window = 350 - 500 ms for Left Fusiform
model_LFG <- nf_long %>% 
  filter(between(ms, 350, 500), source == "FUS") %>% 
  lmer(mv ~ trial * AGE * RAVLT_DEL + (1|pid), data = .)
#summary of data
summ(model_LFG, digits = 3, center = TRUE)
# visualize interaction
interact_plot(model_LFG, pred = AGE, modx = trial, mod2 = RAVLT_DEL, y.label = "Mean Amplitude (nA)") +
  theme_apa()

#### Additional models looking at nogo error rate and go RT

# add 100 ms to RT number as per Bob's request

# preSMA RT 3 way interaction
model_rt_presma <- nf_long %>%
  filter(between(ms, 250, 450), source == "Pre-SMA") %>% 
  lmer(mv ~ trial * AGE * GNG_GO_RT + (1|pid), data = .)
summ(model_rt_presma, digits = 3, center = TRUE)

model_error_presma <- nf_long %>% 
  filter(between(ms, 250, 450), source == "Pre-SMA") %>% 
  lmer(mv ~ trial * AGE * GNG_NOGO_FA + (1|pid), data = .)
summ(model_error_presma, digits = 3, center = TRUE)

# rACC
model_rt_rACC <- nf_long %>%
  filter(between(ms, 250, 500), source == "Rostral ACC") %>% 
  lmer(mv ~ trial * AGE * GNG_GO_RT + (1|pid), data = .)
summ(model_rt_rACC, digits = 3, center = TRUE)
interact_plot(model_rt_rACC, pred = AGE, modx = trial, mod2 = GNG_GO_RT) +
  theme_apa()

model_error_rACC <- nf_long %>%
  filter(between(ms, 250, 500), source == "Rostral ACC") %>% 
  lmer(mv ~ trial * AGE * GNG_NOGO_FA + (1|pid), data = .)
summ(model_error_rACC, digits = 3, center = TRUE)
interact_plot(model_error_rACC, pred = AGE, modx = trial, mod2 = GNG_NOGO_FA) +
  theme_apa()

# LFG
model_rt_LFG <- nf_long %>%
  filter(between(ms, 350, 500), source == "Left Fusiform Gyrus") %>% 
  lmer(mv ~ trial * AGE * GNG_GO_RT + (1|pid), data = .)
summ(model_rt_LFG, digits = 3, center = TRUE)
interact_plot(model_rt_LFG, pred = AGE, modx = trial, mod2 = GNG_GO_RT) +
  theme_apa()

model_error_LFG <- nf_long %>%
  filter(between(ms, 350, 500), source == "Left Fusiform Gyrus") %>% 
  lmer(mv ~ trial * AGE * GNG_NOGO_FA + (1|pid), data = .)
summ(model_error_rACC, digits = 3, center = TRUE)
interact_plot(model_error_LFG, pred = AGE, modx = trial, mod2 = GNG_NOGO_FA) +
  theme_apa()

# memory by age to predict RT or ACC, to neuro data
model_1 <- lm(GNG_GO_RT ~ RAVLT_DEL * AGE, data = nf_long)
summ(model_1, digits = 3, center = TRUE)
interact_plot(model_1, pred = RAVLT_DEL, modx = AGE) +
  theme_apa()

model_2 <- nf_long %>% 
  filter(between(ms, 250, 500), source == "Rostral ACC") %>% 
  lmer(mv ~ RAVLT_DEL * AGE + (1|pid), data = .)
summ(model_2, digits = 3, center = TRUE)

model_3 <- nf_long %>% 
  filter(between(ms, 350, 500), source == "Left Fusiform Gyrus") %>% 
  lmer(mv ~ RAVLT_DEL * AGE + (1|pid), data = .)
summ(model_3, digits = 3, center = TRUE)

model_4 <- lm(GNG_NOGO_CR ~ RAVLT_DEL * AGE, data = nf_long)
summ(model_4)
interact_plot(model_4, pred = RAVLT_DEL, modx = AGE) +
  theme_apa()

## THREE WAY HEAT MAP INTERACTION PLOTS










#path analysis with age predicting memory score and with rACC and preSMA as
#mediators
nf_pa <- nf_long %>%
  filter(source %in% c("Pre-SMA", "Rostral ACC")) %>%
  spread(source, mv) %>% 
  rename(Rostral_ACC = "Rostral ACC",
         "pSMA" = "Pre-SMA")

#derive contrast measures for SEM analyses
acc_mean_go <- nf_long %>%
  filter(between(ms, 250, 450), source == "Rostral ACC", trial == "go") %>%
  group_by(pid) %>% 
  summarize(rACC_go_mean = mean(mv, na.rm = TRUE))
acc_mean_nogo <- nf_long %>%
  filter(between(ms, 250, 450), source == "Rostral ACC", trial == "no_go") %>% 
  group_by(pid) %>% 
  summarize(rACC_nogo_mean = mean(mv, na.rm = TRUE))
acc_mean_contrast <- full_join(acc_mean_go, acc_mean_nogo, by = "pid") %>% 
  mutate(rACC_contrast = rACC_nogo_mean - rACC_go_mean) %>% 
  select(pid, rACC_contrast)

pSMA_mean_go <- nf_long %>%
  filter(between(ms, 250, 500), source == "Pre-SMA", trial == "go") %>%
  group_by(pid) %>% 
  summarize(pSMA_go_mean = mean(mv, na.rm = TRUE))
pSMA_mean_nogo <- nf_long %>%
  filter(between(ms, 250, 500), source == "Pre-SMA", trial == "no_go") %>% 
  group_by(pid) %>% 
  summarize(pSMA_nogo_mean = mean(mv, na.rm = TRUE))
pSMA_mean_contrast <- full_join(pSMA_mean_go, pSMA_mean_nogo, by = "pid") %>% 
  mutate(pSMA_contrast = pSMA_go_mean - pSMA_nogo_mean) %>% 
  select(pid, pSMA_contrast)

LFG_mean_go <- nf_long %>% 
  filter(between(ms, 350, 500), source == "Left Fusiform Gyrus", trial == "go") %>% 
  group_by(pid) %>% 
  summarize(LFG_go_mean = mean(mv, na.rm = TRUE))
LFG_mean_nogo <- nf_long %>% 
  filter(between(ms, 350, 500), source == "Left Fusiform Gyrus", trial == "no_go") %>% 
  group_by(pid) %>% 
  summarize(LFG_nogo_mean = mean(mv, na.rm = TRUE))
LFG_mean_contrast <- full_join(LFG_mean_go, LFG_mean_nogo, by = "pid") %>% 
  mutate(LFG_contrast = LFG_go_mean - LFG_nogo_mean) %>% 
  select(pid, LFG_contrast)

mean_contrasts <- full_join(acc_mean_contrast, pSMA_mean_contrast, by = "pid") %>% 
  full_join(LFG_mean_contrast, by = "pid") %>% 
  filter(!is.na(rACC_contrast))

path_analysis <- full_join(mean_contrasts, behavior, by = c("pid" = "Subject"))

model <- ' 
RAVLT_DEL ~ c*AGE
pSMA_contrast ~ a1*AGE
RAVLT_DEL ~ b1*pSMA_contrast
rACC_contrast ~ a2*AGE
RAVLT_DEL ~ b2*rACC_contrast
a1b1 := a1*b1
a2b2 := a2*b2
total := c + (a2*b2) + (a2*b2)
'

fit <- sem(model, data = path_analysis, se = "bootstrap", bootstrap = 5000)
summary(fit)









#write paragraph explaining these results check correlation with task
#performance. Send bob correlation matrix with narrow and wide window. both ERPS
#(racc and sma) both wide and narrow ways. Measure amplitudes of go and no go,
#as well as the different between the two. Also No go correct total count

#short window values
nf_long %>% 
  filter(between(ms, 400, 500)) %>% 
  group_by(pid) %>%
  summarize(short_window_nv = mean(mv))

##########################
##########################
##########################
nf_trans <- nf_long %>%
  filter(between(ms, 350, 450), source == "Pre-SMA") %>% 
  ggplot(data = ., aes(mv)) +
  geom_density()



#no conrast preSMA super ager
go_sa_nc <- nf_long %>% 
  filter(source == "Pre-SMA", trial == "go", group == "SA") %>% 
  filter(between(ms, 350, 450)) %>% 
  group_by(pid) %>% 
  summarize(mv = mean(mv, na.rm = TRUE))
#no contrast preSMA super ager control
go_sac_nc <- nf_long %>% 
  filter(source == "Pre-SMA", trial == "go", group == "SA Control") %>% 
  filter(between(ms, 350, 450)) %>% 
  group_by(pid) %>% 
  summarize(mv = mean(mv, na.rm = TRUE))

##Descriptive statistics
pids <- nf_long %>% 
  group_by(pid) %>% 
  summarize(mean = mean(mv)) %>% 
  select(pid) %>% 
  pull()

behavior %>% 
  filter(!is.na(AGE)) %>% 
  summarize(Range_min = min(range(AGE)),
            Range_max = max(range(AGE)),
            Mean = mean(AGE),
            SD = sd(AGE),
            mem_Range_min = min(range(RAVLT_DEL)),
            mem_Range_max = max(range(RAVLT_DEL)),
            mem_Mean = mean(RAVLT_DEL),
            mem_SD = sd(RAVLT_DEL))
