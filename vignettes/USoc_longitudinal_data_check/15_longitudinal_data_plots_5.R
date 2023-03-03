#######################################################################
### (5) By wave population proportions of smoking prevalence

smkprev <- copy(fulldata)
smkprev <- smkprev[ , c("id","wave_no","d_age_12cat","d_sex", "s_current_smoker")]
smkprev <- smkprev[ , .N, by = .(wave_no, s_current_smoker, d_sex, d_age_12cat)]
smkprev <- smkprev[! is.na(s_current_smoker),]

# Retaining waves 5-11
smkprev <- smkprev[wave_no %in% 5:12, ]

## Frequency plots

p <- ggplot(data = smkprev, aes(x = wave_no, y = N, fill = s_current_smoker)) +
  geom_col() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Smoking \nprevalence") +
  scale_fill_manual(values = c("#f5f5f5","#CD9653"))
png("25_plots/check_panel_data/05_smk.png", units = "in", width=10/1.5, height=6/1.5, res=600)
print(p)
dev.off()

# Split by gender

p <- ggplot(data = smkprev, aes(x = wave_no, y = N, fill = s_current_smoker)) +
  geom_col() +
  facet_wrap(~ d_sex) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Smoking \nprevalence") +
  scale_fill_manual(values = c("#f5f5f5","#CD9653"))
png("25_plots/check_panel_data/05_smk_by_sex.png", units = "in", width=9/1.5, height=9/1.5, res=600)
print(p)
dev.off()

# Split by age

p <- ggplot(data = smkprev, aes(x = wave_no, y = N, fill = s_current_smoker)) +
  geom_col() +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Smoking \nprevalence") +
  scale_fill_manual(values = c("#f5f5f5","#CD9653"))
png("25_plots/check_panel_data/05_smk_by_age.png", units = "in", width=12/1.5, height=16/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(data = smkprev, aes(x = d_age_12cat, y = N, fill = s_current_smoker, frame = as.integer(wave_no))) +
  labs(x="Age category", y = "Frequency",
       caption = "Wave: {closest_state}",
       fill = "Smoking \nprevalence") +
  geom_col() +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 0.5,
                    state_length = 1) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("#f5f5f5","#CD9653"))
animate(p, renderer = gifski_renderer(), units = "in", width=9/1.5, height=9/1.5, res=150)

anim_save("05_smk_by_age.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()


## Proportionate plots

smkprev_prop <- copy(smkprev)
smkprev_prop <- smkprev_prop[order(wave_no, s_current_smoker), ]
smkprev_prop <- smkprev_prop[, .(N = sum(N)), by = .(wave_no, s_current_smoker)]
smkprev_prop <- smkprev_prop[, prop := N/sum(N), by = c("wave_no")]
smkprev_prop[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no")]

p <- ggplot(data = smkprev_prop, aes(x = wave_no, y = N, fill = s_current_smoker)) +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = s_current_smoker), size = 3, show.legend = FALSE) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Smoking \nprevalence") +
  scale_fill_manual(values = c("#f5f5f5","#CD9653")) +
  scale_colour_manual(values = rep("#000000", 2))
png("25_plots/check_panel_data/05_smk_proportion.png", units = "in", width=9/1.5, height=9/1.5, res=600)
print(p)
dev.off()

# Split by gender

smkprev_sex <- copy(smkprev)
smkprev_sex <- smkprev_sex[order(wave_no, d_sex, s_current_smoker), ]
smkprev_sex <- smkprev_sex[, .(N = sum(N)), by = .(wave_no, d_sex, s_current_smoker)]
smkprev_sex <- smkprev_sex[, prop := N/sum(N), by = c("wave_no", "d_sex")]
smkprev_sex[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_sex")]

p <- ggplot(data = smkprev_sex, aes(x = wave_no, y = N, fill = s_current_smoker)) +
  facet_wrap(~ d_sex) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Smoking \nprevalence") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = s_current_smoker), size = 3, show.legend = FALSE) +
  scale_fill_manual(values = c("#f5f5f5","#CD9653")) +
  scale_colour_manual(values = rep("#000000", 2))
png("25_plots/check_panel_data/05_smk_by_sex_proportion.png", units = "in", width=12/1.5, height=9/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(data = smkprev_sex, aes(x = d_sex, y = N, fill = s_current_smoker, frame = as.integer(wave_no))) +
  labs(x="Gender", y = "Cumulative %",
       caption = "Wave: {closest_state}",
       fill = "Smoking \nprevalence") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = s_current_smoker), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("quadratic-in-out") +
  enter_fade() +
  exit_fade() +
  scale_fill_manual(values = c("#f5f5f5","#CD9653")) +
  scale_colour_manual(values = rep("#000000", 2))
animate(p, renderer = gifski_renderer(), units = "in", width=6/1.5, height=9/1.5, res=150)

anim_save("05_smk_by_sex_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by age

smkprev_age <- copy(smkprev)
smkprev_age <- smkprev_age[order(wave_no, d_age_12cat, s_current_smoker), ]
smkprev_age <- smkprev_age[, .(N = sum(N)), by = .(wave_no, d_age_12cat, s_current_smoker)]
smkprev_age <- smkprev_age[, prop := N/sum(N), by = c("wave_no", "d_age_12cat")]
smkprev_age[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_age_12cat")]

p <- ggplot(data = smkprev_age, aes(x = wave_no, y = N, fill = s_current_smoker)) +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Smoking \nprevalence") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = s_current_smoker), size = 3, show.legend = FALSE) +
  scale_fill_manual(values = c("#f5f5f5","#CD9653")) +
  scale_colour_manual(values = rep("#000000", 2))
png("25_plots/check_panel_data/05_smk_by_age_proportion.png", units = "in", width=24/1.5, height=12/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(data = smkprev_age, aes(x = d_age_12cat, y = N, fill = s_current_smoker, frame = as.integer(wave_no))) +
  labs(x="Age category", y = "Cumulative %",
       caption = "Wave: {closest_state}",
       fill = "Smoking \nprevalence") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = s_current_smoker), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("quadratic-in-out") +
  enter_fade() +
  exit_fade() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("#f5f5f5","#CD9653")) +
  scale_colour_manual(values = rep("#000000", 2))
animate(p, renderer = gifski_renderer(), units = "in", width=12/1.5, height=9/1.5, res=150)

anim_save("05_smk_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

