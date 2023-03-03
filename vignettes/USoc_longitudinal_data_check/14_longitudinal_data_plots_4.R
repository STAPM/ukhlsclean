#######################################################################
### (4) By wave population proportions of abstention

abstain <- copy(fulldata)
abstain <- abstain[ , c("id","wave_no","d_age_12cat","d_sex", "a_current_abstainer", "a_always_abstainer")]
abstain <- abstain[ , .N, by = .(wave_no, a_current_abstainer, d_sex, d_age_12cat)]
abstain <- abstain[! is.na(a_current_abstainer),]

# Retaining wave 7, 9 , 11
abstain <- abstain[wave_no %in% c(7, 9 , 11), ]

## Frequency plots

ggplot(data = abstain, aes(x = wave_no, y = N, fill = a_current_abstainer)) +
  geom_col() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Abstainers") +
  theme_custom() +
  scale_fill_manual(values = c("#cc4704","#49a303"))
png(file = "25_plots/check_panel_data/04_abstain.png", units = "in", width=10/1.5, height=6/1.5, res=600)


# Split by gender

p <- ggplot(data = abstain, aes(x = wave_no, y = N, fill = a_current_abstainer)) +
  geom_col() +
  facet_wrap(~ d_sex) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Abstainers") +
  scale_fill_manual(values = c("#cc4704","#49a303"))
png("25_plots/check_panel_data/04_abstain_by_sex.png", units = "in", width=12/1.5, height=6/1.5, res=600)
print(p)
dev.off()

# Split by age

p <- ggplot(data = abstain, aes(x = wave_no, y = N, fill = a_current_abstainer)) +
  geom_col() +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Abstainers") +
  scale_fill_manual(values = c("#cc4704","#49a303"))
png("25_plots/check_panel_data/04_abstain_by_age.png", units = "in", width=12/1.5, height=6/1.5, res=600)
print(p)
dev.off()


## Proportionate plots

abstain_prop <- copy(abstain)
abstain_prop <- abstain_prop[order(wave_no, a_current_abstainer), ]
abstain_prop <- abstain_prop[, .(N = sum(N)), by = .(wave_no, a_current_abstainer)]
abstain_prop <- abstain_prop[, prop := N/sum(N), by = c("wave_no")]
abstain_prop[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no")]

p <- ggplot(data = abstain_prop, aes(x = wave_no, y = N, fill = a_current_abstainer)) +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = a_current_abstainer), size = 3, show.legend = FALSE) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Abstainer") +
  scale_fill_manual(values = c("#cc4704","#49a303")) +
  scale_colour_manual(values = rep("#FFFFFF", 2))
png("25_plots/check_panel_data/04_abstain_proportion.png", units = "in", width=6/1.5, height=9/1.5, res=600)
print(p)
dev.off()

# Split by gender

abstain_sex <- copy(abstain)
abstain_sex <- abstain_sex[order(wave_no, d_sex, a_current_abstainer), ]
abstain_sex <- abstain_sex[, .(N = sum(N)), by = .(wave_no, d_sex, a_current_abstainer)]
abstain_sex <- abstain_sex[, prop := N/sum(N), by = c("wave_no", "d_sex")]
abstain_sex[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_sex")]

p <- ggplot(data = abstain_sex, aes(x = wave_no, y = N, fill = a_current_abstainer)) +
  facet_wrap(~ d_sex) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Abstainer") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = a_current_abstainer), size = 3, show.legend = FALSE) +
  scale_fill_manual(values = c("#cc4704","#49a303")) +
  scale_colour_manual(values = rep("#FFFFFF", 2))
png("25_plots/check_panel_data/04_abstain_by_sex_proportion.png", units = "in", width=12/1.5, height=9/1.5, res=600)
print(p)
dev.off()


# Split by age

abstain_age <- copy(abstain)
abstain_age <- abstain_age[order(wave_no, d_age_12cat, a_current_abstainer), ]
abstain_age <- abstain_age[, .(N = sum(N)), by = .(wave_no, d_age_12cat, a_current_abstainer)]
abstain_age <- abstain_age[, prop := N/sum(N), by = c("wave_no", "d_age_12cat")]
abstain_age[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_age_12cat")]

p <- ggplot(data = abstain_age, aes(x = wave_no, y = N, fill = a_current_abstainer)) +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Abstention \nrate") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = a_current_abstainer), size = 3, show.legend = FALSE) +
  scale_fill_manual(values = c("#cc4704","#49a303")) +
  scale_colour_manual(values = rep("#FFFFFF", 2))
png("25_plots/check_panel_data/04_abstain_by_age_proportion.png", units = "in", width=14/1.5, height=9/1.5, res=600)
print(p)
dev.off()

######################################
######### ANIMATED PLOTS #############

#animated
#p <- ggplot(data = abstain_sex, aes(x = d_sex, y = N, fill = a_current_abstainer, frame = as.integer(wave_no))) +
#  labs(x="Gender", y = "Cumulative %",
#       caption = "Wave: {closest_state}",
#       fill = "Abstention rate") +
#  geom_col(position = "fill") +
#  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = a_current_abstainer), size = 3, show.legend = FALSE) +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 2,
#                    state_length = 2) +
#  ease_aes("quadratic-in-out") +
#  enter_fade() +
#  exit_fade() +
#  scale_fill_manual(values = c("#cc4704","#49a303")) +
#  scale_colour_manual(values = rep("#FFFFFF", 2))
#animate(p, renderer = gifski_renderer())

#anim_save("04_abstain_by_sex_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()


#animated
#p <- ggplot(data = abstain_age, aes(x = d_age_12cat, y = N, fill = a_current_abstainer, frame = as.integer(wave_no))) +
#  labs(x="Age category", y = "Cumulative %",
#       caption = "Wave: {closest_state}",
#       fill = "Abstention \nrate") +
#  geom_col(position = "fill") +
#  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = a_current_abstainer), size = 3, show.legend = FALSE) +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 2,
#                    state_length = 2) +
#  ease_aes("quadratic-in-out") +
#  enter_fade() +
#  exit_fade() +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  scale_fill_manual(values = c("#cc4704","#49a303")) +
#  scale_colour_manual(values = rep("#FFFFFF", 2))
#animate(p, renderer = gifski_renderer(), units = "in", width=14/1.5, height=8/1.5, res=150)

#anim_save("04_abstain_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()

