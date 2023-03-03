#######################################################################
### (2) By wave population proportions of regions

region <- copy(fulldata)
region <- region[ , c("id","wave_no","d_age_12cat","d_sex", "d_gor")]
region <- region[ , .N, by = .(wave_no, d_gor, d_sex, d_age_12cat)]

## Frequency plots

ggplot(data = region, aes(x = wave_no, y = N, fill = d_gor)) +
  geom_col() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Region") +
  theme_custom() +
  scale_fill_brewer(palette = "Paired")
ggsave(paste0(path,"/25_plots/02_region.png"),
       units = "in", width = 12/1.5, height = 12/1.5)

# Split by gender

ggplot(data = region, aes(x = wave_no, y = N, fill = d_gor)) +
  geom_col() +
  facet_wrap(~ d_sex) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Region") +
  theme_custom() +
  scale_fill_brewer(palette = "Paired")
ggsave(paste0(path,"/25_plots/02_region_by_sex.png"),
       units = "in", width = 12/1.5, height = 12/1.5)

# Split by age

ggplot(data = region, aes(x = d_age_12cat, y = N, fill = d_gor)) +
  geom_col() +
  facet_wrap(~ wave_no) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Region") +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Paired")
ggsave(paste0(path,"/25_plots/02_region_by_age.png"),
       units = "in", width = 12/1.5, height = 12/1.5)


## Proportionate plots

region_prop <- copy(region)
region_prop <- region_prop[order(wave_no, d_gor), ]
region_prop <- region_prop[, .(N = sum(N)), by = .(wave_no, d_gor)]
region_prop <- region_prop[, prop := N/sum(N), by = c("wave_no")]
region_prop[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no")]

ggplot(data = region_prop, aes(x = wave_no, y = N, fill = d_gor)) +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = d_gor), size = 3, show.legend = FALSE) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Region") +
  theme_custom() +
  scale_fill_brewer(palette = "Paired") +
  scale_colour_manual(values = rep("#000000", 12))
ggsave(paste0(path,"/25_plots/02_region_proportion.png"),
       units = "in", width = 12/1.5, height = 12/1.5)

# Split by gender

ggplot(data = region, aes(x = wave_no, y = N, fill = d_gor)) +
  geom_col(position = "fill") +
  facet_wrap(~ d_sex) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Region") +
  theme_custom() +
  scale_fill_brewer(palette = "Paired")
ggsave(paste0(path,"/25_plots/02_region_by_sex_proportion.png"),
       units = "in", width = 12/1.5, height = 12/1.5)



# Split by age

region_age <- copy(region)
region_age <- region_age[order(wave_no, d_age_12cat, d_gor), ]
region_age <- region_age[, .(N = sum(N)), by = .(wave_no, d_age_12cat, d_gor)]
region_age <- region_age[, prop := N/sum(N), by = c("wave_no", "d_age_12cat")]
region_age[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_age_12cat")]

ggplot(data = region_age, aes(x = d_age_12cat, y = N, fill = d_gor)) +
  geom_col(position = "fill") +
  facet_wrap(~ wave_no) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Region") +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Paired")
ggsave(paste0(path,"/25_plots/02_region_by_age_proportion.png"),
       units = "in", width = 12/1.5, height = 12/1.5)


##################################
##### ANIMATED PLOTS ############


#animated

#p <- ggplot(data = region, aes(x = d_sex, y = N, fill = d_gor, frame = as.integer(wave_no))) +
#  labs(x="Gender", y = "Frequency",
#       caption = "Wave: {previous_state}",
#       fill = "Region") +
#  scale_fill_brewer(palette = "Paired") +
#  geom_col() +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 0.5,
#                    state_length = 1) +
#  ease_aes("linear") +
#  enter_fade() +
#  exit_fade()

#animate(p, renderer = gifski_renderer())

#anim_save("02_region_by_sex.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()


#animated
#p <- ggplot(data = region, aes(x = d_age_12cat, y = N, fill = d_gor, frame = as.integer(wave_no))) +
#  labs(x="Age category", y = "Frequency",
#       caption = "Wave: {closest_state}",
#       fill = "Region") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  scale_fill_brewer(palette = "Paired") +
#  geom_col() +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 0.5,
#                    state_length = 1) +
#  ease_aes("linear") +
#  enter_fade() +
#  exit_fade()

#animate(p, renderer = gifski_renderer())

#anim_save("02_region_by_age.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()


#p <- ggplot(data = region_age, aes(x = d_age_12cat, y = N, fill = d_gor, frame = as.integer(wave_no))) +
#  labs(x="Age category", y = "Cumulative %",
#       caption = "Wave: {closest_state}",
#       fill = "Region") +
#  geom_col(position = "fill") +
#  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = d_gor), size = 3, show.legend = FALSE) +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 2,
#                    state_length = 2) +
#  ease_aes("exponential-out") +
#  enter_fade() +
#  exit_fade() +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  scale_fill_brewer(palette = "Paired") +
#  scale_colour_manual(values = rep("#000000", 12))

#animate(p, renderer = gifski_renderer())

#anim_save("02_region_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()




#animated
#region_sex <- copy(region)
#region_sex <- region_sex[order(wave_no, d_sex, d_gor), ]
#region_sex <- region_sex[, .(N = sum(N)), by = .(wave_no, d_sex, d_gor)]
#region_sex <- region_sex[, prop := N/sum(N), by = c("wave_no", "d_sex")]
#region_sex[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_sex")]

#p <- ggplot(data = region_sex, aes(x = d_sex, y = N, fill = d_gor, frame = as.integer(wave_no))) +
#  labs(x="Gender", y = "Frequency",
#       caption = "Wave: {closest_state}",
#       fill = "Region") +
#  geom_col(position = "fill") +
#  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = d_gor), size = 3, show.legend = FALSE) +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 2,
#                    state_length = 2) +
#  ease_aes("exponential-out") +
#  enter_fade() +
#  exit_fade() +
#  scale_fill_brewer(palette = "Paired") +
#  scale_colour_manual(values = rep("#000000", 12))

#animate(p, renderer = gifski_renderer())

#anim_save("02_region_by_sex_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()
