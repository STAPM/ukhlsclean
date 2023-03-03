#######################################################################
### (1) By wave population proportions of highest qualifications

quali <- copy(fulldata)
quali <- quali[! is.na(d_hiqual) , c("id","weight_xw","wave_no","d_age_12cat","d_sex", "d_hiqual")]
#factorise hiqual, region and econstat data
#quali[ , d_hiqual := factor(d_hiqual, levels = c("other_he","degree","other_qual","alevel","gcse","no_qual",NA))]
#quali <- quali[ ! is.na(d_hiqual), ]
quali <- quali[ , .(sum = sum(weight_xw)), by = c("d_hiqual","wave_no","d_age_12cat","d_sex")]

##################
## Frequency plots

ggplot(data = quali, aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual))) +
  geom_col() +
  #  theme_custom() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Highest \nqualification") +
  theme_custom() +
  scale_fill_viridis_d(direction = -1)
ggsave(paste0(path,"/25_plots/01_highest_qual.png"),
       units = units, height = height, width = width, dpi = dpi)

# By gender

ggplot(data = quali, aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual))) +
  geom_col() +
  facet_wrap(~ d_sex) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Highest \nqualification") +
  theme_custom() +
  scale_fill_viridis_d(direction = -1)
ggsave(paste0(path,"/25_plots/01_highest_qual_by_sex.png"),
       units = units, height = height, width = width, dpi = dpi)

# By age (broken ? )

ggplot(data = quali, aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual))) +
  geom_col() +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Highest \nqualification") +
  theme_custom() +
  scale_fill_viridis_d(direction = -1)
ggsave(paste0(path,"/25_plots/01_highest_qual_by_age.png"),
       units = units, width = 12/1.5, height = 12/1.5, dpi = dpi)


######################
## Proportionate plots

# Split by gender

ggplot(data = quali[! is.na(d_hiqual), ], aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual))) +
  geom_col(position = "fill") +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Highest \nqualification") +
  facet_wrap(~ d_sex) +
  theme_custom() +
  scale_fill_viridis_d(direction = -1)
ggsave(paste0(path,"/25_plots/01_highest_qual_by_sex_proportion.png"),
       units = units, width = 12/1.5, height = 12/1.5, dpi = dpi)

# Split by age

quali_age <- copy(fulldata)
quali_age <- quali_age[! is.na(d_hiqual) , c("id","wave_no","d_age_12cat", "d_hiqual")]
quali_age <- quali_age[order(wave_no, d_age_12cat), ]
quali_age <- quali_age[, .N, keyby = c("d_hiqual","wave_no")]

quali_age[, prop := N/sum(N), by = c("wave_no")]
quali_age[, label_y := (cumsum(prop) - 0.5 * prop), by = "wave_no"]

ggplot(data = quali_age, aes(x = wave_no, y = N, fill = forcats::fct_rev(d_hiqual))) +
  geom_col(position = "fill") +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Highest \nqualification") +
  theme_custom() +
  geom_text(aes(y = label_y, label = paste0(round(prop*100,1),"%"), colour = d_hiqual), size = 3) +
  scale_fill_viridis_d(direction = -1) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  guides(colour = "none")
ggsave(paste0(path,"/25_plots/01_highest_qual_proportion.png"),
       units = "in", width = 12/1.5, height = 12/1.5)


################################
#### ANIMATED ##################


# By age (animated)

#p1 <- ggplot(quali, aes(x = d_age_12cat, y = sum, fill = forcats::fct_rev(d_hiqual), frame = as.integer(wave_no))) +
#  labs(x="Age Category", y = "Frequency",
#       caption = "Wave: {previous_state}",
#       fill = "Highest \nqualification") +
#  scale_fill_viridis_d(direction = -1) +
#  geom_col() +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 0.5,
#                    state_length = 1) +
#  ease_aes("linear") +
#  enter_fade() +
#  exit_fade()

#animate(p1, renderer = gifski_renderer())

#anim_save("01_highest_qual_by_age.gif", animation = last_animation(), path = paste0(path,"25_plots/"))
#dev.off()




# Age animated

#p <- ggplot(data = quali, aes(x = d_age_12cat, y = sum, fill = forcats::fct_rev(d_hiqual), frame = as.integer(wave_no))) +
#  labs(x="Age Category", y = "Cumulative %",
#       caption = "Wave: {previous_state}",
#       fill = "Highest \nqualification") +
#  scale_fill_viridis_d(direction = -1) +
#  geom_col(position = "fill") +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 0.5,
#                    state_length = 1) +
#  ease_aes("linear") +
#  enter_fade() +
#  exit_fade()

#animate(p, renderer = gifski_renderer())

#anim_save("01_highest_qual_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()
