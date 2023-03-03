

#######################################################################
### (3) By wave population proportions of economic statuses

econstat <- copy(fulldata)
econstat <- econstat[ , c("id","wave_no","d_age_12cat","d_sex", "l_econ_stat_7cat")]
econstat <- econstat[ , .N, by = .(wave_no, l_econ_stat_7cat, d_sex, d_age_12cat)]

econstat[, l_econ_stat_7cat := factor(l_econ_stat_7cat,
                                      levels = c("employed","self_employed","unemployed","sick",
                                                 "retired","education","other"),
                                      labels = c("Employed","Self-Employed","Unemployed","Long-term sick",
                                                 "Retired","Education","Other"))]
## Frequency plots

ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Economic \nstatus") +
  theme_custom() +
  scale_fill_viridis_d(option = "plasma")
ggsave(paste0(path,"/25_plots/03_econstat.png"),
       units = "in", width = 12/1.5, height = 12/1.5)


# Split by gender

ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col() +
  facet_wrap(~ d_sex) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Economic \nstatus") +
  theme_custom() +
  scale_fill_viridis_d(option = "plasma")
ggsave(paste0(path,"/25_plots/03_econstat_by_sex.png"),
       units = "in", width = 12/1.5, height = 12/1.5)

# Split by age

ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col() +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Economic \nstatus") +
  theme_custom() +
  scale_fill_viridis_d(option = "plasma")
ggsave(paste0(path,"/25_plots/03_econstat_by_age.png"),
       units = "in", width = 12/1.5, height = 12/1.5)

## Proportionate plots

econstat_prop <- copy(econstat)
econstat_prop <- econstat_prop[order(wave_no, l_econ_stat_7cat), ]
econstat_prop <- econstat_prop[, .(N = sum(N)), by = .(wave_no, l_econ_stat_7cat)]
econstat_prop <- econstat_prop[, prop := N/sum(N), by = c("wave_no")]
econstat_prop[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no")]

ggplot(data = econstat_prop, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = l_econ_stat_7cat), size = 3, show.legend = FALSE) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Economic /nstatus") +
  scale_fill_viridis_d(option = "plasma") +
  theme_custom() +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))
ggsave(paste0(path,"/25_plots/03_econstat_proportion.png"),
       units = "in", width = 12/1.5, height = 12/1.5)


# Split by gender

ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col(position = "fill") +
  facet_wrap(~ d_sex) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Economic \nstatus") +
  scale_fill_viridis_d(option = "plasma") +
  theme_custom() +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))
ggsave(paste0(path,"/25_plots/03_econstat_by_sex_proportion.png"),
       units = "in", width = 12/1.5, height = 12/1.5)


# Split by age

ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col(position = "fill") +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Economic \nstatus") +
  theme_custom() +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))
ggsave(paste0(path,"/25_plots/03_econstat_by_age_proportion.png"),
       units = "in", width = 12/1.5, height = 12/1.5)



############################
##### ANIMATED PLOTS #######

#animated
#econstat_sex <- copy(econstat)
#econstat_sex <- econstat_sex[order(wave_no, d_sex, l_econ_stat_7cat), ]
#econstat_sex <- econstat_sex[, .(N = sum(N)), by = .(wave_no, d_sex, l_econ_stat_7cat)]
#econstat_sex <- econstat_sex[, prop := N/sum(N), by = c("wave_no", "d_sex")]
#econstat_sex[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_sex")]

#p <- ggplot(data = econstat_sex, aes(x = d_sex, y = N, fill = l_econ_stat_7cat, frame = as.integer(wave_no))) +
#  labs(x="Gender", y = "Cumulative %",
#       caption = "Wave: {closest_state}",
#       fill = "Economic status") +
#  geom_col(position = "fill") +
#  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = l_econ_stat_7cat), size = 3, show.legend = FALSE) +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 2,
#                    state_length = 2) +
#  ease_aes("quadratic-in-out") +
#  enter_fade() +
#  exit_fade() +
#  scale_fill_viridis_d(option = "plasma") +
#  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))

#animate(p, renderer = gifski_renderer())

#anim_save("03_econstat_by_sex_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()


#animated
#econstat_age <- copy(econstat)
#econstat_age <- econstat_age[order(wave_no, d_age_12cat, l_econ_stat_7cat), ]
#econstat_age <- econstat_age[, .(N = sum(N)), by = .(wave_no, d_age_12cat, l_econ_stat_7cat)]
#econstat_age <- econstat_age[, prop := N/sum(N), by = c("wave_no", "d_age_12cat")]
#econstat_age[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_age_12cat")]

#p <- ggplot(data = econstat_age, aes(x = d_age_12cat, y = N, fill = l_econ_stat_7cat, frame = as.integer(wave_no))) +
#  labs(x="Age category", y = "Cumulative %",
#       caption = "Wave: {closest_state}",
#       fill = "Economic status") +
#  geom_col(position = "fill") +
#  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = l_econ_stat_7cat), size = 3, show.legend = FALSE) +
#  transition_time(as.integer(wave_no)) +
#  transition_states(as.integer(wave_no),
#                    transition_length = 2,
#                    state_length = 2) +
#  ease_aes("quadratic-in-out") +
#  enter_fade() +
#  exit_fade() +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  scale_fill_viridis_d(option = "plasma") +
#  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))

#animate(p, renderer = gifski_renderer(), units = "in", width=14/1.5, height=16/1.5, res=150)

#anim_save("03_econstat_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
#dev.off()

