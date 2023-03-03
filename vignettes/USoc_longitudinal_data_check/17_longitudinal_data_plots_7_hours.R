


#######################################################################
### (8) 1 Wave Distribution of hours worked (Wave 12)

work_12 <- copy(fulldata[wave_no == max(as.numeric(wave_no)),])
work_12 <- work_12[ , c("id","d_age_5cat","d_age_12cat","d_sex","d_gor","w_hours_empl")]
work_12 <- work_12[! is.na(w_hours_empl)]

ggplot(work_12, aes(x = w_hours_empl)) +
  geom_density(aes(y = ..density..), alpha = 0.3, stat = "density", position = "identity")

p <- ggplot(work_12) +
  geom_density(aes(x = w_hours_empl, fill = w_hours_empl)) +
  labs(x = "Hours worked per week",
       y = "Distribution of hours worked")
png("25_plots/check_panel_data/10_hrs.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()


# Split by age
work_12_age <- work_12[ , .(mean_hours = mean(w_hours_empl), sd_hours = sd(w_hours_empl), n = .N), by = c("d_age_12cat")]
work_12_age

p <- ggplot(work_12_age, aes(x = d_age_12cat, y = mean_hours)) +
  geom_col(aes(fill = d_age_12cat)) +
  geom_errorbar(aes(ymin = mean_hours - sd_hours,
                    ymax = mean_hours + sd_hours,
                    width = 0.2)) +
  labs(x = "Age category",
       y = "Mean hours worked per week") +
  scale_fill_viridis_d(direction = -1)
png("25_plots/check_panel_data/10_hrs_by_age.png", units = "in", width=12/1.5, height=16/1.5, res=600)
print(p)
dev.off()

# Split by gender
work_12_gender <- work_12[ , .(mean_hours = mean(w_hours_empl), sd_hours = sd(w_hours_empl), n = .N), by = c("d_sex")]
work_12_gender

p <- ggplot(work_12_gender, aes(x = d_sex, y = mean_hours)) +
  geom_col(aes(fill = d_sex)) +
  geom_errorbar(aes(ymin = mean_hours - sd_hours,
                    ymax = mean_hours + sd_hours,
                    width = 0.2)) +
  labs(x = "Gender",
       y = "Mean hours worked per week") +
  scale_fill_viridis_d(direction = -1)
png("25_plots/check_panel_data/10_hrs_by_sex.png", units = "in", width=12/1.5, height=16/1.5, res=600)
print(p)
dev.off()

# Split by region
work_12_region <- work_12[ , .(mean_hours = mean(w_hours_empl), sd_hours = sd(w_hours_empl), n = .N), by = c("d_gor")]
work_12_region

p <- ggplot(work_12_region, aes(x = d_gor, y = mean_hours)) +
  geom_col(aes(fill = d_gor)) +
  geom_errorbar(aes(ymin = mean_hours - sd_hours,
                    ymax = mean_hours + sd_hours,
                    width = 0.2)) +
  labs(x = "Region",
       y = "Mean hours worked per week") +
  scale_fill_viridis_d(direction = -1) +
  coord_flip()
png("25_plots/check_panel_data/10_hrs_by_region.png", units = "in", width=12/1.5, height=16/1.5, res=600)
print(p)
dev.off()


#######################################################################
### (9) By wave Distribution of hours worked
work_hours <- copy(fulldata)
work_hours <- work_hours[ , c("id","wave_no","d_age_5cat","d_sex","d_gor","w_hours_empl")]
work_hours <- work_hours[! is.na(w_hours_empl)]

p <- ggplot(work_hours, aes(x = w_hours_empl)) +
  geom_density() +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked")
png("25_plots/check_panel_data/11_hrs_wave.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

p <- ggplot(data = work_hours, aes(x = w_hours_empl)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       fill = "Wave")
png("25_plots/check_panel_data/11_hrs_wave_overlay.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

p <- ggplot(work_hours, aes(x = w_hours_empl, fill = factor(wave_no), frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       caption = "Wave: {previous_state}",
       fill = "Wave") +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade() +
  shadow_mark(alpha = alpha/4, colour = alpha("grey", .25)) +
  guides(fill = "none")
animate(p, renderer = gifski_renderer(), units = "in", width=12/1.5, height=12/1.5, res=150)

anim_save("11_hrs_wave_animate.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by age
p <- ggplot(data = work_hours, aes(x = w_hours_empl)) +
  geom_density(aes(y = ..density.., fill = `d_age_5cat`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       fill = "Age \ncategory")
png("25_plots/check_panel_data/11_hrs_wave_overlay_by_age.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(work_hours, aes(x = w_hours_empl, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = d_age_5cat)) +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       caption = "Wave: {previous_state}",
       fill = "Age \ncategory") +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade() +
  shadow_mark(alpha = alpha/4, colour = alpha("grey", .25)) +
  guides(fill = "legend")
animate(p, renderer = gifski_renderer(), units = "in", width=12/1.5, height=12/1.5, res=150)

anim_save("11_hrs_wave_overlay_by_age.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by gender
p <- ggplot(data = work_hours, aes(x = w_hours_empl)) +
  geom_density(aes(y = ..density.., fill = `d_sex`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       fill = "Gender")
png("25_plots/check_panel_data/11_hrs_wave_overlay_by_sex.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(work_hours, aes(x = w_hours_empl, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = d_sex)) +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       caption = "Wave: {previous_state}",
       fill = "Gender") +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade() +
  shadow_mark(alpha = alpha/4, colour = alpha("grey", .25)) +
  guides(fill = "legend")
animate(p, renderer = gifski_renderer(), units = "in", width=12/1.5, height=12/1.5, res=150)

anim_save("11_hrs_wave_overlay_by_sex.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by region
p <- ggplot(data = work_hours, aes(x = w_hours_empl)) +
  geom_density(aes(y = ..density.., fill = `d_gor`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       fill = "Region")
png("25_plots/check_panel_data/11_hrs_wave_overlay_by_region.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(work_hours, aes(x = w_hours_empl, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = d_gor)) +
  labs(x = "Mean hours worked per week",
       y = "Distribution of hours worked",
       caption = "Wave: {previous_state}",
       fill = "Region") +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade() +
  shadow_mark(alpha = alpha/4, colour = alpha("grey", .25)) +
  guides(fill = "legend")
animate(p, renderer = gifski_renderer(), units = "in", width=12/1.5, height=12/1.5, res=150)

anim_save("11_hrs_wave_overlay_by_region.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split into 4 waves

work_hours_4 <- work_hours[wave_no %in% c(1,4,9,12),]

ggplot(data = work_hours_4, aes(x = w_hours_empl)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity")

p <- ggplot(data = work_hours_4, aes(x = w_hours_empl)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Mean hours worked",
       y = "Distribution of hours worked",
       fill = "Wave")
png("25_plots/check_panel_data/11_hrs_4wave_overlay.png", units = "in", width=15/1.5, height=9/1.5, res=600)
print(p)
dev.off()
