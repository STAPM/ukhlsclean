

#### WIP
#######################################################################
### (6) By wave distribution of real usual earnings

usual_earn <- copy(fulldata)
usual_earn <- usual_earn[ , c("id","wave_no","d_age_5cat","d_age_12cat","d_sex", "l_real_grss_earnings_usual_empl")]
#usual_earn <- usual_earn[ , .N, by = .(wave_no, l_real_grss_earnings_usual_empl, d_sex, d_age_12cat)]
usual_earn <- usual_earn[! is.na(l_real_grss_earnings_usual_empl),]

#### Distribution of earnings (all waves)
p <- ggplot(usual_earn, aes(x = l_real_grss_earnings_usual_empl)) +
  geom_density() +
  labs(x = "Real gross earnings",
       y = "Distribution of Earnings")
png("25_plots/check_panel_data/06_earnings.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

####

### By wave
# ggplot(usual_earn, aes(x = l_real_grss_earnings_usual,
#                        #colour = wave_no,
#
#                        )) +
#   geom_density(aes(y = ..density..,
#                    fill = wave_no,
#                    alpha = 0.3))

p <- ggplot(data = usual_earn, aes(x = l_real_grss_earnings_usual_empl)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
       fill = "Wave")
png("25_plots/check_panel_data/06_earnings_wave_overlay.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

## By wave distribution stacked !!!!! -> may be better for visualising ? ~ need this one ?
ggplot(usual_earn, aes(x = l_real_grss_earnings_usual_empl, fill = wave_no, alpha = 0.01)) +
  geom_density(position = "stack")

## By wave distributions faceted by wave
p <- ggplot(usual_earn, aes(x = l_real_grss_earnings_usual_empl)) +
  geom_density(aes(fill = wave_no, alpha = 0.3)) +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
       fill = "Wave") +
  facet_wrap(~ wave_no)
png("25_plots/check_panel_data/06_earnings_wave_facet.png", units = "in", width=24/1.5, height=18/1.5, res=300)
print(p)
dev.off()

#animated
p <- ggplot(usual_earn, aes(x = l_real_grss_earnings_usual_empl, fill = factor(wave_no), frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5) +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
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

anim_save("06_earnings_wave_animate.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by age
p <- ggplot(data = usual_earn, aes(x = l_real_grss_earnings_usual_empl)) +
  geom_density(aes(y = ..density.., fill = `d_age_5cat`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
       fill = "Age \ncategory")
png("25_plots/check_panel_data/06_earnings_wave_overlay_by_age.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(usual_earn, aes(x = l_real_grss_earnings_usual_empl, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = d_age_5cat)) +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
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

anim_save("06_earnings_wave_overlay_by_age.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()


## Wave 1, 4, 8, 12 plot
usual_earn_4 <- copy(usual_earn)
usual_earn_4 <- usual_earn_4[wave_no %in% c(1,4,9,12),]

# fill
p <- ggplot(data = usual_earn_4, aes(x = l_real_grss_earnings_usual_empl)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
       fill = "Wave")
png("25_plots/check_panel_data/06_earnings_4wave_overlay.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

# line
p <- ggplot(data = usual_earn_4, aes(x = l_real_grss_earnings_usual_empl)) +
  geom_density(aes(y = ..density.., colour = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
       colour = "Wave")
png("25_plots/check_panel_data/06_earnings_4wave_line.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()


#######################################################################
### 1 wave distribution of real usual vs last earnings

wave12 <- copy(fulldata[wave_no == max(as.numeric(wave_no)),])
wave12 <- wave12[ , c("id","wave_no","d_age_5cat","d_age_12cat","d_sex", "l_real_grss_earnings_usual_empl","l_real_grss_earnings_last_empl")]
wave12 <- wave12[! is.na(l_real_grss_earnings_usual_empl),]
wave12 <- wave12[! is.na(l_real_grss_earnings_last_empl)]


p <- ggplot(wave12) +
  geom_density(aes(x = l_real_grss_earnings_usual_empl, fill = l_real_grss_earnings_usual_empl, colour = "usual")) +
  geom_density(aes(x = l_real_grss_earnings_last_empl, fill = l_real_grss_earnings_last_empl, colour = "last"), linetype = "dashed") +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings") +
  scale_colour_manual(name = "Real gross earnings",
                      breaks = c("usual","last"),
                      values = c("usual"="#5A5A5A", "last"="red"))
png("25_plots/check_panel_data/07_earnlast.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

# Split by gender

p <- ggplot(wave12) +
  geom_density(aes(x = l_real_grss_earnings_usual_empl, fill = l_real_grss_earnings_usual_empl, colour = "usual")) +
  geom_density(aes(x = l_real_grss_earnings_last_empl, fill = l_real_grss_earnings_last_empl, colour = "last"), linetype = "dashed") +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings") +
  scale_colour_manual(name = "Real gross earnings",
                      breaks = c("usual","last"),
                      values = c("usual"="#5A5A5A", "last"="red")) + # able to specify colours here
  facet_wrap(~d_sex)
png("25_plots/check_panel_data/07_earnlast_by_sex.png", units = "in", width=16/1.5, height=12/1.5, res=600)
print(p)
dev.off()
#
# ## Difference
#
# wave12[, diffEarn := l_real_grss_earnings_usual - l_real_grss_earnings_last]
# ggplot(wave12, aes(x = diffEarn)) +
#   geom_density()





####### REVIEW THIS ONE - EMPL-SEMPL SPLIT BUT ALSO EMPL AND SEMPL EARNINGS


#######################################################################
### (6) 1 wave usual earnings by employment/self-employment

wave12_semp <- copy(fulldata[wave_no == max(as.numeric(wave_no)),])
wave12_semp <- wave12_semp[ , c("id","d_age_5cat","d_age_12cat","d_sex", "l_real_grss_earnings_usual_empl", "l_econ_stat_7cat")]
wave12_semp <- wave12_semp[! is.na(l_real_grss_earnings_usual_empl)]
wave12_semp <- wave12_semp[! is.na(l_econ_stat_7cat)]
wave12_semp <- wave12_semp[l_econ_stat_7cat %in% c("employed", "self_employed")]

p <- ggplot(wave12_semp) +
  geom_density(aes(x = l_real_grss_earnings_usual_empl, fill = l_econ_stat_7cat), alpha = 0.5) +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
       fill = "Employment \nstatus") +
  facet_wrap(~ d_sex)
png("25_plots/check_panel_data/08_earnsemp_by_sex.png", units = "in", width=16/1.5, height=12/1.5, res=600)
print(p)
dev.off()

#######################################################################
### (6b) All wave usual earnings by employment/self-employment

semp_earn <- copy(fulldata)
semp_earn <- semp_earn[ , c("id","wave_no","d_age_5cat","d_age_12cat","d_sex", "l_real_grss_earnings_usual_empl", "l_econ_stat_7cat")]
semp_earn <- semp_earn[! is.na(l_real_grss_earnings_usual_empl)]
semp_earn <- semp_earn[! is.na(l_econ_stat_7cat)]
semp_earn <- semp_earn[l_econ_stat_7cat %in% c("employed", "self_employed")]

ggplot(semp_earn) +
  geom_density(aes(x = l_real_grss_earnings_usual, fill = l_econ_stat_7cat, alpha = 0.2)) +
  facet_wrap(~ wave_no)

p <- ggplot(semp_earn, aes(x = l_real_grss_earnings_usual_empl, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = l_econ_stat_7cat)) +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
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

anim_save("08_earnsemp_wave_overlay.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by gender

p <- ggplot(semp_earn, aes(x = l_real_grss_earnings_usual_empl, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = l_econ_stat_7cat)) +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
       caption = "Wave: {previous_state}",
       fill = "Employment \nstatus") +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade() +
  shadow_mark(alpha = alpha/4, colour = alpha("grey", .25)) +
  guides(fill = "legend") +
  facet_wrap(~ d_sex)
animate(p, renderer = gifski_renderer(), units = "in", width=20/1.5, height=12/1.5, res=150)

anim_save("08_earnsemp_wave_overlay_by_sex.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by age

p <- ggplot(semp_earn, aes(x = l_real_grss_earnings_usual_empl, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = d_age_5cat)) +
  labs(x = "Real gross earnings",
       y = "Distribution of earnings",
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
  guides(fill = "legend") +
  facet_wrap(~ l_econ_stat_7cat)
animate(p, renderer = gifski_renderer(), units = "in", width=20/1.5, height=12/1.5, res=150)

anim_save("08_earnsemp_age_overlay_by_econstat.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()


#######################################################################
### (7) Distribution of annual earnings

usual_earn_yr <- copy(usual_earn)
usual_earn_yr <- usual_earn_yr[ , .(annual = l_real_grss_earnings_usual_empl*12), by = c("wave_no","d_age_5cat","d_sex")]

#### total
ggplot(usual_earn_yr, aes(x = annual)) +
  geom_density()

ggplot(data = usual_earn_yr, aes(x = annual)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity")

## check???

#### Distribution of earnings (all waves)
p <- ggplot(usual_earn_yr, aes(x = annual)) +
  geom_density() +
  labs(x = "Real gross annual earnings",
       y = "Distribution of Annual Earnings")
png("25_plots/check_panel_data/09_annual_earnings.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

p <- ggplot(data = usual_earn_yr, aes(x = annual)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross annual earnings",
       y = "Distribution of annual earnings",
       fill = "Wave")
png("25_plots/check_panel_data/09_annual_earnings_wave_overlay.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

## By wave distribution stacked !!!!! -> may be better for visualising ? ~ need this one ?
ggplot(usual_earn_yr, aes(x = annual, fill = wave_no, alpha = 0.01)) +
  geom_density(position = "stack")

## By wave distributions faceted by wave
p <- ggplot(usual_earn_yr, aes(x = annual)) +
  geom_density(aes(fill = wave_no, alpha = 0.3)) +
  labs(x = "Real gross annual earnings",
       y = "Distribution of annual earnings",
       fill = "Wave") +
  facet_wrap(~ wave_no)
png("25_plots/check_panel_data/09_annual_earnings_wave_facet.png", units = "in", width=24/1.5, height=18/1.5, res=300)
print(p)
dev.off()

#animated
p <- ggplot(usual_earn_yr, aes(x = annual, fill = factor(wave_no), frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5) +
  labs(x = "Real gross annual earnings",
       y = "Distribution of annual earnings",
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

anim_save("09_annual_earnings_wave_animate.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by age
p <- ggplot(data = usual_earn_yr, aes(x = annual)) +
  geom_density(aes(y = ..density.., fill = `d_age_5cat`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross annual earnings",
       y = "Distribution of annual earnings",
       fill = "Age \ncategory")
png("25_plots/check_panel_data/09_annual_earnings_wave_overlay_by_age.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

#animated
p <- ggplot(usual_earn_yr, aes(x = annual, frame = as.integer(wave_no))) +
  geom_density(alpha = 0.5, aes(fill = d_age_5cat)) +
  labs(x = "Real gross annual earnings",
       y = "Distribution of annual earnings",
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

anim_save("09_annual_earnings_wave_overlay_by_age.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()


## Wave 1, 4, 8, 12 plot
usual_earn_yr_4 <- copy(usual_earn_yr)
usual_earn_yr_4 <- usual_earn_yr_4[wave_no %in% c(1,4,9,12),]

# fill
p <- ggplot(data = usual_earn_yr_4, aes(x = annual)) +
  geom_density(aes(y = ..density.., fill = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross annual earnings",
       y = "Distribution of annual earnings",
       fill = "Wave")
png("25_plots/check_panel_data/09_annual_earnings_4wave_overlay.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()

# line
p <- ggplot(data = usual_earn_yr_4, aes(x = annual)) +
  geom_density(aes(y = ..density.., colour = `wave_no`), alpha = 0.3, stat = "density", position = "identity") +
  labs(x = "Real gross annual earnings",
       y = "Distribution of annual earnings",
       colour = "Wave")
png("25_plots/check_panel_data/09_annual_earnings_4wave_line.png", units = "in", width=12/1.5, height=12/1.5, res=600)
print(p)
dev.off()
