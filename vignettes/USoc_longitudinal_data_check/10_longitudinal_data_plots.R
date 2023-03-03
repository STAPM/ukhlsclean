library(ggplot2)
library(ukhlsclean)
library(data.table)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)) )
}


#######################
#### read in the data

### plot dimensions

units <- "in"
width <- 10/1.5
height <- 5/1.5
dpi <- 600


# Factorise wave numbers
fulldata[ , wave_no := as.factor(wave_no)]

### colour palette
#colourCount = 12
#getPalette = colorRampPalette(brewer.pal(11, "PuOr"))#(colourCount)

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
  scale_fill_viridis_d(direction = -1)
ggsave(paste0(path,"25_plots/01_highest_qual.png"),
       units = units, height = height, width = width, dpi = dpi)

# By gender

ggplot(data = quali, aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual))) +
  geom_col() +
  facet_wrap(~ d_sex) +
  #theme_custom() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Highest \nqualification") +
  scale_fill_viridis_d(direction = -1)
ggsave(paste0(path,"25_plots/01_highest_qual_by_sex.png"),
       units = units, height = height, width = width, dpi = dpi)

# By age (broken ? )

ggplot(data = quali, aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual))) +
  geom_col() +
  facet_wrap(~ d_age_12cat) +
  #theme_custom() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Highest \nqualification") +
  scale_fill_viridis_d(direction = -1)
ggsave(paste0(path,"25_plots/01_highest_qual_by_age.png"))

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




######################
## Proportionate plots

p <-ggplot(data = quali[! is.na(d_hiqual), ], aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual) )) +
  geom_col(position = "fill") +
  labs(y = "Cumulative %", x = "Wave", fill = "Highest \nqualification") +
  scale_fill_viridis_d(direction = -1)
print(p)
png("25_plots/check_panel_data/01_highest_qual_proportion.png", units=units, width=width, height=height, res=600)
dev.off()

# Split by gender

ggplot(data = quali[! is.na(d_hiqual), ], aes(x = wave_no, y = sum, fill = forcats::fct_rev(d_hiqual))) +
  geom_col(position = "fill") +
  labs(y = "Cumulative %", x = "Wave", fill = "Highest \nqualification") +
  facet_wrap(~ d_sex) +
  scale_fill_viridis_d(direction = -1)
ggsave("25_plots/check_panel_data/01_highest_qual_by_sex_proportion.png", units = units, height = height, width = width, dpi = dpi)

# Split by age

quali_age <- copy(fulldata)
quali_age <- quali_age[! is.na(d_hiqual) , c("id","wave_no","d_age_12cat", "d_hiqual")]
quali_age <- quali_age[order(wave_no, d_age_12cat), ]
quali_age <- quali_age[, .N, keyby = c("d_hiqual","wave_no")]

quali_age[, prop := N/sum(N), by = c("wave_no")]
quali_age[, label_y := (cumsum(prop) - 0.5 * prop), by = "wave_no"]

ggplot(data = quali_age, aes(x = wave_no, y = N, fill = forcats::fct_rev(d_hiqual))) +
  geom_col(position = "fill") +
  labs(y = "Cumulative %", x = "Wave", fill = "Highest \nqualification") +
  geom_text(aes(y = label_y, label = paste0(round(prop*100,1),"%"), colour = d_hiqual), size = 3) +
  scale_fill_viridis_d(direction = -1) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  guides(colour = "none")
#scale_colour_viridis_d(option = "E", direction = -1)
ggsave("25_plots/check_panel_data/01_highest_qual_by_age_proportion.png", units = "in", width = 12/1.5, height = 12/1.5)

# Age animated

p <- ggplot(data = quali, aes(x = d_age_12cat, y = sum, fill = forcats::fct_rev(d_hiqual), frame = as.integer(wave_no))) +
  labs(x="Age Category", y = "Cumulative %",
       caption = "Wave: {previous_state}",
       fill = "Highest \nqualification") +
  scale_fill_viridis_d(direction = -1) +
  geom_col(position = "fill") +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 0.5,
                    state_length = 1) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(p, renderer = gifski_renderer())

anim_save("01_highest_qual_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

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
  scale_fill_brewer(palette = "Paired")
ggsave("25_plots/check_panel_data/02_region.png", units = "in", width = 12/1.5, height = 12/1.5)

# Split by gender

ggplot(data = region, aes(x = wave_no, y = N, fill = d_gor)) +
  geom_col() +
  facet_wrap(~ d_sex) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Region") +
  scale_fill_brewer(palette = "Paired")
ggsave("25_plots/check_panel_data/02_region_by_sex.png", units = units, height = height, width = width, dpi = dpi)

ggplot(data = region, aes(x = d_sex, y = N, fill = d_gor)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Paired")

#animated

p <- ggplot(data = region, aes(x = d_sex, y = N, fill = d_gor, frame = as.integer(wave_no))) +
  labs(x="Gender", y = "Frequency",
       caption = "Wave: {previous_state}",
       fill = "Region") +
  scale_fill_brewer(palette = "Paired") +
  geom_col() +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 0.5,
                    state_length = 1) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(p, renderer = gifski_renderer())

anim_save("02_region_by_sex.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by age

ggplot(data = region, aes(x = d_age_12cat, y = N, fill = d_gor)) +
  geom_col() +
  facet_wrap(~ wave_no) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Region") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Paired")

ggsave("25_plots/check_panel_data/02_region_by_age.png")

#animated
p <- ggplot(data = region, aes(x = d_age_12cat, y = N, fill = d_gor, frame = as.integer(wave_no))) +
  labs(x="Age category", y = "Frequency",
       caption = "Wave: {closest_state}",
       fill = "Region") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Paired") +
  geom_col() +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 0.5,
                    state_length = 1) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(p, renderer = gifski_renderer())

anim_save("02_region_by_age.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()


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
  scale_fill_brewer(palette = "Paired") +
  scale_colour_manual(values = rep("#000000", 12))
ggsave("25_plots/check_panel_data/02_region_proportion.png")

# Split by gender

ggplot(data = region, aes(x = wave_no, y = N, fill = d_gor)) +
  geom_col(position = "fill") +
  facet_wrap(~ d_sex) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Region") +
  scale_fill_brewer(palette = "Paired")
ggsave("25_plots/check_panel_data/02_region_by_sex_proportion.png")

#animated
region_sex <- copy(region)
region_sex <- region_sex[order(wave_no, d_sex, d_gor), ]
region_sex <- region_sex[, .(N = sum(N)), by = .(wave_no, d_sex, d_gor)]
region_sex <- region_sex[, prop := N/sum(N), by = c("wave_no", "d_sex")]
region_sex[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_sex")]

p <- ggplot(data = region_sex, aes(x = d_sex, y = N, fill = d_gor, frame = as.integer(wave_no))) +
  labs(x="Gender", y = "Frequency",
       caption = "Wave: {closest_state}",
       fill = "Region") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = d_gor), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("exponential-out") +
  enter_fade() +
  exit_fade() +
  scale_fill_brewer(palette = "Paired") +
  scale_colour_manual(values = rep("#000000", 12))

animate(p, renderer = gifski_renderer())

anim_save("02_region_by_sex_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Paired")
ggsave("25_plots/check_panel_data/02_region_by_age_proportion.png")

p <- ggplot(data = region_age, aes(x = d_age_12cat, y = N, fill = d_gor, frame = as.integer(wave_no))) +
  labs(x="Age category", y = "Cumulative %",
       caption = "Wave: {closest_state}",
       fill = "Region") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = d_gor), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("exponential-out") +
  enter_fade() +
  exit_fade() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Paired") +
  scale_colour_manual(values = rep("#000000", 12))

animate(p, renderer = gifski_renderer())

anim_save("02_region_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()


#######################################################################
### (3) By wave population proportions of economic statuses

econstat <- copy(fulldata)
econstat <- econstat[ , c("id","wave_no","d_age_12cat","d_sex", "l_econ_stat_7cat")]
econstat <- econstat[ , .N, by = .(wave_no, l_econ_stat_7cat, d_sex, d_age_12cat)]


## Frequency plots

p <- ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Economic \nstatus") +
  scale_fill_viridis_d(option = "plasma")
#ggsave("25_plots/check_panel_data/03_econstat.png")
png(file = "25_plots/check_panel_data/03_econstat.png", units = "in", width=10/1.5, height=6/1.5, res=600)
print(p)
dev.off()

# Split by gender

p <- ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col() +
  facet_wrap(~ d_sex) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Economic \nstatus") +
  scale_fill_viridis_d(option = "plasma")
png("25_plots/check_panel_data/03_econstat_by_sex.png", units = "in", width=10/1.5, height=6/1.5, res=600)
print(p)
dev.off()

# Split by age

p <- ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col() +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Economic \nstatus") +
  scale_fill_viridis_d(option = "plasma")
png("25_plots/check_panel_data/03_econstat_by_age.png", units = "in", width=15/1.5, height=12/1.5, res=600)
print(p)
dev.off()

## Proportionate plots

econstat_prop <- copy(econstat)
econstat_prop <- econstat_prop[order(wave_no, l_econ_stat_7cat), ]
econstat_prop <- econstat_prop[, .(N = sum(N)), by = .(wave_no, l_econ_stat_7cat)]
econstat_prop <- econstat_prop[, prop := N/sum(N), by = c("wave_no")]
econstat_prop[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no")]

p <- ggplot(data = econstat_prop, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = l_econ_stat_7cat), size = 3, show.legend = FALSE) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Economic /nstatus") +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))
png("25_plots/check_panel_data/03_econstat_proportion.png", units = "in", width=12/1.5, height=9/1.5, res=600)
print(p)
dev.off()

# Split by gender

p <- ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col(position = "fill") +
  facet_wrap(~ d_sex) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Economic \nstatus") +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))
png("25_plots/check_panel_data/03_econstat_by_sex_proportion.png", units = "in", width=18/1.5, height=9/1.5, res=600)
print(p)
dev.off()

#animated
econstat_sex <- copy(econstat)
econstat_sex <- econstat_sex[order(wave_no, d_sex, l_econ_stat_7cat), ]
econstat_sex <- econstat_sex[, .(N = sum(N)), by = .(wave_no, d_sex, l_econ_stat_7cat)]
econstat_sex <- econstat_sex[, prop := N/sum(N), by = c("wave_no", "d_sex")]
econstat_sex[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_sex")]

p <- ggplot(data = econstat_sex, aes(x = d_sex, y = N, fill = l_econ_stat_7cat, frame = as.integer(wave_no))) +
  labs(x="Gender", y = "Cumulative %",
       caption = "Wave: {closest_state}",
       fill = "Economic status") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = l_econ_stat_7cat), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("quadratic-in-out") +
  enter_fade() +
  exit_fade() +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))

animate(p, renderer = gifski_renderer())

anim_save("03_econstat_by_sex_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()

# Split by age

p <- ggplot(data = econstat, aes(x = wave_no, y = N, fill = l_econ_stat_7cat)) +
  geom_col(position = "fill") +
  facet_wrap(~ d_age_12cat) +
  labs(y = "Cumulative %",
       x = "Wave",
       fill = "Economic \nstatus") +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))
png("25_plots/check_panel_data/03_econstat_by_age_proportion.png", units = "in", width=18/1.5, height=9/1.5, res=600)
print(p)
dev.off()

#animated
econstat_age <- copy(econstat)
econstat_age <- econstat_age[order(wave_no, d_age_12cat, l_econ_stat_7cat), ]
econstat_age <- econstat_age[, .(N = sum(N)), by = .(wave_no, d_age_12cat, l_econ_stat_7cat)]
econstat_age <- econstat_age[, prop := N/sum(N), by = c("wave_no", "d_age_12cat")]
econstat_age[, label_y := 1-((cumsum(prop) - 0.5 * prop)), by = c("wave_no", "d_age_12cat")]

p <- ggplot(data = econstat_age, aes(x = d_age_12cat, y = N, fill = l_econ_stat_7cat, frame = as.integer(wave_no))) +
  labs(x="Age category", y = "Cumulative %",
       caption = "Wave: {closest_state}",
       fill = "Economic status") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = l_econ_stat_7cat), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("quadratic-in-out") +
  enter_fade() +
  exit_fade() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_manual(values = c(rep("#FFFFFF",3),rep("#000000",4)))

animate(p, renderer = gifski_renderer(), units = "in", width=14/1.5, height=16/1.5, res=150)

anim_save("03_econstat_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()


#######################################################################
### (4) By wave population proportions of abstention

abstain <- copy(fulldata)
abstain <- abstain[ , c("id","wave_no","d_age_12cat","d_sex", "a_current_abstainer", "a_always_abstainer")]
abstain <- abstain[ , .N, by = .(wave_no, a_current_abstainer, d_sex, d_age_12cat)]
abstain <- abstain[! is.na(a_current_abstainer),]

# Retaining wave 7, 9 , 11
abstain <- abstain[wave_no %in% c(7, 9 , 11), ]

## Frequency plots

p <- ggplot(data = abstain, aes(x = wave_no, y = N, fill = a_current_abstainer)) +
  geom_col() +
  labs(y = "Frequency",
       x = "Wave",
       fill = "Abstainers") +
  scale_fill_manual(values = c("#cc4704","#49a303"))
png(file = "25_plots/check_panel_data/04_abstain.png", units = "in", width=10/1.5, height=6/1.5, res=600)
print(p)
dev.off()

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

#animated
p <- ggplot(data = abstain_sex, aes(x = d_sex, y = N, fill = a_current_abstainer, frame = as.integer(wave_no))) +
  labs(x="Gender", y = "Cumulative %",
       caption = "Wave: {closest_state}",
       fill = "Abstention rate") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = a_current_abstainer), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("quadratic-in-out") +
  enter_fade() +
  exit_fade() +
  scale_fill_manual(values = c("#cc4704","#49a303")) +
  scale_colour_manual(values = rep("#FFFFFF", 2))
animate(p, renderer = gifski_renderer())

anim_save("04_abstain_by_sex_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
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

#animated
p <- ggplot(data = abstain_age, aes(x = d_age_12cat, y = N, fill = a_current_abstainer, frame = as.integer(wave_no))) +
  labs(x="Age category", y = "Cumulative %",
       caption = "Wave: {closest_state}",
       fill = "Abstention \nrate") +
  geom_col(position = "fill") +
  geom_text(aes(y = label_y, label = as.factor(paste0(round(prop*100,1),"%")), colour = a_current_abstainer), size = 3, show.legend = FALSE) +
  transition_time(as.integer(wave_no)) +
  transition_states(as.integer(wave_no),
                    transition_length = 2,
                    state_length = 2) +
  ease_aes("quadratic-in-out") +
  enter_fade() +
  exit_fade() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("#cc4704","#49a303")) +
  scale_colour_manual(values = rep("#FFFFFF", 2))
animate(p, renderer = gifski_renderer(), units = "in", width=14/1.5, height=8/1.5, res=150)

anim_save("04_abstain_by_age_proportion.gif", animation = last_animation(), path = "25_plots/check_panel_data/")
dev.off()



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



#### WIP
#######################################################################
### (5) By wave distribution of real usual earnings

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
### (6) 1 wave distribution of real usual vs last earnings

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
