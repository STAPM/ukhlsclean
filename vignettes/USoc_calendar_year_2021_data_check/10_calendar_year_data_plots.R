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

### read in and bind each calendar year dataset

data <- copy(data2021)

data[, wave_no := as.factor(wave_no)]

data_year <- max(data$year)

#######################################################################
### (1) By wave population proportions of highest qualifications

quali <- copy(data)
quali <- quali[! is.na(d_hiqual) & year == data_year , c("id","pop_factor","wave_no","d_sex","d_age_12cat","d_hiqual")]

quali[, d_hiqual := factor(d_hiqual,
                           levels = c("no_qual","other_qual","gcse","alevel","other_he","degree"),
                           labels = c("No qualification","Other qualification","GCSE or equivalent","A-Level or equivalent","Other HE qualifications","Degree"))]


quali_palette <- c("#ade8f4", "#90e0ef", "#48cae4", "#00b4d8", "#0096c7", "#0077b6")

##########
## By Sex

## Frequency

quali_sex <- quali[ , .(sum = sum(pop_factor)), by = c("d_hiqual","wave_no","d_sex")]

ggplot(quali_sex) +
  aes(x = d_sex, y = sum/1000000, fill = d_hiqual) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(y = "Frequency (millions)",
       x = "",
       fill = "Highest \nqualification") +
  theme_custom() +
  scale_fill_manual(values = quali_palette) +
  scale_y_continuous(breaks = 1:10)

## Proportionate

quali_sex <- quali_sex[order(wave_no, d_sex, d_hiqual),]
quali_sex[, prop := sum/sum(sum), by = "d_sex"]
quali_sex[, label_y := cumsum(prop) - 0.5 * prop, by = "d_sex"]

ggplot(quali_sex) +
  aes(x = d_sex, y = prop ) +
  geom_col(aes(fill = forcats::fct_rev(d_hiqual)), color = "black") +
  geom_text(aes(y = label_y, label = paste0(round(prop*100,1),"%") ), size = 3) +
  labs(y = "Cumulative % with qualification",
       x = "",
       fill = "Highest \nqualification") +
  theme_custom() +
  scale_fill_manual(values = quali_palette) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))
ggsave(paste0(path, "/25_plots/01_highest_qual_by_sex.png"),
       units = units, height = height, width = width, dpi = dpi)

################
## By age band

quali_age <- quali[ , .(sum = sum(pop_factor)), by = c("d_hiqual","wave_no","d_age_12cat")]
quali_age <- quali_age[!(d_age_12cat %in% c("16-19","20-24")), ]

quali_age <- quali_age[order(wave_no, d_age_12cat, d_hiqual),]
quali_age[, prop := sum/sum(sum), by = "d_age_12cat"]
quali_age[, label_y := cumsum(prop) - 0.5 * prop, by = "d_age_12cat"]

### NA categories with small bars so they aren't labelled
quali_age[d_age_12cat %in% c("25-29","30-34","35-39","40-44","45-49","50-54") & d_hiqual == "No qualification", label_y := NA]
quali_age[d_age_12cat %in% c("25-29","30-34","35-39","40-44") & d_hiqual == "Other qualification", label_y := NA]


ggplot(quali_age) +
  aes(x = d_age_12cat, y = prop ) +
  geom_col(aes(fill = forcats::fct_rev(d_hiqual)), color = "black") +
  geom_text(aes(y = label_y, label = paste0(round(prop*100),"%") ), size = 3) +
  labs(y = "Cumulative % with qualification",
       x = "",
       fill = "Highest \nqualification") +
  theme_custom() +
  scale_fill_manual(values = quali_palette) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))
ggsave(paste0(path, "/25_plots/01_highest_qual_by_age.png"),
       units = units, height = height, width = width, dpi = dpi)


#######################################################################
### (2) Employment plots

econstat <- copy(data)
econstat <- econstat[! is.na(l_econ_stat_3cat) & year == data_year & d_age %in% 16:64,
                     c("id","pop_factor","wave_no","d_sex","d_age_12cat","d_hiqual","s_current_smoker","l_econ_stat_3cat")]

econstat_palette <- c("#4f772d", "#ff9505", "#a98467")

########################
#### By Sex

## Frequency plots

econstat_sex <- econstat[ , .(sum = sum(pop_factor)), by = c("l_econ_stat_3cat","d_sex")]

ggplot(econstat_sex) +
  aes(x = d_sex, y = sum/1000000, fill = l_econ_stat_3cat) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(y = "Frequency (millions)",
       x = "",
       fill = "Economic status") +
  theme_custom() +
  scale_fill_manual(values = econstat_palette) +
  scale_y_continuous(breaks = 1:20)

## proportionate plots

econstat_sex <- econstat_sex[order(d_sex, l_econ_stat_3cat),]
econstat_sex[, prop := sum/sum(sum), by = "d_sex"]
econstat_sex[, label_y := cumsum(prop) - 0.5 * prop, by = "d_sex"]

econstat_palette <- c("#a98467", "#ff9505", "#4f772d")

ggplot(econstat_sex) +
  aes(x = d_sex, y = prop ) +
  geom_col(aes(fill = forcats::fct_rev(l_econ_stat_3cat)), color = "black") +
  geom_text(aes(y = label_y, label = paste0(round(prop*100,1),"%") ), size = 4) +
  labs(y = "Cumulative %",
       x = "",
       fill = "Economic status") +
  theme_custom() +
  scale_fill_manual(values = econstat_palette) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))
ggsave(paste0(path, "/25_plots/02_econstat_by_sex.png"),
       units = units, height = height, width = width, dpi = dpi)

##################
### By Age

econstat_age <- econstat[ , .(sum = sum(pop_factor)), by = c("l_econ_stat_3cat","d_age_12cat")]

econstat_age <- econstat_age[order(d_age_12cat, l_econ_stat_3cat),]
econstat_age[, prop := sum/sum(sum), by = "d_age_12cat"]
econstat_age[, label_y := cumsum(prop) - 0.5 * prop, by = "d_age_12cat"]

ggplot(econstat_age) +
  aes(x = d_age_12cat, y = prop ) +
  geom_col(aes(fill = forcats::fct_rev(l_econ_stat_3cat)), color = "black") +
  geom_text(aes(y = label_y, label = paste0(round(prop*100,1),"%") ), size = 3) +
  labs(y = "Cumulative %",
       x = "Age category",
       fill = "Economic status") +
  theme_custom() +
  scale_fill_manual(values = econstat_palette) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))
ggsave(paste0(path, "/25_plots/02_econstat_by_age.png"),
       units = units, height = height, width = width, dpi = dpi)

##################
### By highest qualification

econstat_qual <- econstat[ , .(sum = sum(pop_factor)), by = c("l_econ_stat_3cat","d_hiqual")]

econstat_qual <- econstat_qual[!is.na(d_hiqual),]
econstat_qual[, d_hiqual := factor(d_hiqual,
                                   levels = c("no_qual","other_qual","gcse","alevel","other_he","degree"),
                                   labels = c("No qualification","Other qualification","GCSE or equivalent","A-Level or equivalent","Other HE qualifications","Degree"))]

econstat_qual <- econstat_qual[order(d_hiqual, l_econ_stat_3cat),]
econstat_qual[, prop := sum/sum(sum), by = "d_hiqual"]
econstat_qual[, label_y := cumsum(prop) - 0.5 * prop, by = "d_hiqual"]

ggplot(econstat_qual) +
  aes(x = d_hiqual, y = prop ) +
  geom_col(aes(fill = forcats::fct_rev(l_econ_stat_3cat)), color = "black") +
  geom_text(aes(y = label_y, label = paste0(round(prop*100,1),"%") ), size = 3) +
  labs(y = "Cumulative %",
       x = "",
       fill = "Economic status") +
  theme_custom() +
  scale_fill_manual(values = econstat_palette) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1)) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
ggsave(paste0(path, "/25_plots/02_econstat_by_qual.png"),
       units = units, height = height, width = width, dpi = dpi)


##################
### By smoker status

econstat_smk <- econstat[ , .(sum = sum(pop_factor)), by = c("l_econ_stat_3cat","s_current_smoker")]

econstat_smk <- econstat_smk[!is.na(s_current_smoker),]
econstat_smk[, s_current_smoker := factor(s_current_smoker,
                                          levels = c("non_smoker","smoker"),
                                          labels = c("Current non-smoker","Current smoker"))]

econstat_smk <- econstat_smk[order(s_current_smoker, l_econ_stat_3cat),]
econstat_smk[, prop := sum/sum(sum), by = "s_current_smoker"]
econstat_smk[, label_y := cumsum(prop) - 0.5 * prop, by = "s_current_smoker"]

ggplot(econstat_smk) +
  aes(x = s_current_smoker, y = prop ) +
  geom_col(aes(fill = forcats::fct_rev(l_econ_stat_3cat)), color = "black") +
  geom_text(aes(y = label_y, label = paste0(round(prop*100,1),"%") ), size = 3) +
  labs(y = "Cumulative %",
       x = "",
       fill = "Economic status") +
  theme_custom() +
  scale_fill_manual(values = econstat_palette) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))
ggsave(paste0(path, "/25_plots/02_econstat_by_smkstat.png"),
       units = units, height = height, width = width, dpi = dpi)


#######################################################################
### (3) Earnings distribution plots (for employment earnings only)

earn <- copy(data)
earn <- earn[l_econ_stat_7cat == "employed" & year == data_year & d_age %in% 16:64,
              c("id","pop_factor","weight_xw","d_sex","l_grss_earnings_last_empl","l_grss_earnings_usual_empl")]
earn[, l_grss_earnings_last := l_grss_earnings_last_empl*12]
earn[, l_grss_earnings_usual := l_grss_earnings_usual_empl*12]

### adjust to April of the calendar year

cpi_index <- as.numeric(ukhlsclean::cpih[month == 4 & year == data_year,"index"])

earn[, l_grss_earnings_last  := l_grss_earnings_last*(cpi_index/100)]
earn[, l_grss_earnings_usual := l_grss_earnings_usual*(cpi_index/100)]


################################################
## Aggregate distribution of gross annual pay

mean <- weighted.mean(earn$l_grss_earnings_usual, w = earn$weight_xw, na.rm = TRUE)
median <- median(earn$l_grss_earnings_usual, na.rm = TRUE)

ggplot(earn) +
  geom_density(aes(x = l_grss_earnings_usual/1000), fill = "#a7c957", alpha = 0.2) +
  geom_vline(xintercept = median/1000, color = "#27187e", linetype = 2, linewidth = 1) +
  geom_vline(xintercept = mean/1000, color = "#758bfd", linetype = 2, linewidth = 1) +
  #geom_text() +
  labs(y = "", x = "Gross Annual Earnings (000s)",
       caption = paste0("Earnings inflated to April ",data_year," | Note that earnings are top-coded to £100,000 per annum")) +
  theme_custom() +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = seq(0,100,10) , labels=scales::dollar_format(prefix = "£"))
ggsave(paste0(path, "/25_plots/03_earnings_distribution.png"),
       units = units, height = height, width = width, dpi = dpi)







