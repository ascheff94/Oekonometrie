#

print("Some simple code.")

library(foreign)
library(dplyr)
library(ggplot2)

c_data <- read.dta("unbundle.dta")
f_data <- read.dta("unbundle_firm.dta")
attach(c_data)
attach(f_data)

summary(c_data)

browseVignettes(package = "dplyr")

### Relevante Variablen ####
#
# sdformalism -> Legal Formalism (Baseline measure [#1] for F_c-proxy)
# ecproccompindex -> Procedural Complexity (measure #2 for F_c-proxy)
# ecnumprocedures -> Number of Procedures (measure #3 for F_c-proxy)
# xcon1990sj -> Constraint on Executive (Basline measure [#1] for I_c-proxy)
# avexpr -> Average Protection Against Expropriation (measure #2 for I_c-proxy)
# efhrpr7 -> Private Property (measure #3 for I_C-proxy)
# loggdppc1995 -> Log GDP per capita in 1995 (PPP measure)
# avgci1990s -> Average investment-GDP ratio
# credittops1998 -> Credit to the private sector
# mcapsj -> Stock market capitalization
# logem4 -> Log settler mortality
# lpd1500s -> Log population density in 1500
#
# ex2col (binär) zeigt an ob das Land Kolonialgeschichte hat.
# sjlouk (binär) zeigt an ob das Land "legal origins" = uk ist.
# sjlofr (binär) zeigt an ob das Land "legal origins" = fr (bzw. Rest) ist.
# 
### IRRELEVANTE VARIABLEN: ####
#
# lat_abst -> Latitude: "It might also be useful to control for latitude because countries that
# are closer to the equator are often argued to be poorer, perhaps because
# of the hotter climate or because they are exposed to more virulent
# diseases.
# logavgcpinflat7098 -> log of average annual inflation, 1970–97
# rich4 -> "Neo-Europes": Australia, Canada, New Zealand, and the U.S.
# averagegovsh5 -> average government consumption (?).
# avgbereale7098 -> exchange rate overvaluation (?).
#
### Notizen ####
#
# rm(list = ls.str(mode = 'function')) // Clearing Glo. Env.
#
# Im Text erwähnt er auf S. 14, Kapitel III., dass als "measure of contracting
# institutions" die "number of procedures for collecting a debt" gut sind,
# da dafür am meisten Daten vorhanden sind.
# Gesucht würde danach mit:
# c_data_ex2col_numb_proc <- c_data %>%
#   filter(ex2col == 1, is.na(ecnumprocedures) == FALSE)
#
# FILTER FÜR c_data ohne die IRRELEVANTEN VARIABLEN:
#
# c_data_filtered <- c_data %>%
# select(!c(lat_abst, rich4, logavgcpinflat7098, averagegovsh5, avgbereale7098,
#          catho80, muslim80, protmg80, no_cpm80))
#
### Testing reproducing Table 1, row = legal formalism: ####
#
# Column: World Sample

world_sdformalism <- c_data %>%
  select(country, sdformalism) %>%
  filter(is.na(sdformalism) == FALSE) %>%
  summarize(mean_world_sdformalism = mean(sdformalism),
            sd_world_sdformalism = sd(sdformalism))
paste(world_sdformalism)

# Column: Ex-Colonies Sample (ex2col)

ex2col_sdformalism <- c_data %>%
  select(country, ex2col, sdformalism) %>%
  filter(ex2col == 1, is.na(sdformalism) == FALSE) %>%
  summarize(mean_ex2col_sdformalism = mean(sdformalism),
            sd_ex2col_sdformalism = sd(sdformalism))
paste(ex2col_sdformalism)

# Column: English Ex-Colonies (sjlouk)

uk_excol_sdformalism <- c_data %>%
  select(country, sjlouk, ex2col, sdformalism) %>%
  filter(sjlouk == 1, ex2col == 1, is.na(sdformalism) == FALSE) %>%
  summarise(mean_uk_excol_sdformalism = mean(sdformalism),
            sd_uk_excol_sdformalism = sd(sdformalism))
paste(uk_excol_sdformalism)

# Column: English Ex-Colonies: Low Settler Mortality

uk_excol_sm_sdformalism <- c_data %>%
  select(country, sjlouk, ex2col, logem4, sdformalism) %>%
  filter(sjlouk == 1, ex2col == 1, is.na(logem4) == FALSE)
median_logem4_uk <- median(uk_excol_sm_sdformalism$logem4)
uk_excol_sm_sdformalism_low <- uk_excol_sm_sdformalism %>%
  filter(logem4 <= median_logem4_uk, is.na(sdformalism) == FALSE) %>%
  summarise(mean_uk_excol_sm_sdformalism_low = mean(sdformalism),
            sd_uk_excol_sm_sdformalism_low = sd(sdformalism))
paste(uk_excol_sm_sdformalism_low)

# Column: English Ex-Colonies: High Settler Mortality

uk_excol_sm_sdformalism <- c_data %>%
  select(sjlouk, ex2col, logem4, sdformalism) %>%
  filter(sjlouk == 1, ex2col == 1, is.na(logem4) == FALSE)
median_logem4_uk <- median(uk_excol_sm_sdformalism$logem4)
uk_excol_sm_sdformalism_high <- uk_excol_sm_sdformalism %>%
  filter(logem4 > median_logem4_uk, is.na(sdformalism) == FALSE) %>%
  summarise(mean_uk_excol_sm_sdformalism_high = mean(sdformalism),
            sd_uk_excol_sm_sdformalism_high = sd(sdformalism))
paste(uk_excol_sm_sdformalism_high)

# Column: French Ex-Colonies (sjlofr)

fr_excol_sdformalism <- c_data %>%
  filter(sjlofr == 1, ex2col == 1, is.na(sdformalism) == FALSE) %>%
  summarise(mean_fr_excol_sdformalism = mean(sdformalism),
            sd_fr_excol_sdformalism = sd(sdformalism))
paste(fr_excol_sdformalism)

# Column: French Ex-Colonies: Low Settler Mortality

fr_excol_sm_sdformalism <- c_data %>%
  select(country, sjlofr, ex2col, logem4, sdformalism) %>%
  filter(sjlofr == 1, ex2col == 1, is.na(logem4) == FALSE)
median_logem4_fr <- median(fr_excol_sm_sdformalism$logem4)
fr_excol_sm_sdformalism_low <- fr_excol_sm_sdformalism %>%
  filter(logem4 <= median_logem4_fr, is.na(sdformalism) == FALSE) %>%
  summarise(mean_fr_excol_sm_sdformalism_low = mean(sdformalism),
            sd_fr_excol_sm_sdformalism_low = sd(sdformalism))
paste(fr_excol_sm_sdformalism_low)

# Column: French Ex-Colonies: High Settler Mortality

fr_excol_sm_sdformalism <- c_data %>%
  select(country, sjlofr, ex2col, logem4, sdformalism) %>%
  filter(sjlofr == 1, ex2col == 1, is.na(logem4) == FALSE)
median_logem4_fr <- median(fr_excol_sm_sdformalism$logem4)
fr_excol_sm_sdformalism_high <- fr_excol_sm_sdformalism %>%
  filter(logem4 > median_logem4_fr, is.na(sdformalism) == FALSE) %>%
  summarise(mean_fr_excol_sm_sdformalism_high = mean(sdformalism),
            sd_fr_excol_sm_sdformalism_high = sd(sdformalism))
paste(fr_excol_sm_sdformalism_high)

### Test auf Genauigkeit: ####
### bei Log GDP per capita in 1995 (PPP measure) auf English Ex-Colonies, 
### English Ex-Colonies with low settler mortality und French Ex-Colonies 
### with high settler mortality

# Column: English Ex-Colonies:

uk_excol_loggdppc1995 <- c_data %>%
  select(country, sjlouk, ex2col, loggdppc1995) %>%
  filter(sjlouk == 1, ex2col == 1, is.na(loggdppc1995) == FALSE) %>%
  summarise(mean_uk_excol_loggdppc1995 = mean(loggdppc1995),
            sd_uk_excol_loggdppc1995 = sd(loggdppc1995))
paste(uk_excol_loggdppc1995)

# Column: English Ex-Colonies with low settler mortality:

uk_excol_sm_loggdppc1995 <- c_data %>%
  select(country, sjlouk, ex2col, logem4, loggdppc1995) %>%
  filter(sjlouk == 1, ex2col == 1, is.na(logem4) == FALSE)
median_logem4_uk <- median(uk_excol_sm_loggdppc1995$logem4)
uk_excol_sm_loggdppc1995_low <- uk_excol_sm_loggdppc1995 %>%
  filter(logem4 <= median_logem4_uk, is.na(loggdppc1995) == FALSE) %>%
  summarise(mean_uk_excol_sm_loggdppc1995_low = mean(loggdppc1995),
            sd_uk_excol_sm_loggdppc1995_low = sd(loggdppc1995))
paste(uk_excol_sm_loggdppc1995_low)

# Column: French Ex-Colonies with high settler mortality:

fr_excol_sm_loggdppc1995 <- c_data %>%
  select(country, sjlofr, ex2col, logem4, loggdppc1995) %>%
  filter(sjlofr == 1, ex2col == 1, is.na(logem4) == FALSE)
median_logem4_fr <- median(fr_excol_sm_loggdppc1995$logem4)
fr_excol_sm_loggdppc1995_high <- fr_excol_sm_loggdppc1995 %>%
  filter(logem4 > median_logem4_fr, is.na(loggdppc1995) == FALSE) %>%
  summarise(mean_fr_excol_sm_loggdppc1995_high = mean(loggdppc1995),
            sd_fr_excol_sm_loggdppc1995_high = sd(loggdppc1995))
paste(fr_excol_sm_loggdppc1995_high)

### Probierecke: #### 