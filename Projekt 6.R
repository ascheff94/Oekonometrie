### Intro ####

library(foreign)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ivreg)

c_data <- read.dta("unbundle.dta")
f_data <- read.dta("unbundle_firm.dta")
attach(c_data)

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
### Table 1 Funktionen: ####
### def function für World Sample:

world_sample <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, {{column_name}}) %>%
    filter(is.na({{column_name}}) == FALSE) %>%
    summarise(mean_world = mean({{column_name}}),
              sd_world = sd({{column_name}}))
  paste(y)
}
### def function Ex-Colonies Sample (ex2col):

ex2col <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, ex2col, {{column_name}}) %>%
    filter(ex2col == 1, is.na({{column_name}}) == FALSE) %>%
    summarise(mean_ex2col = mean({{column_name}}),
              sd_ex2col = sd({{column_name}}))
  paste(y)
}
### def function English Ex-Colonies Sample (sjlouk):

excol_uk <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, sjlouk, ex2col, {{column_name}}) %>%
    filter(sjlouk == 1, ex2col == 1, is.na({{column_name}}) == FALSE) %>%
    summarise(mean_excol_uk = mean({{column_name}}),
              sd_excol_uk = sd({{column_name}}))
  paste(y)
}
### def function English Ex-Colonies: Low Setter Mortality:

excol_uk_sm_low <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, sjlouk, ex2col, logem4, {{column_name}}) %>%
    filter(sjlouk == 1, ex2col == 1, is.na(logem4) == FALSE)
  median_logem4_uk <- median(y$logem4)
  excol_uk_low <- y %>%
    filter(logem4 <= median_logem4_uk, is.na({{column_name}}) == FALSE) %>%
    summarise(mean_excol_uk_low = mean({{column_name}}),
              sd_excol_uk_low = sd({{column_name}}))
  paste(excol_uk_low)
}
### def function English Ex-Colonies: High Settler Mortality:

excol_uk_sm_high <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, sjlouk, ex2col, logem4, {{column_name}}) %>%
    filter(sjlouk == 1, ex2col == 1, is.na(logem4) == FALSE)
  median_logem4_uk <- median(y$logem4)
  excol_uk_high <- y %>%
    filter(logem4 > median_logem4_uk, is.na({{column_name}}) == FALSE) %>%
    summarise(mean_excol_uk_high = mean({{column_name}}),
              sd_excol_uk_high = sd({{column_name}}))
  paste(excol_uk_high)
}
### def function French Ex-Colonies Sample (sjlofr):

excol_fr <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, sjlofr, ex2col, {{column_name}}) %>%
    filter(sjlofr == 1, ex2col == 1, is.na({{column_name}}) == FALSE) %>%
    summarise(mean_excol_fr = mean({{column_name}}),
              sd_excol_fr = sd({{column_name}}))
  paste(y)
}
### def function French Ex-Colonies: Low Settler Mortality:

excol_fr_sm_low <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, sjlofr, ex2col, logem4, {{column_name}}) %>%
    filter(sjlofr == 1, ex2col == 1, is.na(logem4) == FALSE)
  median_logem4_fr <- median(y$logem4)
  excol_fr_low <- y %>%
    filter(logem4 <= median_logem4_fr, is.na({{column_name}}) == FALSE) %>%
    summarise(mean_excol_fr_low = mean({{column_name}}),
              sd_excol_fr_low = sd({{column_name}}))
  paste(excol_fr_low)
}
### def function French Ex-Colonies: High Settler Mortality:

excol_fr_sm_high <- function(dataframe, column_name){
  y <- dataframe %>%
    select(country, sjlofr, ex2col, logem4, {{column_name}}) %>%
    filter(sjlofr == 1, ex2col == 1, is.na(logem4) == FALSE)
  median_logem4_fr <- median(y$logem4)
  excol_fr_high <- y %>%
    filter(logem4 > median_logem4_fr, is.na({{column_name}}) == FALSE) %>%
    summarise(mean_excol_fr_high = mean({{column_name}}),
              sd_excol_fr_high = sd({{column_name}}))
  paste(excol_fr_high)
}
### Table 2, Panel A&B:  ####
#
# Dependent Variables:    -Log GDP per Capita in 1995 (Panel A)
#                           [loggdppc1995]
#                         -Average Ratio of Investment to GDP in 1990s (Panel A)
#                           [avgci1990s]
#                         -Credit to the Private Sector (Panel B)
#                           [credittops1998]
#                         -Stock Market Capitalization, Average Over '90 - '95 (Panel B)
#                           [mcapsj]
#
# Explanatory Variables:  -Legal Formalism [sdformalism]
#                         -Procedural Complexity [ecproccompindex]
#                         -Number of Procedures [ecnumprocedures]
#
# World OLS:

Table_2_World_Sample_OLS <- function(dep_var, exp_var){
  x <- lm({{dep_var}}~ {{exp_var}}, data = c_data)
  x <- summary(x)
  print(x)
}

# Example: 

Table_2_World_Sample(dep_var = loggdppc1995, exp_var = sdformalism)

# Excol OLS:

Table_2_Excol_OLS <- function(dep_var, exp_var){
  x <- lm({{dep_var}}~ {{exp_var}}, data = c_data, subset = ex2col == 1)
  x <- summary(x)
  print(x)
}

# Example: 

Table_2_Excol_OLS(dep_var = loggdppc1995, exp_var = sdformalism)

# Excol 2SLS:

Table_2_Excol_2SLS <- function(dep_var, exp_var){
  x <- ivreg({{dep_var}}~ {{exp_var}} | sjlouk, data = c_data, subset = ex2col == 1)
  x2 <- summary(x)
  print(x2)
}

# Example: 

Table_2_Excol_2SLS(dep_var = loggdppc1995, exp_var = sdformalism)

### Table 3, Panel A: ####
#
# Dependent Variables:    -Legal Formalism [sdformalism]
#                         -Procedural Complexity [ecproccompindex]
#                         -Number of Procedures [ecnumprocedures]
#
# Explanatory Variables:  -Log Settler Mortality [logem4]
#                         -Log Population Density in 1500 [lpd1500s]
#

### Probierecke: #### 

Table_3_Panel_A <- function(dep_var, exp_var){
  x <- lm({{dep_var}}~ sjlouk + {{exp_var}}, data = c_data, subset = ex2col == 1)
  x2 <- summary(x)
  print(x2)
}

help(ivreg)

Table_3_Panel_A(dep_var = sdformalism, exp_var = logem4)

Test <- c_data %>%
  filter(ex2col == 1)
x <- lm(sdformalism~ sjlouk + logem4, data = Test)
x2 <- summary(x)
print(x2)