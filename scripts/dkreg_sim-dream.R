#SIMULERET REGISTERDATA#
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(haven)
library(sas7bdat)
library(SASxport)
library(ggplot2)
library(TraMineR)
library(readxl)

#--#PARAMETERS#--#

#File paths#
mat_path <- file.path('.', 'material', sep = "/")
data_outpath <- file.path('.', 'simdata', sep = "/")

#Number of observations#
act_n <- 12967

#PNR#
pnr_vec <- as.character(base::sample(c(1:99999999), size = act_n, replace = FALSE)) %>%
  str_pad(., 8, side = "left", pad = "0")


#--#MAIN FUNCTIONS#--#
##DAN VEKTORER##
create_katvec <- function(val, size, prob = NULL) {
  katvec <- base::sample(val, size, replace = TRUE, prob = prob)
  return(katvec)
}

##DAN ÅRSDATASÆT##
create_simdata <- function(pnr, valproblist, size) {
  varval <- lapply(valproblist, `[[`, "values")
  varprob <- lapply(valproblist, `[[`, "probs")
  output_list <- list()
  for (i in 1:length(varprob)) {
    output_list[[i]] <- create_katvec(varval[[i]], size = size, prob = varprob[[i]])
  }
  output_list$pnr <- pnr
  outputdata <- as.data.frame(output_list, stringsAsFactors = FALSE)
  colnames(outputdata) <- c(names(valproblist), "PNR")
  return(outputdata)
}

#--#DREAM VARIABLE#--#

##BRANCHE##

#brug af seqgen (traminer)

branch_months <- seq(ymd('2010-01-01'), ymd('2015-12-12'), by = 'month')
branch_months <- substr(branch_months, 1, 7)
branch_months <- gsub("-", "_", branch_months)
branch_months <- paste0('br_', branch_months)

branch_val_df <- read_delim(paste0(mat_path, 'db07.csv'), delim = ";") %>%
  mutate(db2cif = floor(db07/10000)) %>%
  arrange(db2cif)

branch2cif_val <- branch_val_df$db2cif

db07_db10vec <- c(rep.int(1, length(which(branch2cif_val < 4))),
                  rep.int(2, length(which(branch2cif_val >= 4 & branch2cif_val < 41))),
                  rep.int(3, length(which(branch2cif_val >= 41 & branch2cif_val < 45))),
                  rep.int(4, length(which(branch2cif_val >= 45 & branch2cif_val < 58))),
                  rep.int(5, length(which(branch2cif_val >= 58 & branch2cif_val < 64))),
                  rep.int(6, length(which(branch2cif_val >= 64 & branch2cif_val < 68))),
                  rep.int(7, length(which(branch2cif_val >= 68 & branch2cif_val < 69))),
                  rep.int(8, length(which(branch2cif_val >= 69 & branch2cif_val < 84))),
                  rep.int(9, length(which(branch2cif_val >= 84 & branch2cif_val < 90))),
                  rep.int(10, length(which(branch2cif_val >= 90 & branch2cif_val < 100))))

db07katprob <- data.frame(db10g = seq(10),
                          db10g_label = c('Landbrug, skovbrug og fiskeri', 'Industri, råstofindvinding og forsyningsvirksomhed', 'Bygge og anlæg', 'Handel og transport mv.',
                                          'Information og kommunikation', 'Finansiering og forsikring', 'Ejendomshandel og udlejning', 'Erhvervsservice', 
                                          'Offentlig administration, undervisning og sundhed', 'Kultur, fritid og anden service'),
                          db10g_prob = c(59, 375, 156, 587, 103, 8, 25, 221, 900, 131) / sum(c(59, 375, 156, 587, 103, 8, 25, 221, 900, 131)), stringsAsFactors = FALSE) #2010K1

branch_val_df <- branch_val_df %>%
  mutate(db10g = db07_db10vec) %>%
  select(db07, label, db10g) %>%
  group_by(db10g) %>%
  mutate(db10g_count = n()) %>%
  inner_join(db07katprob, by = 'db10g') %>%
  mutate(db07_prob = db10g_prob / db10g_count)

branch_val <- branch_val_df$db07
branch_prob <- branch_val_df$db07_prob

#generate sequences#
dream_branch <- data.frame(seqgen(length(BEF_2015$PNR), length(branch_months), branch_val, branch_prob))
colnames(dream_branch) <- branch_months


#filter pnr - gøres til sidst#
dream_branch$PNR <- base::sample(BEF_2015$PNR, length(BEF_2015$PNR), replace = TRUE)
dream_branch <- dream_branch[, c('PNR', branch_months)]

#remove random pnr - gøres til sidst#
for (i in 2:length(dream_branch)) {
  row_ind <- base::sample(c(1:12447), floor(0.1 * 12447), replace = FALSE)
  dream_branch[row_ind, i] <- NA
}

#SAS Options#
library(SASxport)
label(dream_branch) <- 'DREAM_sim_2015'

#Gem DREAM datasæt#
setwd(save_path)

write.xport(dream_branch, file = paste0("DREAM_sim_2015", ".xpt"))
write_csv(dream_branch, path = paste0('DREAM_sim_2015', '.csv'))
write_dta(dream_branch, "dream_sim_2015.dta")


##DREAM##
dream_weeks <- expand.grid(
  c(as.character(seq(97, 99, by = 1)), 
    str_pad(as.character(seq(0, 15, by = 1)), 2, side = "left", pad = "0")), 
  str_pad(as.character(seq(1, 52, by = 1)), 2, side = "left", pad = "0")) %>%
  arrange(Var1, Var2)
dream_weeks <- paste0('y_', dream_weeks$Var1, dream_weeks$Var2)

#Indlæs dream koder og sandsynligheder
dreamprobs_df <- read_excel(file.path(mat_path, "dream_prob-y.xlsx"))
dream_val <- dreamprobs_df$ycode
dream_prob <- dreamprobs_df$prob

# Indlæs eksisterende PNR
datap <- file.path("D:", "OneDrive", "OneDrive - Aalborg Universitet", 
                   "CALDISS_projects", "reg_sim", "simdata", "simuleret data",
                   "stata", sep = "/")

dream_df <- read_dta(file.path(datap, "dream_sim_2015.dta"))

# Dan sekvenser
dream_y <- data.frame(seqgen(length(dream_df$PNR), length(dream_weeks), dream_val, dream_prob))
colnames(dream_y) <- dream_weeks
dream_y$PNR <- dream_df$PNR
dream_y <- select(dream_y, PNR, everything())

# Gem data
write_sas(dream_y, file.path(data_outpath, "dream_sim_y_97-15.sas7bdat"))
