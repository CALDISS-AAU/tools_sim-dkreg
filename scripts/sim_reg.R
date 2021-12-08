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

#--#PARAMETERS#--#

#File paths#
mat_path <- file.path('~', 'material', sep = "/")
data_outpath <- file.path('~', 'simdata', sep = "/")

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


#--#BEF DATASÆT#--#

#FOED_DAG#
bdates_val <- seq(ymd('1935/01/01'), ymd('1992/01/01'), by="day")
bdates_prob <- sort(rnorm(length(bdates_val), mean = 0, sd = 1))
bdates_prob <- bdates_prob + (min(bdates_prob) * -1)
bdates_prob <- bdates_prob / max(bdates_prob)

bdates_valprob <- list(values = bdates_val, probs = bdates_prob)

#IE_TYPE#
ie_val <- c(dansk = 1, indvandrere = 2, efterkommere = 3)
ie_prob <- c(dansk = 4992000/5534738, indvandrere = 414422/5534738, efterkommere = 128318/5534738)

ie_valprob <- list(values = ie_val, probs = ie_prob)

#KOM#
kom_val_df <- read_delim(paste0(mat_path, "kom", ".txt"), delim = ":", col_names = FALSE) %>%
  mutate(code = gsub("^0.*9999", "", X1)) %>%
  mutate(code = str_trim(code), 
         name = X2) %>%
  filter(nchar(code) < 4) %>%
  mutate(code = as.numeric(code)) %>%
  select(code, name)

kom_val <- kom_val_df$code

kom_valprob <- list(values = kom_val, probs = NULL)

#CIVST#
civ_val <- c(enke = 'E', skilt = 'F', gift = 'G', langlev = 'L', ophpart = 'O', regpart = 'P', ugift = 'U')
civ_prob <- c(enke = 309828/5534738, skilt = 440026/5534738, gift = 2181004/5534738, langlev = 457/5534738, ophpart = 1887/5534738, regpart = 8364/5534738, ugift = 2593172/5534738)

civ_valprob <- list(values = civ_val, probs = civ_prob)

##DAN BEF DATASÆT##
bef_valproblist <- list(FOED_DAG = bdates_valprob, IE_TYPE = ie_valprob, KOM = kom_valprob, CIVST = civ_valprob)

BEF_2010 <- create_simdata(pnr = pnr_vec, valproblist = bef_valproblist, size = act_n)
BEF_2010$KOEN <- ifelse(as.numeric(BEF_2010$PNR)%%2 == 0, 2, 1)   #KOEN dannet ud fra sidste ciffer i PNR#
BEF_2010 <- select(BEF_2010, PNR, KOEN, FOED_DAG, IE_TYPE, KOM, CIVST)

shuffle_bef <- function(bef) {
  deadprob <- sample(seq(0.005, 0.01, by = 0.001), 1)
  dead_pnr <- sample(bef$PNR, (deadprob * length(bef$PNR)), replace = FALSE)
  new_bef <- bef[which(!(bef$PNR %in% dead_pnr)),]
  new_bef$KOM <- sample(new_bef$KOM)
  new_bef$CIVST <- sample(new_bef$CIVST)
  return(new_bef)
}

SASformat(BEF_2010$FOED_DAG) <- 'yymmdds10'

BEF_2011 <- shuffle_bef(BEF_2010)
BEF_2012 <- shuffle_bef(BEF_2011)
BEF_2013 <- shuffle_bef(BEF_2012)
BEF_2014 <- shuffle_bef(BEF_2013)
BEF_2015 <- shuffle_bef(BEF_2014)

#SAS options#
library(SASxport)
label(BEF_2010) <- 'BEF_SIM_2010'
label(BEF_2011) <- 'BEF_SIM_2011'
label(BEF_2012) <- 'BEF_SIM_2012'
label(BEF_2013) <- 'BEF_SIM_2013'
label(BEF_2014) <- 'BEF_SIM_2014'
label(BEF_2015) <- 'BEF_SIM_2015'


#Gem BEF datasæt#
setwd(save_path)

bef_list <- list(BEF_2010, BEF_2011, BEF_2012, BEF_2013, BEF_2014, BEF_2015)

write.xport(list = bef_list, file = paste0("bef_sim_2010-2015", ".xpt"))

library(haven)
year <- 2010
for (i in bef_list){
  write_dta(i, paste0("bef_sim_", year, ".dta"))
  year <- year + 1
}


#--#IDAP VARIABLE#--#

#PSTILL#
pstill_val_df <- read_csv(paste0(mat_path, 'PSTILL.csv'), col_names = FALSE, skip = 1) %>%
  separate(X1, c('code', 'name'), ":") %>%
  mutate(code = gsub("^0.*3000", "", code)) %>%
  mutate(code = str_trim(code)) %>%
  filter(nchar(code) < 3)

ude_am <- as.character(c(40:45, 90:98))

pstill_val <- sort(pstill_val_df$code)
pstill_prob <- c(rep((2676095/5560628)/23, 17), 128831/5560628, rep((2755702/5560628)/21, 12), rep((2676095/5560628)/23, 6), rep((2755702/5560628)/21, 9))

pstill_valprob <- list(values = pstill_val, probs = pstill_prob)

#LEDFULD#
ledmis <- 5134199/5475791
led_val <- str_pad(seq(0, 52, by = 1), 2, side = "left", "0")
led_2007num <- c(49924,34117,24332,20189,16583,14295,11651,10678,10499,9431,9193,8114,8071,7085,6838,6075,6029,5547,5415,4858,4754,4595,4050,4026,3575,3629,
                 2998,3259,2774,3041,2488,2592,2418,2018,2263,1800,1958,1530,1608,1468,1522,1337,1132,1317,1094,1108,1049,991,755,1174,580,3765)  #AGGREGEREDE LEDIGHEDSTAL FOR 2007#

led_prob <- c(ledmis, led_2007num/5475791)

led_valprob <- list(values = led_val, probs = led_prob)

#LONIND#
lon_mean <- 281460 #2010 mean
lon_sd <- 244893 #2010 sd
lon_lwval <- 20000

##DAN IDAP DATASÆT##
idap_valproblist <- list(PSTILL = pstill_valprob, LEDFULD = led_valprob)

IDAP_2010 <- create_simdata(pnr = pnr_vec, valproblist = idap_valproblist, size = act_n)
IDAP_2010$LONIND <- rnorm(act_n, mean = lon_mean, sd = lon_sd)
IDAP_2010$LONIND[IDAP_2010$LONIND < lon_lwval] <- 20000
IDAP_2010$LONIND[which(IDAP_2010$PSTILL %in% ude_am)] <- NA
IDAP_2010 <- select(IDAP_2010, PNR, PSTILL, LEDFULD, LONIND)

shuffle_idap <- function(idap, bef) {
  new_idap <- idap[idap$PNR %in% bef$PNR,] %>%
    mutate(PSTILL = sample(PSTILL),
           LEDFULD = sample(LEDFULD),
           LONIND = rnorm(n(), mean = lon_mean, sd = lon_sd))
  new_idap$LONIND[new_idap$LONIND < lon_lwval] <- 20000
  new_idap$LONIND[which(new_idap$PSTILL %in% ude_am)] <- NA
  return(new_idap)
}

IDAP_2011 <- shuffle_idap(IDAP_2010, BEF_2011)
IDAP_2012 <- shuffle_idap(IDAP_2011, BEF_2012)
IDAP_2013 <- shuffle_idap(IDAP_2012, BEF_2013)
IDAP_2014 <- shuffle_idap(IDAP_2013, BEF_2014)
IDAP_2015 <- shuffle_idap(IDAP_2014, BEF_2015)

#SAS options#
library(SASxport)
label(IDAP_2010) <- 'IDAP_SIM_2010'
label(IDAP_2011) <- 'IDAP_SIM_2011'
label(IDAP_2012) <- 'IDAP_SIM_2012'
label(IDAP_2013) <- 'IDAP_SIM_2013'
label(IDAP_2014) <- 'IDAP_SIM_2014'
label(IDAP_2015) <- 'IDAP_SIM_2015'

#Gem IDAP datasæt#
setwd(save_path)

idap_list <- list(IDAP_2010, IDAP_2011, IDAP_2012, IDAP_2013, IDAP_2014, IDAP_2015)

write.xport(list = idap_list, file = paste0("idap_sim_2010-2015", ".xpt"))

library(haven)
year <- 2010
for (i in idap_list){
  write_dta(i, paste0("idap_sim_", year, ".dta"))
  year <- year + 1
}

#--#UDDA VARIABLE#--#

#HFAUDD#
udd_val_df <- read_delim(paste0(mat_path, 'udd_codes.txt'), delim = ":", col_names = c('udd4', 'name'), skip = 1) %>%
  mutate(udd4 = str_trim(udd4)) %>%
  mutate(udd4 = str_pad(udd4, 4, side = 'left', pad = "0"))

audd_format <- read_delim(paste0(mat_path, 'audd_format.txt'), delim = "=", col_names = c('udd4', 'udd2'), skip = 2) %>%
  filter(is.na(udd2)==FALSE) %>%
  mutate(udd4 = gsub("'", "", udd4),
         udd2 = gsub("'", "", udd2)) %>%
  separate(udd2, c('udd2', 'label'), "(?<=[0-9])\\s")

udd_val_df <- inner_join(udd_val_df, audd_format, by = 'udd4') %>%
  group_by(udd2) %>%
  mutate(numudd = n())

udd_katcount <- table(udd_val_df$udd2)

hfudd_katprob <- c(miss = 1063782, grund = 1616446, gymerhv = (244568 + 89220 + 1442186), kvu = 178049, mvubach = (546064 + 68315), lvu = 270271, forsk = 15837) / 5534738 #hfaudd 2010#

audd_katprob <- c(gr1 = ((hfudd_katprob['grund'] / sum(udd_katcount[1:3])) * udd_katcount[1]), 
                  gr2 = ((hfudd_katprob['grund'] / sum(udd_katcount[1:3])) * udd_katcount[2]),
                  gr3 = ((hfudd_katprob['grund'] / sum(udd_katcount[1:3])) * udd_katcount[3]),
                  gymerhv = hfudd_katprob['gymerhv'], 
                  kvu = hfudd_katprob['kvu'],
                  mvubach = hfudd_katprob['mvubach'],
                  lvu = hfudd_katprob['lvu'],
                  forsk = hfudd_katprob['forsk'],
                  uopl = hfudd_katprob['miss'])


udd_val_katcount <- data.frame(names(udd_katcount), audd_katprob, stringsAsFactors = FALSE)
colnames(udd_val_katcount) <- c('udd2', 'katprob')

udd_val_df <- inner_join(udd_val_df, udd_val_katcount, by = 'udd2') %>%
  mutate(prob = katprob / numudd) %>%
  arrange(udd4)

audd_val <- udd_val_df$udd4
audd_prob <- udd_val_df$prob

audd_valprob <- list(values = audd_val, probs = audd_prob)

##DAN UDDA DATASÆT##
udda_valproblsit <- list(HFAUDD = audd_valprob)
audd_niv_translate <- udd_val_df[, c('udd4', 'udd2')]
colnames(audd_niv_translate) <- c('HFAUDD', 'NIV')
audd_niv_translate$NIV <- as.numeric(audd_niv_translate$NIV)
audd_niv_translate$NIV[audd_niv_translate$NIV == 90] <- 9

UDDA_2010 <- create_simdata(pnr = pnr_vec, valproblist = udda_valproblsit, size = act_n)
UDDA_2010 <- inner_join(UDDA_2010, audd_niv_translate, by = "HFAUDD")
UDDA_2010$NIV <- as.numeric(UDDA_2010$NIV)
UDDA_2010$NIV[UDDA_2010$NIV == 9] <- 90
UDDA_2010 <- select(UDDA_2010, PNR, HFAUDD, NIV)

shuffle_udda <- function(udda, bef) {
  udda$NIV[udda$NIV == 90] <- 9
  new_udda <- udda[udda$PNR %in% bef$PNR,] %>%
    mutate(old_hfaudd = HFAUDD,
           old_niv = NIV,
           HFAUDD = sample(old_hfaudd)) %>%
    select(PNR, HFAUDD, old_hfaudd, old_niv, HFAUDD) %>%
    inner_join(audd_niv_translate, by = 'HFAUDD') %>%
    mutate(HFAUDD = ifelse(NIV >= old_niv, HFAUDD, old_hfaudd)) %>%
    select(PNR, HFAUDD)
  new_udda <- inner_join(new_udda, audd_niv_translate, by = "HFAUDD")
  new_udda$NIV[new_udda$NIV == 9] <- 90
  return(new_udda)
}

UDDA_2011 <- shuffle_udda(UDDA_2010, BEF_2011)
UDDA_2012 <- shuffle_udda(UDDA_2011, BEF_2012)
UDDA_2013 <- shuffle_udda(UDDA_2012, BEF_2013)
UDDA_2014 <- shuffle_udda(UDDA_2013, BEF_2014)
UDDA_2015 <- shuffle_udda(UDDA_2014, BEF_2015)

#SAS options#
library(SASxport)
label(UDDA_2010) <- 'UDDA_SIM_2010'
label(UDDA_2011) <- 'UDDA_SIM_2011'
label(UDDA_2012) <- 'UDDA_SIM_2012'
label(UDDA_2013) <- 'UDDA_SIM_2013'
label(UDDA_2014) <- 'UDDA_SIM_2014'
label(UDDA_2015) <- 'UDDA_SIM_2015'

#Gem UDDA datasæt#
setwd(save_path)

udda_list <- list(UDDA_2010, UDDA_2011, UDDA_2012, UDDA_2013, UDDA_2014, UDDA_2015)
j <- 1
for (i in udda_list) {
  udda_list[[j]] <- select(i, PNR, HFAUDD)
  j <- j + 1
}

write.xport(list = udda_list, file = paste0("udda_sim_2010-2015", ".xpt"))

library(haven)
year <- 2010
for (i in udda_list){
  write_dta(i, paste0("udda_sim_", year, ".dta"))
  year <- year + 1
}


#--#UDDF VARIABLE#--#

create_uddf <- function(firstdate, values, maxyear, probs){
  first_date <- firstdate + years(18)
  max_date <- ymd(paste0(as.character(maxyear), '12', '12', sep = "-"))
  start_date <- base::sample(seq(first_date, max_date, by="day"), 1)
  end_date <- start_date + base::sample(seq(30, 1100, by = 1), 1, prob = qexp((30:1100)/1101)[c(1:1071)])
  rand_udd <- base::sample(values, 1, prob = probs)
  rand_uddf_vec <- list(HF_VFRA = start_date, HF_VTIL = end_date, HFAUDD = rand_udd)
  return(rand_uddf_vec)
}

##Dan forløb fra 18 år frem til 2015##
udd_len <- act_n * 5
uddf_list <- list()
for (i in 1:udd_len) {
  uddf_list[[i]] <- create_uddf(ymd('1935-01-01'), audd_val, 2015, audd_prob)
}

uddf_df <- map_df(uddf_list, c)
uddf_df$PNR <- base::sample(BEF_2015$PNR, udd_len, replace = TRUE)


#Filter PNR#
for (i in uddf_df$PNR) {
  min_date <- BEF_2015[BEF_2015$PNR == i, 'FOED_DAG'] + years(18)
  filter_rows <- which(uddf_df$PNR == i & uddf_df$HF_VFRA < min_date)
  if (is_empty(filter_rows) == FALSE) {
    uddf_df <- uddf_df[-filter_rows, ]
  }
}

uddf_df <- uddf_df %>%
  inner_join(audd_niv_translate, by = 'HFAUDD') %>%
  mutate(YEAR = as.numeric(substr(HF_VTIL, 1, 4))) %>%
  inner_join(UDDA_2010, by = 'PNR', suffix = c("", "_2010")) %>%
  inner_join(UDDA_2011, by = 'PNR', suffix = c("", "_2011")) %>%
  inner_join(UDDA_2012, by = 'PNR', suffix = c("", "_2012")) %>%
  inner_join(UDDA_2013, by = 'PNR', suffix = c("", "_2013")) %>%
  inner_join(UDDA_2014, by = 'PNR', suffix = c("", "_2014")) %>%
  inner_join(UDDA_2015, by = 'PNR', suffix = c("", "_2015")) %>%
  arrange(PNR, YEAR)


rows_ind10 <- which(uddf_df$YEAR <= 2010 & uddf_df$NIV >= uddf_df$NIV_2010)
uddf_df <- uddf_df[-rows_ind10,]
rows_ind11 <- which(uddf_df$YEAR <= 2011 & uddf_df$NIV >= uddf_df$NIV_2011)
uddf_df <- uddf_df[-rows_ind11,]
rows_ind12 <- which(uddf_df$YEAR <= 2012 & uddf_df$NIV >= uddf_df$NIV_2012)
uddf_df <- uddf_df[-rows_ind12,]
rows_ind13 <- which(uddf_df$YEAR <= 2013 & uddf_df$NIV >= uddf_df$NIV_2013)
uddf_df <- uddf_df[-rows_ind13,]
rows_ind14 <- which(uddf_df$YEAR <= 2014 & uddf_df$NIV >= uddf_df$NIV_2014)
uddf_df <- uddf_df[-rows_ind14,]
rows_ind15 <- which(uddf_df$YEAR <= 2015 & uddf_df$NIV >= uddf_df$NIV_2015)
uddf_df <- uddf_df[-rows_ind15,]

uddf_df <- select(uddf_df, PNR, HF_VFRA, HF_VTIL, HFAUDD)
uddf_df$PNR <- as.character(uddf_df$PNR)
uddf_df$HF_VFRA <- ymd(uddf_df$HF_VFRA)
uddf_df$HF_VTIL <- ymd(uddf_df$HF_VTIL)
uddf_df$HFAUDD <- as.character(uddf_df$HFAUDD)

uddf_df <- as.data.frame(uddf_df)


#SAS Options#
library(SASxport)
SASformat(uddf_df$HF_VFRA) <- 'yymmdd10'
SASformat(uddf_df$HF_VTIL) <- 'yymmdd10'
label(uddf_df) <- 'UDDF_SIM_2015'


#Gem UDDF datasæt#
setwd(save_path)

write.xport(uddf_df, file = paste0("uddf_sim_2015", ".xpt"))

write_dta(uddf_df, "uddf_sim_2015.dta")

uddf_dta <- read_dta("uddf_sim_2015.dta")

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


##DREAM - Skippes##
dream_weeks <- paste0('y_', str_pad(as.character(seq(1001, 1552, by = 1)), 2, side = "left", pad = "0"))
dream_weeks <- dream_weeks[as.numeric(substr(dream_weeks, 5, 6)) < 53 & as.numeric(substr(dream_weeks, 5, 6)) > 0]




