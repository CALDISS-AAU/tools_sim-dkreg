library(haven)
library(SASxport)

befpat <- '//ADM.AAU.DK/Users/kgk/Documents/Programmering/R projekter/reg_sim/simdata/bef/'
BEF_2012 <- read_dta(paste0(befpat, 'BEF_SIM_2012.dta'))


esspat <- 'D:/ESS/'
ess_2012 <- read_dta(paste0(esspat, 'ESS_2012_DK.dta'))

ess_2012 <- ess_2012[, -c(1:6)]
ess_2012 <- ess_2012[, c(1:14)]

pnr <- base::sample(BEF_2012$PNR, 1650)

ess_2012$PNR <- pnr
ess_2012 <- ess_2012[, c(15, c(1:14))]

ess_2012[, c(2:15)] <- lapply(ess_2012[,c(2:15)], as.numeric)


write_dta(ess_2012, paste0(esspat, 'ESS_2012_sim.dta'))
ess_2012$PNR <- as.numeric(ess_2012$PNR)
write.xport(ess_2012, file = paste0(esspat, 'ESS_sim_2012.xpt'), autogen.formats = FALSE)