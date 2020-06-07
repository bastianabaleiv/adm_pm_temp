setwd("/home/bastian/Dropbox/paper_hospital_pm")

load('data/adm_pm_temp.RData')

library(Rssa)

adm_ssa <- ssa(adm_pm_temp$adm)
summary(adm_ssa)

plot(adm_ssa)

# Identify Eigenvectors for trend, seasonality and noise
plot(adm_ssa, type = "vector")

# Identify combinations of Eigenvectors responsible for seasonality
# (circular patterns)
plot(adm_ssa, type = "paired")

# Correlation between Eigenvectors
plot(adm_ssa, type = "wcor")

adm_recon <- reconstruct(adm_ssa,
                         groups = list(
                           trend = 1,
                           yearseas = c(2,3,4),
                           weekseas = c(5))
)
plot(adm_recon)
adm_res <- residuals(adm_recon)
plot(adm_res, type = "l")

adm_recon <- reconstruct(adm_ssa)
plot(adm_recon)
adm_res <- residuals(adm_recon)
plot(adm_res, type = "l")



adm_wcor <- wcor(adm_ssa, groups = 1:100)
plot(adm_wcor, grid = c(2,4,5,7))
