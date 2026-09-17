# devtools::install_github("njerezlillo/pldis")
library(pldis)
library(parallel)
library(doParallel)
source("clauset.R")
source("application/icon/run/fun_base.R")

number_of_cores <- 30
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores = number_of_cores)

ptm <- proc.time() #start

estimates <- foreach(k = 3201:3600, .packages = c("poweRlaw", "msm", "VGAM"),
                     .multicombine = TRUE) %dopar% FF(k)

proc.time() - ptm #final

out <- data.frame(t(sapply(estimates, c)))
out <- apply(out, 2, as.character)

write.table(out, paste0("application/icon/results/icon_09.txt"))
