library(stringr)

# Objects -----------------------------------------------------------------

tbl <- NULL

dir_res <- "application/icon/results/"
files <- dir(dir_res)

dir_net <- "application/icon/data/"
nets <- dir(dir_net)

# Read --------------------------------------------------------------------

for (i in 1:length(files)) {
  tbl <- rbind(tbl, read.table(paste0(dir_res, files[i])))
}

# Average degree ----------------------------------------------------------

tbl$avg.degree <- NA

for (i in 1:nrow(tbl)) {
  temp <- read.delim(paste0(dir_net, nets[i]), sep = ",")
  tbl$avg.degree[i] <- sum(temp$xvalue * temp$counts) / sum(temp$counts)
}

# Classification ----------------------------------------------------------

tbl$class <- NA

for (i in 1:nrow(tbl)) {
  if (str_detect(nets[i], "Social")) {
    tbl$class[i] <- "Social"
  } else if (str_detect(nets[i], "Technological")) {
    tbl$class[i] <- "Technological"
  } else if (str_detect(nets[i], "Transportation")) {
    tbl$class[i] <- "Transportation"
  } else if (str_detect(nets[i], "Informational")) {
    tbl$class[i] <- "Informational"
  } else {
    tbl$class[i] <- "Biological"
  }
}

tbl$class <-
  factor(
    tbl$class,
    levels = c(
      "Biological",
      "Social",
      "Technological",
      "Transportation",
      "Informational"
    )
  )

# Match -------------------------------------------------------------------

tbl$match <- ifelse(((tbl$K < 0.1) == (tbl$U2 < 0.1)) |
                    ((tbl$K >= 0.1) == (tbl$U2 >= 0.1)), "yes", "no")

# Output ------------------------------------------------------------------

write.table(tbl, file = "application/icon/tbl_results.txt")
