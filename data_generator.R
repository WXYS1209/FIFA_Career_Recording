if (!dir.exists("./data")) {
  dir.create("./data")
}

setwd("./data")

for (i in 1:10) {
  dir.create(paste0("Season", i))
  for (comp in c("PL", "EC", "Carabao_Cup", "FA_Cup", "Other")) {
    dir.create(paste0("Season", i, "/", comp))
  }
}

