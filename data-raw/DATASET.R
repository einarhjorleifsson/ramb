## code to prepare `DATASET` dataset goes here

creel <-
  read.table("data-raw/doi_10.5061_dryad.k80bp46__v1.txt",
             header=TRUE, sep=",") %>%
  select(ID, date, x, y, behaviour)
usethis::use_data(creel, overwrite = TRUE)
