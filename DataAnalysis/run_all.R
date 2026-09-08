# run_all.R -- render the four reports in code/ into out/, in order.
# Run from the project root:  Rscript run_all.R
#
# 02, 03, and 04 load the environment that 01 writes to data/proc/, so the
# order matters. Every report starts with rm(list = ls()); rendering each one
# into a fresh environment keeps that from touching this script's workspace.

if (!file.exists("code/01_data_processing.Rmd")) {
  stop("please run this script from the DataAnalysis directory")
}

for (f in list.files("code", pattern = "^0[1-4].*[.]Rmd$", full.names = TRUE)) {
  rmarkdown::render(f, output_dir = "out", envir = new.env())
}
