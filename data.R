library(divDyn)

data(tens)
str(tens)

data(stages)
str(stages)

library(chronosphere)

# Download data (June 02, 2025)
# dat <- chronosphere::fetch("pbdb", ser="occs3", ver="20250602")

# The most up-to-date version
# dat <- chronosphere::fetch("pbdb")

# to a given directory 
dat <- chronosphere::fetch("pbdb", ser="occs3", ver="20250602", datadir="/home/leo/Downloads/")
str(dat)

# filter records not identified at least to genus
dat <-dat[dat$accepted_rank %in% c("genus", "species", "subgenus", "subspecies"),]
