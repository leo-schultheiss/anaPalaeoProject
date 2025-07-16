# Marine Animals Preprocessing ####

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
dat <- chronosphere::fetch("pbdb",
                           ser = "occs3",
                           ver = "20250602",
                           datadir = "/home/leo/Downloads/")
str(dat)

# filter records not identified at least to genus
dat <- dat[dat$accepted_rank %in% c("genus", "species", "subgenus", "subspecies"), ]


marineNoPlant <- c(
  "",
  "Agmata",
  "Annelida",
  "Bilateralomorpha",
  "Brachiopoda",
  "Bryozoa",
  "Calcispongea",
  "Chaetognatha",
  "Cnidaria",
  "Ctenophora",
  "Echinodermata",
  "Entoprocta",
  "Foraminifera",
  "Hemichordata",
  "Hyolitha",
  "Mollusca",
  "Nematoda",
  "Nematomorpha",
  "Nemertina",
  "Onychophora",
  "Petalonamae",
  "Phoronida",
  "Platyhelminthes",
  "Porifera",
  "Problematica",
  "Rhizopodea",
  "Rotifera",
  "Sarcomastigophora",
  "Sipuncula",
  "Uncertain",
  "Vetulicolia",
  ""
)

# which rows?
bByPhyla <- dat$phylum %in% marineNoPlant

#B. classes
#   levels(factor(noNeed$class))
needClass <- c(
  "Acanthodii",
  "Actinopteri",
  "Actinopterygii",
  "Agnatha",
  "Cephalaspidomorphi",
  "Chondrichthyes",
  "Cladistia",
  "Coelacanthimorpha",
  "Conodonta",
  "Galeaspida",
  "Myxini",
  "Osteichthyes",
  "Petromyzontida",
  "Plagiostomi",
  "Pteraspidomorphi",
  # here come the Arthropods
  "Artiopoda",
  "Branchiopoda",
  "Cephalocarida",
  "Copepoda",
  "Malacostraca",
  "Maxillopoda",
  "Megacheira",
  "Merostomoidea",
  "Ostracoda",
  "Paratrilobita",
  "Pycnogonida",
  "Remipedia",
  "Thylacocephala",
  "Trilobita",
  "Xiphosura"
)

# which rows?
bNeedClass <- dat$class %in% needClass

#C.  mammals
#   mammals <- dat[dat$class=="Mammalia",]
#   levels(factor(mammals$order))
needMammalOrd <- c("Cetacea", "Sirenia")

# which rows?
bMammalOrder <- dat$order %in% needMammalOrd

# the carnivores
#   carnivores <- dat[dat$order=="Carnivora",]
#   levels(factor(carnivores$family))
needFam <- c("Otariidae", "Phocidae", "Desmatophocidae")

# which rows?
bNeedMamFam <- dat$family %in% needFam

# D. Reptiles
#   reptiles <- dat[dat$class=="Reptilia",]
#   levels(factor(reptiles$order))
needReptOrd <- c(
  "Eosauropterygia",
  "Hupehsuchia",
  "Ichthyosauria",
  "Placodontia",
  "Sauropterygia",
  "Thalattosauria"
)

# which rows?
bRept <- dat$order %in% needReptOrd

# E. turtles
#   turtles <- dat[dat$order=="Testudines",]
#   levels(factor(turtles$family))

# Chelonioidea turtles
needTurtleFam <- c(
  "Cheloniidae",
  "Protostegidae",
  "Dermochelyidae",
  "Dermochelyoidae",
  "Toxochelyidae",
  "Pancheloniidae"
)

# which rows?
bTurtle <- dat$family %in% needTurtleFam

# now we have defined lots of (boolean) filters

# subset the data
dat <- dat[bByPhyla |
             bNeedClass | bMammalOrder | bNeedMamFam | bRept | bTurtle , ]

# fix homonymy issues (species with accidentally same name)
dat$clgen <- paste(dat$class, dat$genus)

nrow(dat)

# marine fossils usually don't end up in terrestrial environments
levels(factor((dat$environment)))

# so we can filter the entries that come from those environments
omitEnv <- c(
  "\"floodplain\"",
  "alluvial fan",
  "cave",
  "\"channel\"",
  "channel lag" ,
  "coarse channel fill",
  "crater lake",
  "crevasse splay",
  "dry floodplain",
  "delta plain",
  "dune",
  "eolian indet.",
  "fine channel fill",
  "fissure fill",
  "fluvial indet.",
  "fluvial-lacustrine indet.",
  "fluvial-deltaic indet.",
  "glacial",
  "interdune",
  "karst indet.",
  "lacustrine - large",
  "lacustrine - small",
  "lacustrine delta front",
  "lacustrine delta plain",
  "lacustrine deltaic indet.",
  "lacustrine indet.",
  "lacustrine interdistributary bay",
  "lacustrine prodelta",
  "levee",
  "loess",
  "mire/swamp",
  "pond",
  "sinkhole",
  "spring",
  "tar",
  "terrestrial indet.",
  "wet floodplain"
)

# actual omission
dat <- dat[!dat$environment %in% omitEnv, ]
nrow(dat)

# omiting unlithified sediments (typically means unusually good preservation, which might cause temporal bias in sampling)
# therfore omitting them can be a good idea to prevent sampling bias
dat <- dat[dat$lithification1 != "unlithified", ]
nrow(dat)


data(stages)
str(stages)

# binning
data(keys)
# B. the stg entries (lookup)
stgMin <- categorize(dat[, "early_interval"], keys$stgInt)
stgMax <- categorize(dat[, "late_interval"], keys$stgInt)
stgMin <- as.numeric(stgMin)
stgMax <- as.numeric(stgMax)

# add empty column with default value
# empty container
dat$stg <- rep(NA, nrow(dat))

# select entries, where
stgCondition <- c(
  # the early and late interval fields indicate the same stg
  which(stgMax == stgMin),
  # or the late_intervarl field is empty
  which(stgMax == -1)
)

dat$stg[stgCondition] <- stgMin[stgCondition]

# load data
load(
  url(
    "https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"
  )
)

# correct it with this function
source(
  "https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R"
)

# load data
load(
  url(
    "https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"
  )
)

# correct it with this function
source(
  "https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R"
)

library(wordcloud2) 

# have a look to the example dataset
# head(demoFreq)

# Basic plot
diet_table = sort(table(dat$diet), decreasing = TRUE)
diet_table
log_table = log(diet_table, base = 7)
log_table
diet_df = as.data.frame(log_table)
cloud = wordcloud2(data=diet_df, size=0.4, shuffle = FALSE, color = "random-dark", minSize = 20)
cloud

### split data into carnivores and non-carnivores ####


carnivores = dat[dat$diet == 'carnivore', ]
nonCarnivores = dat[dat$diet != 'carnivore', ]



#### what phyla do the samples belong to?####
par(mar = c(4, 11, 4, 4))
barplot(sort(table(carnivores$phylum), decreasing = TRUE),
        las = 2,
        horiz = TRUE)
barplot(sort(table(nonCarnivores$phylum), decreasing = TRUE),
        las = 2,
        horiz = TRUE)
par(mar = c(4, 4, 4, 4))

# ammonites make up much of the carnivores after the triassic
carnivores_ammoniteEx = dat[dat$diet == 'carnivore' & dat$class != "Cephalopoda", ]
nonCarnivores_ammoniteEx = dat[dat$diet != 'carnivore' | dat$class == "Cephalopoda", ]

# trilobites account for most predators in the cambrium
carnivores_trilobiteEx = dat[dat$diet == 'carnivore' & dat$class != "Trilobita", ]
nonCarnivores_trilobiteEx = dat[dat$class == "Trilobita", ]

# cephalopoda make up much of the predators in the early triassic
trias_carnivores = dat[dat$max_ma < 250 & dat$min_ma > 200 & dat$diet == 'carnivore', ]
barplot(sort(table(trias_carnivores$class)), las=2, horiz=TRUE)


# create and save dataframe containing diversity analysis ####
divStg = function(x) {
  merge(stages, divDyn(x, bin = "stg", tax = "clgen"), by = "stg")
}



allDiv = divStg(dat)
carniDiv = divStg(carnivores)
nonCarniDiv = divStg(nonCarnivores)

carniCephaExDiv = divStg(carnivores_ammoniteEx)
noncarniCephaExDiv = divStg(nonCarnivores_ammoniteEx)

carniTriloExDiv = divStg(carnivores_trilobiteEx)
noncarniTriloExDiv = divStg(nonCarnivores_trilobiteEx)


library(dplyr)
mass_extinction_stages = c("Katian", "Famennian", "Changhsingian", "Rhaetian", "Maastrichtian")
food_mass_extinctions = c("Katian", "Famennian", "Maastrichtian")

allDiv$diet = "Both"

carniDiv$diet = "Carnivore"
nonCarniDiv$diet = "Non-Carnivore"
df = rbind(allDiv, carniDiv, nonCarniDiv)
df = df %>% mutate(mass_extinction = stage %in% mass_extinction_stages)
df = df %>% mutate(food_mass_extinction = stage %in% food_mass_extinctions)

write.csv(df, "mass_extinction_divDyn.csv", row.names=FALSE, quote=FALSE)

carniCephaExDiv$diet = "Carnivore"
noncarniCephaExDiv$diet = "Non-Carnivore"
df_cephaEx = rbind(allDiv, carniCephaExDiv, noncarniCephaExDiv)
df_cephaEx = df_cephaEx %>% mutate(mass_extinction = stage %in% mass_extinction_stages)
df_cephaEx = df_cephaEx %>% mutate(food_mass_extinction = stage %in% food_mass_extinctions)

write.csv(df_cephaEx, "mass_extinction_divDyn_cephalopoda_not_predators.csv", row.names=FALSE, quote=FALSE)


carniTriloExDiv$diet = "Carnivore"
noncarniTriloExDiv$diet = "Non-Carnivore"
df_triloEx = rbind(allDiv, carniTriloExDiv, noncarniTriloExDiv)
df_triloEx = df_triloEx %>% mutate(mass_extinction = stage %in% mass_extinction_stages)
df_triloEx = df_triloEx %>% mutate(food_mass_extinction = stage %in% food_mass_extinctions)

write.csv(df_triloEx, "mass_extinction_divDyn_trilobites_not_predators.csv", row.names=FALSE, quote=FALSE)

