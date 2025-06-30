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


marineNoPlant <- c("", "Agmata", "Annelida","Bilateralomorpha","Brachiopoda","Bryozoa",
                   "Calcispongea","Chaetognatha","Cnidaria","Ctenophora","Echinodermata",
                   "Entoprocta","Foraminifera","Hemichordata","Hyolitha","Mollusca",
                   "Nematoda","Nematomorpha","Nemertina","Onychophora","Petalonamae",
                   "Phoronida","Platyhelminthes","Porifera","Problematica","Rhizopodea",
                   "Rotifera","Sarcomastigophora","Sipuncula","Uncertain","Vetulicolia",
                   ""
)

# which rows?
bByPhyla <- dat$phylum%in% marineNoPlant

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
bNeedMamFam<- dat$family%in%needFam

# D. Reptiles
#   reptiles <- dat[dat$class=="Reptilia",]
#   levels(factor(reptiles$order))
needReptOrd<-c(
  "Eosauropterygia",
  "Hupehsuchia",
  "Ichthyosauria",
  "Placodontia",
  "Sauropterygia",
  "Thalattosauria"
)

# which rows?
bRept <- dat$order%in%needReptOrd

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
bTurtle <- dat$family%in%needTurtleFam

# now we have defined lots of (boolean) filters

# subset the data
dat <- dat[bByPhyla | bNeedClass | bMammalOrder | bNeedMamFam | bRept | bTurtle , ]

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
dat<-dat[!dat$environment%in%omitEnv, ]
nrow(dat)

# omiting unlithified sediments (typically means unusually good preservation, which might cause temporal bias in sampling)
# therfore omitting them can be a good idea to prevent sampling bias
dat <- dat[dat$lithification1!="unlithified",]
nrow(dat)


data(stages)
str(stages)

# binning
data(keys)
# B. the stg entries (lookup)
stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
stgMax<-categorize(dat[,"late_interval"],keys$stgInt)
stgMin<-as.numeric(stgMin)
stgMax<-as.numeric(stgMax)

# add empty column with default value
# empty container
dat$stg <- rep(NA, nrow(dat))

# select entries, where
stgCondition <- c(
  # the early and late interval fields indicate the same stg
  which(stgMax==stgMin),
  # or the late_intervarl field is empty
  which(stgMax==-1))

dat$stg[stgCondition] <- stgMin[stgCondition]

# load data
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))

# correct it with this function
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

# load data
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))

# correct it with this function
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")

##### DONE with preprocessing ########
par(mar=c(4,12,3,3))

barplot(sort(table(dat$diet), decreasing = TRUE),las=2,horiz = TRUE, )

carnivores = dat[dat$diet == 'carnivore',]
predators = carnivores
nonpredators = dat[dat$diet != 'carnivore',]
herbivores = dat[dat$diet == 'herbivore' | dat$diet =='grazer', ]
other = dat[dat$diet != 'carnivore' & dat$diet != 'herbivore' & dat$diet != 'grazer',]


# what phyla do the samples belong to?
par(mar=c(4,11,4,4))
barplot(sort(table(carnivores$phylum), decreasing = TRUE),las=2,horiz = TRUE)
barplot(sort(table(herbivores$phylum), decreasing = TRUE),las=2,horiz = TRUE)
barplot(sort(table(other$phylum), decreasing = TRUE),las=2,horiz = TRUE)
par()


# analyze diversity dynamics of omnivores and others
divStg = function(x) {
  merge(stages, divDyn(x, bin="stg", tax="clgen"), by="stg")
}

allDiv = divStg(dat)
carniDiv = divStg(carnivores)
predatorDiv = carniDiv
nonPredDiv = divStg(nonpredators)
herbiDiv = divStg(herbivores)
otherDiv = divStg(other)

full_tsplot <- function(x, ys, cols=c("red", "green", "black"), labels=c("carnivores", "herbivores", "other"),
                           xlim = 4:95, ylim = c(0, 1),
                           ylab = "Diversity Proportion",
                           boxes = "sys", shading = "sys",
                           event_lines = c(66, 201, 252, 372, 445),
                           legend_pos = "topleft", legend_inset = c(0.05, 0.01),
                           legend_cex = 1.3,
                           tit="Diversity") {
  # Plot the tsplot background
  tsplot(stages, boxes = boxes, shading = shading, xlim = xlim, ylim = ylim,
         ylab = ylab)
  
  # Add the lines
  for (i in seq_along(ys)) {
    lines(x, ys[[i]], col = cols[i], lwd = 2)
  }
  
  # Add vertical event lines
  abline(v = event_lines, col = "gray40", lty = 2)
  
  # add horizontal line
  abline(h=0)
  
  
  # Add legend
  legend(legend_pos, bg = "white", legend = labels,
         col = cols, lwd = 2, inset = legend_inset, cex = legend_cex)
  
  # Add title
  title(tit)
}


# relative extinction rates
full_tsplot(x=allDiv$mid, ys=list(carniDiv$extProp, herbiDiv$extProp, otherDiv$extProp), 
            tit="Proportional Extinctions",
            ylab="Proportion")


# diversity rates
full_tsplot(x=allDiv$mid, ys=list(carniDiv$divSIB, herbiDiv$divSIB, otherDiv$divSIB), 
            tit="SIB Diversity",
            ylab="Diversity",
            ylim=c(0, max(carniDiv$divSIB, herbiDiv$divSIB, otherDiv$divSIB, na.rm = TRUE)))

# zscored extinction rates (raw data, foote metric, since biases don't matter for this analysis)
zscore = function(x) {
  return (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
carniExtZscore = zscore(carniDiv$extPC)
nonPredExtZscore = zscore(nonPredDiv$extPC)

full_tsplot(allDiv$mid, ys=list(carniExtZscore, otherExtZscore), ylim=c(-2, 2), tit="Z-Scored Extinction Foote Metric", cols=c("red", "black"), labels=c("Predators", "Other"))

# difference between zscores
carniExtDiffZscore = carniExtZscore - nonPredExtZscore
full_tsplot(allDiv$mid, ys=list(carniExtDiffZscore), ylim=c(-1,1), tit="Difference of Zscores Extinctions", cols=c("red"), labels=c("Predators - Non-Predators"))
