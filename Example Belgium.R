#
# EXAMPLE WITH BELIGIUM POPULATION (slides SM-without-microdata)

library(mipfp)

# Read global cross-table (to be used as constraint ans initial weight matrix)
# and read constraints

global <- read.delim(file = "data/Belgium/BelgiqueConting.txt", sep = "\t")
names(global) <- c("age", "sex", "dipl", "status", "Freq")

conAge <- read.delim(file = "data/Belgium/ContrainteAge.txt", sep = "\t")
names(conAge)[2] <- "age"

conDipl <- read.delim(file = "data/Belgium/ContrainteDipl.txt", sep = "\t")

conSex <- read.delim(file = "data/Belgium/ContrainteGenre.txt", sep = "\t")
names(conSex)[2] <- "sex"

conStatus <- read.delim(file = "data/Belgium/ContrainteStatut.txt", sep = "\t")
names(conStatus)[2] <- "status"

# check the categories are the same in constraint and global
table(unique(conAge[2]) == unique(global$age))
table(unique(conDipl[2]) == as.data.frame(unique(global$dipl)))
unique(conSex[2]) == as.data.frame(unique(global$sex))
unique(conStatus[2]) == as.data.frame(unique(global$status))

# check the sum of constraints are the same
totconAge <- aggregate(conAge$COUNT, by = list(conAge$com), FUN = sum)
totconDipl <- aggregate(conDipl$COUNT, by = list(conDipl$com), FUN = sum)
totconSex <- aggregate(conSex$COUNT, by = list(conSex$com), FUN = sum)
totconStatus <- aggregate(conStatus$COUNT, by = list(conStatus$com), FUN = sum)

table(totconAge[2] == totconDipl[2])
table(totconAge[2] == totconSex[2])
table(totconAge[2] == totconStatus[2])


# creating the cross-tab
global_table <- xtabs(Freq~., data=global)
dimnames(global_table)

# ------ Parameters for IPFP ------

# -- Seed (global table)
seed <- global_table

# -- Target (constraints)
zone = "92094"
conAge1 <- as.numeric(conAge$COUNT[conAge$com == zone])
conDipl1 <- as.numeric(conDipl$COUNT[conDipl$com == zone])
conSex1 <- as.numeric(conSex$COUNT[conSex$com == zone])
conStatus1 <- as.numeric(conStatus$COUNT[conStatus$com == zone])

# The constraints have to enter in the same order as in the seed (weights table)
dimnames(global_table)
cons = list(conAge1, conSex1, conDipl1, conStatus1)

# -- Target list (order of dimensions)
targetlist <- list(c(1,2,3,4))

# -- Function IPFP
res <- Ipfp(seed = seed, target.list = targetlist, target = cons)
res$conv

# ---- for all zones ----
#-------------------------#
# -- Parameters for IPFP -----

# -- Seed (global table)
# The seed is replicated as many times a number of zones
numCom <- nrow(totconAge)
names <- c(list(unique(totconAge$Group.1)), as.list(dimnames(global_table)))

seedALL <- array(rep(global_table, each = nrow(totconAge)), 
                 dim = c(numCom, dim(global_table)),
                dimnames = names)

# -- Target (constraints)
conAge2 <- array(conAge$COUNT, dim = c(numCom, length(unique(conAge$age))))
conDipl2 <- array(conDipl$COUNT, dim = c(numCom, length(unique(conDipl$dipl))))
conSex2 <- array(conSex$COUNT, dim = c(numCom, length(unique(conSex$sex))))
conStatus2 <- array(conStatus$COUNT, dim = c(numCom, length(unique(conStatus$status))))

consALL <- list(conAge2, conSex2, conDipl2, conStatus2)

# -- Target list (order of dimensions)
# Now I'm adding a fifth dimension (zone) in the first position (see SEED)
targetlistALL <- list(c(1,2),c(1,3),c(1,4),c(1,5))

# -- Function IPFP
resALL <- Ipfp(seed = seedALL, target.list = targetlistALL, target = consALL)
resALL$conv

weightALL <- resALL$x.hat

# Validation: check that the solution meets the constraints
seed_conAge <- apply(weightALL, c(1,2), sum)
seed_conSex <- apply(weightALL, c(1,3), sum)
seed_conDipl <- apply(weightALL, c(1,4), sum)
seed_conStatus <- apply(weightALL, c(1,5), sum)
seed_conALL <- cbind(seed_conAge, seed_conSex, seed_conDipl, seed_conStatus)

# TO FINISH THIS PART I NEED TO HAVE ALL THE CONSTRAINTS IN FORM OF A TABLE 
# AS IN "seed_conALL"


# plot(as.matrix(ind_agg[1,]), as.matrix(cons[1,]), xlab = 'Simulated', ylab='Theoretical', 
     # main =' Validation for zone 1')
# cor(as.vector(as.matrix(ind_agg)),as.vector(as.matrix(cons)))

# CorVec <- rep (0, nrow(cons))
# 
# for (i in 1:nrow(cons)){
#   CorVec[i] = cor(as.numeric(ind_agg[i,]),as.numeric(cons[i,]))
# }
# 
# which(CorVec< 0.99)



# Integerisation ----
# see here for code:
# https://github.com/Robinlovelace/spatial-microsim-book/blob/master/R/functions.R

expa2 = as.data.frame.table(weightALL)
expa2_namur <- expa2[expa2$Var1 == "92094",]

source("R/functions.R") # loads functions into memory
expa2_namur$int = int_trs(expa2_namur$Freq)
expa2$int <- int_trs(expa2$Freq)

exp_indices2 = int_expand_vector(expa2$int)
synth_namur = expa2[exp_indices2,]
names(synth_namur)[1] <- "id"
  
# Generate spatial microdata

# Getting spatial data for Belgium
# Download map (SpatialPolygonsDataFrame)
u_bel = "http://biogeo.ucdavis.edu/data/gadm2.8/rds/BEL_adm4.rds"
download.file(u_bel, "BEL_adm4.rds")
bel = readRDS("BEL_adm4.rds")
plot(bel)
d = bel@data
# Keep the map only for Namur
nam = bel[bel$NAME_2 == "Namur",]
uz = unique(synth_namur$id)
plot(nam)
d = nam@data

# We don't have a common ID (in theory we should), so we artificially asign
# an ID to match a dataset built from the synthetic population (pmale)
# with the map object nam. We do it by randomly asigning a value from uz

# nam = nam[sample(length(nam), length(uz)),]
# str(nam) # show structure

nam$id = uz[sample(length(uz), length(uz))]

# Aggregate the synthetic population by zone
library(dplyr)
sexratio <- synth_namur %>%
    group_by(id) %>%
    summarise(sexratio = sum(sex == "Hommes") / sum(sex == "Femmes"))

pmale <- synth_namur %>%
    group_by(id) %>%
    summarise(pmale = sum(sex == "Hommes") / n())


# check the ids match
summary(nam$id %in% pmale$id)
nam@data = inner_join(nam@data, pmale, by="id")
head(nam@data)
tmap::qtm(nam, "pmale")

nam@data = inner_join(nam@data, sexratio, by="id")
head(nam@data)
tmap::qtm(nam, "sexratio")


library(tmap)
tmap_mode("view")
qtm(nam, "pmale", n = 3)
tm_shape(nam) +
  tm_fill(col = "pmale",
          breaks = c(0, 0.5, 1))

# Challenges:
# 1: Write a for loop to create a spatial microdataset
# for all zones in namur (don't just copy my code!)
# 2: Create a map of a different variable (not % male)
# 3: Implement the methods on your own data
