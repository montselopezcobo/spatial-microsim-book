# Welcome to the course's R notes
# All course material found/linked to:
# https://github.com/Robinlovelace/spatial-microsim-book
# examples will go here

# First challenge: get set-up on RStudio server
# https://rstudio.jrc.es/

# test if your RStudio account works:
# example of interactive plotting
library(tmap)
tmap_mode("view")
example(qtm)

# downloading and unzipping data
u = "https://github.com/Robinlovelace/spatial-microsim-book/archive/master.zip"
download.file(u, destfile = "master.zip")
unzip("master.zip")

# # for spatial data
# u = "https://github.com/Robinlovelace/vspd-base-shiny-data/archive/master.zip"
# download.file(u, destfile = "master.zip")
# unzip("master.zip")
# dir.create("data")
# f = list.files(path = "vspd-base-shiny-data-master/",
#                full.names = T)
# file.copy(f, "data")

# spatial data with R


# Notes on project management
# https://csgillespie.github.io/efficientR/
      

# If you open the project (up-right corner), this is equivalent to setwd:
# setwd("/home/NET1/lopemon/training/Spatial_Microsimulation/spatial-microsim-book-master")




x <- 1:99
x = 1:99
y <- x^3
z <- plot(x,y)


# "=" vs "<-" (intercheangable most of the time, but not always)
system.time(x=1:99) # Error
system.time(x <- 1:99) # Ok
system.time({x=1:99}) # Ok

a = b = c = x
x = 1
x <- 1

# The vbles a,b and c don't change after x is changed. The asignation command 
# is not a pointer


# You can specify arguments to system commands 
system2(command = "ls", args = "-hal")


# Shortcuts: Alt + Shift + k


# Reading: autocompletion if you include the extension + tab (e.g. csv)
# d <- read.csv("data/SimpleWorld/csv")

ind <- read.csv("data/SimpleWorld/ind-full.csv")

# classes
class(ind)
class(ind$age)

# class coertion
ind_mat <- as.matrix(ind)
class(ind_mat[1,]) 
# cannot use $ because ind_mat is a vector
# class(ind_mat$age) 
# Error in ind_mat$age : $ operator is invalid for atomic vectors

ind[5,] # row 5
ind[,3] # col sex (vector)
ind[3] # col sex (as data.frame)
ind[,"sex"] #col sex as vector
ind["sex"] #col sex as data.frame
ind$sex #col sex as vector

library(dplyr)
# dplyr always returns a data.frame (concept: type stability)
slice(ind,5) #rows
select(ind,sex) #variables (columns)


#################################33
#################################

# Microdata
ind <- read.csv("data/CakeMap/ind.csv")
# number of individuals in microdata
nrow(ind)

# Constraints
cons <- read.csv("data/CakeMap/cons.csv")
# Number of zones
nrow(cons)

# Variables with constraints: age, sex, car and NSSEC

names(cons)
# Load constraints separately - normally this would be first stage
# age-sex, car, Socio-economic level

con1 <- cons[1:12] # load the age/sex constraint
con2 <- cons[13:14] # load the car/no car constraint
con3 <- cons[15:24] # socio-economic class

# Rename the categories in "ind" to correspond to the one of cons
str(ind)

ind$Car <- sapply(ind$Car, FUN = switch, "Car", "NoCar")
ind$Sex <- sapply(ind$Sex, FUN = switch, "m", "f")
ind$NSSEC8 <- as.factor(ind$NSSEC8)
levels(ind$NSSEC8) <- colnames(con3)
ind$ageband4 <- 
  gsub(pattern = "-", replacement = "_", x = ind$ageband4)

# Number of people per constraint (to check if it's the same number)
# rowSums(con1) #that's for all the zones

sum(con1[1,])
sum(con2[1,])
sum(con3[1,])

sum(rowSums(con1) == rowSums(con2))
sum(rowSums(con1) == rowSums(con3))

# It holds for only 52 zones. But we're working only with zone 1


# -- Initialise the table to the cross-table of the individual level data.
# This  creates cross-tabulation for all the variables
weight_init_1zone <- table(ind)
dimnames(weight_init_1zone)

# weight for the cross NCakes = <1, Car = Car, Sex = f, ...
weight_init_1zone[1,1,1,1,1]

library(mipfp)

# Transform con1 into an 3D-array : con1_convert
names <- c(dimnames(weight_init_1zone)[c(3,5)])
con1_convert <- array(NA, dim=c(2,6), dimnames = names)


  for (sex in dimnames(con1_convert)$Sex){
    for (age in dimnames(con1_convert)$ageband4){
      con1_convert[sex,age] <- con1[1,paste(sex,age,sep="")]
    }
  }

# # Alternative: but have to be careful with order of variables and categories (sex: m,f...)
a <- as.data.frame(matrix(con1[1,], 2, 6, byrow = T))

dimnames(weight_init_1zone)
target <- list(con1_convert,as.matrix(con2[1,]),as.matrix(con3[1,]))
descript <- list(c(3,5),2,4) #age-sex, car, nssec8
res <- Ipfp(weight_init_1zone,descript,target)
# It doesn't matter if seed has 5 vbles. 
# Ipfp only takes care of those included in target.list

res$x.hat
res$error.margins
res$conv

apply(res$x.hat,c(3,5),sum)


# -- Ipfp for all zones (we know that the constraints don't match for all the zones (only 52))

# a <- as.data.frame(array(con1, dim=c(2, 6, 124)))
# 
# dimnames(weight_init_1zone)
# target <- list(con1_convert,as.matrix(con2[1,]),as.matrix(con3[1,]))
# descript <- list(c(3,5),2,4) #age-sex, car, nssec8
# res <- Ipfp(weight_init_1zone,descript,target)


# Adding the spatial dimension: repeat the initial matrix

init_cells <- rep(weight_init_1zone, each = nrow(cons))


# Define the names
names <- c(list(rownames(cons)),
           as.list(dimnames(weight_init_1zone)))

# Structure the data
weight_all <- array(init_cells, dim = 
                       c(nrow(cons), dim(weight_init_1zone)),
                     dimnames = names)

# Need to convert con1 for each zone
names <- c(list(rownames(cons)),dimnames(weight_all)[c(4,6)])
con1_convert <- array(NA, dim=c(nrow(cons),2,6), dimnames = names)

for(zone in rownames(cons)){
  for (sex in dimnames(con1_convert)$Sex){
    for (age in dimnames(con1_convert)$ageband4){
      con1_convert[zone,sex,age] <- con1[zone,paste(sex,age,sep="")]
    }
  }
}

# Execute the ipf for each zone

for (i in 1:nrow(cons)){
  target <- list(con1_convert[i,,],as.matrix(con2[i,]),as.matrix(con3[i,]))
  descript <- list(c(3,5),2,4)
  res <- Ipfp(weight_init_1zone,descript,target)
  weight_all[i,,,,,] <- res$x.hat
}

# There are warnings because the constraints do not add up
# Check the constraint's totals per zone
table(rowSums(con2) == rowSums(con1))
table(rowSums(con3) == rowSums(con1))
table(rowSums(con2) == rowSums(con3))

dimnames(weight_all)

apply(weight_all[,,,,,], c(1,4,6), sum)[1,,]






########################################################
#### --- spatial data with R - CakeMap for all zones

ind <- read.csv("../data/CakeMap/ind.csv")
cons <- read.csv("../data/CakeMap/cons.csv")
# Load constraints separately - normally this would be first stage
con1 <- cons[1:12] # load the age/sex constraint
con2 <- cons[13:14] # load the car/no car constraint
con3 <- cons[15:24] # socio-economic class

# Rename the categories in "ind" to correspond to the one of cons
ind$Car <- sapply(ind$Car, FUN = switch, "Car", "NoCar")
ind$Sex <- sapply(ind$Sex, FUN = switch, "m", "f")
ind$NSSEC8 <- as.factor(ind$NSSEC8)
levels(ind$NSSEC8) <- colnames(con3)
ind$ageband4 <- 
  gsub(pattern = "-", replacement = "_", x = ind$ageband4)

# Initialise weights
weight_init_1zone <- table(ind)
init_cells <- rep(weight_init_1zone, each = nrow(cons))

# Define the names
names <- c(list(rownames(cons)),
           as.list(dimnames(weight_init_1zone)))

# Structure the data
weight_all <- array(init_cells, dim = 
                      c(nrow(cons), dim(weight_init_1zone)),
                    dimnames = names)

# Transform con1 into an 3D-array : con1_convert
names <- c(list(rownames(cons)),dimnames(weight_all)[c(4,6)])
con1_convert <- array(NA, dim=c(nrow(cons),2,6), dimnames = names)

for(zone in rownames(cons)){
  for (sex in dimnames(con1_convert)$Sex){
    for (age in dimnames(con1_convert)$ageband4){
      con1_convert[zone,sex,age] <- con1[zone,paste(sex,age,sep="")]
    }
  }
}

# Rescale con3 since it has some inconsistent constraints
con3_prop <- con3*rowSums(con2)/rowSums(con3)

# Load mipfp package
library(mipfp)

# Loop on the zones and make each time the mipfp
for (i in 1:nrow(cons)){
  target <- list(con1_convert[i,,],as.matrix(con2[i,]),as.matrix(con3_prop[i,]))
  descript <- list(c(3,5),2,4)
  res <- Ipfp(weight_init_1zone,descript,target)
  weight_all[i,,,,,] <- res$x.hat
}

# Results for zone 1
weight_init_1zone <- weight_all[1,,,,,]


# Aggregate to check if weights match constraints: age and sex, con2 and con3
aggr <- apply(weight_all[,,,,,], c(1,6,4), sum)
aggr <- aggr[,,c(2,1)] #order of sex to fit cons
aggr1 <- as.data.frame(aggr)
con2 <- apply(weight_all[,,,,,], c(1,3), sum)
con3 <- apply(weight_all[,,,,,], c(1,5), sum)
ind_aggr <- cbind(aggr1,con2,con3)

# To make de comparison I plot simulated counts and theoretical counts
plot(as.matrix(ind_aggr[1,]), as.matrix(cons[1,]), xlab = "simulated",
     ylab="theoretical", main="Validation for zone 1")

# To have a measure, we compute the Pearson's coefficient
# pearsoncor = function(a,b) cor(as.vector(as.matrix(ind_aggr)), as.vector(as.matrix(cons)))
# a<-  split(ind_aggr, rownames(ind_aggr))
# b <- split(ind_aggr, rownames(cons))
# pearson_all <- as.data.frame(mapply(pearsoncor, a, b))
# summary(pearson_all)

CorVec <- rep(0,nrow(cons))
for (i in 1:nrow(cons)) {
  CorVec[i] <- cor(as.numeric(ind_aggr[i,]), as.numeric(cons[i,]))
}

boxplot(CorVec)
summary(CorVec)
which.min(CorVec)
which(CorVec<.999)
which(CorVec<mean(CorVec))

# Method TRS to create a individual level dataset
# Expand de data
expa=as.data.frame.table(weight_init_1zone, responseName = "COUNT")
# Truncate
truncated <- expa 
truncated$COUNT <- floor(expa$COUNT)
# Sample
p = expa$COUNT - truncated$COUNT
n_missing = sum(p)
index <- sample(1:nrow(truncated), size = n_missing, prob = p, rep = F)
truncated$COUNT[index] <- truncated$COUNT[index] + 1
sum(truncated$COUNT)
