# CREATE A TABLE OF FUNCTIONAL TRAIT STRENGTHS PER SAMPLING AREA OUT OF
#    - TABLE OF SPECIES ABUNDANCES PER SAMPLING AREA
#    - TABLE OF SPECIES TRAITS

setwd("F:/Office/R_Python_Scripts/functional trait strengths table")
getwd()

traitindices1<-read.table("2017-07-13 Traits based on species list of fish in Bavaria.txt", sep="\t", header=T)
fish<-read.table("2017-07-13 Fish abundance per WFD sampling site 2004-2013.txt", header=T, sep="\t")

# remove column with species names
traitindices<-traitindices1[,-1]

# look at dataframes
ncol(traitindices)
head(traitindices)

traitindices[,48:52]
ncol(fish)
nrow(fish)
head(fish)

# Most sites were sampled several times - the average of samples at each site are calculated here - AverageFactor = sample sites
fishmean<-aggregate(.~AverageFactor, FUN=mean, data=fish)
ncol(fishmean)
nrow(fishmean)
head(fishmean)

# Prepare new empty data frames for trait-strength calculationS
#remove(alltraitindices)
#remove(multipliedtraitindices)
#remove(summultipliedtraitindices)
multipliedtraitindices<-data.frame(matrix(vector(), 56, 89))
summultipliedtraitindices<-data.frame(matrix(vector(), 1, 89))
alltraitindices<-data.frame(matrix(vector(), 399, 89))

head(fishmean)
nrow(fishmean)
traitindices[1,]

# each ROW is one SAMPLING AREA - ALL SPECIES ABUNDANCES OF ONE SAMPLING AREA ARE MULTIPLIED WITH  
# THE RESPECTIVE TRAIT STRENGTHS, then added up and introduced as single row in final data frame:
for(i in 1:nrow(fishmean)){
  for(j in 1:nrow(traitindices)){
    # multiply each species of one sampling area with all trait strengths of that species
    # - ONE ROW PER SPECIES; ONE DATAFRAME PER SAMPLING AREA (ABUNDANCE OF EACH SPECIES MULTIPLIED BY ALL RESPECTIVE TRAITS)
    multipliedtraitindices[j,]<-(as.numeric(fishmean[i,j]))*(as.numeric(traitindices[j,]))
  }
  # sum all species-traits product rows per dataframe (sampling area), 
  # such that the sum of each of the columns of each dataframe forms one row of the final dataframe, 
  # in which each row represents the total relative strength of each trait across all species at each sampling area. 
  alltraitindices[i,]<-colSums(multipliedtraitindices)
}

# export functional traits table
write.csv(alltraitindices, "F:/Office/R_Python_Scripts/functional trait strengths table/functionaltraits_table.csv")

# export the names of the sampling sites of the aggregated fish data where there is one aggregated sample per site.
write.csv(fishmean[,1], "F:/Office/R_Python_Scripts/functional trait strengths table/samplingsites.csv")

