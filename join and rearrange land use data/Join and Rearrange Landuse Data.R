# JOIN AND REARRANGE DATAFRAMES OF LANDUSE PATTERNS ACCORDING TO SPECIFIC ORDER

setwd("F:/Office/R_Python_Scripts/join and rearrange land use data")

# load header list - represents correct order of variables - "%" should be replaced with "X." prior to loading. Any spaces should be replaced with ".".
headerlist<-read.table("HeaderList.txt", header = F, sep = ";")
headerlist

# change data type of header list to "character" - it loads as "factor"
headerlist<-as.character(headerlist[,1])
headerlist
length(headerlist)


# load filename list
filenamelist<-read.table("FileNameList.txt", header = F, sep = ";")
filenamelist

filenamelist<-as.character(filenamelist[,1])
filenamelist
length(filenamelist)


# load list of filenames without txt suffix for export
filenamelistnotxt<-read.table("FileNameListNotxt.txt", header = F, sep = ";")
filenamelistnotxt

filenamelistnotxt<-as.character(filenamelistnotxt[,1])
filenamelistnotxt
length(filenamelistnotxt)


rivername<-read.table(filenamelist[1], header = T, sep = "\t", dec = ",")
head(rivername)
nrow(rivername)


for(k in 1:length(filenamelist)){
  
# load variable data
  rivername<-read.table(filenamelist[k], header = T, sep = "\t", dec = ",")

# Create new empty dataframe for sorted data - the number of rows has to be changed to that of the input data
  rivernameSorted<-data.frame(matrix(ncol = 44, nrow = nrow(rivername)))

# Assemble new dataframe from old ones according to headerlist order
  for (i in 1:length(headerlist)){
    for (j in 1:ncol(rivername)){
      if(colnames(rivername)[j]==headerlist[i]){
        rivernameSorted[,i]<-rivername[,j]
      } 
    }
  }

  # Export dataframe
  write.csv(rivernameSorted, filenamelistnotxt[k], row.names = F)

}



