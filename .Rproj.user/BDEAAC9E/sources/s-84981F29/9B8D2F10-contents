################################################################################

#                                 DATA CLEANING                                #

################################################################################
#This section includes the code to clean the data so that it is useable for our 
#analyses. 

#First, make a data frame including only the relevent colums and check 
#that it worked using the head function 
bison<-data.frame(raw.data$Elevation..ASL..m, raw.data$Long, raw.data$Lat,
                  raw.data$GISP2.Temp..IntCal13., raw.data$DstL)
head(bison)
str(bison)

# Get rid of the missing data points and check that it works using the structure 
# function 
biso <- na.omit(bison)
str(biso)

#Rename the columns to make them more usable and check that it works
names(biso)[names(biso)== "raw.data.Elevation..ASL..m"] <- "elev"
names(biso)[names(biso)== "raw.data.Long"] <- "long"
names(biso)[names(biso)== "raw.data.Lat"] <- "lat"
names(biso)[names(biso)== "raw.data.GISP2.Temp..IntCal13."] <- "temp"
names(biso)[names(biso)== "raw.data.DstL"] <- "dstl"
head(biso)

#Find the number of rows contained in dstl (needed to convert dstl to mass)
str(biso$dstl)
## 849 Rows

#Create a new vector containing dstl converted to mass 
#The formula for converting to mass was given in the paper
#Check that it worked using the head function 
mass <- (((biso$dstl[1:849])/11.43)^3)
head(mass)

#Add a new column to the data frame
bis <- cbind(biso, mass)

# Finally, check that the data frame is working and has all that you want 
str(bis)

#Now save the clean data to the clean.data folder

write.csv(bis, paste(cd.path, "bis.csv"),
          row.names = FALSE)


