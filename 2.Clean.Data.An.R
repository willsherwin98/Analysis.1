

#                   DATA CLEANING    #

#First, make a data frame including only the relevent colums and check 
#that it worked using the head function 
A.data <-data.frame(Bones$island, Bones$sagrei.present,Bones$sex,Bones$svl,
                    Bones$mass)
head(A.data)
str(A.data)


# Get rid of the missing data points and check that it works using the structure 
# function 

# change unk's to NAs
A.data[A.data == "unk"] <- NA
#take out NA values and see if it worked
An.d <- na.omit(A.data)
View(An.d)
str(An.d)

# Get rid of Line Cedars island data 
A.data[A.data=="line.cedars"] <- NA
An.d <- na.omit(A.data)
View(An.d)

#Rename the columns to make them more usable and check that it works
names(An.d)[names(An.d)== "Bones.island"] <- "island"
names(An.d)[names(An.d)== "Bones.sagrei.present"] <- "presence"
names(An.d)[names(An.d)== "Bones.sex"] <- "sex"
names(An.d)[names(An.d)== "Bones.svl"] <- "svl"
names(An.d)[names(An.d)== "Bones.mass"] <- "Mass"
head(An.d)

#Now save the clean data to the clean.data folder
write.csv(An.d, paste(cd.path, "Clean.Anole.csv"),
          row.names = FALSE)