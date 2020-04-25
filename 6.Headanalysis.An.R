#
# Will Sherwin 
# 24/04/2020 

# This is code of a regresssion of head traits compared to size of Anole (SVL)

#======================== First make the data easy to work with  ===============


# make a data frame including only the relevent colums and check 
#that it worked using the head function 
H.Data <-data.frame(Bones$island, Bones$sagrei.present,Bones$sex,Bones$svl,
                    Bones$real.lower.jaw.length, Bones$head.width.quadrates, 
                    Bones$head.ht, Bones$outlever, Bones$opening.inlever,
                    Bones$closing.inlever, Bones$frontal.parietal.to.snout)
head(H.Data)
str(H.Data)
View(H.Data)

# Get rid of the missing data points and check that it works 

# change unk's to NAs
H.Data[H.Data == "unk"] <- NA
# Get rid of Line Cedars island data 
H.Data[H.Data=="line.cedars"] <- NA

#take out NA values and see if it worked
h.d <- na.omit(H.Data)
View(h.d)
str(h.d)

# Rename the columns so they are easier to work with. 
names(h.d)[names(h.d)== "Bones.island"] <- "island"
names(h.d)[names(h.d)== "Bones.sagrei.present"] <- "presence"
names(h.d)[names(h.d)== "Bones.sex"] <- "sex"
names(h.d)[names(h.d)== "Bones.svl"] <- "svl"
names(h.d)[names(h.d)== "Bones.real.lower.jaw.length"] <- "Jaw.length"
names(h.d)[names(h.d)== "Bones.head.width.quadrates"] <- "Width"
names(h.d)[names(h.d)== "Bones.head.ht"] <- "Height"
names(h.d)[names(h.d)== "Bones.outlever"] <- "Outlever"
names(h.d)[names(h.d)== "Bones.opening.inlever"] <- "Open.inlever"
names(h.d)[names(h.d)== "Bones.closing.inlever"] <- "Close.inlever"
names(h.d)[names(h.d)== "Bones.frontal.parietal.to.snout"] <- "Head.length"

head(h.d)

# create a CSV of the new data and save it to the clean data folder. 
write.csv(h.d, paste(cd.path, "Clean.HeadMorph.csv"),
          row.names = FALSE)    







#try to make a loop to see the lm output for males and females on each island for each trait==== 


# here is the position of each trait 

d.tr <- c(5,6,7,8,9,10,11)
# here is the names of the columns for naming purposes 
var.names <- colnames(h.d)[d.tr]

# i <- 1
# t <- 1
#j <- 1

#first select what sex will be looked at 
for(j in 1:2){
  ###### determin what sex
  S.1 <- h.d[h.d$sex == sex[j],]
  
  
  #next select what island will be looked at with the specified sex 
  for(t in 1:length(is.1)){
    
    ####### islands for that sex 
    is.1 <- unique(S.1$island)
    
    d.h.3 <- S.1[S.1$island == is.1[t],]
    
    
    #Now look at the particular trait for that sex on that island
    for(i in 1:length(d.tr)){
      
    
      model.i <- lm(d.h.3[,d.tr[i]] ~ d.h.3$svl)
      

      
      #capture the output for of the model 
      sum.m <- capture.output(summary(model.i))
      
    #store in the analysis folder  
      cat(sum.m, file = paste(an.path, var.names[i],".txt", sep = ""))
      
    }
    
  }
}

# End loop 








