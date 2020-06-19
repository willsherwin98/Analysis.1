#
# Will Sherwin 
# 24/04/2020 

#1. This is code of a regresssion of head traits compared to size of Anole (SVL)

#2. This code correcting for size 

#3. this code is to run a model on the size corrected traits 

# 1. ===================== First make the data easy to work with  ===============


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
  S.1 <- h.d[h.d$sex == sex[1],]
  
  
  #next select what island will be looked at with the specified sex 
  for(t in 1:length(is.1)){
    
    ####### islands for that sex 
    is.1 <- unique(S.1$island)
    
    d.h.3 <- S.1[S.1$island == is.1[1],]
    
    d.h.3
    
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


#determin what sex
S.1 <- h.d[h.d$sex == sex[2],]

####### islands for that sex 

d.h.3 <- S.1[S.1$island == is.1[11],]

model.i <- lm(d.h.3[,d.tr[7]] ~ d.h.3$svl)

summary(model.i)


model.1 <-  lm(h.d$Jaw.length ~ h.d$svl)
summary(model.1)

model.2 <-  lm(h.d$Width ~ h.d$svl)
summary(model.2)
model.3 <-  lm(h.d$Height ~ h.d$svl)
summary(model.3)
model.4 <-  lm(h.d$Outlever ~ h.d$svl)
summary(model.4)
model.5 <-  lm(h.d$Open.inlever ~ h.d$svl)
summary(model.5)
model.6 <-  lm(h.d$Close.inlever ~ h.d$svl)
summary(model.6)
model.7 <-  lm(h.d$Head.length ~ h.d$svl)
summary(model.7)



#================= 2. Size Corecting ===========================================


is.1 <- (S.1$island) #list of islands 
d.tr <- c(5,6,7,8,9,10,11) # here is the position of each trait 

Length  <-  h.d$svl #the trait we are size correcting against. 

is.land <-  is.1 #the random factor in the linear mixed model. This is the section
dataframe.to.use <-  h.d #the data
Traits.correct  <-  h.d[d.tr] #the columns in dataframe.to.use that are to be size corrected

f.sizecorrect <- function(Length , Traits.correct , dataframe.to.use, is.land) {
  #Calculate overall mean standard length (Mean.L)
  Mean.L <- mean(Length , na.rm = TRUE) 
  #Call individual standard length
  L0 <- Length 
  
  
  #calculate slope with  log10 values. 
  # treat each section as a group for random factor
  b.vector.lmm <- vector()
  i <- 5
  for (i in Traits.correct ) {
    abcd <- (dataframe.to.use[i])
    
  # b = slope 
    b.model <- lmer(log10(abcd[,])~log10(Length ) + (1|is.land))
    b <- coef(summary(b.model))[2,1]
    b.vector.lmm <- c(b.vector.lmm, b)
  }
     
    # trying to trouble shoot Error in model.frame.default(drop.unused.levels
   # = TRUE, formula = log10(abcd[,  : variable lengths differ (found for 'is.land') 
    
     model.frame(lmer(log10(abcd[,])~log10(Length ) + (1|is.land)), 
                data = h.d, subset = NULL, na.action = na.fail,
                drop.unused.levels = FALSE, xlev = NULL)
    
    
     
  # size correct
  xx <- dataframe.to.use  
  columnnames <- colnames(xx)
  i= 5
  j=1
  for (i in Traits.correct ) {
    M0 <- xx[,i] #grab the column of the trait you are interested in  
    Ms = M0 * ((Mean.L/L0)^b.vector.lmm[j]) #size correction formula
    j=j+1
    columnnames <- c(columnnames, paste(colnames(xx[i]), "sc", sep = "."))
    xx <- cbind(xx, Ms)
  }
  colnames(xx) <- columnnames # Rename columns in the  dataframe xx
  return(xx) #Output a new dataframe with the name provided in "outputfilename"
}
# end 


# ============== Alternative size to size correction??  =======================

# variables 
p.tr <- c(5,6,7,8,9,10,11) #  position of each trait 
h.trait <-  h.d[p.tr] 
s.ex <- h.d$sex
n.spp <- h.d$presence
len.gth <- h.d$svl
isle <- h.d$island

#formula 

alt.sc <- h.trait~s.ex+n.spp+len.gth+s.ex*n.spp+(1|isle)
### dont think this works for ou purposes. 












