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

# TV you can also do this using 'colnames()' 

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



#================= 2. Size Correcting ===========================================


is.1 <- (S.1$island) #list of islands 
d.tr <- c(5,6,7,8,9,10,11) # here is the position of each trait 

Length  <-  h.d$svl #the trait we are size correcting against. 

is.land <-  is.1 #the random factor in the linear mixed model. This is the section
dataframe.to.use <-  h.d #the data
Traits.correct  <-  h.d[d.tr] #the columns in dataframe.to.use that are to be size corrected


head(h.d)
str(h.d)

#### 4 f.size.correct ####
# OKE ET AL COMMON GARDEN MANUSCRIPT METHOD
#"All relative warps and univariate shape traits were allometrically standardized to a common body size (Reist 1985; Lleonart et al. 2000) based on Ms = M0 (Ls / L0)^b, where Ms is the standardized trait value (mm), M0 is the non-standardized trait value (mm), Ls is the overall mean centroid size (for RWs) or standard length (mm, for univariate shape traits), and L0 is the centroid size or standard length (mm) of the individual. The common within-group slope, b, was calculated from a linear mixed model of log10(M0) regressed on log10(L0), with group included as a random factor (Reist 1985; Lleonart et al. 2000)." - Oke et al.    The size correction formula described above: Ms = M0(Ls/L0)^b

#variables:
#standard.length = h.d$svl 
#watershed = h.d$island
#dataframe.to.use =  h.d
#vector.of.columns = c(5:11)


#The output will have all the original columns from dataframe.to.use with the size-corrected columns added on at the end.
f.sizecorrect <- function(standard.length, vector.of.columns, dataframe.to.use, watershed) {
  #Calculate overall mean standard length (Ls)
  Ls <- mean(standard.length, na.rm = TRUE) 
  #Call individual standard length
  L0 <- standard.length
  #Calculate common, within-group slope, b, for each trait. 
  #Here, I am treating each section as a group for random factor
  b.vector.lmm <- vector()
  for (i in vector.of.columns) {
    abcd <- (dataframe.to.use[i])
    #b.model <- lmer((abcd[,])~(standard.length) + (1|watershed))
    #On 16 November, realized that this is wrong. Calculation of Beta needs log(M0) ~ Log(L0). This wasn't logged.
    #Re-running now with log10 values.
    b.model <- lmer(log10(abcd[,])~log10(standard.length) + (1|watershed))
    b <- coef(summary(b.model))[2,1]
    b.vector.lmm <- c(b.vector.lmm, b)
  }
  # size correct
  xx <- dataframe.to.use  
  columnnames <- colnames(xx)
  j=1
  for (i in vector.of.columns) {
    M0 <- xx[,i] #grab the appropriate column of data
    Ms = M0 * ((Ls/L0)^b.vector.lmm[j]) #size correction formula
    j=j+1
    columnnames <- c(columnnames, paste(colnames(xx[i]), "sc", sep = "."))
    xx <- cbind(xx, Ms)
  }
  colnames(xx) <- columnnames # Rename thh columns in the temporary dataframe xx
  return(xx) #Output a new dataframe with the name provided in "outputfilename"
}
# end 4 f.size.correct

# run the function on the anole data
#variables:
#standard.length = h.d$svl 
#watershed = h.d$island
#dataframe.to.use =  h.d
#vector.of.columns = c(5:11)


h.d.sc <- f.sizecorrect(h.d$svl, c(5:11),  h.d, h.d$island)

#----------------------------------------------------
# data frame with size corrected trait
head(h.d.sc)

#----- next analyses of size corrected traits

# need library lmerTest and cAIC4
library(lmerTest)  
# CHECK not 100% sure this library is needed 

# check the input data set layout
head(h.d.sc)

# names of the different models. Must match the model sequence in loop below
model.names <- c("sex * presence", "sex + presence", "sex","presence",
                 "intercept")

# from full model including interaction to intercept only

# make loop to go through the size corrected variables
# location of variables
var.loc <- c(12:ncol(h.d.sc))
var.names <- colnames(h.d.sc[var.loc])

# list to store all the model outputs for each variable in
list.AIC <- vector(mode = "list", length(var.loc))


for(i in 1:length(var.loc)){
  #i <- 1
  # select the response variable
  var.sel <- h.d.sc[,var.loc[i]]
  
  # make data storage object
  data.t <- as.data.frame(matrix(NA, ncol = 7, nrow = 5))
  colnames(data.t) <- c("model", "df", "AIC", "intercept", "sexm", "presence","sexm*presence")
  data.t[,1] <- model.names
  
  # go through all models 
  m.1 <- lmer(var.sel ~ h.d.sc$sex * h.d.sc$presence + (1|h.d.sc$island))
  m.2 <- lmer(var.sel ~ h.d.sc$sex + h.d.sc$presence + (1|h.d.sc$island))
  m.3 <- lmer(var.sel ~ h.d.sc$sex  + (1|h.d.sc$island))
  m.4 <- lmer(var.sel ~ h.d.sc$presence + (1|h.d.sc$island))
  m.5 <- lmer(var.sel ~ 1 + (1|h.d.sc$island))
  
  # assign model output to data storage object
  data.t[,2:3] <- AIC(m.1, m.2, m.3, m.4, m.5)
  # per model add fixes effects
  #  model m.1
  data.t[1,4:7] <- fixef(m.1)
  # model m.2
  data.t[2,4:6] <- fixef(m.2)
  # model m.3
  data.t[3,4:5] <- fixef(m.3)
  # model m.4
  data.t[4,4] <- fixef(m.4)[1]
  data.t[4,6] <- fixef(m.4)[2]
  # model m.5 
  data.t[5,4] <- fixef(m.5)
  
  # add variable names ( for each iteration of the loop new)
  m.results.all <- cbind.data.frame(rep(var.names[i], 5), data.t)
  m.results.all.s <- m.results.all[order(m.results.all$AIC),]
  colnames(m.results.all.s)[1] <- "variable.name"
  # store in list
  list.AIC[[i]] <- m.results.all.s
}

# merge all AIC scores
all.AIC <- do.call("rbind", list.AIC)

# save file as .csv to working directory
write.csv(all.AIC, "corrected.trait.model.output.csv", row.names = FALSE)









#  -----------------------------------------------------------------------------
# ----code WiLL ----
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












