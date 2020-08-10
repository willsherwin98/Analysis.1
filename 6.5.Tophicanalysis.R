#
#  Will Sherwin 
# 24/04/2020 

#1. This is code of a regresssion of head traits compared to size of Anole 

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
    
    # model.i is running a linear regression on each trait for each sex on each island 
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
df <-  h.d #the data
Traits.correct  <-  h.d[d.tr] #the columns in df that are to be size corrected


head(h.d)
str(h.d)

#### 4 f.size.correct ####
# OKE ET AL COMMON GARDEN MANUSCRIPT METHOD
#"All relative warps and univariate shape traits were allometrically standardized to a common body size (Reist 1985; Lleonart et al. 2000) based on Ms = M0 (Ls / L0)^b, where Ms is the standardized trait value (mm), M0 is the non-standardized trait value (mm), Ls is the overall mean centroid size (for RWs) or standard length (mm, for univariate shape traits), and L0 is the centroid size or standard length (mm) of the individual. The common within-group slope, b, was calculated from a linear mixed model of log10(M0) regressed on log10(L0), with group included as a random factor (Reist 1985; Lleonart et al. 2000)." - Oke et al.    The size correction formula described above: Ms = M0(Ls/L0)^b

#variables:
sc.svl <-  h.d$svl 
sc.island <- h.d$island
df <- h.d
vector.of.columns = c(5:11)


#standard.length = h.d$svl 
#watershed = h.d$island
#dataframe.to.use =  h.d
#vector.of.columns = c(5:11)


#The output will have all the original columns from df with the size-corrected columns added on at the end.
f.sizecorrect <- function(sc.svl, vector.of.columns, df, sc.island) {
  #Calculate overall mean standard length (Ls)
  Ls <- mean(sc.svl, na.rm = TRUE) 
  #Call individual standard length
  L0 <- sc.svl
  #Calculate common, within-group slope, b, for each trait. 
  #Here, I am treating each section as a group for random factor
  b.vector.lmm <- vector()
  for (i in vector.of.columns) {
    abcd <- (df[i])
    #b.model <- lmer((abcd[,])~(sc.svl) + (1|sc.island))
    #On 16 November, realized that this is wrong. Calculation of Beta needs log(M0) ~ Log(L0). This wasn't logged.
    #Re-running now with log10 values.
    b.model <- lmer(log10(abcd[,])~log10(sc.svl) + (1|sc.island))
    b <- coef(summary(b.model))[2,1]
    b.vector.lmm <- c(b.vector.lmm, b)
  }
  # size correct
  xx <- df  
  columnnames <- colnames(xx)
  j=1
  i=5
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

f.sizecorrect(h.d$svl, c(5:11),  h.d, h.d$island)


# data frame with size corrected trait
sc.t <- f.sizecorrect(df$svl, (5:11), df, df$island)
#----- next analyses of size corrected traits


#======================= Model selection =======================================
#
#


# This section should be discarted and is no longer relavent to the analysis 


####################### variables 
#jaw.l <- sc.t$Jaw.length.sc
#width <- sc.t$Width.sc
#height <- sc.t$Height.sc
#out.l <- sc.t$Outlever.sc
#open.in <- sc.t$Open.inlever.sc
#close.in <- sc.t$Close.inlever.sc
#head.l <- sc.t$Head.length.sc
sex <- sc.t$sex
pres <- sc.t$presence
isl <- sc.t$island
trait <-  c("Jaw.length.sc","Width.sc", "Height.sc", "Outlever.sc", 
            "Open.inlever.sc", "Close.inlever.sc","Head.length.sc")
var.names <- colnames(sc.t)[12:18]



##################### Model 1 with presence + sex ##############################
i <- 1
for(i in 1:7){
  M1x <- aov(sc.t[,trait[i]]~pres+sex)
  
  
  # M1.out <- capture.output(summary.lm(M1x)) #capture the output of the summary of model.3 . 
  
  #cat(M1.out, file= paste(an.path,var.names[i],"m1.lm.txt" , sep = "")) 
  
  
  
  ######################## model 2 with comp + island ############################
  j <- 1
  for(j in 1:7){
    M2x <- aov(sc.t[,trait[j]]~pres+isl)
    
    #M2.out <- capture.output(summary.lm(M2x)) #capture the output of the summary of model.3 . 
    
    #cat(M2.out, file= paste(an.path,var.names[j],"m2.lm.txt" , sep = ""))
    
    
    
    #########################  model 3 effect of competition  ##############################
    t <- 1
    for(t in 1:7){
      M3x <- aov(sc.t[,trait[t]]~pres)
      
      #M3.out <- capture.output(summary.lm(M3x)) #capture the output of the summary of model.3 . 
      
      #cat(M3.out, file= paste(an.path,var.names[t],"m3.lm.txt" , sep = ""))
      
      
      ######################### model 4 effect of island ###########################
      e <- 1
      for(e in 1:7){
        M4x <- aov(sc.t[,trait[e]]~isl)
        
        #M4.out <- capture.output(summary.lm(M4x)) #capture the output of the summary of model.3 . 
        
        # cat(M4.out, file= paste(an.path,var.names[e],"m4.lm.txt" , sep = ""))
        
        
        ######################## model 5 effect of sex   ################################
        y <- 1
        for(y in 1:7){
          M5x <- aov(sc.t[,trait[y]]~sex)
          
          # M5.out <- capture.output(summary.lm(M5x)) #capture the output of the summary of model.3 . 
          
          #cat(M5.out, file= paste(an.path,var.names[y],"m5.lm.txt" , sep = ""))
          
          aic.out <- capture.output(AIC(M1x,M2x,M3x,M4x,M5x)) # Here is where we compare all the models (AIC)
          cat(aic.out,file=paste(an.path,var.names[y],"aic.txt", sep = ""))
        }
      } 
    }
  }
}
#========================= End 




###########         Models for SD of trophic traits ################

# model.1  sc trait ~  sex +# species + (1|island) 

# model.2  sc trait ~sex * # species + (1|island) 

# model.3 sc trait ~sex + # species + sex * # species + (1|island)

#==========================Variables for the model loop.======================== 
expl.variable <-  c("Jaw.length.sc","Width.sc", "Height.sc", "Outlever.sc", 
                    "Open.inlever.sc", "Close.inlever.sc","Head.length.sc")  #The position of each explanatory column
var.names <- colnames(sc.t)[12:18]  #here is where the names are assigned

#==================== loop for model 1 =========================================
# model.1  sc trait ~  sex +# species + (1|island)  ## W/O interaction 
i <- 1
for(i in 1:7){ #create a for loop to test each trophic trait 
  
  model.1 <- lmer(sc.t[,expl.variable[i]] ~ sc.t$sex + sc.t$presence + 
                    (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each explanatory variable.
  
  #sum.t <- capture.output(summary(model.1)) #capture the output of the summary of model.1 . 
  
  #cat(sum.t, file= paste(an.path,var.names[i],"M1.txt" , sep = "")) #cat = cancatonate and paste as a txt file in the results folder.  





#==================== loop for model 2 ================================================
# model.2  sc trait ~sex * # species + (1|island) ## With interaction of presence 

j <- 1
for(j in 1:7){ #create a for loop to test each tropic trait 
  
  model.2 <- lmer(sc.t[,expl.variable[j]] ~ sc.t$sex * sc.t$presence + 
                    (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each trait.
  
  #sum.t <- capture.output(summary(model.2)) #capture the output of the summary of model.2 . 
  
 #cat(sum.t, file= paste(an.path,var.names[j],"M2.txt" , sep = "")) # paste as a txt file in the results folder.  

##############       End loop for model 2        ##############



#==================== loop for model 3 ================================================
# model.3 sc trait ~sex + # species + sex * # species + (1|island) 

t <- 1
for(t in 1:7){ #create a for loop to for each trophic trait.  
  
  model.3 <- lmer(sc.t[,expl.variable[t]] ~ sc.t$sex + sc.t$presence + sc.t$sex
                  * sc.t$presence + (1|sc.t$island), data =sc.t, REML = FALSE)  #create a model for each sc trait .
  
  
  #sum.t <- capture.output(summary(model.3)) #capture the output of the summary of model.3 . 
  
  #cat(sum.t, file= paste(an.path,var.names[t],"M3.txt" , sep = "")) # paste as a txt file in the results folder.  

  AIC.OUT <- capture.output(AIC(model.1,model.2,model.3)) # Here is where we compare all the models (AIC)
  cat(AIC.OUT,file=paste(an.path,var.names[i],"ModAic.txt", sep = ""))
}
  }
    }
  
##############       End loop for models 1-3       ##############


