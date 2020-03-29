#LDFA mess around 

# Data 
Bones <- read.csv("~/Desktop/Quest/Quest Year 4 /Semester 2/Keystone /KEYSTONE /Data/ Bones.csv")

View(Bones)

#clean Data 
#First, make a data frame including only the relevent colums and check 
#that it worked using the head function 
data.DFA <-data.frame(Bones$island, Bones$sagrei.present,Bones$sex,Bones$head.ht,
                      Bones$outlever, Bones$real.lower.jaw.length, Bones$snout.width.eye,
                      Bones$head.width.quadrates, Bones$head.width.jugals) 
                      
                   
head(data.DFA)


# Get rid of the missing data points and check that it works using the structure 
# function 

# change unk's to NAs
data.DFA[data.DFA == "unk"] <- NA
#take out NA values and see if it worked
An.DFA <- na.omit(data.DFA)
View(An.DFA)


library(MASS)
# Fit the model
model.A <- lda(Bones.sagrei.present~Bones.head.ht+Bones.real.lower.jaw.length,
             data = An.DFA)
model.B <- lda(Bones.sex~Bones.head.ht+Bones.real.lower.jaw.length,
               data = An.DFA)



plot(model.B)

summary(predictions)


# Make predictions
predictions <-  predict(model.A, data = An.DFA)
# Model accuracy
mean(predictions$class==test.transformed$Species)


###########################Plot messs aroun##############
install.packages("klaR")
library(klaR)
partimat(Bones.sagrei.present ~ Bones.head.ht+Bones.real.lower.jaw.length,
         data = An.DFA, method="lda")

partimat(model.A)


# Scatterplot for 3 Group Problem 
pairs(An.DFA[c("Bones.head.ht","Bones.real.lower.jaw.length","Bones.head.width.jugals")], main="Title ", pch=22,
      bg=c("red", "yellow", "blue","green", "pink","orange","gray", "maroon2","firebrick","dark blue","dark green","white")[unclass(An.DFA$Bones.island)])
                                    
# cool!! not sure if super usefull doe 






#######COmputing Linear Discriminant Function Analysis with Scaled data#########

#Computing lda

#Compute lda using population number
Liz.lda.pop <- lda(Bones.island~+lenght.z+tox.z , data = Frogz)
Frogz.lda.pop
prop.pop = Frogz.lda.pop$svd^2/sum(Frogz.lda.pop$svd^2)
prop.pop
# first exaplain 65%, second expalins 31%, the third explains 5%

# Compute lda using weigth
Frogz.lda.weight <-lda(weigh.z~population.z+lenght.z+tox.z, data = Frogz)
Frogz.lda.weight
prop.weight = Frogz.lda.weight$svd^2/sum(Frogz.lda.weight$svd^2)
prop.weight
#First explains 75%. second explains 18% and third exaplains 0.06%


# Compute pca using svl ratio
Frogz.lda.length <- lda(lenght.z~population.z+weigh.z+tox.z, data = Frogz)
Frogz.lda.length
prop.length = Frogz.lda.length$svd^2/sum(Frogz.lda.length$svd^2)
prop.length

# Compute using toxicity
Frogz.lda.tox<- lda(tox.z~population.z+weigh.z+lenght.z, data = Frogz)
#error variables 1,2 and 3 appear to be constant within group

# Generate table
lda.data<-data.frame(prop.pop, prop.length, prop.weight)
lda.data