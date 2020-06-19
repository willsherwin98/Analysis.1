################################################################################
#                                Main Script 

# Will Sherwin <- 02/20/2020

# R version 3.6.2 (2019-12-12)
################################################################################

#----------------------------TABLE OF CONTENTS----------------------------------

# - Overview
# - Packages downloaded
# - Libraries
# - Data Download 
# - Workflow 
# - Source the rest of the code

################################################################################
 
#===============================Overview========================================
#This project is to  analyze sexual size dimorphism of anolis lizards on 
#islands with and without a competitve congener. The following are the hypotheses 
#being tested: 

#Ho1 - island will have no effect on size
#Ho2 - competition will have no effect on size
#Ho3 - Competition and island will have no effect on size 

#The following contents within this main directory include the packages, 
#libraries, dataset and the workflow that is used in this analysis.
#scripts should be run in the folling order for replicatory results:

#    1.MainScript.An.R:(Data organization with the overview of the workflow used
#                   in this project)
#
#    2.Clean.Data.An.R: (How to clean the data that is used for these analyses)
#
#
#    3. Analysis.SVL.R: (This includes the code used to run mixed effect model 
#                           on SVL and the graph production)
#
#    4.Analysis.mass.R: (This includes the code used to run mixed effect model 
#                           on mass and the graph production)
#
#     5.Graphs.An.R (This contains all the code that is required to create the 
#                        graphs used from the analysis)

#=====================Packages needed for this analysis========================
#install.packages("car")
#install.packages("MASS")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lme4")
#=============================Load Packages=====================================
library(car)
library(MASS)
library(dplyr)
library(ggplot2)
library(lme4)

#============================Pathways===========================================

# The file of the raw data can be accessed here.
working.dir <- getwd()

# This should be set to the working directory of folder containing the data 
#after downloading this repo
Bones <- read.csv(" Bones.csv")
#-----------------------------------WORKFLOW------------------------------------

# In this working directory there 4 pathways to different folders with 
# specific outputs 

#           - 1.Raw.Data  -> the path to this folder is: rd.path
#                 (This folder contains a copy of the original downloaded
#                   data set without changes.)
#
#           - 2.Clean.Data -> the path to this folder is: cd.path
#                 (This folder contains  the cleaned data set.)
#
#           - 3.Analysis -> the path to this folder is: an.path
#                 (This folder contains saved outputs of our anaylsis.)
#
#           - 4.Graphs -> the path to this folder is: gr.path
#                 (This folder contains all visualizations of data.)

#create the folders
output.folders <- c("1.Raw.Data","2.Clean.Data","3.Analysis","4.Graphs")
# Make the folders using this loop code 
for(i in 1:length(output.folders)) 
  if(file.exists(output.folders[i]) == FALSE) 
    dir.create(output.folders[i])

#loop checks the output.folders list and checks to see 
# if the folders exist in the working directory. If they don't it will create 
# them. 


# Path to 1.Raw.Data folder
rd.path <- paste(working.dir,"/",output.folders[1], "/", sep="")

# Path to 2.Clean.Data
cd.path <- paste(working.dir,"/",output.folders[2], "/", sep="")

# Path to 3.Analysis
an.path <- paste(working.dir,"/",output.folders[3], "/", sep="")

# Path to 4.Graphs
gr.path <- paste(working.dir,"/",output.folders[4], "/", sep="")


# Now we can save the raw data into the raw data file. 
write.csv(Bones, paste(rd.path, "Bones.csv"),
          row.names = FALSE)


#========================Source the other Scripts==================================
source("2.Clean.Data.An.R")
source("3.Analysis.SVL.R")
source("4.Analysis.mass .R")
source("5.Graphs.An.R")




