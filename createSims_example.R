# reads a "base" sim file (XML) finds out a list of met files to run sim files to run these met files
# 7 Sep 2016: adapted from RA2 (new folder structure)
# 28 Nov 2016 Adjust start date

library(XML)
library(gsubfn) 

# identation function
xmlFormat <- function(doc, indent = 3) {
  s <- strsplit(saveXML(doc), "\n")[[1]]
  g <- gsubfn("^( +)", x ~ sprintf("%*s", indent * nchar(x), " "), s)
  paste(g, collapse = "\n")
}


# #Set folder locations (comment the ones not used) ------------------------------------

# Define paths  <---- INPUT HERE (Obs: change metFolder and climate together!)

rootFolder <- "C:\\GitHubRepos\\2017_DiscoveryScience" # high level location of files

# GCM <- c("HadGEM2-ES", "CESM1-CAM5", "GISS-EL-R", "BCC-CSM1.1", "GFDL-CM3", "NorESM1-M")

GCM <- c("HadGEM2-ES")

df_scn <- data.frame(climates = c("a_base","c_endCentury"), 
                     stRun = c(1971,1981), # reruns as start data was incorrect
                     enRun = c(1981,1990)
)

RCP <- c("RCP85")

# fns <- 3 # file name split position to ensure grid-cell number is taken from file name # FIXME: make names similar in structure - ERA = 2, RCP = 3

#### --- ERA set up is different (uncoment three lines below and run)
# GCM <- "ERA"
# df_scn <- data.frame(climates = "a_base", stRun = c(1971), enRun = c(2000))
# RCP <- "ERA"
# fns <- 2 # file name split position to ensure grid-cell number is taken from file name # FIXME: make names similar in structure- ERA = 2, RCP = 3

baseSimFolder <- paste0(rootFolder,"\\baseSim\\")# read base .sim files FROM here
simFolder <- paste0(rootFolder,"\\simFiles\\") # output new sims TO here

# Select the climate and cultivar scenario to run (!!!!! Atention !!!!!)  <---- INPUT HERE
climates <- as.character(df_scn$climates)
# cultivars <- c("short", "long")
# soilTypes <- c("highWHC", "lowWHC")
cultivars <- c("hybrid") # selected automaticaly in simulation
soilTypes <- c("BaseSim") # assumed high initially - all irrigated

# 5 loops (GCMs, climates, RCPs, ciltivars, soils) FIXME: A bit too much for nested loop ... please simplify
# gc = so = cl = rc = cv = 1
my.counter <- 0
for(gc in 1:length(GCM)){
  
  for(so in 1:length(soilTypes)) {
    
    for(cl in 1: length(climates)) {
      
      for(rc in 1:length(RCP)) {
        
        for (cv in 1:length(cultivars)) {
          
          #  my.counter <- my.counter+1
          #  print(paste0(my.counter," ",GCM[gc],soilTypes[so],climates[cl],RCP[rc], cultivars[cv]))
          
          # Define location of met files (default folder structure dependednt - Attention!!!!)
          metFolder <- paste0(rootFolder,"\\MetFiles\\",RCP[rc],"\\",GCM[gc],"\\",climates[cl] ,"\\")
          
          # find soil related sim
          # soils <- soilTypes[so]
         # rootSimFile <- paste0(baseSimFolder,soilTypes[so],".sim")
          rootSimFile <- paste0(baseSimFolder,"BaseSim.sim") # simple structure - not tested
          print(paste0("Running: ",rootSimFile))
          
          # get sim related info
          doc <- xmlTreeParse(rootSimFile, useInternalNodes = TRUE)
          nodesMet <- getNodeSet(doc, "//filename") 
          nodesOut <- getNodeSet(doc, "//outputfile")
          nodesStDate <- getNodeSet(doc, "//start_date")
          nodesEnDate <- getNodeSet(doc, "//end_date")
          nodesCV <- getNodeSet(doc, "//CultivarName")
          simNameRoot <- xmlRoot(doc)
          
          # sort out cultivar if needed
          # lapply(nodesCV, function(n) {     
          #   for (i in 1:length(cultivars)) {
          #     if(xmlValue(n) == cultivars[i]) # only change names if crop has these cultivars
          #       xmlValue(n) = cultivars[cv]
          #   }
          # })
          
          
          # sort out start and end dates of simulations
          stDate <- as.character(paste0("01/01/", df_scn$stRun[cl]))
          enDate <- as.character(paste0("01/01/", df_scn$enRun[cl]))
          
          lapply(nodesStDate, function(n) {
            xmlValue(n) = stDate
          })
          lapply(nodesEnDate, function(n) {
            xmlValue(n) = enDate
          })
          
          #----------------------------------------------------
          # Loop through met file names to create one .sim file for each met file
          #-----------------------------------------------------
          
          metFiles <- list.files(metFolder,pattern='.met', 
                                 recursive = TRUE,
                                 full.names=FALSE, 
                                 include.dirs = FALSE) # Option 1: gets all met files in a folder
          
          for(i in seq_along(metFiles)) {
      #      for(i in 1:10) { # For testing
            # get file name from each met and creates file names and attributes to change in new XML files
            metName <- metFiles[i]
            splitName <- unlist(strsplit(metFiles[i],"[.,/]"))
            # simName = paste(splitName[1],".sim", sep = "")
            
            # define out file name
            gridName <- splitName[1]  # ATTENTION!!! FIXME: this is folder structure dependant (2 for ERA 3 for GCM)
            outName <- paste(gridName, "_", 
                             climates[cl],"_", 
                             soilTypes[so], "_", 
                             cultivars[cv], "_",
                             RCP[rc], "_",
                             GCM[gc],
                             ".out", sep = "")
            
            # change attribute name of simulation 
            xmlAttrs(simNameRoot) = c(name = gridName)
            
            #  find address to point out to right met files
            newMetNode = paste(metFolder,metName,sep ="")
            
            # change met location
            lapply(nodesMet, function(n) {
              xmlValue(n) = newMetNode
            })
            
            # change outfile name
            lapply(nodesOut, function(n) {
              xmlValue(n) = outName
            })
            
            # FIXME: Create folder for GCM and time slice and RCP  - add these to sim name
            # suppressWarnings(dir.create(as.character()))
            
            # FIXME: identation of saved XML file is corrupted when it has text but no problem with functionality 
            saveXML(doc, file = paste0(simFolder,
                                       gridName, "_",
                                       climates[cl],"_", 
                                       soilTypes[so], "_", 
                                       cultivars[cv], "_",
                                       RCP[rc], "_",
                                       GCM[gc],
                                       ".sim"), indent=TRUE)
            
            
          }  # END MET FILES
          
          #-------------------------------------------------------------------
          
        } # END loop cultivars
        
      } # End loop RCPs
      
    } # END loop climates
    
  } # END LOOP SOILS
  
} # END LOOP GCMS

