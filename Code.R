#Load Packages
library(ergm)
library(network)
library(GGally)
library(ggplot2)
library(patchwork)

# Set Working Directory
setwd("/Users/mschalberger/Documents/Network Seminar")

# Load Data 
load('nigeriaMatList_acled_v7.rda') #loads yList object
load('exoVars.rda') #load xNodeL, xDyadL

#Focus on post 2000 data [few actors beforehand]
yrs = as.character(2000:2016)
yList = yList[yrs] ; xDyadL = xDyadL[yrs] ; xNodeL = xNodeL[yrs]

#Remove diagonal NAs
xDyadL <- lapply(xDyadL, function(x) {
  for (i in (1:dim(x)[3])) {
    diag(x[,,i]) <- 0
  };x
  })

#Turn xNodeL into numeric
xNodeL <- lapply(xNodeL, function(x) {
  lapply(x, as.numeric)
;x})

#Add Node Categories
xNodeL <- Map(cbind, xNodeL, actorcat = 0)

xNodeL <- lapply(xNodeL, function(x) {
   for (row in rownames(x)) {
     if (grepl("Ethnic Militia",row, fixed = TRUE)) {
       x[row,10] <- "Ethnic Militia"
    } else if (grepl("Forces of Nigeria",row, fixed = TRUE)) {
       x[row,10] <- "Government"
    } else if (grepl("Muslim",row, fixed = TRUE)) {
       x[row,10] <- "Religious Militia"
    } else if (grepl("Christian",row, fixed = TRUE)) {
       x[row,10] <- "Religious Militia"
    } else if (grepl("Boko Haram",row, fixed = TRUE)) {
       x[row,10] <- "Religious Militia"
    } else if (grepl("Kalo-Kato",row, fixed = TRUE)) {
       x[row,10] <- "Religious Militia"
    } else if (grepl("Kutep",row, fixed = TRUE)) {
       x[row,10] <- "Ethnic Militia"
    } else if (grepl("MASSOB",row, fixed = TRUE)) {
      x[row,10] <- "Separatist"
    } else if (grepl("MEND",row, fixed = TRUE)) {
      x[row,10] <- "Separatist"
    } else if (grepl("OPC",row, fixed = TRUE)) {
      x[row,10] <- "Separatist"
    } else {
       x[row,10] <- "Other"
    } 
   };
  x
})

# Change Names
yList <- lapply(yList, function(x) {c(rownames(x) <- gsub(' (Nigeria)', '', rownames(x), fixed = TRUE), 
                                    rownames(x) <- gsub(' Militia', '', rownames(x), fixed = TRUE),
                                    rownames(x) <- gsub("(:).*","",rownames(x)),
                                    rownames(x) <- gsub(" Forces of Nigeria","",rownames(x)),
                                    rownames(x) <- gsub("Ethnic","",rownames(x)),
                                    rownames(x) <- gsub(" Muslim","",rownames(x))); x})
yList <- lapply(yList, function(x) {c(colnames(x) <- gsub(' (Nigeria)', '', colnames(x), fixed = TRUE), 
                                      colnames(x) <- gsub(' Militia', '', colnames(x), fixed = TRUE),
                                      colnames(x) <- gsub("(:).*","",colnames(x)),
                                      colnames(x) <- gsub(" Forces of Nigeria","",colnames(x)),
                                      colnames(x) <- gsub("Ethnic","",colnames(x)),
                                      colnames(x) <- gsub(" Muslim","",colnames(x))); x})
xNodeL <- lapply(xNodeL, function(x) {c(rownames(x) <- gsub(' (Nigeria)', '', rownames(x), fixed = TRUE), 
                                      rownames(x) <- gsub(' Militia', '', rownames(x), fixed = TRUE),
                                      rownames(x) <- gsub("(:).*","",rownames(x)),
                                      rownames(x) <- gsub(" Forces of Nigeria","",rownames(x)),
                                      rownames(x) <- gsub("Ethnic","",rownames(x)),
                                      rownames(x) <- gsub(" Muslim","",rownames(x))); x})
xDyadL <- lapply(xDyadL, function(x) {c(rownames(x) <- gsub(' (Nigeria)', '', rownames(x), fixed = TRUE), 
                                        rownames(x) <- gsub(' Militia', '', rownames(x), fixed = TRUE),
                                        rownames(x) <- gsub("(:).*","",rownames(x)),
                                        rownames(x) <- gsub(" Forces of Nigeria","",rownames(x)),
                                        rownames(x) <- gsub("Ethnic","",rownames(x)),
                                        rownames(x) <- gsub(" Muslim","",rownames(x))); x})
xDyadL <- lapply(xDyadL, function(x) {c(colnames(x) <- gsub(' (Nigeria)', '', colnames(x), fixed = TRUE), 
                                        colnames(x) <- gsub(' Militia', '', colnames(x), fixed = TRUE),
                                        colnames(x) <- gsub("(:).*","",colnames(x)),
                                        colnames(x) <- gsub(" Forces of Nigeria","",colnames(x)),
                                        colnames(x) <- gsub("Ethnic","",colnames(x)),
                                        colnames(x) <- gsub(" Muslim","",colnames(x))); x})


#Create Non-Directed Network
Net04 <- network(x = yList$`2004`, directed = FALSE)
network.vertex.names(Net04) <- rownames(yList$`2004`)

Net09 <- network(x = yList$`2009`, directed = FALSE)
network.vertex.names(Net09) <- rownames(yList$`2009`)

Net16 <- network(x = yList$`2016`, directed = FALSE)
network.vertex.names(Net16) <- rownames(yList$`2016`)

#Add Vertex Attributes 
Net04%v%"vioCivEvents" <- as.numeric(xNodeL$`2004`[,1])
Net04%v%"riotsProtestsAgainst" <- as.numeric(xNodeL$`2004`[,8])
Net04%v%"groupSpread" <- as.numeric(xNodeL$`2004`[,9])
Net04%v%"actorcat" <- xNodeL$`2004`[,10]

Net09%v%"vioCivEvents" <- as.numeric(xNodeL$`2009`[,1])
Net09%v%"riotsProtestsAgainst" <- as.numeric(xNodeL$`2009`[,8])
Net09%v%"groupSpread" <- as.numeric(xNodeL$`2009`[,9])
Net09%v%"actorcat" <- xNodeL$`2009`[,10]

Net16%v%"vioCivEvents" <- as.numeric(xNodeL$`2016`[,1])
Net16%v%"riotsProtestsAgainst" <- as.numeric(xNodeL$`2016`[,8])
Net16%v%"groupSpread" <- as.numeric(xNodeL$`2016`[,9])
Net16%v%"actorcat" <- xNodeL$`2016`[,10]

#Summary Network
palette <- c("Government" = "#FF6863", "Ethnic Militia" = "#bece6f", "Religious Militia" = "#a6d0c8", "Separatist" = "#936fce", "Other" = "#949494") 

set.seed(123)
plot04 <- ggnet2(Net04, 
                 label = TRUE, 
                 color= "actorcat", 
                 palette = palette, 
                 size = "degree", 
                 size.min = 1)+
  guides(size = FALSE, color = FALSE)+
  ggtitle("2004")+
  theme(panel.background = element_rect(color = "grey50", fill = NA))+
  scale_x_continuous(limits = c(-0.1,1.1), breaks = NULL)

set.seed(12)
plot09 <- ggnet2(Net09, 
                 label = TRUE, 
                 color= "actorcat", 
                 palette = palette, 
                 size = "degree", 
                 size.min = 1)+
  guides(size = FALSE, color = FALSE)+
  ggtitle("2009")+
  theme(panel.background = element_rect(color = "grey50", fill = NA))+
  scale_x_continuous(limits = c(-0.1,1.1), breaks = NULL)

set.seed(123)
plot16 <- ggnet2(Net16, 
                 label = TRUE, 
                 color= "actorcat", 
                 palette = palette, 
                 size = "degree", 
                 size.min = 1,
                 legend.position = "right")+
  guides(size = FALSE)+
  ggtitle("2016")+
  theme(panel.background = element_rect(color = "grey50", fill = NA))+
  scale_x_continuous(limits = c(-0.1,1.1), breaks = NULL)

(plot04 | plot09 | plot16)

#ERGM Model
set.seed(123)
Mod04 <- ergm(Net04 ~ edges 
              + nodecov('vioCivEvents') 
              + nodematch('actorcat', diff = T, levels = c(#"Government",
                                                           "Ethnic Militia", "Religious Militia")) 
              + nodecov('riotsProtestsAgainst')
              + nodecov('groupSpread')
              + gwdegree(.5, fixed = T)
)
summary(Mod04)


set.seed(123)
Mod09 <- ergm(Net09 ~ edges 
              + nodecov('vioCivEvents') 
              + nodematch('actorcat', diff = T, levels = c(#"Government",
                                                           "Ethnic Militia", "Religious Militia")) 
              + nodecov('riotsProtestsAgainst')
              + nodecov('groupSpread')
              + gwdegree(.5, fixed = T)
              )
summary(Mod09)

set.seed(123)
Mod16 <- ergm(Net16 ~ edges 
              + nodecov('vioCivEvents') 
              + nodematch('actorcat', diff = T, levels = c(#"Government", 
                                                           "Ethnic Militia", "Religious Militia")) 
              + nodecov('riotsProtestsAgainst')
              + nodecov('groupSpread')
              + gwdegree(.5, fixed = T)
              )
summary(Mod16)

#Goodness of Fit
Mod04_gof <- gof(Mod04)
par(mfrow = c(2,1))
plot(Mod04_gof)

Mod09_gof <- gof(Mod09)
par(mfrow = c(2,1))
plot(Mod09_gof)

Mod16_gof <- gof(Mod16)
par(mfrow = c(2,1))
plot(Mod16_gof)

#Simulate
set.seed(1234)
sim_net04 = simulate(Mod04, nsim = 1)
sim04 <- ggnet2(sim_net04, label = TRUE, color= "actorcat", palette = palette,  size = "degree", size.min = 1, legend.position = "left")+
  guides(size = FALSE)+
  theme(panel.background = element_rect(color = "grey50", fill = NA))+
  scale_x_continuous(limits = c(-0.1,1.1), breaks = NULL)+
  ggtitle("Simulated")

(plot04 | sim04)

set.seed(1234)
sim_net09 = simulate(Mod09, nsim = 1)
sim09 <- ggnet2(sim_net09, label = TRUE, color= "actorcat", palette = palette,  size = "degree", size.min = 1, legend.position = "left")+
  guides(size = FALSE)+
  theme(panel.background = element_rect(color = "grey50", fill = NA))+
  scale_x_continuous(limits = c(-0.1,1.1), breaks = NULL)+
  ggtitle("Simulated")

(plot09 | sim09)

set.seed(1234)
sim_net16 = simulate(Mod16, nsim = 1)
sim16 <- ggnet2(sim_net16, label = TRUE, color= "actorcat", palette = palette,  size = "degree", size.min = 1)+
  guides(size = FALSE, color = FALSE)+
  theme(panel.background = element_rect(color = "grey50", fill = NA))+
  scale_x_continuous(limits = c(-0.1,1.1), breaks = NULL)+
  ggtitle("Simulated")

(plot16 | sim16)
