library(randomForest)
library(RJSONIO)
library(ROCR)

# Research Questions
# 1. What are the ranked importances of node encodings?
# 2. What are the ranked importances of edge encodings?
# 3. How important is network structure to noticeability?
# 3a. Where do participants tend to click?


#Converting HEX color to INT
rgbToInt <- function(red,green,blue) {
  # formula from https://www.shodor.org/stella2java/rgbint.html
  return(256*256*red+256*green+blue)
}
calcPerceivedBrightness <- function(red,green,blue) {
  # formula from http://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
  # Another potential option http://stackoverflow.com/questions/12043187/how-to-check-if-hex-color-is-too-black
  return( (0.299*red + 0.587*green + 0.114*blue) )
}

### Connecting through MONGO ####
## http://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite
library(rJava)
library(RMongo)
library(plyr)
pilotdb <- mongoDbConnect('pilot')
dbGetQuery(pilotdb, 'evaldata', '{}')['X_id']

connectSurveyToClickData <- function() {
  uniqueSessions <- unlist(unique(dbGetQuery(pilotdb, 'evaldata', '{}', skip=0, limit=1000000)['user']))
  connectedData <- c()
  for (u in uniqueSessions) {
    cat(u,'\n')
    cldata <- dbGetQuery(pilotdb, 'evaldata', paste('{ user : "',u,'", "page" : { "$ne" : "survey"} }', sep=''))
    sudata <- dbGetQuery(pilotdb, 'evaldata', paste('{ user : "',u,'", "page" : "survey" }', sep=''))
    if (dim(sudata)[1] == 0 || dim(sudata)[2] == 0) {
      next
    }
    else {
      sudata <- sudata[c("question1", "question2", "question3", "question4", "question5", "question6")]
    }
    combinedData <- cbind(cldata, sudata)
    connectedData <- rbind(connectedData, combinedData)
  }
  return(  data.frame(connectedData)  )
}


surveydata <- dbGetQuery(pilotdb, 'evaldata', '{ page: "survey" }')

# Survey Data
survey.df <- data.frame(surveydata[c("question1", "question2", "question3", "question4", "question5", "question6")])
par(mar=c(5.1, 13 ,4.1 ,2.1))
barplot(table(survey.df[,1]), las=2, horiz = T, main="Level of Education")
barplot(table(survey.df[,2]), las=2, horiz = T)
barplot(table(survey.df[,3]), las=2, horiz = T)
barplot(table(survey.df[,4]), las=2, horiz = T)
barplot(table(survey.df[,5]), las=2, horiz = T)

# Click Data
#collectedData <- dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}  }')
#collectedData <- dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}, "user":"488238d8-99be-e65d-ebb8-ce7c04c92b25"  }')
#expd.dat <- data.frame(collectedData[names(head(collectedData))])
expd.dat <- connectSurveyToClickData()
expd.dat$linewidth <- as.numeric(gsub('px','',expd.dat$linewidth))
expd.dat$nodeheight <- as.numeric(gsub('px','',expd.dat$nodeheight))
expd.dat$nodeborderwidth <- as.numeric(gsub('px','',expd.dat$nodeborderwidth))

#replace "cy.js selection blue" with "normal gray"
#expd.dat$nodebackground <- revalue(expd.dat$nodebackground, c("#0169D9"="#999999"))
expd.dat$nodebackground <- revalue(expd.dat$nodebackground, c("#999"="#999999"))
#expd.dat$linecolor <- revalue(expd.dat$linecolor, c("#0169D9"="#999999"))
expd.dat$linecolor <- revalue(expd.dat$linecolor, c("#999"="#999999"))
#rgbtpint
# tt <- makeRGBMat(expd.dat, 5)
# expd.dat[,5] <- as.numeric(rgbToInt(tt[,1], tt[,2], tt[,3]))
# tt2 <- makeRGBMat(expd.dat, 15)
# expd.dat[,15] <- as.numeric(rgbToInt(tt2[,1], tt2[,2], tt2[,3]))
#brightness
# expd.dat <- cbind(expd.dat, as.numeric(calcPerceivedBrightness(tt[,1], tt[,2], tt[,3])), as.numeric(calcPerceivedBrightness(tt2[,1], tt2[,2], tt2[,3])))
# colnames(expd.dat) <- c(colnames(expd.dat)[c(-35,-36)],"nodeBrightness", "lineBrightness")

nodett <- t(rgb2hsv((col2rgb(expd.dat$nodebackground))))
edgett <- t(rgb2hsv((col2rgb(expd.dat$linecolor))))
expd.dat <- cbind(expd.dat, 
                  as.numeric(nodett[,1]), as.numeric(nodett[,2]), as.numeric(nodett[,3]), 
                  as.numeric(edgett[,1]), as.numeric(edgett[,2]), as.numeric(edgett[,3]))
colnames(expd.dat) <- c(colnames(expd.dat)[1:(length(colnames(expd.dat))-6)], "nodeHue", "nodeSaturation", "nodeValue", "edgeHue", "edgeSaturation", "edgeValue")

# Without Binned Data
expd.dat.nobin <- expd.dat[Reduce(intersect, list( grep("bin", expd.dat$nodeEncoding2, invert = T), grep("bin", expd.dat$nodeEncoding1, invert = T), grep("bin", expd.dat$edgeEncoding1, invert = T), grep("bin", expd.dat$edgeEncoding2, invert = T) )), ]
expd.dat <- expd.dat.nobin


# sampleBalancedData <- function(ds,type) {
#   pos <- 0;
#   if (type == "nodes") {
#     pos = "NA"
#   }
#   else {
#     pos = "edge"
#   }
#   users <- unique(ds$user)
#   networks <- unique(ds$network)
#   newds <- c()
#   for (u in users) {
#     for (n in networks) {
#       selected <- ds[which( (ds$user == u) & (ds$network == n) & (ds$xposition != pos) & (ds$selected == 1)  ),]
#       numNotSelected <- length(which( (ds$user == u) & (ds$network == n) & (ds$xposition != pos) & (ds$selected != 1)  ))
#       notSelected <- ds[which( (ds$user == u) & (ds$network == n) & (ds$xposition != pos) & (ds$selected != 1)  ),][sample(1:numNotSelected, 1, replace = F),]
#       newds <- rbind(newds, selected, notSelected)
#       #cat(selected$selected, '\n')
#       #cat(notSelected$selected, '\n')
#     }
#   }
#   return( newds )
# }

sampleBalancedData <- function(ds,type) {
  pos <- 0;
  if (type == "nodes") {
    users <- unique(ds$user)
    networks <- unique(ds$network)
    newds <- c()
    for (u in users) {
      for (n in networks) {
        selected <- ds[which( (ds$user == u) & (ds$network == n) & (ds$eletype == "node") & (ds$selected == 1)  ),]
        numNotSelected <- length(which( (ds$user == u) & (ds$network == n) & (ds$eletype == "node") & (ds$selected != 1)  ))
        notSelected <- ds[which( (ds$user == u) & (ds$network == n) & (ds$eletype == "node") & (ds$selected != 1)  ),][sample(1:numNotSelected, dim(selected)[1], replace = F),]
        newds <- rbind(newds, selected, notSelected)
      }
    }
    return( newds )
  }
  else {
    users <- unique(ds$user)
    networks <- unique(ds$network)
    newds <- c()
    for (u in users) {
      for (n in networks) {
        selected <- ds[which( (ds$user == u) & (ds$network == n) & (ds$eletype == "edge") & (ds$selected == 1)  ),]
        numNotSelected <- length(which( (ds$user == u) & (ds$network == n) & (ds$eletype == "edge") & (ds$selected != 1)  ))
        notSelected <- ds[which( (ds$user == u) & (ds$network == n) & (ds$eletype == "edge") & (ds$selected != 1)  ),][sample(1:numNotSelected, dim(selected)[1], replace = F),]
        newds <- rbind(newds, selected, notSelected)
      }
    }
    return( newds )
  }
}

expd.dat.nodes.balanced <- sampleBalancedData(expd.dat, "nodes")
expd.dat.edges.balanced <- sampleBalancedData(expd.dat, "edges")

# Sampling Idea
# I suppose I can have up to 6 nodes without having to use SMOTE
# I will put this on hold because I don't think I need to balance classes yet
#dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}, "selected":0, "network":"rn2", "name" : {"$ne" : "NA"}  }')

# Click Map
plot(expd.dat$xposition, expd.dat$yposition, xlim=c(0,max(expd.dat$xposition, na.rm=T)), ylim=c(0,max(expd.dat$yposition, na.rm=T)), col="gray" )
points(expd.dat$xposition[which(expd.dat$selected == 1)], expd.dat$yposition[which(expd.dat$selected == 1)], col="red")
points(expd.dat$clickX, expd.dat$clickY, col="red")

# expd.nodes <- data.frame(expd.dat[which(!is.na(expd.dat$xposition)),])
# expd.nodes <- expd.nodes[which(as.numeric(as.character(expd.nodes$selected)) <= 1),]
# expd.edges <- data.frame(expd.dat[which(is.na(expd.dat$xposition)),])
# expd.edges <- expd.edges[which(as.numeric(as.character(expd.edges$selected)) <= 1),]

expd.edges <- expd.dat.edges.balanced
expd.nodes <- expd.dat.nodes.balanced

# Node Encodings Only Model / Selection
#expd.nodes.1 <- data.frame(expd.nodes[,c(3,5,10,14,19,24,35,42:44)])
expd.nodes <- expd.nodes[-which(expd.nodes$question4 == "colorBlind"),] # Removing colorblind
expd.nodes <- expd.nodes[which(!is.na(expd.nodes$selected)),]           # Removing NA

expd.nodes.1 <- data.frame(expd.nodes[,c(5,9,10,14,19,24,35,42:44)])
expd.nodes.1$nodeshape <- as.factor(expd.nodes.1$nodeshape)
expd.nodes.1$network <- as.factor(expd.nodes.1$network)
# Should I be excluding those who answered as "colorblind"? I removed them for the analysis above
#expd.nodes.1$network <- as.factor(expd.nodes.1$network)
#expd.nodes.1$nodebackground <- as.factor(expd.nodes.1$nodebackground)
expd.nodes.1$selected <- as.factor(expd.nodes.1$selected) # This will make it classification
expd.nodes.1$selected <- as.numeric(as.character(expd.nodes.1$selected)) # This will make it regression
expd.nodes.1$eletype <- as.factor(expd.nodes.1$eletype)
expd.nodes.1$question1 <- as.factor(expd.nodes.1$question1)
expd.nodes.1$question2 <- as.factor(expd.nodes.1$question2)
expd.nodes.1$question3 <- as.factor(expd.nodes.1$question3)
expd.nodes.1$question4 <- as.factor(expd.nodes.1$question4)
expd.nodes.1$question5 <- as.factor(expd.nodes.1$question5)

##  Downsampling is necessary if minority class is 15% or less
# Removing NA Values
# expd.nodes.1 <- expd.nodes.1[-which(is.na(expd.nodes.1$selected)),]
# Calculating minorty class presence
sum(expd.nodes.1$selected == 1) / length(expd.nodes.1$selected)
# Shows us that minority class is 16.9%, which is borderline!

expd.nodes.1c <- expd.nodes.1
expd.nodes.1c$selected <- as.factor(as.character(expd.nodes.1c$selected))


# Check for multicollinearily
library(rfUtilities)
#multi.collinear(expd.nodes.1[,c(-2)])
multi.collinear(expd.nodes.1[,c(-1,-2)])



# I could consider using "network" as strata below
#selectedPrevalence.nodes.1 <- sum(as.numeric(expd.nodes.1$selected))/length(as.numeric(expd.nodes.1$selected))
#unselectedPrevalence.nodes.1 <- 100-sum(as.numeric(expd.nodes.1$selected))/length(as.numeric(expd.nodes.1$selected))
tuneRF(x = expd.nodes.1[,c(-4)], y = expd.nodes.1$selected, importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1))
#rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1[,c(-3, -14)], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1))
#rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1[,c(-3, -8:-14)], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1))
#rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1[,c(-5, -6, -13)], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1))
#rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1[,-7], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1), keep.inbag = TRUE)
#rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1, importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1), keep.inbag = TRUE)
rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1[,-6], importance=TRUE, proximity=TRUE, keep.inbag = TRUE)
rf1.nodes.1c <- randomForest(as.factor(selected) ~ ., data=expd.nodes.1[,-6], importance=TRUE, proximity=TRUE, keep.inbag = TRUE)
#rf1.nodes.1c <- randomForest(selected ~ ., data=expd.nodes.1c[,-6], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1), keep.inbag = TRUE, ntree=1500)
print(rf1.nodes.1)
rf1.nodes.1$importance
write.table(rf1.nodes.1$importance, file="~/Documents/Evaluation of Importance Experiment/node_importance.csv", quote = F, row.names = T, col.names = T, sep=",")
varImpPlot(rf1.nodes.1,type=2, main = "Node Variable Importance")
nlabs <- rev(c("Network", "Node Border Size", "Node Size", "Node Saturation", "Node Value", "Node Shape", "Node Hue", "Node Degree"))
jpeg(filename="~/Documents/Evaluation of Importance Experiment/nodeVarImp.jpg", res=300, height = 1000, width=1500)
varImpPlot(rf1.nodes.1,type=2, main = "Node Variable Importance", labels=nlabs)
dev.off()
unique(expd.dat$participantResponse[which(!is.na(expd.dat$participantResponse))])
print(rf1.nodes.1c)
rf1.nodes.1c$importance
varImpPlot(rf1.nodes.1c,type=2)
# rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1[,c(1,3,4,7,14,15,16)], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1))
# print(rf1.nodes.1)
# rf1.nodes.1$importance
# varImpPlot(rf1.nodes.1,type=2)
# http://stats.stackexchange.com/questions/144700/negative-r2-at-random-regression-forest
# http://stats.stackexchange.com/questions/21152/obtaining-knowledge-from-a-random-forest

abline(v = abs(min(rf1.nodes.1$importance[,4])), lty="longdash", lwd=2)
rf1.nodes.1.p <- classCenter(expd.nodes.1[,c(-4)], expd.nodes.1[,4], rf1.nodes.1$proximity)
write.table(rf1.nodes.1.p, file="~/Documents/Evaluation of Importance Experiment/node_prototype.csv", quote = F, row.names = T, col.names = T, sep=",")

# Node Encodings Only Model / Reaction Time?

# Edge Encodings Only Model / Selection
expd.edges <- expd.edges[-which(expd.edges$question4 == "colorBlind"),] # Removing colorblind
expd.edges <- expd.edges[which(!is.na(expd.edges$selected)),]           # Removing NA

#expd.edges.1 <- data.frame(expd.edges[,c(3, 14, 21, 23, 45:47)])
expd.edges.1 <- data.frame(expd.edges[,c(9, 14, 21, 23, 45:47)])
expd.edges.1$linestyle <- as.factor(expd.edges.1$linestyle)
expd.edges.1$network <- as.factor(expd.edges.1$network)

##  Downsampling is necessary if minority class is 15% or less
# Calculating minorty class presence
sum(expd.edges.1$selected == 1) / length(expd.edges.1$selected)
# Shows us that minority class is 16.9%, which is borderline!

#multi.collinear(expd.edges.1[,c(-4)])
multi.collinear(expd.edges.1[,c(-1,-4)])

rf1.edges.1 <- randomForest(selected ~ ., data=expd.edges.1, importance=TRUE, proximity=TRUE, keep.inbag = TRUE)
print(rf1.edges.1)
rf1.edges.1$importance
write.table(rf1.edges.1$importance, file="~/Documents/Evaluation of Importance Experiment/edge_importance.csv", quote = F, row.names = T, col.names = T, sep=",")
varImpPlot(rf1.edges.1,type=2)
elabs <- rev(c("Line Width", "Network", "Edge Saturation", "Edge Value", "Edge Hue", "Line Pattern"))
jpeg(filename="~/Documents/Evaluation of Importance Experiment/edgeVarImp.jpg", res=300, height = 1000, width=1500)
varImpPlot(rf1.edges.1,type=2, main = "Edge Variable Importance", labels=elabs)
dev.off()
rf1.edges.1.p <- classCenter(expd.edges.1[,c(-2)], expd.edges.1[,2], rf1.edges.1$proximity)
write.table(rf1.edges.1.p, file="~/Documents/Evaluation of Importance Experiment/edge_prototype.csv", quote = F, row.names = T, col.names = T, sep=",")


# Edge Encodings Only Model / Reaction Time?

# negative value means the mean error is larger than the variance of the response
# y. This could be because the predictor performs really poorly but also
# because of some calibration issue.

# Try to attach demographic information to the DF and see how that affects selection
# cbind(expd.both, surveydata)


# NOTE THAT THE TREE IS UNBALANCED RIGHT NOW, AND MUST BE SAMPLED
# BALANCED BEFORE RESULTS ARE RELIABLE

rf1.perf.nodes = performance(  prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)  ,"tpr","fpr")
rf1.perf.edges = performance(  prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)  ,"tpr","fpr")

par(mfrow=c(1,2))
#plot the curve
plot(rf1.perf.nodes,main="ROC Curve for Random Forest (Nodes)",col=2,lwd=2)
lines(unlist(rf1.perf.nodes@x.values),unlist(rf1.perf.nodes@y.values), col=4, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

jpeg(filename="~/Documents/Evaluation of Importance Experiment/nodesROC.jpg", res=300, height = 1500, width=1200)
plot(rf1.perf.nodes,main="ROC Curve for Nodes (AUC = 0.78)",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
dev.off()

plot(rf1.perf.edges,main="ROC Curve for Random Forest (Edges)",col=2,lwd=2)
lines(unlist(rf1.perf.edges@x.values),unlist(rf1.perf.edges@y.values), col=4, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

jpeg(filename="~/Documents/Evaluation of Importance Experiment/edgesROC.jpg", res=300, height = 1500, width=1200)
plot(rf1.perf.edges,main="ROC Curve for Edges (AUC = 0.86)",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
dev.off()

#compute area under curve
auc.rf1.nodes <- performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"auc")
auc.rf1.nodes <- unlist(slot(auc.rf1.nodes, "y.values"))
minauc<-min(round(auc.rf1.nodes, digits = 2))
maxauc<-max(round(auc.rf1.nodes, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct
auc.rf1.edges <- performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"auc")
auc.rf1.edges <- unlist(slot(auc.rf1.edges, "y.values"))
minauc<-min(round(auc.rf1.edges, digits = 2))
maxauc<-max(round(auc.rf1.edges, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct

# Younden's Index (https://en.wikipedia.org/wiki/Youden%27s_J_statistic)
# Nodes
# TPR + TNR - 1 (http://stats.stackexchange.com/questions/29719/how-to-determine-best-cutoff-point-and-its-confidence-interval-using-roc-curve-i)
tpr.nodes.1 <- unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"tpr"), "y.values") )
tnr.nodes.1 <- unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"tnr"), "y.values") )
yi.nodes.1 <- tpr.nodes.1 + tnr.nodes.1 - 1 
tpr.nodes.1[which( (yi.nodes.1) == max(yi.nodes.1))]
tnr.nodes.1[which( (yi.nodes.1) == max(yi.nodes.1))]

unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"err"), "y.values") )[which( (yi.nodes.1) == max(yi.nodes.1))[1]]
unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"ppv"), "y.values") )[which( (yi.nodes.1) == max(yi.nodes.1))[1]]
unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"npv"), "y.values") )[which( (yi.nodes.1) == max(yi.nodes.1))[1]]
unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"cal"), "y.values") )[which( (yi.nodes.1) == max(yi.nodes.1))[1]]
unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"acc"), "y.values") )[which( (yi.nodes.1) == max(yi.nodes.1))[1]]
unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"fpr"), "y.values") )[which( (yi.nodes.1) == max(yi.nodes.1))[1]]
unlist(slot(performance(    prediction(labels = expd.nodes.1$selected, predictions = rf1.nodes.1$predicted)   ,"fnr"), "y.values") )[which( (yi.nodes.1) == max(yi.nodes.1))[1]]

# Edges
# TPR + TNR - 1
tpr.edges.1 <- unlist(slot(performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"tpr"), "y.values") )
tnr.edges.1 <- unlist(slot(performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"tnr"), "y.values") )
yi.edges.1 <- tpr.edges.1 + tnr.edges.1 - 1 
tpr.edges.1[which( (yi.edges.1) == max(yi.edges.1))]
tnr.edges.1[which( (yi.edges.1) == max(yi.edges.1))]

unlist(slot(performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"err"), "y.values") )[which( (yi.edges.1) == max(yi.edges.1))]
unlist(slot(performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"ppv"), "y.values") )[which( (yi.edges.1) == max(yi.edges.1))]
unlist(slot(performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"npv"), "y.values") )[which( (yi.edges.1) == max(yi.edges.1))]
unlist(slot(performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"cal"), "y.values") )[which( (yi.edges.1) == max(yi.edges.1))]
unlist(slot(performance(    prediction(labels = expd.edges.1$selected, predictions = rf1.edges.1$predicted)   ,"acc"), "y.values") )[which( (yi.edges.1) == max(yi.edges.1))]


# Forest Floor
library(forestFloor)
ff = forestFloor(
  rf.fit = rf1.nodes.1,       # mandatory
  X = expd.nodes.1,              # mandatory
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = FALSE  # takes no effect here when rfo$type="regression"
)

ffe = forestFloor(
  rf.fit = rf1.edges.1,       # mandatory
  X = expd.edges.1,              # mandatory
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = FALSE  # takes no effect here when rfo$type="regression"
)

#plot partial functions of most important variables first
plot(ff,                       # forestFloor object
     plot_seq = 1:9,           # optional sequence of features to plot
     orderByImportance=TRUE,    # if TRUE index sequence by importance, else by X column  
     col=ifelse(ff$Y, "red", "gray")
)

jpeg(filename="~/Documents/Evaluation of Importance Experiment/nodesPartial.jpg", res=300, height = 3000, width=3000)
plot(ff,                       # forestFloor object
     plot_seq = 1:9,           # optional sequence of features to plot
     orderByImportance=TRUE,    # if TRUE index sequence by importance, else by X column  
     col=ifelse(ff$Y, "red", "gray")
)
dev.off()

plot(ffe,                       # forestFloor object
     plot_seq = 1:9,           # optional sequence of features to plot
     orderByImportance=TRUE,    # if TRUE index sequence by importance, else by X column  
     col=ifelse(ffe$Y, "red", "gray")
)

jpeg(filename="~/Documents/Evaluation of Importance Experiment/edgesPartial.jpg", res=300, height = 3000, width=3000)
plot(ffe,                       # forestFloor object
     plot_seq = 1:9,           # optional sequence of features to plot
     orderByImportance=TRUE,    # if TRUE index sequence by importance, else by X column  
     col=ifelse(ff$Y, "red", "gray")
)
dev.off()

# Barplot of the selected networks
barplot(table(expd.nodes.1$network[(expd.nodes.1$selected == 1)]), las=2, horiz = T)

par(mfrow=c(3,3))
for(i in 1:9) partialPlot(rf1.nodes.1,expd.nodes.1,x.var=eval(names(expd.nodes.1)[i]))


partialPlot(rf1.nodes.1, expd.nodes.1, nodeshape)
partialPlot(rf1.nodes.1, expd.nodes.1, nodeheight)
partialPlot(rf1.nodes.1, expd.nodes.1, nodeHue)


library(rfPermute)
library(ggRandomForests)



##ggRandomForest
library(ggplot2)
library(RColorBrewer)
library(plot3D)
library(dplyr)
library(reshape)
library(reshape2)
library(randomForestSRC)
library(gridExtra)

theme_set(theme_bw())
event.marks <- c(1,4)
event.labels <- c(FALSE, TRUE)
strCol <- brewer.pal(3, "Set1")[c(2,1,3)]

expd.nodes.1.melted <- melt(expd.nodes.1, id.vars=c("nodeshape", "selected"))

ggplot(expd.nodes.1.melted, aes(x=nodeshape, y=value, color=factor(selected)))+
  geom_point(alpha=.4)+
  geom_rug(data=expd.nodes.1.melted %>% filter(is.na(value)))+
  # labs(y="", x=nodeshape) +
  scale_color_brewer(palette="Set2")+
  facet_wrap(~variable, scales="free_y", ncol=3)


rfsc_selected <- rfsrc(selected ~ ., data=expd.nodes.1)

#plot OOB against growth of forests
gg_e <- gg_error(rfsc_selected)
plot(gg_e)

# VIMP
plot(gg_vimp(rfsc_selected))

# Minimal Depth
varsel_node <- var.select(rfsc_selected)
gg_md <- gg_minimal_depth(varsel_node)
plot(gg_md)

# Compare VIMP and Minimal depth
plot(gg_minimal_vimp(gg_md))

# Variable Dependence (this can theoretically be generated by plotting the results from RF for each variable)
gg_v <- gg_variable(rfsc_selected)
xvar <- gg_md$topvars

plot(gg_v, xvar=xvar, panel=TRUE,
  alpha=.4)
  # labs(y=selected, x="")

# Partial Dependence (this is in the randomForest library and works there, so no need for this.)
partial_node <- plot.variable(rfsc_selected, xvar=gg_md$topvars, partial=TRUE, sorted=FALSE, show.plots=FALSE)
gg_p <- gg_partial(partial_node)
plot(gg_p, xvar=xvar, panel=TRUE)


interaction_nodes <- find.interaction(rfsc_selected)
plot(interaction_nodes, xvar=gg_md$topvars, panel=TRUE)
heatmap(interaction_nodes)

## Trying ICEbox
library(ICEbox)

nodes.ice1 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "nodeheight", frac_to_build = .1)
nodes.ice2 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "nodeHue", frac_to_build = .1)
nodes.ice3 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "nodeborderwidth", frac_to_build = .1)
nodes.ice4 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "numEdges", frac_to_build = .1)
#nodes.ice5 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "nodeshape", frac_to_build = .1) #doesn't handle factors...I may be able to make a dummy variable?
nodes.ice6 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "numConnected", frac_to_build = .1)
nodes.ice7 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "nodeValue", frac_to_build = .1)
nodes.ice8 <- ice(object = rf1.nodes.1, X = expd.nodes.1, y = expd.nodes.1$selected, predictor = "nodeSaturation", frac_to_build = .1)
nodes.dice1 <- dice(nodes.ice1)
nodes.dice2 <- dice(nodes.ice2)
nodes.dice3 <- dice(nodes.ice3)
nodes.dice4 <- dice(nodes.ice4)
#nodes.dice5 <- dice(nodes.ice5)
nodes.dice6 <- dice(nodes.ice6)
nodes.dice7 <- dice(nodes.ice7)
nodes.dice8 <- dice(nodes.ice8)

# ICE = individual conditional expectation curves
dev.off()
par(mfrow=c(2,4))
plot(nodes.ice1)
plot(nodes.ice2)
plot(nodes.ice3)
plot(nodes.ice4)
plot(nodes.ice6)
plot(nodes.ice7)
plot(nodes.ice8)

# DICE = Estimates the partial derivative function for each curve in an ice object
dev.off()
par(mfrow=c(2,4))
plot(nodes.dice1)
plot(nodes.dice2)
plot(nodes.dice3)
plot(nodes.dice4)
plot(nodes.dice6)
plot(nodes.dice7)
plot(nodes.dice8)



# Interactions
library(plotmo)
plotmo(rf1.nodes.1c, type="prob")
plotmo(rf1.nodes.1)
plotmo(rf1.edges.1)
plotmo(rf1.nodes.1, pt.col = heat.colors(12))

jpeg(filename="~/Documents/Evaluation of Importance Experiment/nodesInteraction.jpg", res=300, height = 3000, width=3000)
plotmo(rf1.nodes.1)
dev.off()

jpeg(filename="~/Documents/Evaluation of Importance Experiment/edgesInteraction.jpg", res=300, height = 3000, width=3000)
plotmo(rf1.edges.1)
dev.off()


# Saving the RF Models
saveRDS(rf1.nodes.1, file="~/Documents/Evaluation of Importance Experiment/rf1.nodes.1.rds")
saveRDS(rf1.edges.1, file="~/Documents/Evaluation of Importance Experiment/rf1.edges.1.rds")
# to save entire session, use save.image()
# save.image(file="~/Documents/Evaluation of Importance Experiment/Aim3.RData")

# Qualitative Responses
unique(expd.dat.nodes.balanced[!is.na(expd.dat.nodes.balanced$participantResponse),]$participantResponse)
unique(expd.dat.edges.balanced[!is.na(expd.dat.edges.balanced$participantResponse),]$participantResponse)

# Calculate participation drop off?
table(expd.dat$user)


# Webers Law
calcWebersForRows <- function(rowIndexStart, rowIndexEnd, c) {
  nodeCombs <- c()
  for (i in rowIndexStart:rowIndexEnd) {
    for (k in i:rowIndexEnd) {
      cat(i,k,expd.dat[i,c],expd.dat[k,c],
          calcWeber(expd.dat[i,c],expd.dat[k,c]),'\n')
      nodeCombs <- rbind(nodeCombs,
                         c(i,k,expd.dat[i,c],expd.dat[k,c],calcWeber(expd.dat[i,c],expd.dat[k,c])))
    }
  }
  colnames(nodeCombs) <- c("index1", "index2", "value1", "value2", "K")
  return(nodeCombs)
}

calcWeber <- function(a,b) {
  return( abs(a - b) / b )
}

calcWebersForRows(1, 4, 35)
calcWebersForRows(8, 11, 42)


for (n in unique(expd.dat[which(expd.dat$eletype == "node"),9]) ) {
  numeros <- which(expd.dat[which(expd.dat$eletype == "node"),9] == n)
  
  cat(which(expd.dat[which(expd.dat$eletype == "node"),9] == n),'\n')
}

countContiguous <- function(numeros) {
  contig <- list()
  k = 1;
  contig[[k]] <- vector()
  for (i in length(numeros)) {
    if ( numeros[i+1 || i] == numeros[i] + 1 ) {
      contig[[k]] <- append(i, contig[[k]])
    }
    else {
      k = k + 1;
      contig[[k]] <- append(i, contig[[k]])
    }
  }
  return( contig )
}

# Count length of line
subNet <- expd.dat[1:7,c(7,8,13,33)]
for (i in which(is.na(subNet$xposition))) {
  # subNet[subNet[i,]$elesource,]$xposition, subNet[subNet[i,]$elesource,]$yposition
  # subNet[subNet[i,]$eletarget,]$xposition, subNet[subNet[i,]$eletarget,]$yposition
  dd <- sqrt((subNet[subNet[i,]$elesource,]$xposition - subNet[subNet[i,]$eletarget,]$xposition)^2 +
  (subNet[subNet[i,]$elesource,]$yposition - subNet[subNet[i,]$eletarget,]$yposition)^2)
  cat(subNet[i,]$elesource, subNet[i,]$eletarget, dd,'\n')
}

# Attach an "edgeLength" column to expd.dat
edgeLength <- matrix(0, dim(expd.dat)[1])
subNets <- unique(paste(expd.dat$user,'---',expd.dat$network,sep=''))
for (s in subNets) {
  li <- strsplit(s, '---')
  u <- li[[1]][1]
  n <- li[[1]][2]
  inds <- which(expd.dat$network == n & expd.dat$user == u)
  subNet <- expd.dat[inds,c(7,8,13,33)]
  tsn <- c()
  for (i in which(is.na(subNet$xposition))) {
    dd <- sqrt((subNet[subNet[i,]$elesource,]$xposition - subNet[subNet[i,]$eletarget,]$xposition)^2 +
                 (subNet[subNet[i,]$elesource,]$yposition - subNet[subNet[i,]$eletarget,]$yposition)^2)
    cat(subNet[i,]$elesource, subNet[i,]$eletarget, dd,'\n')
    tsn <- append(tsn, dd)
  }
  edgeLength[inds[is.na(expd.dat[inds,]$xposition)]] <- tsn
  
}




# Calculate Steven's Power Law
selnodesonly <- expd.dat[which((expd.dat$selected == 1) & (expd.dat$xposition != 0)),] #selected nodes only
selnodesonly <- selnodesonly[which(selnodesonly$nodeEncoding1 == "node border (bin)" | selnodesonly$nodeEncoding1 == "node border (quant)" | selnodesonly$nodeEncoding2 == "node border (bin)" | selnodesonly$nodeEncoding2 == "node border (quant)"),]
log(selnodesonly[,35])
# Cannot be calculated because this experiment does not capture data
# about the perceived intensity of a data point, rather only the actual intensity
# used to visualize the data



# Node combinations
unique(cbind(expd.nodes$nodeEncoding1,expd.nodes$nodeEncoding2))

# Edge combinations
unique(cbind(expd.edges$edgeEncoding1,expd.edges$edgeEncoding2))


# Regression Models
glmn <- glm(selected ~ ., data = expd.nodes.1, family = "binomial")
glmn <- glm(selected ~ nodeheight + nodeborderwidth + nodeSaturation + nodeValue, data = expd.nodes.1, family = "binomial")
glmn <- glm(selected ~ nodeheight + nodeSaturation + nodeValue + nodeborderwidth + nodeshape + network + user, data = expd.dat, family = "binomial")
glmn <- glm(selected ~ nodeheight + nodeSaturation + nodeValue + nodeborderwidth + nodeshape, data = expd.nodes.1, family = "binomial")
glmn <- glm(selected ~ nodeheight + nodeSaturation + nodeValue + nodeborderwidth, data = expd.nodes.1, family = "binomial")
summary(glmn)
glme <- glm(selected ~ ., data = expd.edges.1, family = "binomial")
summary(glme)

library(pscl)

# Multinomial Logistic Regression Model
library(nnet)

mlrm <- multinom(selected ~ ., data = expd.nodes.1)
summary(mlrm)

# Mixed Effects Model
library(lme4)

lmn <- lm(selected ~ ., data = expd.nodes.1)
summary(lmn)
lme <- lm(selected ~ ., data = expd.edges.1)
summary(lme)
lmner <- lmer(selected ~ nodeheight + nodeSaturation + nodeValue + nodeborderwidth + (1 | user) + (1 | network), data = expd.dat)
lmner2 <- lmer(selected ~ nodeheight + nodeSaturation + nodeValue + nodeborderwidth + nodeHue + nodeshape + (1 | user) + (1 | network), data = expd.dat)
lmner3 <- lmer(selected ~ nodeheight + nodeSaturation + nodeValue + nodeborderwidth + nodeHue + nodeshape + network + (1 | user), data = expd.dat)
lmner4 <- lmer(selected ~ nodeheight + nodeSaturation + nodeValue + nodeborderwidth + nodeshape + network + (1 | user), data = expd.dat)
summary(lmner)
summary(lmner2)
summary(lmner3)
summary(lmner4)
glme <- glm(selected ~ ., data = expd.edges.1)
summary(glme)


library(popbio)
logi.hist.plot(expd.nodes.1$nodeheight, expd.nodes.1$selected, boxp = F, type = "hist", col="gray", xlabel = "node size")
logi.hist.plot(expd.nodes.1$nodeborderwidth, expd.nodes.1$selected, boxp = F, type = "hist", col="gray", xlabel = "node border width")
logi.hist.plot(expd.nodes.1$nodeValue, expd.nodes.1$selected, boxp = F, type = "hist", col="gray", xlabel = "node value")
logi.hist.plot(expd.nodes.1$nodeSaturation, expd.nodes.1$selected, boxp = F, type = "hist", col="gray", xlabel = "node saturation")
logi.hist.plot(expd.nodes.1$numConnected, expd.nodes.1$selected, boxp = F, type = "hist", col="gray", xlabel = "node degree")

logi.hist.plot(expd.edges.1$linewidth, expd.edges.1$selected, boxp = F, type = "hist", col="gray", xlabel = "edge width")
logi.hist.plot(expd.edges.1$edgeSaturation, expd.edges.1$selected, boxp = F, type = "hist", col="gray", xlabel = "edge saturation")
logi.hist.plot(expd.edges.1$edgeValue, expd.edges.1$selected, boxp = F, type = "hist", col="gray", xlabel = "edge value")
logi.hist.plot(expd.edges.1$edgeHue, expd.edges.1$selected, boxp = F, type = "hist", col="gray", xlabel = "edge hue")

plot(expd.edges.1$edgeValue, expd.edges.1$linewidth, col=ifelse(expd.edges.1$selected, "red", "gray"))
pairs(expd.edges.1[,-2], col=ifelse(expd.edges.1$selected, "red", "gray"))

plot( performance(  prediction(labels = expd.nodes.1$selected, predictions = ifelse(predict(glmn, type = "response") >= 0.1, 1, 0))  ,"tpr","fpr"), main="ROC Curve for Logistic Regression (Nodes)")


useCoefs <- function(invec) { return( sum(glmn$coefficients*invec) ) }
useCoefsCalcProb <- function(odr) { return( exp(odr) / (exp(odr) + 1)) }

useCoefsCalcProb(useCoefs(c(1,27,0,1,0)))

pvec <- c()
for (i in 1:100) {
  pvec <- append(pvec, useCoefsCalcProb(useCoefs(c(1,i,0,0,0))))
}
plot(1:100, pvec, type="l", ylim=c(0,1), xlim=c(0,100), main="nodeheight vs prob")

pvec2 <- c()
for (i in 1:10) {
  pvec2 <- append(pvec2, useCoefsCalcProb(useCoefs(c(1,0,i,0,0))))
}
plot(1:10, pvec2, type="l", ylim=c(0,1), xlim=c(0,10), main="nodeborder vs prob")

pvec3 <- c()
for (i in (1:100/100)) {
  pvec3 <- append(pvec3, useCoefsCalcProb(useCoefs(c(1,0,0,i,0))))
}
plot(1:100, pvec3, type="l", ylim=c(0,1), xlim=c(0,100), main="nodesaturation vs prob")

pvec4 <- c()
for (i in (1:100/100)) {
  pvec4 <- append(pvec4, useCoefsCalcProb(useCoefs(c(1,0,0,0,i))))
}
plot(1:100, pvec4, type="l", ylim=c(0,1), xlim=c(0,100), main="nodevalue vs prob")



# Function for Cohen's D to determine effect size
cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
}

# > res <- cohens_d(expd.dat$nodeheight[expd.dat$selected == 1], expd.dat$nodeheight[expd.dat$selected == 0])
# > res
# [1] 0.02592582
# > res <- cohens_d(expd.dat$nodeborderwidth[expd.dat$selected == 1], expd.dat$nodeborderwidth[expd.dat$selected == 0])
# > res
# [1] 1.01351

library(RColorBrewer)
g1 <- erdos.renyi.game(11, 1)
V(g1)$size <- c(sample(15:20, 10, replace=T), sample(21:30, 1, replace=T))
# V(g1)$color <- brewer.pal(n = 11, name = "RdBu")[order(V(g1)$size)]
V(g1)$color <- c(rep(hsv(h = 0.2, s = 0.5, v = 0.8), 10), rep(hsv(h = 0.2, s = 1, v = 0.3), 1))
plot(g1)
get.edgelist(g1)



# NLME
library(nlme)
plot(expd.nodes.1)
model1 = lm(selected ~ ., data=expd.nodes.1)
summary(model1)
model2 = lme(selected ~ ., data=expd.nodes.1, random= ~1|network)
summary(model2)





































#########################################

col2rgb(htmlcolors)

black	#000000	0,0,0
silver	#C0C0C0	192,192,192
gray	#808080	128,128,128
white	#FFFFFF	255,255,255
maroon	#800000	128,0,0
red	#FF0000	255,0,0
purple	#800080	128,0,128
fuchsia	#FF00FF	255,0,255
green	#008000	0,128,0
lime	#00FF00	0,255,0
olive	#808000	128,128,0
yellow	#FFFF00	255,255,0
navy	#000080	0,0,128
blue	#0000FF	0,0,255
teal	#008080	0,128,128
aqua	#00FFFF	0,255,255

# https://www.w3.org/TR/css3-color/
eucColor <- function(inhex) {
  htmlcolornames <- c("black", "silver", "gray", "white", "maroon", "red", "purple",
                      "fuchsia", "green", "lime", "olive", "yellow", "navy", "blue", "teal",
                      "aqua")
  htmlcolors <- c("#000000",
                  "#C0C0C0",
                  "#808080",
                  "#FFFFFF",
                  "#800000",
                  "#FF0000",
                  "#800080",
                  "#FF00FF",
                  "#008000",
                  "#00FF00",
                  "#808000",
                  "#FFFF00",
                  "#000080",
                  "#0000FF",
                  "#008080",
                  "#00FFFF")
  qrgb <- col2rgb(inhex)
  htem <- matrix(c((col2rgb(htmlcolors)[1,] - qrgb[1,])^2,
           (col2rgb(htmlcolors)[2,] - qrgb[2,])^2,
           (col2rgb(htmlcolors)[3,] - qrgb[3,])^2), 16, 3)
  vals <- sqrt(apply(htem, FUN=sum, MAR=1))
  return(  htmlcolornames[which(vals == min(vals))]  )
}

"#cadef1" "#dde8f8" "#eff3ff" "#c6dbef"
eucColor("#cadef1")
eucColor("#dde8f8")
eucColor("#eff3ff")
eucColor("#c6dbef")
eucColor("#999999")
sapply(expd.dat$nodebackground, FUN=eucColor)
sapply(expd.dat$linecolor, FUN=eucColor)






# RF UTILITIES
library(rfUtilities)

multi.collinear(expd.nodes.1[,2:7])
# This shows that I can remove nodeheight from the model since it mirrors nodewidth

multi.collinear(expd.edges.1[,-3])
# No multicollinearity

multi.collinear(expd.both[,c(-3, -7, -10, -15)])
# In addition to nodeheight, windowheight, windowwidth, and nodeBrightness
# may be removed due to collinearity


# RF UTILITIES CLASS BALANCE
# https://cran.r-project.org/web/packages/rfUtilities/rfUtilities.pdf
rf.nodes.1.balanced <- rf.classBalance(ydata = expd.nodes.1[,4], xdata = expd.nodes.1[,c(2,3,5,6)])






# Future Functions Below



clickmap <- function() {
  #plot(as.numeric(as.character(expd$clickX)) / as.numeric(as.character(expd$windowWidth)), as.numeric(as.character(expd$clickY)) / as.numeric(as.character(expd$windowHeight)))
  plot(as.numeric(as.character(expd$xpos[which(expd$selected == 0)])) / as.numeric(as.character(expd$windowWidth[which(expd$selected == 0)])), as.numeric(as.character(expd$ypos[which(expd$selected == 0)])) / as.numeric(as.character(expd$windowHeight[which(expd$selected == 0)])), col="gray",
       xlab="Normalized X Coordinate Position",
       ylab="Normalized Y Coordinate Position",
       main="Click Map of Selected Nodes Versus Unselected")
  points(as.numeric(as.character(expd$xpos[which(expd$selected == 1)])) / as.numeric(as.character(expd$windowWidth[which(expd$selected == 1)])), as.numeric(as.character(expd$ypos[which(expd$selected == 1)])) / as.numeric(as.character(expd$windowHeight[which(expd$selected == 1)])), col="red", pch=2)
}

# Basic Visualizations
clickmap()

barplot(table(expd[which(expd$selected == 1),3]), horiz = T, las=1)
barplot(table(paste(expd[which(expd$selected == 1),4],expd[which(expd$selected == 1),5])), horiz=T, las=1)
tpch <- rep(1,dim(expd)[1])
tpch[which(expd.mod$selected == 1)] <- 1
ts <- rep(1,dim(expd)[1])
ts[which(expd.mod$selected == 1)] <- 1
pairs(expd.mod[,2:4], col=as.character(expd$nodecolor), pch=tpch, cex=ts)
pairs(expd.mod[,2:4], col=ifelse(expd.mod$selected == 1, as.character(expd$nodecolor), "gray"), pch=tpch, cex=ts)


#euclidian distance from center
for (f in levels(expd[,1])) { 
  cat(f,'\n')
  #expd[which(expd[,1] == f),12] <- calcDistanceFromCenterOfNetwork(f)
  expd[which(expd[,1] == f),13] <- calcDistanceFromCenterOfNetworkDoubleEnc(f)
}

calcDistanceFromCenterOfNetworkDoubleEnc <- function(file) {
  return(  c(expd[which(expd[,1] == file),10] - mean(expd[which(expd[,1] == file),11]) + expd[which(expd[,1] == file),10] - mean(expd[which(expd[,1] == file),11]))^2  )
}


# Add centrality data to data frame
expdwcent <- expd
expdwcent <- data.frame(cbind(expdwcent,
                              rep(centralization.betweenness(genemania.network.graph)$res, dim(expd)[1]),
                              rep(centralization.closeness(genemania.network.graph)$res, dim(expd)[1]),
                              rep(centralization.degree(genemania.network.graph)$res, dim(expd)[1]),
                              rep(centralization.evcent(genemania.network.graph)$vector, dim(expd)[1])
))
colnames(expdwcent) <- c("file","id", "name", "encoding1", "encoding2", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected", "clickX", "clickY", "windowHeight", "windowWidth", "betweenness", "closeness", "degree", "eigenvector")


colnames(expd) <- c("file","id", "name", "encoding1", "encoding2", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected","distCent")
expd[,13] <- as.numeric(as.character(expd[,13]))
expd <- as.data.frame(cbind(expd[,1:11],expd[,13],expd[,12]))
colnames(expd) <- c(colnames(expd)[1:11],"distCent","selected")

library(lme4)
expd.model1 <- lmer(as.numeric(selected) ~ as.numeric(nodeborder) + (1|name) + (1|file), data=expd)
expd.model2 <- lmer(as.numeric(selected) ~ as.numeric(nodesize) + (1|name) + (1|file), data=expd)
anova(expd.model1,expd.model2)

summary(expd.model1)
coef(expd.model1)

summary(lm(as.numeric(selected) ~ as.numeric(nodeborder) + log10(distCent) + as.numeric(nodesize) + name, data=expd))


mod.coef <- coef(lmer(as.numeric(selected) ~ as.numeric(nodeborder) + as.numeric(nodesize) + nodecolor + as.numeric(xpos) + as.numeric(ypos) + (1|name) + (1|file) + (1|encoding1) + (1|encoding2), data=expd))
mod.coef$name
heatmap(as.matrix(mod.coef$name), margins = c(10,10))


# randomly sampling an equal number rows of zero and one selection values
rsexpd <- expd[c(c(sample(which(expd[,11] == 0), length(which(expd[,11] == 1)))),c(which(expd[,11] == 1))),]
coef(lmer(as.numeric(selected) ~ as.numeric(nodesize) + (1|name) + (1|file), data=rsexpd))
coef(lmer(as.numeric(selected) ~ as.numeric(nodesize) + (1|name) + (1|encoding), data=rsexpd))
summary(lmer(as.numeric(selected) ~ as.numeric(nodesize) + as.numeric(nodeborder) + log10(distCent) + (1|name) + (1|encoding), data=rsexpd))
coef(lmer(as.numeric(selected) ~ as.numeric(nodesize) + as.numeric(nodeborder) + log10(distCent) + (1|name) + (1|encoding), data=rsexpd))


# Let's use an RF
library(randomForest)
expd.mod <- expd[,c(1,3,6:12)] #until 13 if I want to include distCent
#rf1 <- randomForest(as.numeric(selected) ~ ., data=expd, importance=TRUE, proximity=TRUE)
rf1 <- randomForest(as.numeric(selected) ~ ., data=expd.mod, importance=TRUE, proximity=TRUE)
print(rf1)
rf1$importance
varImpPlot(rf1,type=2)


voodoo <- c()
for (i in 1:dim(expd)[1]) {
  if (expd[i,4] == "#999") {
    voodoo <- append(voodoo, t(col2rgb("#999999")))
  }
  else {
    voodoo <- append(voodoo, t(col2rgb(expd[i,4])))
  }
}
mycolors <- t(matrix(voodoo, 3, length(voodoo)/3))
#r, g, b cols

expd.mod <- data.frame(cbind(expd[,3],mycolors[,1:3],expd[,7:18])) #until 13 if I want distCent
colnames(expd.mod) <- c("name", "R", "G", "B", colnames(expd)[7:18])
rf2 <- randomForest(as.numeric(selected) ~ ., data=expd.mod, importance=TRUE, proximity=TRUE, do.trace = TRUE)
print(rf2)
rf2$importance
varImpPlot(rf2,type=2)

# unsupervised
expd.urf <- randomForest(expd.mod[, -11])
MDSplot(expd.urf, expd$selected)

#regression
predict(rf2, expd.mod[sample(which(expd.mod[,11] == 1), 1),-11])
predict(rf2, expd.mod[sample(which(expd.mod[,11] == 0), 1),-11])

plot(rf2$predicted)

# optimizing mtry
tuneRF(x = expd.mod[,-11], y = expd.mod[,11], plot = T, doBest = T)


#trying to balance classes for RF
expd.mod.bal <- expd.mod[c(c(sample(which(expd.mod[,11] == 0), length(which(expd.mod[,11] == 1)))),c(which(expd.mod[,11] == 1))),]
tuneRF(x = expd.mod.bal[,-11], y = expd.mod.bal[,11], plot = T, doBest = T)
rf3 <- randomForest(as.numeric(selected) ~ ., data=expd.mod.bal, importance=TRUE, proximity=TRUE, do.trace = F, mtry=2)
print(rf3)
rf3$importance
varImpPlot(rf3,type=2)
rf4 <- randomForest(selected ~ ., data=expd.mod, importance=TRUE, proximity=TRUE, do.trace = F, mtry=2, strata = selected, sampsize = sum(expd.mod[,11] == 1))
print(rf4)
rf4$importance
varImpPlot(rf3,type=2)

#compare balanced vs unbalanced
library(ROCR)

rf3.perf = performance(  prediction(labels = expd.mod.bal$selected, predictions = rf3$predicted)  ,"tpr","fpr")
rf4.perf = performance(  prediction(labels = expd.mod$selected, predictions = rf4$predicted)  ,"tpr","fpr")

#plot the curve
plot(rf4.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
lines(unlist(rf3.perf@x.values),unlist(rf3.perf@y.values), col=4, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve

auc.rf3 <- performance(    prediction(labels = expd.mod.bal$selected, predictions = rf3$predicted)   ,"auc")
auc.rf4 <- performance(    prediction(labels = expd.mod$selected, predictions = rf4$predicted)   ,"auc")

auc.rf3 <- unlist(slot(auc.rf3, "y.values"))
auc.rf4 <- unlist(slot(auc.rf4, "y.values"))

minauc<-min(round(auc.rf3, digits = 2))
maxauc<-max(round(auc.rf3, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct

minauc<-min(round(auc.rf4, digits = 2))
maxauc<-max(round(auc.rf4, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct



# Threshold that Neil provided
gthresh <- function(numNodes) { return(  ceiling(dim(combn(1:numNodes, 2))[2]*(log(numNodes)/numNodes))  ) }
plot(10:300, unlist(lapply(10:300, FUN=gthresh)), type="l")



# More ideas for color analysis
t(rgb2hsv((col2rgb(expd.dat$nodebackground))))
library(scatterplot3d)
tcolor <- rgb2hsv((col2rgb(expd.dat$nodebackground)))
scatterplot3d(tcolor[1,], tcolor[2,], tcolor[3,], color = expd.dat$nodebackground)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(t(rgb2hsv((col2rgb(expd.dat$nodebackground)))), col = expd.dat$nodebackground, upper.panel=panel.cor,diag.panel=panel.hist)
pairs(t(rgb2hsv((col2rgb(expd.dat$nodebackground)))), col = expd.dat$nodebackground, upper.panel = NULL,diag.panel=panel.hist)
hist(tcolor[1,], main = "Distribution of Hues")
hist(tcolor[2,], main = "Distribution of Saturation")
hist(tcolor[3,], main = "Distribution of Values")



# library(jsonlite)
# tn <- as.data.frame(fromJSON(gsub("\'", "\"", "[{ 'data': { 'id': '1', 'name' : 'ENSG00000068793', 'dimension' : 'area', 'value' : '4.40646151205377'  } },{ 'data': { 'id': '2', 'name' : 'ENSG00000162627', 'dimension' : 'area', 'value' : '5.38202560777306'  } },{ 'data': { 'id': '3', 'name' : 'ENSG00000170266', 'dimension' : 'area', 'value' : '1.26156626101008'  } },{ 'data': { 'id': '4', 'name' : 'ENSG00000175315', 'dimension' : 'area', 'value' : '4.40646151205377'  } },{ 'data': { 'id': '5', 'source': '1', 'target': '2', 'dimension': 'weight', 'value':'0.000085'} },{ 'data': { 'id': '6', 'source': '1', 'target': '3', 'dimension': 'weight', 'value':'0.000037'} },{ 'data': { 'id': '7', 'source': '2', 'target': '3', 'dimension': 'weight', 'value':'0.000086'} },{ 'data': { 'id': '8', 'source': '3', 'target': '4', 'dimension': 'weight', 'value':'0.000099'} }]")))
# nodeRows <- which(!is.na(tn[1:dim(tn)[1],]$name))
# edgeRows <- which(is.na(tn[1:dim(tn)[1],]$name))
# 
# edgeData <- cbind(tn[edgeRows,]$source,tn[edgeRows,]$target, tn[edgeRows,]$value) 
# colnames(edgeData) <- c("from", "to", "weight")
# 
# nodeData <- cbind(tn[nodeRows,]$id, tn[nodeRows,]$value, tn[nodeRows,]$name) 
# colnames(nodeData) <- c("id", "area", "name")
# 
# igobj <- graph.data.frame(edgeData, directed = F, vertices = nodeData)
# for (v in 1:length(V(igobj))) {
#   print(length(neighbors(igobj, v, mode=("in"))))
# }
# 













