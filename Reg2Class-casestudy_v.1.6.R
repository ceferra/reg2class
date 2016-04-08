##############################################################################
#
# Reg2Class-casestudies.R: part of the RROCcurves library
#
# RROCcurves library
#  JOSE HERNANDEZ-ORALLO, UNIVERSITAT POLITECNICA DE VALENCIA, SPAIN
#  jorallo@dsic.upv.es
#
# See "INFO.TXT" for copyright information, history, related papers, etc.
#
##############################################################################




#####################
#### DIRECTORIES ####

# Directories where the other .R files must be found and the PDF files will be output. 
# Customise this for your computer and OS.
# WORKDIR <- "E:/__FAENA__/_RESEARCH/_SUBMISSIONS/2014/reg2class/script-jose2" # poli



source("Reg2Class-functions_v1.9.R")


require("quantreg")
######################
###### OPTIONS #######
######################

#EXPERIMENT <- "airfoilT80"
#EXPERIMENT <- "airfoilT67"        #  used
#EXPERIMENT <- "airfoilT50"
#EXPERIMENT <- "airfoilT50rev"
#EXPERIMENT <- "airfoilT33"         # used
#EXPERIMENT <- "airfoilT20"
#EXPERIMENT <- "airfoilT10"
#EXPERIMENT <- "airfoilT50-25-0"    # used
#EXPERIMENT <- "airfoilT50-0-25"    # used

# EXPERIMENT <- "airfoilDOM1"
# EXPERIMENT <- "airfoilDOM2"
# EXPERIMENT <- "yachtbasic"        # used
#EXPERIMENT <- "yachtreg"          # used
#EXPERIMENT <- "zillow"          # used
EXPERIMENT <- "zillow" 

# DATASET
if (EXPERIMENT %in% c("airfoilT80", "airfoilT67", "airfoilT33", "airfoilT50", "airfoilT20", "airfoilT10", "airfoilT50rev", "airfoilT50-25-0", "airfoilT50-0-25", "airfoilDOM1", "airfoilDOM2")) {
  DATASET_NAME <- "airfoil"  
} else {
  DATASET_NAME <- "zillow"
}  
APPLY_LOG <- FALSE

CLIP_REGRESSORS <- "NO"
if (DATASET_NAME == "yacht") {
   CLIP_REGRESSORS <- "ZERO" # "NO", "ZERO" # "TRAIN_MINIMUM"  # Options "NO", "ZERO", "TRAIN_MINIMUM"  # Useful for "yacht" and also for "airfoil" as the output value is a positive magnitude
}
   
# Filenames
#K <- 10
FILE_NAME <- EXPERIMENT # paste(EXPERIMENT, "-K", K, sep="") # paste(DATASET_NAME, "Regr-LnR", sep="-")
FILE_NAME<-"zillow90A"
# General options
HANDLE_TIES <- FALSE       # Do we treat ties separately or just using >= for the threshold

if (EXPERIMENT %in% c("zillow","airfoilT80", "airfoilT67", "airfoilT33", "airfoilT50", "airfoilT20", "airfoilT10", "airfoilT50rev", "airfoilT50-25-0", "airfoilT50-0-25", "airfoilDOM1", "airfoilDOM2")) {
  reg_technique_names <- c("Regr-LnR", "Regr-kNN", "Regr-M5P","Regr-qr")
#  reg_technique_names <- c("Regr-LnR")
#  class_technique_names <- c("Clas-LgR", "Clas-kNN", "Clas-J48")  
class_technique_names <- c()
#class_technique_names <-c()
} else if (EXPERIMENT %in% c("yachtbasic", "zillowbasic") ){
  reg_technique_names <- c("Regr-Max", "Regr-Min", "Regr-Avg", "Regr-Med", "Regr-Bad")
 # reg_technique_names <- c("Regr-Max", "Regr-Min")
 class_technique_names <- c()
#  class_technique_names <- c("Clas-Pos", "Clas-Neg", "Clas-Maj")
} else {
  reg_technique_names <- c("Regr-LnR")
  class_technique_names <- NULL
}


num_regmodels <- length(reg_technique_names)
num_classmodels <- length(class_technique_names)


NUM_CUTOFFS_UNIFORM <- -1 # -1 # 100 # -1  # NUM_CUTOFFS_UNIFORM for the UCE plots and the UCE areas. If -1 that means that it will be the same as the number of examples
# General split
DEPLOYPERC <- 0.9 # 0.33         # Percentage of the data for deployment
TRAINPERC <- 1 - DEPLOYPERC
VALPERC <- 0#0.25
  
PLOT_HEIGHT <- 0.4 # 1 # 0.35        # Height of the plot. Put something below 1 especially if costs are small

if (EXPERIMENT %in% c("airfoilT20")) {
  DEPLOYPERC <- 0.80 # 0.33         # Percentage of the data for deployment
  TRAINPERC <- 1 - DEPLOYPERC
} else if (EXPERIMENT %in% c("airfoilT80")) {
  DEPLOYPERC <- 0.20 # 0.33         # Percentage of the data for deployment
  TRAINPERC <- 1 - DEPLOYPERC
} else if (EXPERIMENT %in% c("airfoilT67")) {
  DEPLOYPERC <- 0.33 # 0.33         # Percentage of the data for deployment
  TRAINPERC <- 1 - DEPLOYPERC
} else if (EXPERIMENT %in% c("airfoilT50", "airfoilT50rev")) {
  DEPLOYPERC <- 0.5 # 0.33         # Percentage of the data for deployment
  TRAINPERC <- 1 - DEPLOYPERC
} else if (EXPERIMENT %in% c("airfoilT33")) {
  DEPLOYPERC <- 0.67 # 0.33         # Percentage of the data for deployment
  TRAINPERC <- 1 - DEPLOYPERC
} else if (EXPERIMENT %in% c("airfoilT10")) {
  DEPLOYPERC <- 0.90 # 0.33         # Percentage of the data for deployment
  TRAINPERC <- 1 - DEPLOYPERC
}  else if (EXPERIMENT %in% c("airfoilT50-25-0")) {
  VALPERC <- 0.25
  DEPLOYPERC <- 0  
  TRAINPERC <- 0.5
}  else if (EXPERIMENT %in% c("airfoilT50-0-25")) {
  VALPERC <- 0
  DEPLOYPERC <- 0.25
  TRAINPERC <- 0.5  
} else if (EXPERIMENT %in%  c("yachtbasic", "zillowbasic")) {
  NUM_CUTOFFS_UNIFORM <- 1333 # -1 # 100 # -1  # NUM_CUTOFFS_UNIFORM for the UCE plots and the UCE areas. If -1 that means that it will be the same as the number of examples
  PLOT_HEIGHT <- 1
} else if (EXPERIMENT %in% "yachtreg") {
  NUM_CUTOFFS_UNIFORM <- 1333 
  PLOT_HEIGHT <- 0.35  
}

if (EXPERIMENT == "airfoilT50rev") {
  REVERSE_SPLIT <- TRUE
} else {
  REVERSE_SPLIT <- FALSE
}


  
# Graphic options
NEWPLOT <- TRUE            # Draws RROC space. Use whenever you want to start a new plot. If you want to draw curves on an existing plot, set this to FALSE

MY_STYLES <-  rep(1, 200)    # If everything continuous try this.

if (EXPERIMENT %in% c("zillow","airfoilT80", "airfoilT67", "airfoilT33", "airfoilT50", "airfoilT20", "airfoilT10", "airfoilT50rev", "airfoilT50-25-0", "airfoilT50-0-25", "airfoilDOM1", "airfoilDOM2")) { 
  MY_COLOURS <- rep(c("blue3", "red3", "green4", "cyan", "pink", "lawngreen", "orange", "purple"), 20)
  UCE_CURVE_OCE_POINTS_SIZE <- 0.1 #  1 # 0.25 # 0.1 # 0.25
  UCE_PLOT_LEGEND_PLACE <- "topleft" # NULL # "right" # "topright" # "left" # "topleft"
  OCE_PLOT_LEGEND_PLACE <- "topleft" # "left" # "topright" 
} else if (EXPERIMENT %in%  c("yachtbasic", "zillowbasic")) {
  MY_STYLES <-  rep(c("44", "13", "1343", "73", "4114", "2262", "4462", "131373"), 20)    # If many curves choose this
  MY_COLOURS <- rep(c("red", "blue", "purple", "green4", "yellow2", "pink", "cyan", "orange2"), 20)
  UCE_CURVE_OCE_POINTS_SIZE <- 0.1 #  1 # 0.25 # 0.1 # 0.25
  UCE_PLOT_LEGEND_PLACE <- "right" # NULL # "right" # "topright" # "left" # "topleft"
  OCE_PLOT_LEGEND_PLACE <- "left" # "left" # "topright" 
} else {  # EXPERIMENT == "yachtreg"
  MY_COLOURS <- rep("blue", 20)
  UCE_CURVE_OCE_POINTS_SIZE <- 0.75 #  1 # 0.25 # 0.1 # 0.25
  UCE_PLOT_LEGEND_PLACE <- NULL # "topleft" # NULL # "right" # "topright" # "left" # "topleft"
  OCE_PLOT_LEGEND_PLACE <- NULL # "topleft" # "left" # "topright"
  MY_STYLES <- 1:200                                                              # If only one or two curves choose this
}

OCE_CURVE_OCE_POINTS_SIZE <- 0 # 0.1 # 0.25






# PDF options
PDFOPEN <- FALSE            # If the plots are output on a PDF file
PDFCLOSE <- PDFOPEN        # Close the PDF file. This should match PDFOPEN, except when you want to draw several curves before closing.
PDFheight= 5.5             # 7 is the default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one plot
PDFwidth= 5                # (as above for width) 7 by default






#######################
###### LIBRARIES ######
#######################

require("RWeka") || install.packages("RWeka")   # For tree modelling
library(RWeka)  # Classification and Regression techniques: LinearRegression,  M5Rules, M5P, IBk

require("xtable") || install.packages("xtable")   # For tree modelling
library(xtable)  # Classification and Regression techniques: LinearRegression,  M5Rules, M5P, IBk

 

#########################
##### LOADS DATASET #####
#########################
  

if (DATASET_NAME == "airfoil") {
  pdfdatasetname <- "airfoil_self_noise.dat"  # airfoils (UCI)
  input_col_names <- c("Frequency", "Angle", "ChordLength", "Velocity", "Thickness")
  output_col_name <- "Sound"
  dataset_sep <- "\t"
} else if (DATASET_NAME == "yacht") {
  pdfdatasetname <- "yacht_hydrodynamics.data"  # airfoils (UCI)
  input_col_names <- c("Position", "Prismatic", "Displacement", "BeamDraught", "LengthBeam", "Froude")
  output_col_name <- "Resistance"
  dataset_sep <- ""  # By default, it is multiple spaces
} else if (DATASET_NAME == "zillow") {
  pdfdatasetname <- "zillow.csv"  # airfoils (UCI)
  
  dataset_sep <- ","  # By default, it is multiple spaces
}else {
  stop("Unknown dataset")
}

if  (DATASET_NAME == "zillow") 
{
  dat<-read.csv("HousePricesData-Address-City-Features_fromZillow.csv")
  datalen <- nrow(dat)
  mort<-read.csv("MortgagesData_2013_from_HMDA.csv")
  dat=dat[,!colnames(dat) %in% c("street")]
  cutsm<-sort(mort[["LoanAmount"]])
  output_col_name<-"price"
  input_col_names<-names(dat)[1:(length(dat[1,])-1)]
}else                                         
{
dat <- read.delim(pdfdatasetname, header = FALSE, sep = dataset_sep, dec=".")

# dat <- dat[1:73,]  # Only a few examples (when debugging, to have a fast execution)

datalen <- nrow(dat)

  
columnnames <- c(input_col_names, output_col_name)
names(dat) <- columnnames
# 1. Frequency, in Hertzs. 
# 2. Angle of attack, in degrees. 
# 3. Chord length, in meters. 
# 4. Free-stream velocity, in meters per second. 
# 5. Suction side displacement thickness, in meters. 
# 6. Scaled sound pressure level, in decibels. (OUTPUT)
}


if (APPLY_LOG == TRUE) {
  dat[["output_col_name"]] <- log(dat[["output_col_name"]])  # If we use a logarithmic transformation, we also need to transform the adjusted function points 
}




#########################################################
###### SPLITS BETWEEN WORK DATA AND DEPLOYMENT DATA #####
#########################################################

set.seed(0) # 0 # We fix the seed for repeatability of results 

# SHUFFLE!!!
shufindx <- sample(1:datalen, datalen)  
if (REVERSE_SPLIT) {
  shufindx <- rev(shufindx)   # The split is performed with reverse ordering
}
dat <- dat[shufindx,]


#dat2 <- dat
#row.names(dat2) <- 1:nrow(dat2)  # This puts the row names sequentially, but this affects methods such as IBk (a bug)
#row.names(dat) <- paste("name", row.names(dat2), sep="")  # Ensures that they are treated like strings by any weka method


# SPLIT
dattrainlen <- trunc(datalen*(TRAINPERC))
dattrain <- dat[1:dattrainlen,]

datvallen <- trunc(datalen*(VALPERC))
if (VALPERC > 0) {
  datval <- dat[(dattrainlen+1):(dattrainlen+datvallen),]
}

datdeploylen <- trunc(datalen*(DEPLOYPERC))
if (DEPLOYPERC > 0) { 
  datdeploy <- dat[(dattrainlen+datvallen+1):datalen,]  
}  

# For the moment we can't treat validation and deployment at the same time.
if (DEPLOYPERC > 0) {  # I give preference to set the test set equal to the deployment if there is deployment
  dattest <- datdeploy
} else {  
  dattest <- datval
}

#dattest <- datval #cesar
dattestlen <- nrow(dattest)

# Sort the test dataset by the output value. This is crucial for the OCE plots. Well, now it may be redundant, as the function Reg2Class_Reframing_Points does this by default
res <- sort(dattest[[output_col_name]], index.return=TRUE)
dattest <- dattest[res$ix,]

train_actual <- dattrain[[output_col_name]]
test_actual <- dattest[[output_col_name]]




#########################################################
########### TRAINS REGRESSION regmodel_list #############
#########################################################



formulareg <- paste(output_col_name, "~", paste(input_col_names, collapse="+"))



regmodel_list <- c()

for (i in 1:num_regmodels) {
  if (reg_technique_names[i] == "Regr-LnR") {
    res <- LinearRegression(formulareg, dattrain)
  } else if (reg_technique_names[i] == "Regr-kNN") {
    weka_options <- Weka_control(K = 10, I=TRUE ) # By default K= 1 and is unweighted
	#  -I weighted by the inverse of the distance.
	#  -F weighted by 1- distance
    res <- IBk(formulareg, dattrain, control = weka_options)
  } else if (reg_technique_names[i] == "Regr-M5P") {
    res <- M5P(formulareg, dattrain)
  } else if (reg_technique_names[i] == "Regr-Max") {
    res <- Inf
  } else if (reg_technique_names[i] == "Regr-qr") {
    formulareg1 <- paste(output_col_name, "~", paste(input_col_names[c(1,4:8)], collapse="+"))
    res<-rq(formulareg1, data = dattrain[,!colnames(dattrain) %in% c("city","state")],method="lasso") 
    #formulareg1 <- paste(output_col_name, "~", paste(input_col_names[c(1,3:8)], collapse="+"))
    #res<-rq(formulareg1, data = dattrain[,!colnames(dattrain) %in% c("city")],method="lasso") 
  } else if (reg_technique_names[i] == "Regr-Min") {
    res <- -Inf
  } else if (reg_technique_names[i] == "Regr-Avg") {
    res <- mean(dattrain[[output_col_name]])	
  } else if (reg_technique_names[i] == "Regr-Med") {
    res <- median(dattrain[[output_col_name]])
  } else if (reg_technique_names[i] == "Regr-Bad") {
    res <- median(dattrain[[output_col_name]])
  }	else {
    print("Unknown regression technique")
	err()
  }
  regmodel_list[[i]] <- res
}





#########################################################
################# APPLIES regmodel_list #################
#########################################################

yhat_list <- NULL
for (i in 1:num_regmodels) {
  if (reg_technique_names[i] == "Regr-Max") {
    yhat_list[[i]] <- rep(regmodel_list[[i]], dattestlen)
  } else if (reg_technique_names[i] == "Regr-Min") {
    yhat_list[[i]] <- rep(regmodel_list[[i]], dattestlen)
  } else if (reg_technique_names[i] == "Regr-Avg") {
    yhat_list[[i]] <- rep(regmodel_list[[i]], dattestlen)
  } else if (reg_technique_names[i] == "Regr-Med") {
    yhat_list[[i]] <- rep(regmodel_list[[i]], dattestlen)
  } else if (reg_technique_names[i] == "Regr-Bad") {
    predictions <- rep(Inf, dattestlen) # We put Infty to all
	md <- regmodel_list[[i]]  # We recover the train median
	high_values <- (test_actual > md)  # Which are the high values?
	predictions[high_values] <- rep(-Inf,sum(high_values))           # -Infty for those that are higher than the train median
	yhat_list[[i]] <- predictions
	print(predictions)
  } else {	
    if  (reg_technique_names[i] == "Regr-qr")  yhat_list[[i]] <- predict(regmodel_list[[i]], dattest[!colnames(dattest) %in% c("city")])
    else yhat_list[[i]] <- predict(regmodel_list[[i]], dattest)
  }	
  
  if (!(reg_technique_names[i] %in% c("Regr-Max", "Regr-Min", "Regr-Avg", "Regr-Med", "Regr-Bad"))) {
    if (CLIP_REGRESSORS == "ZERO") {
      yhat_list[[i]][yhat_list[[i]] < 0] <- 0   # For those that are < 0 set to zero.
    } else if (CLIP_REGRESSORS == "TRAIN_MINIMUM") {
      yhat_list[[i]][yhat_list[[i]] < 0] <- min(train_actual)   # For those that are < 0 set to the minimum in the training set
    }
  }	
}





########################################################################
####### CALCULATES THE POINTS FOR THE SIX APPROACHES AND RESULTS #######
########################################################################

min_test_actual <- min(test_actual) # - 50  # Add or subtract to change the range
max_test_actual <- max(test_actual) # + 50

formula_class <- paste("class", "~", paste(input_col_names, collapse="+"))


v <- unlist(yhat_list)   # All yhat values without infinities.
v <- v[abs(v) != Inf]  
   
miny <- min(min_test_actual, v)  # We calculate the min for all methods
maxy <- max(max_test_actual, v)  # We calculate the max for all methods
   
# We generate the distributions (vectors) of cutoffs   
# if (NUM_CUTOFFS_UNIFORM == -1) {
#  NUM_CUTOFFS_UNIFORM <- dattestlen 
#}  
#cutoffs_uniform <- ((-1:NUM_CUTOFFS_UNIFORM)/(NUM_CUTOFFS_UNIFORM-1)) * (maxy-miny) + miny    # The cutoffs go a little bit beyond the miny and the maxy
cutoffs_uniform <- Reg2Class_UCE_cutoffs(NUM_CUTOFFS_UNIFORM, maxy, miny, dattestlen)

cutoffs_observed <- Reg2Class_OCE_cutoffs(test_actual)

#cutoffs_observed <- test_actual
  
# We prepare the various lists and vectore to store the results
uce_points_list <- list()
oce_points_list <- list()

if  (DATASET_NAME == "zillow")  oce_points_list_mod <- list()


uce_area <- NULL
oce_area <- NULL
mae <- NULL
mse <- NULL
evarN <- NULL
ebias <- NULL
pearson <- NULL
spearman <- NULL
kendall <- NULL

# We do first the regression (reframing) models

if (num_regmodels > 0) {
for (i in 1:num_regmodels) {
  yhats <- yhat_list[[i]]
    
  uce_points <- Reg2Class_Reframing_Points(cutoffs_uniform, yhats, test_actual)  
  oce_points <- Reg2Class_Reframing_Points(cutoffs_observed, yhats, test_actual)  
  if  (DATASET_NAME == "zillow")  oce_points_mod <- Reg2Class_Reframing_Points(cutsm, yhats, test_actual)  
  
  
  uce_points_list[[i]] <- uce_points
  oce_points_list[[i]] <- oce_points
  if  (DATASET_NAME == "zillow")  oce_points_list_mod[[i]]<-oce_points_mod
  # We calculate both areas
  u <- Reg2Class_UCE_area(uce_points, cutoffs_uniform) # mean(uce_points) * (max(cutoffs_uniform) - min(cutoffs_uniform))
  o <- Reg2Class_OCE_area(oce_points) # mean(oce_points)
  
  print("Regression model")
  print(reg_technique_names[i]) 
  print(u) 
  print(o)   
  
  uce_area[i] <- u
  oce_area[i] <- o
  mae[i] <- mean(abs(yhats - test_actual))
  mse[i] <- mean((yhats - test_actual)^2)
  evarN[i] <- (var(yhats-test_actual)/dattestlen)*(dattestlen-1)   # R calculate the sample variance (dividing by N-1). That's why we do (v/N)*(N-1)
  ebias[i] <- mean(yhats) - mean(test_actual)
  pearson[i] <- cor(yhats, test_actual, method="pearson")
  spearman[i] <- cor(yhats, test_actual, method="spearman")
  kendall[i] <- cor(yhats, test_actual, method="kendall")
}
}

# Next, the classification (retraining) ones.

if (num_classmodels > 0) {
for (i in 1: num_classmodels) {

  j <- i + num_regmodels 
  
  uce_points <- Reg2Class_Retraining_Points(cutoffs_uniform, class_technique_names[i], dattrain, dattest, output_col_name, formula_class)   
  oce_points <- Reg2Class_Retraining_Points(cutoffs_observed, class_technique_names[i], dattrain, dattest, output_col_name, formula_class) 
  if  (DATASET_NAME == "zillow")  oce_points_mod <- Reg2Class_Retraining_Points(cutsm, class_technique_names[i], dattrain, dattest, output_col_name, formula_class) 
  
  uce_points_list[[j]] <- uce_points
  oce_points_list[[j]] <- oce_points
  if  (DATASET_NAME == "zillow")  oce_points_list_mod[[j]]<-oce_points_mod
  # We calculate both areas
  u <- Reg2Class_UCE_area(uce_points, cutoffs_uniform) # mean(uce_points) * (max(cutoffs_uniform) - min(cutoffs_uniform))
  o <- Reg2Class_OCE_area(oce_points) # mean(oce_points)
   
  print("Classification model")
  print(class_technique_names[i])
  print(u)
  print(o)
 
  uce_area[j] <- u
  oce_area[j] <- o
  mae[j] <- NaN
  mse[j] <- NaN
  evarN[j] <- NaN
  ebias[j] <- NaN
  pearson[j] <- NaN
  spearman[j] <- NaN
  kendall[j] <- NaN
}  
}

# Converts results into a dataframe and exports to a file

# all_names <- c(paste(reg_technique_names, "(reframing)"), paste(class_technique_names, "(retraining)"))
all_names <- c(reg_technique_names, class_technique_names)
proc <- c(rep("reframe", num_regmodels), rep("retrain", num_classmodels)) 

results <- data.frame(all_names, proc, uce_area, oce_area, mae, mse, evarN, ebias, pearson, spearman, kendall)  
results_filename <- paste(FILE_NAME, "-results.csv", sep="")
write.csv(results, results_filename)
  

print("######### Finished ##########")

print("Some info about the dataset:")  

print("Train")  
length(dattrain[[output_col_name]])
mean(dattrain[[output_col_name]])
median(dattrain[[output_col_name]])
min(dattrain[[output_col_name]])
max(dattrain[[output_col_name]])

print("Test")  
length(dattest[[output_col_name]])
mean(dattest[[output_col_name]])
median(dattest[[output_col_name]])
min(dattest[[output_col_name]])
max(dattest[[output_col_name]])
  
    

#########################################################
##################### PLOTS CURVES ######################
#########################################################

pdfname <- paste(FILE_NAME, "-UCEcurves.pdf", sep="")

if (PDFOPEN) {
  pdf(pdfname, height= PDFheight, width= PDFwidth)
} 

if (NEWPLOT) {  
  Reg2Class_UCE_space(xliminf=miny, xlimsup=maxy, ylimsup=PLOT_HEIGHT, test_vertical_line=median(dattest[[output_col_name]]), test_horizontal_line=NULL)
}


for (i in 1:(num_regmodels+num_classmodels)) {  
  Reg2Class_UCE_curve(cutoffs_uniform, uce_points_list[[i]], colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i, pointsize=1, plot_lines=TRUE, plot_points=FALSE)
  Reg2Class_UCE_curve(cutoffs_observed, oce_points_list[[i]], colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i, pointsize=UCE_CURVE_OCE_POINTS_SIZE, plot_lines=FALSE, plot_points=TRUE)    
}

if (!(is.null(UCE_PLOT_LEGEND_PLACE))) {
  legend(UCE_PLOT_LEGEND_PLACE, legend= all_names, col=MY_COLOURS, lty=MY_STYLES, pch=1:i, cex=0.75,lwd=2)
}

if (PDFCLOSE) {
  dev.off()
}
  
  
  
pdfname <- paste(FILE_NAME, "-UCEcurves_mod.pdf", sep="")

if (PDFOPEN) {
  pdf(pdfname, height= PDFheight, width= PDFwidth)
} 



if (NEWPLOT) {  
  #Reg2Class_UCE_space(xliminf=miny, xlimsup=maxy, ylimsup=PLOT_HEIGHT, test_vertical_line=median(dattest[[output_col_name]]), test_horizontal_line=0.5)
  Reg2Class_OCE_space(ylimsup=PLOT_HEIGHT, xaxis="VALUES", ,title="CE",xlab=c( "Cutoff"), cutoffs=cutsm, test_vertical_line=NULL, test_horizontal_line=0.5, test_diagonal_lines=NULL)
  
}


for (i in 1:(num_regmodels+num_classmodels)) {  
  #Reg2Class_UCE_curve(cutoffs_uniform, uce_points_list[[i]], colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i, pointsize=1, plot_lines=TRUE, plot_points=FALSE)
  #Reg2Class_UCE_curve(cutoffs_observed, oce_points_list[[i]], colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i, pointsize=UCE_CURVE_OCE_POINTS_SIZE, plot_lines=FALSE, plot_points=TRUE)    
  Reg2Class_OCE_curve(oce_points_list_mod[[i]], , colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i, pointsize=OCE_CURVE_OCE_POINTS_SIZE, CURVE="CURVE")    
}

if (!(is.null(UCE_PLOT_LEGEND_PLACE))) {
  legend(UCE_PLOT_LEGEND_PLACE, legend= all_names, col=MY_COLOURS, lty=MY_STYLES, pch=1:i, cex=0.75)
}

if (PDFCLOSE) {
  dev.off()
}

  
  
  


pdfname <- paste(FILE_NAME, "-OCEcurves.pdf", sep="")

if (PDFOPEN) {
  pdf(pdfname, height= PDFheight, width= PDFwidth)
}

if (NEWPLOT) {
  Reg2Class_OCE_space(ylimsup=PLOT_HEIGHT, xaxis="BOTH", xlab=c("True Rank Ratio", "Cutoff"), cutoffs=cutoffs_observed, test_vertical_line=0.5, test_horizontal_line=0.5, test_diagonal_lines=TRUE)
#  Reg2Class_OCE_space(ylimsup=PLOT_HEIGHT, xaxis="VALUES", xlab=c("Cutoff"), cutoffs=cutoffs_observed)
#  Reg2Class_OCE_space(ylimsup=PLOT_HEIGHT, cutoffs=cutoffs_observed)
}
  
for (i in 1:(num_regmodels+num_classmodels)) {
  Reg2Class_OCE_curve(oce_points_list[[i]], , colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i, pointsize=OCE_CURVE_OCE_POINTS_SIZE, CURVE="CURVE")    
}

if (!(is.null(OCE_PLOT_LEGEND_PLACE))) {
  if (OCE_CURVE_OCE_POINTS_SIZE  > 0) {
    legend(OCE_PLOT_LEGEND_PLACE, legend= all_names, col=MY_COLOURS, lty=MY_STYLES, pch=1:i, cex=0.75,lwd=2)
  } else {
    legend(OCE_PLOT_LEGEND_PLACE, legend= all_names, col=MY_COLOURS, lty=MY_STYLES, cex=0.75,lwd=2)
  }
}

if (PDFCLOSE) {
  dev.off()
}
 
 
 
 
 
pdfname <- paste(FILE_NAME, "-AEdiagrams.pdf", sep="")

if (PDFOPEN) {
  pdf(pdfname, height= PDFheight, width= PDFwidth)
}

if (NEWPLOT) {

  max_AE <- 0
  for (i in 1:(num_regmodels)) {
    yhats <- yhat_list[[i]]
    max_AE <- max(max_AE, max(abs(yhats - test_actual)))
	if (max_AE == Inf) {
	  max_AE <- max(test_actual)
	}
  }

  Reg2Class_AE_space(dattestlen, ylimsup=max_AE)    
}
  
for (i in 1:(num_regmodels)) {
  Reg2Class_AE_diagram(test_actual, yhat_list[[i]], colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i)    
}
 
 
if (PDFCLOSE) {
  dev.off()
}
 
 
 
 
 
pdfname <- paste(FILE_NAME, "-IRdiagrams.pdf", sep="")

if (PDFOPEN) {
  pdf(pdfname, height= PDFheight, width= PDFwidth)
}

if (NEWPLOT) {
  Reg2Class_IR_space(dattestlen)
}
  
for (i in 1:(num_regmodels)) {
  Reg2Class_IR_diagram(test_actual, yhat_list[[i]], colour=MY_COLOURS[i], linestyle=MY_STYLES[i], pointcharacter=i)    
}
 
 
if (PDFCLOSE) {
  dev.off()
}
 

 
 
pdfname <- paste(FILE_NAME, "-Histogram.pdf", sep="")
 
if (PDFOPEN) {
  pdf(pdfname, height= PDFheight, width= PDFwidth)
}

hist(test_actual,breaks=20, xlab="True values (deployment)", main="Histogram", col= MY_COLOURS[1])
 
if (PDFCLOSE) {
  dev.off()
}
 
 
 
pdfname <- paste(FILE_NAME, "-Cumulative.pdf", sep="")
 
if (PDFOPEN) {
  pdf(pdfname, height= PDFheight, width= PDFwidth)
}

plot.ecdf(test_actual, xlab="True values (deployment)", ylab="F", main="Cumulative distribution function", col= MY_COLOURS[1])
 
if (PDFCLOSE) {
  dev.off()
}
 
 
RESULTS_IN_LATEX <- TRUE
if (RESULTS_IN_LATEX) { 
  resultats2 <- read.csv(results_filename)  
  my_tex <- xtable(resultats2[-c(1,3)], digits=3)
  print(my_tex, include.rownames=FALSE)
  # writeLines(my_tex, )
  print(my_tex, type="latex", file=paste(FILE_NAME, "-results.tex", sep=""))
}




