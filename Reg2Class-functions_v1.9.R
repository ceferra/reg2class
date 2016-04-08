##############################################################################
#
# Reg2Class-functions.R: part of the RROCcurves library
#
# RROCcurves library
#  JOSE HERNANDEZ-ORALLO, UNIVERSITAT POLITECNICA DE VALENCIA, SPAIN
#  jorallo@dsic.upv.es
#
# See "INFO.TXT" for copyright information, history, related papers, etc.
#
##############################################################################


##############################################################################
################ FUNCTIONS PROVIDED AND A SHORT EXPLANATION ##################
############## (more details inside each function definition) ################
##############################################################################
#
# Reg2Class_Loss(...): Calculates the loss (error) for a regression model as the corresponding classifier error at a given cutoff
#
# Reg2Class_UCE_space(...): Plots the underlying UCE (formerly known as AE) space (use this just before plotting any model)#
#
# Reg2Class_UCE_cutoffs(...): Returns a sequence of equally-spaced cutoffs between maxy and miny.   
#
# Reg2Class_UCE_curve(...): Plots an UCE curve given the cutoffs and their costs
#
# Reg2Class_UCE_area(...): Calculates the area of a curve in the UCE space
#
# Reg2Class_OCE_space(...): Plots the underlying OCE (formerly known as Normalised AE) space (use this just before plotting any model)
#
# Reg2Class_OCE_cutoffs(...): Returns the cutoffs equal to the actual values
#
# Reg2Class_OCE_curve(...): Plots an OCE curve given the cutoffs and their costs
#
# Reg2Class_OCE_area(...): Calculates the area of a curve in the OCE space
#
# Reg2Class_Retraining_Points(...): Gets the points for the retraining case for a vector of cutoffs.
#
# Reg2Class_Reframing_Points(...): Gets the points for the reframing case for a vector of cutoffs.
#
# Reg2Class_UCE_all_in_one(...): Creates cutoffs (uniform and observed), calculates uce_points and oce_points using reframing, plots the UCE curve and calculates area
# 
# Reg2Class_AE_space(...): Plots the AE space
#
# Reg2Class_AE_diagram(...): Plots an AE diagram
#
# Reg2Class_IR_space(...): Plots the IR space
#
# Reg2Class_IR_diagram(...): Plots an IR diagram
#
###########################       DEPRECATED       ###########################
#
# AE_curve(...)  : ### Use Reg2Class_UCE_all_in_one instead ###. Plots an AE curve and returns the areas from the minimum and maximum value or just using the points on the true data
# NAE_curve(...) : ### Use Reg2Class_OCE_all_in_one instead ###. Plots an NAE (Normalised AE) curve and returns the areas from the minimum and maximum value or just using the points on the true data
#
###################### RESERVED (STILL NOT IMPLEMENTED) ######################
# 
# Reg2Class_OCE_all_in_one(...): Creates cutoffs (observed), calculates oce_points using reframing, plots the OCE curve and calculates area
#
##############################################################################






##############################################################################
######################### VERSION HISTORY ####################################
##############################################################################
#
# v.1.0    24/03/2014  First complete version
# v.1.0.1  24/03/2014  Minor things
# v.1.1    24/03/2014  HANDLE_TIES can be disabled for the plots
# v.1.2    02/04/2014  The height and the names of the plots can be modified
# v.1.3    03/04/2014  New function Reg2Class_Retraining_Points to get the points for the retraining case.
# v.1.4    04/04/2014  The original functions are split into functions calculating the points and functions plotting the curves.
#                      Names are changed from AE to UCE and NAE to OCE
# v.1.5    07/04/2014  Some fixes and improvements in function. Reg2Class_Retraining_Points. New models accepted (Pos, Neg and Majority classifiers)
#                      The Reg2Class_OCE_space() function now allows two x-axis.
#                      The Reg2Class_OCE_space() and Reg2Class_UCE_space() functions can now plot horizontal and vertical guiding lines.
# v.1.6    11/04/2014  New argument for Reg2Class_OCE_space: test_diagonal_lines
# v.1.7    13/04/2014  The second xaxis in Reg2Class_OCE_space is now fixed (well centred).
# v.1.8    16/04/2014  New diagrams: AE diagrams and IR diagrams and their corresponding functions
#                      Some corrections to the second axis (the value axis) in the OCE plots
#
##############################################################################




######################
##### FUNCTIONS ######
######################



#### Reg2Class_Loss ####
#
# Calculates the loss (error) for a regression model as the corresponding classifier error at a given cutoff
#
#### Arguments:
# predicted: the predicted values
# actual: the actual values
# c: the cutoff
# handle_ties: whether ties are going to be treated evenly on both sides.
# 
Reg2Class_Loss <- function(predicted, actual, c, handle_ties=FALSE) {
  N <- length(actual)

  
  TN <- sum(actual[which(predicted < c)] < c)
  HN <- 0.5 * (sum(actual[which(predicted < c)] == c))  # Half negatives
  TP <- sum(actual[which(predicted > c)] > c)
  HP <- 0.5 * (sum(actual[which(predicted > c)] == c))  # Half positives
  
  TH <- sum(actual[which(predicted == c)] == c)
  NH <- 0.5 * (sum(actual[which(predicted == c)] < c))
  PH <- 0.5 * (sum(actual[which(predicted == c)] > c))
  
  if (handle_ties) {
    res <- 1 - (TN + HN + TP + HP + TH + NH + PH) / N
  } else {
    res <- 1 - (sum(actual[which(predicted < c)] < c) + sum(actual[which(predicted >= c)] >= c)) / N
  }
  
  res
}





#### Reg2Class_UCE_space ####
#
# Plots the UCE space
#
#### Arguments:
# xliminf: lowest (leftmost) value for the x-axis
# xlimsup: highest (rigthmost) value for the x-axis
# ylimsup: height of the plot.
# title: title for the plot
# xlab: xaxis title
# ylab: yaxis title
# test_vertical_line: if not NULL a vertical line (typically for the median) 
# test_horizontal_line: if not NULL a horizontal line (typically at 0.5) 
#
Reg2Class_UCE_space <- function(xliminf, xlimsup, ylimsup=1, title="UCE plot", xlab="Cutoff", ylab="Error", test_vertical_line=NULL, test_horizontal_line=NULL) {
  
  plot(0, 0, lty="blank", col="white", xlim=c(xliminf,xlimsup), ylim=c(0,ylimsup), xlab=xlab, ylab=ylab, main=title) 

  if (!(is.null(test_vertical_line))) {
    lines(c(test_vertical_line,test_vertical_line), c(0,1), col="black", lwd=0.7, lty="dotted")   # vertical line at median
  }	
  if (!(is.null(test_horizontal_line))) {
    lines(c(xliminf,xlimsup), c(test_horizontal_line,test_horizontal_line), col="black", lwd=0.7, lty="dotted")                   # horizontal line at 0.5
  }
  
}





#### Reg2Class_UCE_cutoffs ####
#
#  Returns a sequence of equally-spaced cutoffs between maxy and miny.  
#
#### Arguments:
# NUM_CUTOFFS_UNIFORM: number of cutoffs (if -1, we take it equal to datdeploylen)
# maxy: lowest (leftmost) value for the cutoff (approx.)
# miny: lowest (leftmost) value for the cutoff (approx.)
# datdeploylen: length of the test set
#
Reg2Class_UCE_cutoffs <- function(NUM_CUTOFFS_UNIFORM, maxy, miny, datdeploylen) {
  # We generate the distributions (vectors) of cutoffs   
  if (NUM_CUTOFFS_UNIFORM == -1) {
    NUM_CUTOFFS_UNIFORM <- datdeploylen 
  }  
  cutoffs_uniform <- ((-1:NUM_CUTOFFS_UNIFORM)/(NUM_CUTOFFS_UNIFORM-1)) * (maxy-miny) + miny    # The cutoffs go a little bit beyond the miny and the maxy
  cutoffs_uniform
}





#### Reg2Class_UCE_all_in_one ####
#
# Creates cutoffs (uniform and observed), calculates uce_points and oce_points, plots the UCE curve and calculates area
#
#### Arguments:
# predicted: an array of predicted values
# actual: an array of true values (size of predicted and actual should match)
# maxy: lowest (leftmost) value for the cutoff (approx.)
# miny: lowest (leftmost) value for the cutoff (approx.)
# NUM_CUTOFFS_UNIFORM: number of cutoffs (if -1, we take it equal to datdeploylen)
# handle_ties: whether ties are going to be treated evenly on both sides.
# colour: colour used for the curve
# linestyle: line style for the curve
# pointcharacter: point used for highlighting the points of the actual vector
# pointsize: a scaling factor applied to pointcharacter. Use small numbers for big datasets
#
#### Returns the UCE points, the OCE points, and their areas with the uniform and the observed cutoff distributions
#
Reg2Class_UCE_all_in_one <- function(predicted, actual, maxy, miny, NUM_CUTOFFS_UNIFORM=10000, handle_ties=HANDLE_TIES, colour="grey", linestyle=1, pointcharacter=1, pointsize=0.25) {

  cutoffs_uniform <- Reg2Class_UCE_cutoffs(NUM_CUTOFFS_UNIFORM, maxy, miny, N) # We generate the distributions (vectors) of cutoffs   
  cutoffs_observed <- Reg2Class_OCE_cutoffs(actual)

  uce_points <- Reg2Class_Reframing_Points(cutoffs_uniform, predicted, actual, handle_ties=HANDLE_TIES)  
  oce_points <- Reg2Class_Reframing_Points(cutoffs_observed, predicted, actual, handle_ties=HANDLE_TIES)  

  Reg2Class_UCE_curve(cutoffs_uniform, uce_points, colour=colour, pointsize=pointsize, pointcharacter=pointcharacter, linestyle=linestyle, plot_lines=TRUE, plot_points=FALSE)
  Reg2Class_UCE_curve(cutoffs_observed, oce_points, colour=colour, pointsize=pointsize, pointcharacter=pointcharacter, linestyle=linestyle, plot_lines=FALSE, plot_points=TRUE)    
      
  u <- Reg2Class_UCE_area(uce_points, cutoffs_uniform) # mean(uce_points) * (max(cutoffs_uniform) - min(cutoffs_uniform))
  o <- Reg2Class_OCE_area(oce_points) # mean(oce_points)

  list(uce_points, oce_points, u,o)  
}  



  
#### Reg2Class_Reframing_Points ####
#
# Gets the points for the reframing case for a vector of cutoffs.
#
# REQUIRES: the test_data to be sorted by the true output. The cutoffs to be sorted
#
#### Arguments:
# cutoffs: cutoff vector to be used (i.e., the distribution of cutoffs)
# predicted: an array of predicted values
# actual: an array of true values (size of predicted and actual should match)
# handle_ties: whether ties are going to be treated evenly on both sides.
# resolution: number of points
# sort_examples: sort the examples by actual. This is need to calculate the OCE well.
#
#### Returns
# a vector with the error for each cutoff
#
Reg2Class_Reframing_Points <- function(cutoffs, predicted, actual, handle_ties=FALSE, sort_examples=FALSE) {  
  
  # sort examples is no longer needed as long as the "cutoffs" come in order.
  if (sort_examples) {  # Necessary if this is going to be used for the OCE. It is unnecessary for the UCE, though.
    res <- sort(actual, decreasing=FALSE, index.return = TRUE)  # We need to sort the actual values, as the x-axis is the true rank ratio
    actual <- res$x
    predicted <- predicted[res$ix]  # We sort the predicted with the same indices as predicted and actual must be paired  

	cutoffs <- sort(cutoffs)  # This is also needed
  }
  
  evec <- NULL
  i <- 1
  for(y in cutoffs) {
    evec[i] <- Reg2Class_Loss(predicted, actual, y, handle_ties)
	i <- i + 1
  }   

  evec
}
  
  


#### Reg2Class_UCE_curve(...) ####
#
# Plots an UCE curve given the cutoffs and their costs
#
#### Arguments:
# cutoffs: the cutoffs values
# evec: the evec values (the errors)
# colour: colour used for the curve
# linestyle: line style for the curve
# pointcharacter: point used for highlighting the points of the actual vector
# pointsize: a scaling factor applied to pointcharacter. Use small numbers for big datasets
# plot_lines: if TRUE plot the curve as lines
# plot_points: if TRUE plot the points
#
Reg2Class_UCE_curve <- function(cutoffs, evec, colour="grey", linestyle=1, pointcharacter=1, pointsize=0.25, plot_lines=TRUE, plot_points=TRUE) {  
  if (plot_lines) {
    lines(cutoffs, evec, col= colour, pch=pointcharacter, cex=pointsize, lty= linestyle,lwd=1)  
  }
  if (plot_points) {
    points(cutoffs, evec, col=colour, pch=pointcharacter, cex=pointsize)  # cex=0.5 for smaller points, which is good when there are many examples
  }
}


  
#### Reg2Class_UCE_area ####
#
# Calculates the area for a curve in the UCE space
#
#### Arguments:
# uce_points: the points of the curve
# cutoffs: cutoffs that have been used to calculate the uce_points
#
#### Returns
# The area
#
Reg2Class_UCE_area <- function(uce_points, cutoffs) {
  mean(uce_points) * (max(cutoffs) - min(cutoffs))
}



#### Reg2Class_OCE_space ####
#
# Plots the OCE space
#
#### Arguments:
# ylimsup: height of the plot.
# title: title for the plot
# xlab: xaxis title
# ylab: yaxis title
# xaxis: one or two xaxis. "RATIOS": shows the xaxis between 0 and 1. "VALUES": shows the xaxis with the actual values. "BOTH": shows both
# cutoffs: needed for "VALUES" and "BOTH"
# test_vertical_line: if not NULL a vertical line (typically at 0.5) 
# test_horizontal_line: if not NULL a horizontal line (typically at 0.5) 
# test_diagonal_lines:  if not NULL the diagonals
#
Reg2Class_OCE_space <- function(ylimsup=1, title="OCE plot", xlab=NULL, ylab="Error", xaxis="RATIOS", cutoffs=NULL, test_vertical_line=NULL, test_horizontal_line=NULL, test_diagonal_lines=NULL) {
  
  plot(0, 0, lty="blank", col="white", xlim=c(0,1), ylim=c(0,ylimsup), xlab="", ylab=ylab, main=title, xaxt='n')  #, ann=FALSE) 
  
  if (xaxis == "BOTH") {
    xaxis_reduction <- 0.75
  } else {
    xaxis_reduction <- 1
  }
  
  if (xaxis == "RATIOS") {
    axis(1)
	if (is.null(xlab)) {
	  xlab <- "True Rank Ratio"
	}
	title(xlab=xlab) 
  } else if (xaxis == "BOTH") {
    axis(1, cex.axis= xaxis_reduction, mgp=c(5,0.32,0))

	if (is.null(xlab)) {
	  xlab <- c("True Rank Ratio","Cutoff")
	}
	
    if (length(xlab) == 1) {
      title(xlab=xlab, mgp=c(4,1,0))   # 4,1,0
	} else {
	  title(xlab=xlab[2], mgp=c(4.1,1,0), cex.lab= 0.8)   
	  title(xlab=xlab[1], mgp=c(1.2,1,0), cex.lab= 0.8)   
	}
  } else if (xaxis == "VALUES") {  
	if (is.null(xlab)) {
	  xlab <- "Cutoff"
	}
  	title(xlab=xlab) 
  }
       
  if ((xaxis == "BOTH") || (xaxis == "VALUES")) {
    ticks <- 21 # If odd, it prints the first and the last, otherwise the last is not printed
    if (!is.null(cutoffs)) {
	  cutoffs <- sort(cutoffs)  # Just in case it is not ordered
	  if (ticks > length(cutoffs)) {
	    ticks <- length(cutoffs)
	  }
      # where <- seq(0,1,length.out=ticks)
	  where <- seq(0+0.5/ticks,1-0.5/ticks,length.out=ticks)   # This is because the ratios have this 0.5 for the segments
      what <- NULL
	  for (i in 1:ticks) {
  #	    w <- cutoffs[(i-1)*length(cutoffs)/ticks + 1] 
	    w <- cutoffs[round(((i-1)/(ticks-1)) * (length(cutoffs)-1)) + 1] 
	    w <- round(w,digits=1)  # No more than one digit for an xaxis
        what[i] <- w
      }

	  if (xaxis == "BOTH") {
	    axis(1, at=where, what, pos= -0.2 * ylimsup, cex.axis= xaxis_reduction, mgp=c(5,0.32,2))   # pos = -0.16 if not plotting to PDF
	  } else { 
  	    axis(1, at=where, what, cex.axis= xaxis_reduction)
	  }
	}
  }	

  if (!(is.null(test_vertical_line))) {
    lines(c(test_vertical_line,test_vertical_line), c(0,1), col="black", lwd=0.7, lty="dotted")   # vertical line at median
  }	
  if (!(is.null(test_horizontal_line))) {
    lines(c(0,1), c(test_horizontal_line,test_horizontal_line), col="black", lwd=0.7, lty="dotted")                   # horizontal line at 0.5
  }  
    
  if (!(is.null(test_diagonal_lines))) {
    if (test_diagonal_lines == TRUE) {
      lines(c(0,1), c(0,1), col="black", lwd=0.7, lty="dotted")                   
      lines(c(0,1), c(1,0), col="black", lwd=0.7, lty="dotted")                   
	}  
  }
  
}




#### Reg2Class_OCE_cutoffs ####
#
#  Returns the cutoffs equal to the actual values.  
#
#### Arguments:
# actual: values of y to be taken for the cutoffs
#
Reg2Class_OCE_cutoffs <- function(actual) {
  sort(actual)
}



#### Reg2Class_OCE_curve(...) ####
#
# Plots an OCE curve given the their costs
#
#### Arguments:
# evec_actual: the evec values (the errors)
# colour: colour used for the curve
# linestyle: line style for the curve
# pointcharacter: point used for highlighting the points of the actual vector
# pointsize: a scaling factor applied to pointcharacter. Use small numbers for big datasets
# CURVE: "NO": only plots the points starting from 0 to 1. "FLAT": plots a flat segment for each point from 0.5/N to N-0.5/N. "CURVE": aS "FLAT" but connecting the segments
# CORNER_POINTS: if the points (0,0) and (1,0) are added to the plot. These always exist for most models, but for the always-positive or the always-negative they don't.
#
Reg2Class_OCE_curve <- function(evec_actual, colour="grey", linestyle=1, pointcharacter=1, pointsize=0.25, CURVE="CURVE", CORNER_POINTS=FALSE) {  
  N <- length(evec_actual)
  if (CURVE == "NO") {  # Place
    points((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
    #  lines((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, type="o", pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
  } else if ((CURVE == "FLAT") || (CURVE == "CURVE")) {  # Place a flat segment for each point so the area matches
    my_points <- ((0:(N-1))+0.5)/N
    points(my_points, evec_actual, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples	
    if (CURVE == "FLAT") {	
	  for (i in 1:N) {
	    segments(my_points[i]-0.5/N, evec_actual[i], my_points[i]+0.5/N, evec_actual[i], col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
 	  }    
    } else {
	  if (CORNER_POINTS) {
  	    curve_vec_x <- 0
	    curve_vec_y <- 0
      } else {
	    curve_vec_x <- NULL
	    curve_vec_y <- NULL
      }
	  for (i in 1:N) {
	    curve_vec_x <- c(curve_vec_x, my_points[i]-0.5/N, my_points[i]+0.5/N)
		curve_vec_y <- c(curve_vec_y, evec_actual[i], evec_actual[i])
 	  }
	  if (CORNER_POINTS) {
  	    curve_vec_x <- c(curve_vec_x, 1)
	    curve_vec_y <- c(curve_vec_y, 0)
	  }	
	  lines(curve_vec_x, curve_vec_y, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize,lwd=1) 
	}    	
  } else {
    points((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
  }  
#  lines((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, type="o", pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
  
}






#### Reg2Class_OCE_area ####
#
# Calculates the area for a curve in the OCE space
#
#### Arguments:
# oce_points: the points of the curve
#
#### Returns
# The area
#
Reg2Class_OCE_area <- function(oce_points) {
  mean(oce_points)
}






  
#### Reg2Class_Retraining_Points ####
#
# Gets the points for the retraining case for a vector of cutoffs.
#
# REQUIRES: the test_data to be sorted by the true output.
#
#### Arguments:
# cutoffs: cutoff vector to be used (i.e., the distribution of cutoffs)
# class_technique: classification technique to be used: currently accepted: "Clas-LgR", "Clas-kNN", "Clas-J48", "Clas-Pos", "Clas-Neg", "Clas-Maj"
	
# train_data: training data
# test_data: test data
# reg_label: name of the output label of the regression training dataset 
# formula_class: formula to be used for learning
# handle_ties: if TRUE, ties are randomly spread as positive and negative
# 
#### Returns
# a vector with the error for each cutoff
#
Reg2Class_Retraining_Points <- function(cutoffs, class_technique, train_data, test_data, reg_label, formula_class, handle_ties=FALSE) {
  N <- nrow(test_data)
  retvec <- NULL
  i <- 1
  for(c in cutoffs) {    
    
	### TRAINING AND PREDICTION TOGEHTER
	
	train_data[["class"]] <- train_data[[reg_label]] >= c   # Discretises the training data
	if (handle_ties) {  # For those with ties, use a random choice
	  train_data[(train_data[[reg_label]] == c)] <- (runif(1)>0.5)
	}
	train_data[,"class"] <- sapply(train_data[,"class"], as.factor)  # We need to re-detect the factors, otherwise the "predict" will do strange things
    uni <- unique(train_data[["class"]])  # Eliminate duplicates from the outputs
		
	# First the trivial cases	
	if (class_technique == "Clas-Pos") {
	  predicted <- rep(TRUE, N)   # Always positive
	} else if (class_technique == "Clas-Neg") {
	  predicted <- rep(FALSE, N)   # Always negative
	} else if (length(uni) == 1) { # All are either TRUE or FALSE
	  predicted <- rep(uni, N)
    } else {	# There are still at least one TRUE and one FALSE
      if (class_technique == "Clas-Maj") {  # Majority class
	     # print(uni)
	     num_label1 <- sum(train_data[["class"]] == uni[1])  # How many examples of class 1
		 num_label2 <- sum(train_data[["class"]] == uni[2])  # How many examples of class 2
		 if (num_label1 > num_label2) {
		   predicted <- rep(uni[1], N)
		 } else if (num_label1 < num_label2) {
		   predicted <- rep(uni[2], N)
		 } else { # We there is a tie, we do it randomly
		   if (runif(1) > 0.5) {
 		     predicted <- rep(uni[1], N)
		   } else {
		     predicted <- rep(uni[2], N)
           }		   
		 }
	  } else { # Here the case where there is training
	    if (class_technique == "Clas-LgR") {
	       m <- Logistic(formula_class, train_data)   # Trains a classifier
	    } else if (class_technique == "Clas-kNN") {
		  weka_options <- Weka_control(K = 10, I=TRUE ) # By default K= 1 and is unweighted
	      #  -I weighted by the inverse of the distance.
	      #  -F weighted by 1- distance
	      m <- IBk(formula_class, train_data, control = weka_options)
	    } else if (class_technique == "Clas-J48") {
	       m <- J48(formula_class, train_data)   # Trains a classifier
	    } else {
  	      print("Unknown classification technique")
	      err()
        }
        predicted <- predict(m, test_data, type="class")     
	  }	
 	}  

	
	actual <- test_data[[reg_label]] 
		
	actual_class <- (actual >=c)
	
	if (handle_ties) {  # For those with ties, use a random choice
	  actual_class[actual == c] <- (runif(1)>0.5)
	}
    retvec[i] <- 1 - (sum(actual_class == predicted)) / N
	i <- i + 1
  } 
  retvec  
}
  
  





#### Reg2Class_AE_space ####
#
# Plots the AE space
#
#### Arguments:
# N: number of examples of the test set.
# ylimsup: height of the plot
# title: title for the plot
# xlab: xaxis title
# ylab: yaxis title
#
Reg2Class_AE_space <- function(N, ylimsup=1, title="AE diagram", xlab="i", ylab="AE") {
  
  plot(1:N, rep(0,N), lty="blank", col="white", xlim=c(1,N), ylim=c(0,ylimsup), xlab=xlab, ylab=ylab, main=title) 
  
}

  
  
  

#### Reg2Class_AE_diagram(...) ####
#
# Plots an AE diagram
#
#### Arguments:
# actual: the actual values
# predicted: the predicted values
# colour: colour used for the curve
# linestyle: line style for the curve
# pointcharacter: point used for highlighting the points of the actual vector
# pointsize: a scaling factor applied to pointcharacter. Use small numbers for big datasets
#
#### Returns the mean of the absolute error
Reg2Class_AE_diagram <- function(actual, predicted, colour="grey", linestyle=1, pointcharacter=1, pointsize=0.25) {  

  ordering <- sort(actual, index.return=TRUE)
  actual <- actual[ordering$ix]
  predicted <- predicted[ordering$ix]
  AE <- abs(actual-predicted)

  N <- length(actual)
  
  i <- 1:N
  points(i, AE, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples  
   
  mean(AE,)  # Returns the "area", i.e., the sum
}
  
  



#### Reg2Class_IR_space ####
#
# Plots the IR space
#
#### Arguments:
# N: number of examples of the test set.
# ylimsup: height of the plot
# title: title for the plot
# xlab: xaxis title
# ylab: yaxis title
#
Reg2Class_IR_space <- function(N, ylimsup=1, title="IR diagram", xlab="i", ylab="IR") {
  
  plot(1:N, rep(0,N), lty="blank", col="white", xlim=c(1,N), ylim=c(0,ylimsup), xlab=xlab, ylab=ylab, main=title) 
  
}

  
  
  

#### Reg2Class_IR_diagram(...) ####
#
# Plots an IR diagram
#
#### Arguments:
# actual: the actual values
# predicted: the predicted values
# colour: colour used for the curve
# linestyle: line style for the curve
# pointcharacter: point used for highlighting the points of the actual vector
# pointsize: a scaling factor applied to pointcharacter. Use small numbers for big datasets
#
#### Returns the mean of the IRs, which is equal to the corresponding area under the OCE curve
#
Reg2Class_IR_diagram <- function(actual, predicted, colour="grey", linestyle=1, pointcharacter=1, pointsize=0.25) {  

  ordering <- sort(actual, index.return=TRUE)
  actual <- actual[ordering$ix]
  predicted <- predicted[ordering$ix]

  N <- length(actual)
  mu <- NULL

  for (i in 1:N) {
    y <- actual[i]
    yhat <- predicted[i]
    mu[i] <- 0
    for (j in 1:N) {
      # if (i != j) {
        if (((yhat < actual[j]) && (actual[j] <= y)) || # in between
	        ((y < actual[j]) && (actual[j] <= yhat))) { # in between
	      mu[i] <- mu[i] + 1
	    }	 
	  # }	
    }  
  }
  mu <- mu / N

  i <- 1:length(actual)
  points(i, mu, ylim = c(0,1), col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples  
   
  mean(mu)  # Returns the "area", i.e., the sum
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
############################ DEPRECATED FUNCTIONS #################################




#### AE_curve ####
#
# Plots an AE curve and returns the areas from the minimum and maximum value or just using the points on the true data
#
#### Arguments:
# predicted: an array of predicted values
# actual: an array of true values (size of predicted and actual should match)
# colour: colour used for the curve
# linestyle: line style for the curve
# pointcharacter: point used for highlighting the points of the actual vector
# pointsize: a scaling factor applied to pointcharacter. Use small numbers for big datasets
# handle_ties: whether ties are going to be treated evenly on both sides.
#
#### Returns
# the areas from the minimum and maximum value or just using the points on the true data (as a two-element vector)
#
AE_curve <- function(predicted, actual, colour="grey", linestyle=1, pointcharacter=1, pointsize=0.25, handle_ties=FALSE) {
  miny <- min(actual, predicted)
  maxy <- max(actual, predicted)
   
  N <- length(actual) 
  
  RESO <- 10000  # resolution
  yvec <- ((-1:RESO)/(RESO-1)) * (maxy-miny) + miny
  
  evec <- NULL
  i <- 1
  for(y in yvec) {
#    acc[i] <- (sum(actual[which(predicted < y)] < y) + sum(actual[which(predicted >= y)] >= y)) / N
    evec[i] <- Reg2Class_Loss(predicted, actual, y, handle_ties)
	i <- i + 1
  }  
  lines(yvec, evec, col= colour)
  
  evec_actual <- NULL
  i <- 1
  for(y in actual) {
    evec_actual[i] <- Reg2Class_Loss(predicted, actual, y, handle_ties)
	i <- i + 1
  }
  points(actual, evec_actual, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize)  # cex=0.5 for smaller points, which is good when there are many examples
  
  #print(maxy)
  #print(miny)
  #print(evec)
  #print(mean(evec))
  
  AUC <- mean(evec) * (maxy-miny)
  AUCweightedByActualDist <- mean(evec_actual)
  
  c(AUC, AUCweightedByActualDist)
}
  
  

#### NAE_curve ####
#
# Plots an NAE curve and returns the areas from the minimum and maximum value or just using the points on the true data
#
#### Arguments:
# predicted: an array of predicted values
# actual: an array of true values (size of predicted and actual should match)
# colour: colour used for the curve
# linestyle: line style for the curve
# pointcharacter: point used for highlighting the points of the actual vector
# pointsize: a scaling factor applied to pointcharacter. Use small numbers for big datasets
# CURVE: "NO": only plots the points starting from 0 to 1. "FLAT": plots a flat segment for each point from 0.5/N to N-0.5/N. "CURVE": al "FLAT" but connecting the segments
# handle_ties: whether ties are going to be treated evenly on both sides.
#
#### Returns
# the area using the points on the true data
#
NAE_curve <- function(predicted, actual, colour="grey", linestyle=1, pointcharacter=1, pointsize=0.25, CURVE="CURVE", handle_ties=TRUE) {
  
  N <- length(actual) 
  
  res <- sort(actual, decreasing=FALSE, index.return = TRUE)  # We need to sort the actual values, as the x-axis is the true rank ratio
  actual <- res$x
  predicted <- predicted[res$ix]  # We sort the predicted with the same indices as predicted and actual must be paired
  
  evec_actual <- NULL
  i <- 1
  for(y in actual) {
    evec_actual[i] <- Reg2Class_Loss(predicted, actual, y, handle_ties)
	i <- i + 1
  }
  
  if (CURVE == "NO") {  # Place
    points((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
    #  lines((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, type="o", pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
  } else if ((CURVE == "FLAT") || (CURVE == "CURVE")) {  # Place a flat segment for each point so the area matches
    my_points <- ((0:(N-1))+0.5)/N
    points((0:(N-1)+0.5)/N, evec_actual, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples	
    if (CURVE == "FLAT") {	
	  for (i in 1:N) {
	    segments(my_points[i]-0.5/N, evec_actual[i], my_points[i]+0.5/N, evec_actual[i], col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
 	  }    
    } else {
	  curve_vec_x <- 0
	  curve_vec_y <- 0
	  for (i in 1:N) {
	    curve_vec_x <- c(curve_vec_x, my_points[i]-0.5/N, my_points[i]+0.5/N)
		curve_vec_y <- c(curve_vec_y, evec_actual[i], evec_actual[i])
 	  }
	  curve_vec_x <- c(curve_vec_x, 1)
	  curve_vec_y <- c(curve_vec_y, 0)
	  lines(curve_vec_x, curve_vec_y, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) 
	}    	
  } else {
    points((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
  }  
#  lines((0:(N-1))/(N-1), evec_actual, col=colour, lty= linestyle, type="o", pch=pointcharacter, cex=pointsize) # cex=0.5 for smaller points, which is good when there are many examples
  
  
  mean(evec_actual)
  
}
