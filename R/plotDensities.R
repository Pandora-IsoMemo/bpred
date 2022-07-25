#' Plot the densities estimated by \code{\link{estimateY}}.
#'
#' @param yEstimates An object created by \code{\link{estimateY}}. Distributions of the dependent variable.
#' @param type A character. Show distribution by "Sample" (default), "Category" (only if supplied by yEstimates) or all individuals "Combined"?
#' @param plotType A character. Form of presentation, choices are "KernelDensity" (default), "Histogram" or "Boxplot"
#' @param nBins An integer Number of bins for histogram
#' @param meanType A character "1" for total variance for category type, "2" for mean variance
#' @param ylabel ylabel
#' @param xlabel xlabel
#' @param headerLabel title label
#' @param xTextSize x label text size
#' @param yTextSize y label text size
#' @param xAxisSize x axis text size
#' @param yAxisSize y axis text size
#' @param showLegend show legend?
#' @param whiskerMultiplier multiplier of boxplot whiskers
#' @param boxQuantile quantile of boxplot
#' 
#' @return A ggplot2 object with densities
#' @export
plotDensities <- function(yEstimates, type = "Sample", plotType = "KernelDensity", nBins = 15, meanType = "1",
                          ylabel = "", xlabel = "", headerLabel = "",
                          xTextSize = 24, yTextSize = 24,
                          xAxisSize = 18, yAxisSize = 18,
                          showLegend = TRUE,
                          #colorPalette = "default",
                          #fontFamily = NULL,
                          whiskerMultiplier = 0.95,
                          boxQuantile = 0.68){
  # fix R CMD check warnings
  Sample <- Value <- Category <- NULL
  if(class(yEstimates) != "list" && length(yEstimates) != 2){
    stop("Please define inputs and click on \"estimate Y \" first")
  }
  
  if (type == "Sample"){
    IndividualData <-
      data.frame(Value = unlist(yEstimates$Y_Samples_Individual),
                 Sample =
                   factor(rep(1:length(yEstimates$Y_Samples_Individual),
                              times = unlist(lapply(yEstimates$Y_Samples_Individual, length)))))
    g <- ggplot(IndividualData, aes(x = Value, fill = Sample)) +
      ggtitle("Density by sample")
    if (plotType == "KernelDensity") g <- g + geom_density(alpha = 0.25)
    if (plotType == "Histogram") g <- g +
      geom_histogram(alpha = 0.75, bins = nBins, position = "identity")
    if (plotType == "Boxplot") {
      lower <- upper <- middle <- q1 <- q3 <- group <- estimate <- iqr <- minim <- maxim <- meanEst <- q32 <- q68 <- q95 <- q05 <- NULL

      dataSummary <- IndividualData %>%
        group_by(Sample) %>%
        summarise(
          # sd = sd(estimate),
          median = median(Value),
          meanEst = mean(Value),
          q68 = quantile(Value, boxQuantile),
          q95 = quantile(Value, 1 - ((1 - whiskerMultiplier) / 2)),
          q32 = quantile(Value, 1- boxQuantile),
          q05 = quantile(Value, (1 - whiskerMultiplier) / 2),
        ) %>%
        ungroup
      
      
      g <- ggplot(dataSummary, aes_(x = ~ Sample, fill = ~ Sample)) #+
      #ylab(ylabel) + xlab(xlabel)
      
      g <- g + geom_boxplot(mapping = aes(
        lower = q32,
        upper = q68,
        middle = median,
        ymin = q05,
        ymax = q95
      ),
      stat = "identity"
      ) + geom_errorbar(aes(ymin = meanEst, ymax = meanEst), linetype = "dashed", data = dataSummary)
      
      #g <- ggplot(IndividualData, aes(y = Value, x = Individual)) + geom_boxplot()
    }
  }
  
  if (type == "Combined"){
    IndividualData <- data.frame(Value = unlist(yEstimates$Y_Samples_Combined), Individual = "Combined")
    
    g <- ggplot(IndividualData, aes(x = Value)) + ggtitle("Combined density")
    if (plotType == "KernelDensity") g <- g + geom_density(alpha = 0.25, fill = "red")
    if (plotType == "Histogram") g <- g +
      geom_histogram(alpha = 0.75, bins = nBins, position = "identity")
    if (plotType == "Boxplot"){
      lower <- upper <- middle <- q1 <- q3 <- group <- estimate <- iqr <- minim <- maxim <- meanEst <- q32 <- q68 <- q95 <- q05 <- NULL
      whiskerMultiplier = 0.95
      boxQuantile = 0.68
      dataSummary <- IndividualData %>%
        group_by(Individual) %>%
        summarise(
          # sd = sd(estimate),
          median = median(Value),
          meanEst = mean(Value),
          q68 = quantile(Value, boxQuantile),
          q95 = quantile(Value, 1 - ((1 - whiskerMultiplier) / 2)),
          q32 = quantile(Value, 1- boxQuantile),
          q05 = quantile(Value, (1 - whiskerMultiplier) / 2),
        ) %>%
        ungroup
      
      g <- ggplot(dataSummary, aes_(x = ~ Individual, fill = ~ Individual)) #+
      #ylab(ylabel) + xlab(xlabel)
      
      g <- g + geom_boxplot(mapping = aes(
        lower = q32,
        upper = q68,
        middle = median,
        ymin = q05,
        ymax = q95
      ),
      stat = "identity"
      ) + geom_errorbar(aes(ymin = meanEst, ymax = meanEst), linetype = "dashed", data = dataSummary)
      
    }
  }
  if (type == "Category"){
    if (is.null(yEstimates$Y_Samples_Category)){
      stop("No categories found. Check your data")
    }
    if(meanType == "1"){
      IndividualData <-
        data.frame(Value = unlist(yEstimates$Y_Samples_Category_Mean),
                   Category = factor(rep(names(yEstimates$Y_Samples_Category_Mean),
                                         times = unlist(lapply(yEstimates$Y_Samples_Category_Mean, length)))))
    } else {
      IndividualData <-
        data.frame(Value = unlist(yEstimates$Y_Samples_Category),
                   Category = factor(rep(names(yEstimates$Y_Samples_Category),
                                         times = unlist(lapply(yEstimates$Y_Samples_Category, length)))))
    }
    
    if(plotType != "TimePlot"){
      g <- ggplot(IndividualData, aes(x = Value, fill = Category)) + ggtitle("Density by category")
      if (plotType == "KernelDensity") g <- g + geom_density(alpha = 0.25)
      if (plotType == "Histogram") g <- g +
        geom_histogram(alpha = 0.75, bins = nBins, position = "identity")
      if (plotType == "Boxplot"){ 
        lower <- upper <- middle <- q1 <- q3 <- group <- estimate <- iqr <- minim <- maxim <- meanEst <- q32 <- q68 <- q95 <- q05 <- NULL
        whiskerMultiplier = 0.95
        boxQuantile = 0.68
        dataSummary <- IndividualData %>%
          group_by(Category) %>%
          summarise(
            # sd = sd(estimate),
            median = median(Value),
            meanEst = mean(Value),
            q68 = quantile(Value, boxQuantile),
            q95 = quantile(Value, 1 - ((1 - whiskerMultiplier) / 2)),
            q32 = quantile(Value, 1- boxQuantile),
            q05 = quantile(Value, (1 - whiskerMultiplier) / 2),
          ) %>%
          ungroup
        
        g <- ggplot(dataSummary, aes_(x = ~ Category, fill = ~ Category)) #+
        #ylab(ylabel) + xlab(xlabel)
        
        g <- g + geom_boxplot(mapping = aes(
          lower = q32,
          upper = q68,
          middle = median,
          ymin = q05,
          ymax = q95
        ),
        stat = "identity"
        ) + geom_errorbar(aes(ymin = meanEst, ymax = meanEst), linetype = "dashed", data = dataSummary)
      }
      
      
    } else {
      IndividualData <- IndividualData %>% group_by(Category) %>% summarise(meanEst = mean(Value), upper = quantile(Value, 0.975), lower = quantile(Value, 0.025))
      g <- ggplot(IndividualData, aes(x = Category, y = meanEst, group = 1)) + ggtitle("Mean and 95\\% unertainty by category") + 
        geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)
    }
    
  }
  g <- g + theme(
    axis.title.x = element_text(size = xTextSize),
    axis.title.y = element_text(size = yTextSize),
    axis.text.x = element_text(size = xAxisSize),
    axis.text.y = element_text(size = yAxisSize)
  )
  if (headerLabel != "") {
    g <- g + labs(title = headerLabel) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  if (!showLegend) {
    g <- g + theme(legend.position = "none")
  }
  if (ylabel != "") {
    g <- g + ylab(ylabel)
  }
  if (xlabel != "") {
    g <- g + xlab(xlabel)
  }
  
  g
}

#' Summarise estimates and check for pairwise differences
#' 
#'
#' @param yEstimates An object created by \code{\link{estimateY}}. Distributions of the dependent variable.
#' @param type A character. Summarise and compare distribution by "Individual" (default), "Category" or all individuals "Combined"?
#' @param checkDifferences A boolean. Check pairwise differences TRUE / FALSE. Only relevant if type = "Individual" or "Category"
#' @param probability A numeric. Level of the credible intervals.
#' @param checkDifferencesReference A boolean. Check differences with reference distribution or sample.
#' @param referenceType A character. Type of reference, one of: "dist", "sample" and "freqTable"
#' @param referenceDist A character. Type of distribution in R, defaults to "norm" (if referenceType = "dist")
#' @param referenceParameters A numeric vector. Parameters of reference distribution (same order as in corresponding r-function)
#' @param referenceSample A numeric vector. Sample from reference distribution (if referenceType = "sample")
#' @param referenceTable A numeric vector. Sample from reference distribution (if referenceType = "sample")
#' @param meanType A character "1" for total variance for category type, "2" for mean variance
#' 
#' @return A ggplot2 object with densities
#' @export
summariseEstimates <- function(yEstimates, type = "Sample",
                               checkDifferences = TRUE, probability = 0.95,
                               checkDifferencesReference = FALSE,
                               referenceType = "dist",
                               referenceDist = "norm",
                               referenceParameters = c(0, 1),
                               referenceSample = c(),
                               referenceTable = matrix(data = NA, nrow = 2),
                               meanType = "1"){
  if (probability >= 1 | probability <= 0){
    stop("Probability must be greater than 0 and smaller than 1")
  }
  
  if (type == "Sample"){
    listEst <- yEstimates$Y_Samples_Individual
    names(listEst) <- 1:length(listEst)
  }
  if (type == "Category"){
    if(meanType == "1"){
      listEst <- yEstimates$Y_Samples_Category_Mean
    } else {
      listEst <- yEstimates$Y_Samples_Category
    }
    #shuffle to account for potentially different sample size
    minSampleSize <- min(unlist(lapply(listEst, length)))
    set.seed(1000)
    listEst <- lapply(listEst, function(x) sample(x, minSampleSize))
  }
  if (type == "Combined"){
    listEst <- list(Combined = yEstimates$Y_Samples_Combined)
  }
  
  if (checkDifferencesReference == TRUE && referenceType == "dist" &&
      !is.null(referenceDist) && !is.null(referenceParameters)){
    refSample <- eval(parse(text = (paste0("r", referenceDist,
                                           "(", "n=", length(listEst[[1]]), ",",
                                           paste0(referenceParameters, collapse = ","), ")"))))
    listEst$reference <- refSample
  }
  
  if (checkDifferencesReference == TRUE && referenceType == "sample" && length(referenceSample) > 1){
    refSample <- rnorm(listEst[[1]], mean = referenceSample,
                       sd = density(referenceSample, kernel = "gaussian")$bw)
    listEst$reference <- refSample
  }
  if (checkDifferencesReference == TRUE && referenceType == "freqTable" &&
      class(referenceTable) == "matrix" && nrow(referenceTable) == 2 && ncol(referenceTable) > 1){
    referenceSample <- rep(referenceTable[1, ], referenceTable[2, ])
    refSample <- rnorm(listEst[[1]], mean = referenceSample,
                       sd = density(referenceSample, kernel = "gaussian")$bw)
    listEst$reference <- refSample
  }
  
  yEst <- unlist(listEst)
  indInd <- unlist(lapply(listEst, length))
  sumdata <- data.frame(type = names(listEst),
                        Mean = round(tapply(yEst, rep(1:length(listEst),
                                                      times = indInd), mean), 3),
                        Sd = round(tapply(yEst, rep(1:length(listEst),
                                                    times = indInd), sd), 3),
                        Int_Lower = round(tapply(yEst, rep(1:length(listEst),
                                                           times = indInd),
                                                 quantile, (1 - probability) / 2), 3),
                        Int_Upper = round(tapply(yEst, rep(1:length(listEst),
                                                           times = indInd),
                                                 quantile, 1 - (1 - probability) / 2), 3))
  names(sumdata)[1] <- type
  print(sumdata, row.names = FALSE)
  if (checkDifferences == TRUE && length(listEst) > 1){
    pairwiseCombinations <- combn(names(listEst), 2)
    result <-
      data.frame(Combination = paste(pairwiseCombinations[1, ], "-", pairwiseCombinations[2, ]),
                 MeanDiff = unlist(lapply(1:ncol(pairwiseCombinations), function(x) 
                   round(mean(unlist(listEst[pairwiseCombinations[1, x]]) -
                                unlist(listEst[pairwiseCombinations[2, x]])), 3))),
                 p_value = unlist(lapply(1:ncol(pairwiseCombinations), function(x)
                   round(sum(unlist(listEst[pairwiseCombinations[1, x]]) -
                               unlist(listEst[pairwiseCombinations[2, x]]) > 0) /
                           length(listEst[[1]]), 3))))
    result$p_value[result$p_value > 0.5] <- 1 - result$p_value[result$p_value > 0.5]
    result$p_value <- result$p_value * 2
    q <- sum(rowSums(do.call("cbind", lapply(listEst, function(x){
      if (mean(x) >= mean(yEst)){
        return(x < mean(yEst))
      } else {
        return(x >= mean(yEst))
      }}))) == length(listEst)) / length(listEst[[1]])
    
    if (q >= ( (1 - probability) ^ (length(listEst) - 1)) / 2 ||
        (nrow(result) == 1 & result$p_value[1] >= (1 - probability))) {
      cat("\n No overall differences between estimates found. \n \n")
    }
    if (q < ( (1 - probability) ^ (length(listEst) - 1)) / 2 ||
        (nrow(result) == 1 & result$p_value[1] < (1 - probability))) {
      cat("\nOverall differences between estimates found. \n")
      cat("Pairwise differences: \n \n")
      
      print(result, row.names = FALSE)
      return(list(Means = sumdata, MeanComparison = result))
    }
  }
  return(list(Means = sumdata))
}

#' Plot functions
#' 
#' @param data data
#' @param xVar independent variable
#' @param yVar dependent variable
#' @param object object
#' @param ylabel ylabel
#' @param xlabel xlabel
#' @param headerLabel title label
#' @param xTextSize x label text size
#' @param yTextSize y label text size
#' @param xAxisSize x axis text size
#' @param yAxisSize y axis text size
#' @param PointSize point size
#' @param LineWidth line width
#' 
#' @export
plotFunctions <- function(data, xVar, yVar, object, 
                          ylabel = "", xlabel = "", headerLabel = "",
                          xTextSize = 24, yTextSize = 24,
                          xAxisSize = 18, yAxisSize = 18,
                          PointSize= 1, LineWidth = 1
){
  xLim <- c(min(data[,xVar], na.rm = TRUE),max(data[,xVar], na.rm = TRUE))
  #predict
  form <- object$form
  varNames <- object$varNames
  parNames <- object$parNames
  otherX <- varNames[-match(xVar, varNames)]
  xPred <- seq(xLim[1], xLim[2], by = abs(diff(range(xLim))) / 200)
  
  predMatrix <- sapply(1:length(xPred), function(k){
    sapply(1:min(nrow(object$beta), 100), function(l){
      xPlot <- xPred[k]
      formNew <- gsub(paste0("\\[", xVar, "\\]"), xPlot, form)
      if(length(otherX) > 0){
        for(m in 1:length(otherX)){
          formNew <- gsub(paste0("\\[", otherX[m], "\\]"), median(data[, otherX[m]], na.rm = TRUE), formNew)
        }
      }
      for(i in 1:length(parNames)){
        formNew <- gsub(paste0("\\{", parNames[i], "\\}"), object$beta[l, i], formNew)
      }
      
      
      eval(parse(text = formNew))
    })
  })
  
  lineData <- data.frame(xPred = xPred, yPred = colMeans(predMatrix))
  
  g <- ggplot(as.data.frame(data), aes_string(x = xVar, y = yVar)) + geom_point(size = PointSize) + 
    geom_line(data = lineData, aes(x = xPred, y = yPred), size = LineWidth)
  g <- g + theme(
    axis.title.x = element_text(size = xTextSize),
    axis.title.y = element_text(size = yTextSize),
    axis.text.x = element_text(size = xAxisSize),
    axis.text.y = element_text(size = yAxisSize)
  )
  if (headerLabel != "") {
    g <- g + labs(title = headerLabel) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  if (ylabel != "") {
    g <- g + ylab(ylabel)
  }
  if (xlabel != "") {
    g <- g + xlab(xlabel)
  }
  
  list(g = g, exportData = lineData)
}