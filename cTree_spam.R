
#############################################################################
# cTree.R
#
# Matthew Sudmann-Day
# Barcelona GSE Data Science
#
# Performs a comparison of test and training errors between the classification
# tree algorithm in the script cTree.R and the library function party::ctree().
#
# When run directly, the script defines the above function, loads the 'spam'
# dataset, runs the function, and generates the results in a PDF.
#
# Uses R packages:
#   formula.tools
#   party
#   caTools
#   ggplot2
#
# Public functions: cTreeTest(), cTreeTestPlot()
#############################################################################

# Activate this code if you do not have the prerequisite libraries.
#install.packages("formula.tools")
#install.packages("party")
#install.packages("caTools")
#install.packages("ggplot2")

#############################################################################
# Public function cTreeTest() evaluates the test errors and training errors of
# this cTree algorithm, and compares it against party::ctree().  It loops through
# a number of maximum depths and produces a plot based on the errors measured.
#
# Parameters:
#   formula: an R-style formula to describe the label and the independent
#     columns in the data
#   data: a data frame containing the data from which both training and test
#     sets will be extracted
#   trainRatio: the portion of the data frame that should be dedicated to
#     training, the remainder being used for testing (default=0.7)
#   costFnc: the cost function to measure the fragmentation of the labels
#     (permitted values are "Entropy" (default), "ME", and "Gini") to be applied
#     to the cTree function only.  party::ctree() will use its own default.
#   minPoints: the minimum number of training observations permitted in a
#     single classification region (default=5).  This applies to cTree only.
#     party::cTree() will use its own default.
#
# Returns:  nothing
cTreeTest <- function(formula, data, trainRatio=0.7, costFnc="Entropy", minPoints=5)
{
  require(formula.tools)
  require(party)
  require(caTools)
  
  # Determine the label column.
  labelColumn <- get.vars(lhs(formula))

  # Split the data between training and test sets.  
  split <- sample.split(data, trainRatio)
  train <- data[split,]
  test <- data[!split,]

  # Initialize a data frame for the results of the test.
  results <- data.frame(depth=NA, actualDepth=NA, trainErrors=NA, testErrors=NA, comparisonTrainErrors=NA, comparisonTestErrors=NA)
  
  # Loop through a number of maximum depths.
  for (depth in (1:13))
  {
    # Train the tree on the training set.
    tree <- cTree(formula, train, depth, minPoints, costFnc)
    actualDepth <- tree$depth
    
    # When we no longer use the authorized depth, there is no point in continuing to increase it.
    if (actualDepth < depth)
    {
      break
    }
    
    # Generate predictions and calculate errors.
    preds <- cTreePredict(tree, train)
    trainErrors <- 1 - mean(train$spam == preds$predLabels)

    preds <- cTreePredict(tree, test)
    testErrors <- 1 - mean(test$spam == preds$predLabels)
    
    # Train the party::ctree() class on the same training set.
    comparisonTree <- ctree(formula, train, controls=ctree_control(maxdepth=depth))

    # Generate predictions and calculate errors.
    preds <- predict(comparisonTree, train)
    comparisonTrainErrors <- 1 - mean(train$spam == round(preds))
    
    preds <- predict(comparisonTree, test)
    comparisonTestErrors <- 1 - mean(test$spam == round(preds))
    
    # Append our results.
    results <- rbind(results, c(depth, actualDepth, trainErrors, testErrors, comparisonTrainErrors, comparisonTestErrors))
  }
  
  results <- results[-1, ]
  return(results)
}

#############################################################################
# Public function cTreeTestPlot() produces a plot based on the results of the
# cTreeTest() function.  It returns the plot and saves it to a PDF.
#
# Parameters:
#   results: a data frame containing the results of cTreeTestPlot()
#   file: the filename or path to which a PDF of the results will be written
#
# Returns: the plot
cTreeTestPlot <- function(results, file)
{
  require(ggplot2)
  
  data1 <- data.frame(depth=results$depth, errors=results$trainErrors, Category="cTree() Training")
  data2 <- data.frame(depth=results$depth, errors=results$testErrors, Category="cTree() Testing")
  data3 <- data.frame(depth=results$depth, errors=results$comparisonTrainErrors, Category="party.ctree() Training")
  data4 <- data.frame(depth=results$depth, errors=results$comparisonTestErrors, Category="party.ctree() Testing")
  data <- rbind(data1, data2, data3, data4)

  plot <- ggplot(data=data)
  plot <- plot + ggtitle("Depth of Classification Tree vs. Error Rates")
  plot <- plot + geom_line(aes(x=depth, y=errors, color=Category))
  plot <- plot + xlab("Tree Depth") + ylab("Errors")
  
  ggsave(file, plot)
  return(plot)
}

# Load the data
data <- read.csv("Spam/spambase.data")

# Perform the test
results <- cTreeTest(spam ~ ., data, trainRatio=0.2)

# Plot the results
cTreeTestPlot(results, "cTree.pdf")
