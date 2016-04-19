# Implementation of a Classification Tree

Implements a classification tree that supports a variety of cost functions: cross-entropy, misclassification error, and Gini index.

For more detail, please see the comments in the R scripts.

## cTree.R

Defines functions for training a classification tree, and for using that tree to generate predictions.

#### Public functions:
	cTree()
	cTreePredict()
	cTreeShow()

#### Packages required:
	formula.tools

## cTree_spam.R

Defines a method for testing the classification tree against the 'party' library, and demonstrating the change in training and test errors as tree depth increases.

When run directly, the script defines the above function, loads the 'spam' dataset, runs the function, and generates the results in a PDF.

#### Public functions:
	cTreeTest()
	cTreeTestPlot()

#### Packages required:
	formula.tools
	caTools
	party
	ggplot2

The test takes a while to run.  Therefore, the output of a previous test is included in the file cTree.pdf.  This shows that both test and training performance improve, and then level off, as tree depth is allowed to increase.  There is a large difference between test and training errors, probably related to overfitting.  Overfitting probably occurs because, for the test, the minimum number of points per region was set to 5, a relatively low number for this purpose.
