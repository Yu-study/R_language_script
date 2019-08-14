library(GGally)
library(ggplot2)
#ggduo(data, mapping = NULL, columnsX = 1:ncol(data),columnsY =1:ncol(data), title = NULL, types = list(continuous ="smooth_loess", comboVertical ="box_no_facet", 
#                                                        comboHorizontal ="facethist",discrete ="ratio"), axisLabels = c("show","none"),
#      columnLabelsX = colnames(data[columnsX]),columnLabelsY = colnames(data[columnsY]), labeller = "label_value",
#      switch= NULL, xlab = NULL, ylab = NULL, showStrips = NULL,legend = NULL, cardinality_threshold =15, l
#      egends = stop("deprecated"))
data(psychademic)
str(psychademic)
(psych_variables <- attr(psychademic,"psychology"))
(academic_variables <- attr(psychademic,"academic"))
ggpairs(psychademic, psych_variables, title = "Within Psychological Variables")
ggpairs(psychademic, academic_variables, title ="Within Academic Variables")
pcc<-ggduo(psychademic, psych_variables, academic_variables,
      types = list(continuous ="smooth_lm"),
  title ="Between Academic and Psychological Variable Correlation",
  xlab = "Psychological",ylab ="Academic")

