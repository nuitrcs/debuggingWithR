calculate_colmeans <- function(df, column_names) {
  # check df is data.frame
  if (!is.data.frame(df)) stop("df is not a data.frame.")
  # check that all column names exist
  if (!all(column_names %in% colnames(df))) stop("Not all column names exist.")
  # check that all columns are numeric
  if (!all(sapply(df,is.numeric)[column_names])) stop("Not all columns are numeric")
  
  # subset columns
  df_subset <- df[,column_names,drop=FALSE]
  
  # calculate column means
  return(colMeans(df[,column_names]))
}

calculate_colmeans(iris,"Sepal.Length")
