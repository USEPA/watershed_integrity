#' Correlation matrix figures
#' 
#' This function takes the input watershed integrity dataset and creates a 
#' correlation matrix for a given study area, set of x variables and set of y 
#' variables.  
#' 
#'  @param data Input data frame.
#'  @param watershed Name, in all lower case, of study area/watershed.
#'  @param x The x variables for the correlation and figure.  These should be 
#'           the catchement and watershed indices.
#'  @param y The y variables for the correlations and figures. These should be
#'           the various indepent variables used to evaluate the catchment and
#'           watershed indices.
#'  @param method The method used for the correlation.  Defaults to "pearson".  
#'                Other options defined by \link[stats]{cor}
#'  @param output_csv Path and name of output csv file with correlations.  
#'                    Default is NULL and no output created.
#'  @param ... Arguments passed to \link[ggplot2]{ggsave}
#'  @export
corr_fig <- function(data, watershed, x, y, method = "pearson", 
                     output_csv = NULL){
  
}