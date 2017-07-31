#' Joyplot/distribution figures
#' 
#' This function takes the input watershed integrity dataset and creates a 
#' joyplot for a given set of input variables and and optional set of facetting 
#' variables.  
#' 
#'  @param df Input data frame.
#'  @param distro A character vector of the variables containing the values to 
#'                plot a distribution.
#'  @param facet The variable containg the faceting values.  Defaults to NULL
#'  @param ... Arguments passed to \link[ggplot2]{ggsave}
#'  @export
distro_fig <- function(df, distro, facet, ...){
  df1 <- dplyr::select(df,comid, watershed, variable, value)
  df1 <- dplyr::filter(df1, variable %in% distro)
  df1 <- unique(df1)
  browser()
  
  if(is.null(facet)){
    gg <- ggplot(df1, aes(x = value, y = variable)) +
      geom_joy(rel_min_height = 0.001, scale = 0.9, bandwidth = 0.04) +
      theme_ipsum() +
      labs(x = "", y = "")
  } else {
    gg <- ggplot(df1, aes(x = value, y = variable)) +
      geom_joy(rel_min_height = 0.001, scale = 0.9, bandwidth = 0.04) +
      theme_ipsum() +
      facet_grid(facet) +
      labs(x = "", y = "")
  }
  
  gg
}