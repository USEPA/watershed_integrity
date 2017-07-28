#' Correlation matrix figures
#' 
#' This function takes the input watershed integrity dataset and creates a 
#' correlation matrix for a given study area, set of x variables and set of y 
#' variables.  
#' 
#'  @param data Input data frame.
#'  @param ws Name, in all lower case, of study area/watershed.
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
#'  @examples
#'  wi <- unique(readr::read_csv(here::here("data/watershed_integrity_new.csv")))
#'  cal_y <- c("logNdif","dN15chironimid","total_in","xcmgw","xembed","fishMMI",
#'  "max_tempC_summer","phase")
#'  cal_x <- c("iwi", "ici","wchem","cchem","whabt","chabt","wsed","csed",
#'             "whyd","chyd","wtemp","ctemp")
#'  corr_fig(wi,"calapooia",cal_x,cal_y, filename = "cal_cor_fig.jpg",width = 7, 
#'           height = 5, units = "in", dpi = 300)
corr_fig <- function(data, ws, x, y, method = "pearson", 
                     output_csv = NULL, ...){
  id_cols <- names(data)[1:4]
  df <- dplyr::filter(data, watershed == ws)
  df <- tidyr::spread(df, key = variable, value = value)
  df <- dplyr::select(df, c(id_cols,y,x))
 
  cor_df <- data.frame(cor(df[,-1:-4],use = "pairwise.complete.obs"))
  cor_df <- tibble::rownames_to_column(cor_df, "index")
  cor_df <- dplyr::filter(cor_df, stringr::str_detect(cor_df$index, paste(x,collapse = "|")))
  cor_df <- dplyr::select(cor_df, c("index",y))
  cor_df <- data.frame(index = cor_df[,1], round(cor_df[-1],2))
  
  cor_df_long <- gather(cor_df, "variable", "value", -1)
  
  cor_gg <- ggplot(cor_df_long, aes(x = index, y = variable)) +
    geom_point(aes(size = abs(value), color = value)) + 
    scale_color_viridis_c(name = "Pearson\nCorrelation", 
                          limits = c(-1,1), #range(cor_df_long$value), 
                          breaks = c(1.0, 0.5, 0.0, -0.5, -1.0) ,#round(seq(max(cor_df_long$value), min(cor_df_long$value), length.out = 5), 2),
                          guide = guide_legend(override.aes = 
                                                 list(size = c(5,2.5,1,2.5,5)), 
                                                 reverse = FALSE)) +
    scale_size(range = c(1,5), guide = FALSE) +
    theme_ipsum() +
    scale_x_discrete(position = "top") +
    labs(x = "", y = "") +
    theme(legend.text = element_text(size = 10),
          axis.text.x = element_text(angle= 45, hjust = 0))
  
  if(!is.null(output_csv)){
    readr::write_csv(cor_df, path = output_csv)
  }
  
  if(length(list(...)) > 0){
    ggsave(plot = cor_gg, ...)
  }
 
  cor_gg
}