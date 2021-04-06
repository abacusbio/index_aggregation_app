#' Create a new table by calculating index BW for each animal
#' \describe {and add those to EBV table
#' This table will be reused by downstream calculations }
#' @param df_ebv_select data.frame, the filtered EBV table
#' @param df_econval data.frame, the economical weight table
#' @param df_description data.frame, the file description table
#' @return a data.frame which columns as ID trait1 trait2... index1 index2...
calculateIndividualBW <- function (input, output, session,
                                   df_ebv_select, df_econval, desc_ebv, desc_ev) {
  # subset traits existing in dt_econval() only
  idx_ev <- which(colnames(df_econval) %in% desc_ev$column_labelling[desc_ev$classifier=="EV"])
# cat("calculateIndividualBW\n")
  idx <- match(colnames(df_ebv_select), colnames(df_econval))
  idx <- idx[idx %in% idx_ev]
  traits <- colnames(df_econval)[sort(idx)]
# cat(" traits:"); print(traits)  
  sub_ebv <- dplyr::select(df_ebv_select,  all_of(traits)) %>% 
    # replace(., is.na(.), 0) %>% # 5june2020
    as.matrix() # animal x trait
  
  econval <- dplyr::select(df_econval, all_of(traits)) %>%     # drop trait column
    as.matrix() %>% t() # trait x index

  indexes <- sub_ebv %*% econval # animal x index
  colnames(indexes) <- df_econval$Index

  ids <- desc_ebv$column_labelling[grep("ID|ClassVar", desc_ebv$classifier)]
  out <- data.frame(df_ebv_select[,ids], sub_ebv, indexes)
  
  return(out)
}