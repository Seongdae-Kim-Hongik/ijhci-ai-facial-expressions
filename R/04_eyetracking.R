## Eye-tracking analysis.
## Trial-level metrics exported by Tobii Pro Lab are compared between stimulus
## sources with paired-samples t tests within each emotion category. The false
## discovery rate is controlled across the whole set of comparisons, which is
## the family referred to in the manuscript.

run_eyetracking <- function(et) {
  df <- dplyr::rename(et, participant_key = "device_key")
  metrics <- setdiff(names(df), c("participant_key", "emotion", "source"))

  res <- contrast_grid(df, measures = metrics) %>%
    dplyr::mutate(p_fdr = stats::p.adjust(.data$p, method = P_ADJUST_METHOD)) %>%
    dplyr::arrange(.data$p)

  write_table(res, "table3_eyetracking")

  message(sprintf("Eye-tracking: %d comparisons (%d metrics x %d categories); %d significant after FDR correction.",
                  nrow(res), length(metrics), length(EMOTIONS), sum(res$p_fdr < ALPHA)))
  res
}
