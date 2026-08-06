## fNIRS analysis.
## Baseline-corrected mean HbO change during the 10 s viewing interval is
## compared between stimulus sources with paired-samples t tests, separately for
## each Brodmann-based region of interest. The false discovery rate is
## controlled across the eight regions within each emotion category.

run_fnirs <- function(fnirs) {
  df <- dplyr::rename(fnirs, participant_key = "device_key")
  rois <- intersect(FNIRS_ROIS, names(df))

  res <- contrast_grid(df, measures = rois) %>%
    dplyr::rename(roi = "measure") %>%
    dplyr::group_by(.data$emotion) %>%
    dplyr::mutate(p_fdr = stats::p.adjust(.data$p, method = P_ADJUST_METHOD)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$emotion, .data$roi)

  write_table(res, "table2_fnirs")
  res
}
