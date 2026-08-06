## fNIRS analysis.
## Baseline-corrected mean HbO change during the 10 s viewing interval.
## Primary: Emotion x Source mixed model per Brodmann-based region of interest,
## with the false discovery rate controlled across the eight regions.
## Post hoc: source contrasts within each emotion category, for the regions
## whose source or interaction term survives correction.

run_fnirs <- function(fnirs) {
  df <- dplyr::rename(fnirs, participant_key = "device_key")
  rois <- intersect(FNIRS_ROIS, names(df))

  omnibus <- omnibus_family(df, rois)
  warranted <- measures_warranting_posthoc(omnibus)
  posthoc <- posthoc_source_by_emotion(df, warranted)
  gg <- gg_sensitivity(df, rois)

  ## Descriptive per-category contrasts, retained because the manuscript tables
  ## report cell means, differences and confidence intervals.
  descriptives <- contrast_grid(df, measures = rois) %>%
    dplyr::rename(roi = "measure") %>%
    dplyr::group_by(.data$emotion) %>%
    dplyr::mutate(p_fdr = stats::p.adjust(.data$p, method = P_ADJUST_METHOD)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$emotion, .data$roi)

  write_table(omnibus, "table2a_fnirs_omnibus")
  write_table(posthoc, "table2b_fnirs_posthoc")
  write_table(descriptives, "table2c_fnirs_descriptives")
  write_table(gg, "tableS3_fnirs_greenhouse_geisser")

  message(sprintf("fNIRS: %d regions; source effect survives FDR in %d, interaction in %d.",
                  nrow(omnibus),
                  sum(omnibus$pFDR_source < ALPHA, na.rm = TRUE),
                  sum(omnibus$pFDR_interaction < ALPHA, na.rm = TRUE)))

  list(omnibus = omnibus, posthoc = posthoc, descriptives = descriptives, gg = gg)
}
