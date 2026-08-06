## Eye-tracking analysis.
## Primary: Emotion x Source repeated-measures ANOVA per Tobii Pro Lab metric,
## with the false discovery rate controlled across the metrics separately for
## each effect. Correction uses the full battery; the main table shows the
## pre-specified core metrics and the remainder is supplementary.
## Post hoc: Bonferroni source contrasts within each emotion category, for the
## metrics whose source or interaction effect survives correction.

## Metrics that are sufficiently observed and actually vary.
valid_et_metrics <- function(df, candidates) {
  keep <- vapply(candidates, function(m) {
    v <- suppressWarnings(as.numeric(df[[m]]))
    coverage <- mean(!is.na(v))
    variance <- stats::sd(v, na.rm = TRUE)
    isTRUE(coverage >= ET_MIN_COVERAGE) && isTRUE(variance > 0)
  }, logical(1))
  candidates[keep]
}

## Metrics that are numerically identical to an earlier metric contribute no
## independent test and are dropped before the correction is applied.
drop_duplicate_metrics <- function(df, metrics) {
  signatures <- character(0)
  keep <- character(0)
  for (m in metrics) {
    sig <- paste(utils::capture.output(print(df[[m]])), collapse = "")
    if (!sig %in% signatures) {
      signatures <- c(signatures, sig)
      keep <- c(keep, m)
    }
  }
  if (length(keep) < length(metrics)) {
    message("Eye-tracking: dropped ", length(metrics) - length(keep),
            " metric(s) numerically identical to another metric: ",
            paste(setdiff(metrics, keep), collapse = ", "))
  }
  keep
}

run_eyetracking <- function(et) {
  df <- dplyr::rename(et, participant_key = "device_key")
  candidates <- setdiff(names(df), c("participant_key", "emotion", "source"))

  metrics <- drop_duplicate_metrics(df, valid_et_metrics(df, candidates))

  omnibus <- omnibus_family(df, metrics)
  warranted <- measures_warranting_posthoc(omnibus)
  posthoc <- posthoc_source_by_emotion(df, warranted)
  gg <- gg_sensitivity(df, intersect(ET_CORE_METRICS, metrics))

  descriptives <- contrast_grid(df, measures = metrics) %>%
    dplyr::arrange(.data$p)

  core <- dplyr::filter(omnibus, .data$measure %in% ET_CORE_METRICS)

  write_table(core, "table3a_eyetracking_omnibus_core")
  write_table(omnibus, "tableS1_eyetracking_omnibus_full")
  write_table(posthoc, "table3b_eyetracking_posthoc")
  write_table(descriptives, "table3c_eyetracking_descriptives")
  write_table(gg, "tableS2_eyetracking_greenhouse_geisser")

  message(sprintf("Eye-tracking: %d metrics in the family; source effect survives FDR in %d, interaction in %d; %d carried to post hoc.",
                  nrow(omnibus),
                  sum(omnibus$pFDR_source < ALPHA, na.rm = TRUE),
                  sum(omnibus$pFDR_interaction < ALPHA, na.rm = TRUE),
                  length(warranted)))

  list(omnibus = omnibus, core = core, posthoc = posthoc,
       descriptives = descriptives, gg = gg)
}
