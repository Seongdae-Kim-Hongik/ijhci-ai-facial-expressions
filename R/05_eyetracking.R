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

run_eyetracking <- function(et) {
  df <- dplyr::rename(et, participant_key = "device_key")
  candidates <- setdiff(names(df), c("participant_key", "emotion", "source"))

  ## Tobii writes several aliases of the same response (the Visit and Glance
  ## families duplicate the fixation family when a single AOI is defined, and
  ## the fixation-linked pupil and eye-openness summaries track their trial-level
  ## counterparts at r > .99). Keeping them would not break the correction, which
  ## stays valid under positive dependence, but it would present one finding as
  ## several. One representative per block enters the family; the mapping is
  ## written out so the collapsed metrics remain visible.
  usable <- drop_duplicate_columns(df, valid_et_metrics(df, candidates),
                                   "Eye-tracking: ")
  redundancy <- collapse_redundant(df, usable, label = "Eye-tracking: ")
  metrics <- redundancy$keep

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

  represented <- tibble::tibble(
    representative = rep(names(redundancy$blocks),
                         lengths(redundancy$blocks)),
    collapsed_into_it = unlist(redundancy$blocks, use.names = FALSE)
  )
  write_table(represented, "tableS6_eyetracking_redundant_metrics")

  message(sprintf("Eye-tracking: %d metrics in the family; source effect survives FDR in %d, interaction in %d; %d carried to post hoc.",
                  nrow(omnibus),
                  sum(omnibus$pFDR_source < ALPHA, na.rm = TRUE),
                  sum(omnibus$pFDR_interaction < ALPHA, na.rm = TRUE),
                  length(warranted)))

  list(omnibus = omnibus, core = core, posthoc = posthoc,
       descriptives = descriptives, gg = gg, redundancy = redundancy)
}
