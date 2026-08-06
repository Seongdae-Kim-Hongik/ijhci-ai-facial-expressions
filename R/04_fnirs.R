## fNIRS analysis.
## Baseline-corrected mean HbO change during the 10 s viewing interval.
## Primary: Emotion x Source repeated-measures ANOVA per Brodmann-based region,
## with the false discovery rate controlled across regions within each effect.
## Because a participant enters a region only with a complete set of cells, the
## number of participants differs between regions and is reported per region.

## The complete-case requirement costs participants, so the omnibus result is
## repeated on all available observations with a mixed model whose random terms
## mirror the error strata of the repeated-measures ANOVA. This shows whether a
## null result reflects the missing-data rule rather than the data.
fnirs_available_case_sensitivity <- function(df, rois) {
  purrr::map_dfr(rois, function(r) {
    d <- df
    d$DV <- suppressWarnings(as.numeric(d[[r]]))
    d$.id <- factor(d$participant_key)
    d <- dplyr::filter(d, is.finite(.data$DV))
    if (dplyr::n_distinct(d$.id) < 3) return(NULL)

    model <- try(suppressMessages(lmerTest::lmer(
      DV ~ emotion * source + (1 | .id) + (1 | .id:emotion) + (1 | .id:source),
      data = d)), silent = TRUE)
    if (inherits(model, "try-error")) return(NULL)

    a <- as.data.frame(stats::anova(model))
    a$term <- rownames(a)
    tibble::tibble(
      roi            = r,
      n_participants = dplyr::n_distinct(d$.id),
      n_obs          = nrow(d),
      F_source       = a$`F value`[a$term == "source"],
      p_source       = a$`Pr(>F)`[a$term == "source"],
      F_interaction  = a$`F value`[a$term == "emotion:source"],
      p_interaction  = a$`Pr(>F)`[a$term == "emotion:source"]
    )
  }) %>%
    dplyr::mutate(
      pFDR_source      = stats::p.adjust(.data$p_source, P_ADJUST_METHOD),
      pFDR_interaction = stats::p.adjust(.data$p_interaction, P_ADJUST_METHOD)
    )
}

run_fnirs <- function(fnirs) {
  df <- dplyr::rename(fnirs, participant_key = "device_key")
  rois <- drop_duplicate_columns(df, intersect(FNIRS_ROIS, names(df)), "fNIRS: ")

  omnibus   <- omnibus_family(df, rois)
  warranted <- measures_warranting_posthoc(omnibus)
  posthoc   <- posthoc_source_by_emotion(df, warranted)
  gg        <- gg_sensitivity(df, rois)
  available <- fnirs_available_case_sensitivity(df, rois)

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
  write_table(available, "tableS5_fnirs_available_case")

  message(sprintf("fNIRS: %d regions (n = %d-%d per region); source effect survives FDR in %d, interaction in %d.",
                  nrow(omnibus), min(omnibus$n_participants), max(omnibus$n_participants),
                  sum(omnibus$pFDR_source < ALPHA, na.rm = TRUE),
                  sum(omnibus$pFDR_interaction < ALPHA, na.rm = TRUE)))
  message(sprintf("fNIRS available-case sensitivity (n = %d-%d): source survives FDR in %d, interaction in %d.",
                  min(available$n_participants), max(available$n_participants),
                  sum(available$pFDR_source < ALPHA, na.rm = TRUE),
                  sum(available$pFDR_interaction < ALPHA, na.rm = TRUE)))

  list(omnibus = omnibus, posthoc = posthoc, descriptives = descriptives,
       gg = gg, available_case = available)
}
