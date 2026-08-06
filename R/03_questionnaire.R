## Questionnaire analysis.
## Intended-label agreement is summarised descriptively, as in the manuscript:
## with one exemplar per source-category cell and cells at 0 and 100 per cent, a
## logistic mixed model is not estimable.
## Empathy and perceived discomfort are analysed with the Emotion x Source mixed
## model, followed by post-hoc source contrasts within each category.

recognition_rates <- function(q) {
  by_cell <- q %>%
    dplyr::group_by(.data$emotion, .data$source) %>%
    dplyr::summarise(
      n         = sum(!is.na(.data$recognised)),
      n_correct = sum(.data$recognised, na.rm = TRUE),
      rate      = 100 * mean(.data$recognised, na.rm = TRUE),
      .groups   = "drop"
    )

  overall <- q %>%
    dplyr::group_by(.data$source) %>%
    dplyr::summarise(
      emotion   = "overall",
      n         = sum(!is.na(.data$recognised)),
      n_correct = sum(.data$recognised, na.rm = TRUE),
      rate      = 100 * mean(.data$recognised, na.rm = TRUE),
      .groups   = "drop"
    )

  dplyr::bind_rows(by_cell, overall) %>%
    dplyr::relocate("emotion")
}

run_questionnaire <- function(q) {
  df <- dplyr::mutate(q, participant_key = .data$questionnaire_name)
  scales <- c("empathy", "discomfort")

  rates   <- recognition_rates(q)
  omnibus <- omnibus_family(df, scales)
  posthoc <- posthoc_source_by_emotion(df, scales)
  gg      <- gg_sensitivity(df, scales)

  descriptives <- contrast_grid(df, measures = scales) %>%
    dplyr::rename(scale = "measure") %>%
    dplyr::group_by(.data$scale) %>%
    dplyr::mutate(p_fdr = stats::p.adjust(.data$p, method = P_ADJUST_METHOD)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$scale, .data$emotion)

  write_table(rates, "table_recognition")
  write_table(omnibus, "table1a_questionnaire_omnibus")
  write_table(posthoc, "table1b_questionnaire_posthoc")
  write_table(descriptives, "table1c_questionnaire_descriptives")
  write_table(gg, "tableS4_questionnaire_greenhouse_geisser")

  list(recognition = rates, omnibus = omnibus,
       posthoc = posthoc, descriptives = descriptives, gg = gg)
}
