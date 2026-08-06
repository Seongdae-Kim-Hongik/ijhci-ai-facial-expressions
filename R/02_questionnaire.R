## Questionnaire analysis.
## Recognition is summarised descriptively; empathy and perceived discomfort are
## compared between stimulus sources within each intended emotion category with
## paired-samples t tests, followed by Benjamini-Hochberg correction.

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

questionnaire_contrasts <- function(q) {
  q <- dplyr::mutate(q, participant_key = .data$questionnaire_name)

  contrast_grid(q, measures = c("empathy", "discomfort")) %>%
    dplyr::rename(scale = "measure") %>%
    ## Correction is applied within each rating scale, i.e. across the seven
    ## emotion categories that form one family of comparisons.
    dplyr::group_by(.data$scale) %>%
    dplyr::mutate(p_fdr = stats::p.adjust(.data$p, method = P_ADJUST_METHOD)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$scale, .data$emotion)
}

run_questionnaire <- function(q) {
  rates     <- recognition_rates(q)
  contrasts <- questionnaire_contrasts(q)

  write_table(rates, "table_recognition")
  write_table(contrasts, "table1_questionnaire")

  list(recognition = rates, contrasts = contrasts)
}
