## Figures reported in the manuscript.
## Greyscale, to match the rest of the article and to stay legible in print.

SOURCE_FILL <- c(AI = "grey30", Human = "grey80")

figure_theme <- ggplot2::theme_classic(base_size = 9) +
  ggplot2::theme(
    axis.text        = ggplot2::element_text(colour = "black"),
    axis.title       = ggplot2::element_text(colour = "black"),
    axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
    strip.background = ggplot2::element_blank(),
    strip.text       = ggplot2::element_text(face = "bold", size = 9),
    legend.title     = ggplot2::element_blank(),
    legend.position  = "top",
    legend.key.size  = ggplot2::unit(3.5, "mm")
  )

save_figure <- function(plot, name, width, height) {
  path <- file.path(FIGURE_DIR, paste0(name, ".png"))
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 600,
                  bg = "white")
  invisible(path)
}

## Significance marker from a Bonferroni-corrected post-hoc p value.
sig_mark <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***",
    ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

title_case <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))

## Figure 5: intended-label agreement, empathy and perceived discomfort.
## Asterisks mark contrasts surviving Bonferroni correction across the seven
## categories; agreement is descriptive and carries no test.
figure_questionnaire <- function(q, rates, posthoc) {
  recognition <- rates %>%
    dplyr::filter(.data$emotion != "overall") %>%
    dplyr::transmute(
      emotion = factor(.data$emotion, levels = EMOTIONS),
      source  = .data$source,
      panel   = "Intended-label agreement (%)",
      value   = .data$rate, lower = NA_real_, upper = NA_real_
    )

  ratings <- q %>%
    tidyr::pivot_longer(dplyr::all_of(c("empathy", "discomfort")),
                        names_to = "panel", values_to = "score") %>%
    dplyr::group_by(.data$emotion, .data$source, .data$panel) %>%
    dplyr::summarise(
      value = mean(.data$score, na.rm = TRUE),
      se    = stats::sd(.data$score, na.rm = TRUE) / sqrt(sum(!is.na(.data$score))),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      .data$emotion, .data$source,
      panel = ifelse(.data$panel == "empathy",
                     "Empathy (1-5)", "Perceived discomfort (1-5)"),
      value = .data$value, lower = .data$value - .data$se,
      upper = .data$value + .data$se
    )

  levels_order <- c("Intended-label agreement (%)", "Empathy (1-5)",
                    "Perceived discomfort (1-5)")
  plot_data <- dplyr::bind_rows(recognition, ratings) %>%
    dplyr::mutate(panel = factor(.data$panel, levels = levels_order))

  marks <- posthoc %>%
    dplyr::transmute(
      emotion = factor(.data$emotion, levels = EMOTIONS),
      panel   = factor(ifelse(.data$measure == "empathy",
                              "Empathy (1-5)", "Perceived discomfort (1-5)"),
                       levels = levels_order),
      label   = sig_mark(.data$p_bonf)
    ) %>%
    dplyr::filter(.data$label != "") %>%
    dplyr::left_join(
      plot_data %>%
        dplyr::group_by(.data$emotion, .data$panel) %>%
        dplyr::summarise(y = max(.data$upper, na.rm = TRUE), .groups = "drop"),
      by = c("emotion", "panel"))

  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(.data$emotion, .data$value, fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.75), width = 0.68,
                      colour = "black", linewidth = 0.25) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      position = ggplot2::position_dodge(0.75), width = 0.18,
      linewidth = 0.3, na.rm = TRUE) +
    ggplot2::geom_text(data = marks,
                       ggplot2::aes(.data$emotion, .data$y, label = .data$label),
                       inherit.aes = FALSE, vjust = -0.3, size = 3) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    ggplot2::scale_fill_manual(values = SOURCE_FILL) +
    ggplot2::scale_x_discrete(labels = title_case) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = NULL) +
    figure_theme

  save_figure(p, "figure5_questionnaire", width = 7.2, height = 3.0)
  p
}

## Figure 6: average pupil diameter by category and source.
## Only the trial-level summary is shown: the fixation-linked summary correlates
## with it at r = .997 and is the same measurement reported twice.
figure_pupil <- function(et, posthoc, measure = "Average_pupil_diameter") {
  cell <- et %>%
    dplyr::filter(is.finite(.data[[measure]])) %>%
    dplyr::group_by(.data$emotion, .data$source) %>%
    dplyr::summarise(
      value = mean(.data[[measure]]),
      se    = stats::sd(.data[[measure]]) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  marks <- posthoc %>%
    dplyr::filter(.data$measure == !!measure) %>%
    dplyr::transmute(emotion = factor(.data$emotion, levels = EMOTIONS),
                     label = sig_mark(.data$p_bonf)) %>%
    dplyr::filter(.data$label != "") %>%
    dplyr::left_join(
      cell %>% dplyr::group_by(.data$emotion) %>%
        dplyr::summarise(y = max(.data$value + .data$se), .groups = "drop"),
      by = "emotion")

  p <- ggplot2::ggplot(cell, ggplot2::aes(.data$emotion, .data$value,
                                          fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.75), width = 0.68,
                      colour = "black", linewidth = 0.25) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$value - .data$se, ymax = .data$value + .data$se),
      position = ggplot2::position_dodge(0.75), width = 0.18, linewidth = 0.3) +
    ggplot2::geom_text(data = marks,
                       ggplot2::aes(.data$emotion, .data$y, label = .data$label),
                       inherit.aes = FALSE, vjust = -0.4, size = 3) +
    ggplot2::scale_fill_manual(values = SOURCE_FILL) +
    ggplot2::scale_x_discrete(labels = title_case) +
    ggplot2::coord_cartesian(ylim = c(3.0, 5.0)) +
    ggplot2::labs(x = NULL, y = "Average pupil diameter (mm)") +
    figure_theme

  save_figure(p, "figure6_pupil", width = 5.0, height = 2.9)
  p
}
