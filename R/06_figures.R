## Figures reported in the manuscript.

SOURCE_COLOURS <- c(AI = "#C0392B", Human = "#2C6FA6")

figure_theme <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    axis.title = ggplot2::element_text(face = "bold"),
    legend.title = ggplot2::element_blank(),
    legend.position = "top"
  )

save_figure <- function(plot, name, width = 7, height = 4.5) {
  path <- file.path(FIGURE_DIR, paste0(name, ".png"))
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 600)
  invisible(path)
}

## Figure 5: recognition, empathy and perceived discomfort by category.
figure_questionnaire <- function(q, rates) {
  recognition <- rates %>%
    dplyr::filter(.data$emotion != "overall") %>%
    dplyr::mutate(emotion = factor(.data$emotion, levels = EMOTIONS),
                  panel = "Intended-label agreement (%)",
                  value = .data$rate, lower = NA_real_, upper = NA_real_) %>%
    dplyr::select("emotion", "source", "panel", "value", "lower", "upper")

  ratings <- q %>%
    tidyr::pivot_longer(dplyr::all_of(c("empathy", "discomfort")),
                        names_to = "panel", values_to = "score") %>%
    dplyr::group_by(.data$emotion, .data$source, .data$panel) %>%
    dplyr::summarise(
      value = mean(.data$score, na.rm = TRUE),
      se    = stats::sd(.data$score, na.rm = TRUE) / sqrt(sum(!is.na(.data$score))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      panel = ifelse(.data$panel == "empathy", "Empathy (1-5)", "Perceived discomfort (1-5)"),
      lower = .data$value - .data$se,
      upper = .data$value + .data$se
    ) %>%
    dplyr::select("emotion", "source", "panel", "value", "lower", "upper")

  plot_data <- dplyr::bind_rows(recognition, ratings) %>%
    dplyr::mutate(panel = factor(.data$panel, levels = c(
      "Intended-label agreement (%)", "Empathy (1-5)", "Perceived discomfort (1-5)")))

  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(.data$emotion, .data$value, fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      position = ggplot2::position_dodge(0.8), width = 0.2, na.rm = TRUE) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_manual(values = SOURCE_COLOURS) +
    ggplot2::labs(x = NULL, y = NULL) +
    figure_theme

  save_figure(p, "figure5_questionnaire", width = 7, height = 8)
  p
}

## Figure 7: pupil summaries for the categories with reported contrasts.
figure_pupil <- function(et, categories = c("disgust", "fear", "surprise")) {
  measures <- c(`Average pupil diameter` = "Average_pupil_diameter",
                `Fixation-linked pupil diameter` = "Average_wholefixation_pupil_dia")
  measures <- measures[measures %in% names(et)]

  plot_data <- et %>%
    dplyr::filter(.data$emotion %in% categories) %>%
    tidyr::pivot_longer(dplyr::all_of(unname(measures)),
                        names_to = "measure", values_to = "value") %>%
    dplyr::mutate(measure = factor(.data$measure, levels = unname(measures),
                                   labels = names(measures))) %>%
    dplyr::group_by(.data$emotion, .data$source, .data$measure) %>%
    dplyr::summarise(
      mean = mean(.data$value, na.rm = TRUE),
      se   = stats::sd(.data$value, na.rm = TRUE) / sqrt(sum(!is.na(.data$value))),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(.data$emotion, .data$mean, fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$mean - .data$se, ymax = .data$mean + .data$se),
      position = ggplot2::position_dodge(0.8), width = 0.2) +
    ggplot2::facet_wrap(~ measure, scales = "free_y") +
    ggplot2::scale_fill_manual(values = SOURCE_COLOURS) +
    ggplot2::labs(x = NULL, y = "Pupil diameter (mm)") +
    figure_theme

  save_figure(p, "figure7_pupil", width = 7, height = 4)
  p
}

## Figure 8: scatterplots of the strongest cross-measure associations.
figure_associations <- function(master, substantial, top_n = 7) {
  selected <- utils::head(substantial, top_n)
  if (nrow(selected) == 0) return(invisible(NULL))

  plot_data <- purrr::pmap_dfr(
    dplyr::select(selected, "emotion", "source", "physiological", "subjective",
                  "r_mean", "bf10"),
    function(emotion, source, physiological, subjective, r_mean, bf10) {
      cell <- dplyr::filter(master, .data$emotion == !!emotion, .data$source == !!source)
      tibble::tibble(
        panel = sprintf("%s, %s\n%s vs %s\nr = %.2f, BF10 = %.1f",
                        stringr::str_to_title(as.character(emotion)), source,
                        physiological, subjective, r_mean, bf10),
        x = cell[[physiological]],
        y = cell[[subjective]]
      )
    })

  plot_data$panel <- factor(plot_data$panel, levels = unique(plot_data$panel))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_point(size = 2, alpha = 0.75, colour = "#2C3E50", na.rm = TRUE) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = "#C0392B",
                         fill = "grey80", na.rm = TRUE) +
    ggplot2::facet_wrap(~ panel, scales = "free", ncol = 3) +
    ggplot2::labs(x = "Physiological index", y = "Rating") +
    figure_theme +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7.5, face = "plain"))

  save_figure(p, "figure8_associations", width = 10, height = 7)
  p
}
