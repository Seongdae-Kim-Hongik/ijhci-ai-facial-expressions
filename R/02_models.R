## Primary analysis: two-way repeated-measures ANOVA.
##
## The design crosses intended emotion category (7 levels) with stimulus source
## (2 levels) fully within participant. Each hypothesis states that the source
## difference *varies across categories*, which is the Emotion x Source
## interaction; separate per-category contrasts cannot test it. The omnibus
## model is therefore the primary analysis and the per-category source contrasts
## are post hoc.
##
## Every effect is tested against its own within-participant error stratum.
## Participants missing any design cell are dropped from that measure, so the
## number of participants is reported per measure.

## Participants contributing exactly one observation to every emotion x source
## cell. Counting rows alone would let a participant with a duplicated cell and
## a missing cell pass, so the cells themselves are checked.
complete_participants <- function(d, id = "participant_key") {
  required <- length(EMOTIONS) * length(SOURCES)

  observed <- d %>%
    dplyr::filter(is.finite(.data$DV)) %>%
    dplyr::count(.data[[id]], .data$emotion, .data$source, name = "per_cell")

  if (any(observed$per_cell > 1)) {
    warning("More than one observation in a design cell for: ",
            paste(unique(observed[[1]][observed$per_cell > 1]), collapse = ", "),
            call. = FALSE)
  }

  observed %>%
    dplyr::filter(.data$per_cell == 1) %>%
    dplyr::count(.data[[id]], name = "cells") %>%
    dplyr::filter(.data$cells == required) %>%
    dplyr::pull(1)
}

## Pulls one term out of the stratified summary of an aov fit.
.aov_terms <- function(fit) {
  strata <- summary(fit)
  purrr::map_dfr(strata, function(s) {
    tab <- as.data.frame(s[[1]])
    tab$term <- trimws(rownames(tab))
    residual_df <- tab$Df[tab$term == "Residuals"]
    residual_ms <- tab$`Mean Sq`[tab$term == "Residuals"]
    tab %>%
      dplyr::filter(.data$term != "Residuals", !is.na(.data$`Pr(>F)`)) %>%
      dplyr::transmute(
        term = .data$term,
        df1  = .data$Df,
        df2  = if (length(residual_df)) residual_df else NA_real_,
        ms_error = if (length(residual_ms)) residual_ms else NA_real_,
        F    = .data$`F value`,
        p    = .data$`Pr(>F)`
      )
  }) %>%
    dplyr::mutate(
      ss_effect = .data$F * .data$df1 * .data$ms_error,
      ss_error  = .data$df2 * .data$ms_error,
      petasq    = (.data$F * .data$df1) / (.data$F * .data$df1 + .data$df2)
    ) %>%
    ## In a fully within-participant design every error stratum is a source of
    ## measurement variability, so generalized eta squared divides the effect by
    ## the effect plus all of them. It is the effect size that is comparable
    ## across designs; partial eta squared is retained for continuity.
    dplyr::mutate(getasq = .data$ss_effect / (.data$ss_effect + sum(unique(.data$ss_error))))
}

rm_anova_omnibus <- function(data, dv, id = "participant_key") {
  d <- data
  d$DV <- suppressWarnings(as.numeric(d[[dv]]))
  d$.id <- as.character(d[[id]])

  keep <- complete_participants(d, ".id")
  d <- d %>%
    dplyr::filter(.data$.id %in% keep) %>%
    dplyr::mutate(.id = factor(.data$.id),
                  emotion = droplevels(factor(.data$emotion)),
                  source  = droplevels(factor(.data$source)))

  if (dplyr::n_distinct(d$.id) < 3 ||
      nlevels(d$emotion) < 2 || nlevels(d$source) < 2 ||
      stats::sd(d$DV) == 0) {
    return(NULL)
  }

  fit <- stats::aov(DV ~ emotion * source + Error(.id / (emotion * source)), data = d)

  list(terms = .aov_terms(fit), n_participants = dplyr::n_distinct(d$.id), data = d)
}

.pick <- function(tbl, term, column) {
  v <- tbl[[column]][tbl$term == term]
  if (length(v) == 0) NA_real_ else v[1]
}

## Runs the omnibus model over a family of measures and controls the false
## discovery rate across the measures separately for each effect, which is the
## family used throughout.
omnibus_family <- function(df, measures, id = "participant_key") {
  res <- purrr::map_dfr(measures, function(m) {
    fit <- rm_anova_omnibus(df, m, id)
    if (is.null(fit)) return(NULL)
    tt <- fit$terms
    tibble::tibble(
      measure        = m,
      n_participants = fit$n_participants,
      df1_emotion    = .pick(tt, "emotion", "df1"),
      df2_emotion    = .pick(tt, "emotion", "df2"),
      F_emotion      = .pick(tt, "emotion", "F"),
      p_emotion      = .pick(tt, "emotion", "p"),
      petasq_emotion = .pick(tt, "emotion", "petasq"),
      getasq_emotion = .pick(tt, "emotion", "getasq"),
      df1_source     = .pick(tt, "source", "df1"),
      df2_source     = .pick(tt, "source", "df2"),
      F_source       = .pick(tt, "source", "F"),
      p_source       = .pick(tt, "source", "p"),
      petasq_source  = .pick(tt, "source", "petasq"),
      getasq_source  = .pick(tt, "source", "getasq"),
      df1_interaction = .pick(tt, "emotion:source", "df1"),
      df2_interaction = .pick(tt, "emotion:source", "df2"),
      F_interaction  = .pick(tt, "emotion:source", "F"),
      p_interaction  = .pick(tt, "emotion:source", "p"),
      petasq_interaction = .pick(tt, "emotion:source", "petasq"),
      getasq_interaction = .pick(tt, "emotion:source", "getasq")
    )
  })

  if (nrow(res) == 0) return(res)

  res %>%
    dplyr::mutate(
      pFDR_emotion     = stats::p.adjust(.data$p_emotion, P_ADJUST_METHOD),
      pFDR_source      = stats::p.adjust(.data$p_source, P_ADJUST_METHOD),
      pFDR_interaction = stats::p.adjust(.data$p_interaction, P_ADJUST_METHOD)
    ) %>%
    dplyr::arrange(.data$p_interaction)
}

## Greenhouse-Geisser sensitivity check for the effects involving the
## seven-level emotion factor, reported alongside the uncorrected result.
gg_sensitivity <- function(df, measures, id = "participant_key") {
  purrr::map_dfr(measures, function(m) {
    d <- df
    d$DV <- suppressWarnings(as.numeric(d[[m]]))
    d$.id <- as.character(d[[id]])
    keep <- complete_participants(d, ".id")
    d <- dplyr::filter(d, .data$.id %in% keep)
    if (dplyr::n_distinct(d$.id) < 3) return(NULL)

    wide <- d %>%
      dplyr::mutate(cell = paste(.data$emotion, .data$source, sep = "_")) %>%
      dplyr::select(".id", "cell", "DV") %>%
      tidyr::pivot_wider(names_from = "cell", values_from = "DV") %>%
      dplyr::arrange(.data$.id)

    y <- as.matrix(dplyr::select(wide, -".id"))
    ## Epsilon is estimated on the covariance of the effect contrasts, so it
    ## needs n - 1 to exceed the effect degrees of freedom, not the cell count.
    if (anyNA(y) || nrow(y) < 3) return(NULL)

    idata <- tibble::tibble(cell = colnames(y)) %>%
      tidyr::separate("cell", into = c("emotion", "source"), sep = "_") %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), factor)) %>%
      as.data.frame()

    fit <- stats::lm(y ~ 1)
    av <- try(car::Anova(fit, idata = idata, idesign = ~ emotion * source, type = 3),
              silent = TRUE)
    if (inherits(av, "try-error")) return(NULL)

    s <- suppressWarnings(summary(av, multivariate = FALSE))
    gg <- as.data.frame(s$pval.adjustments)
    gg$term <- trimws(rownames(gg))
    gg %>%
      dplyr::transmute(
        measure = m,
        term    = .data$term,
        gg_epsilon = .data$`GG eps`,
        p_gg       = .data$`Pr(>F[GG])`
      )
  })
}

## Source contrast within each emotion category, restricted to the same
## complete-case sample as the omnibus model.
##
## These are paired t tests rather than contrasts from a random-intercept mixed
## model. A random intercept alone imposes compound symmetry on all fourteen
## cells, which pools one error term across categories and counts the residual
## degrees of freedom as though the observations were independent; with twenty
## participants that inflates df from 19 to 247 and returns p values several
## orders of magnitude too small. It would also contradict the sphericity
## correction reported for the omnibus model. The paired test uses exactly the
## error term the omnibus interaction stratum is built from. A richer random
## structure is not an option here: with one observation per cell, a model with
## random slopes for emotion and source is saturated and unidentifiable.
##
## Bonferroni is applied across the seven emotion categories. Requesting it from
## emmeans with `~ source | emotion` would silently do nothing, because each
## by-group holds a single contrast and the adjustment is applied within groups.
posthoc_source_by_emotion <- function(df, measures, id = "participant_key") {
  purrr::map_dfr(measures, function(m) {
    d <- df
    d$DV <- suppressWarnings(as.numeric(d[[m]]))
    d$.id <- as.character(d[[id]])

    keep <- complete_participants(d, ".id")
    d <- dplyr::filter(d, .data$.id %in% keep)
    if (length(keep) < 3) return(NULL)

    res <- contrast_grid(d, measures = "DV", id = ".id")
    if (nrow(res) == 0) return(NULL)

    res %>%
      dplyr::mutate(
        measure  = m,
        contrast = "AI - Human",
        p_bonf   = pmin(1, .data$p * length(EMOTIONS))
      ) %>%
      dplyr::select("measure", "emotion", "contrast", "n",
                    "ai_mean", "ai_sd", "human_mean", "human_sd",
                    "difference", "ci_lower", "ci_upper",
                    "t", "df", "p", "p_bonf")
  })
}

## Measures for which the source main effect or the interaction survives
## correction, i.e. those for which a per-category breakdown is warranted.
measures_warranting_posthoc <- function(omnibus, alpha = ALPHA) {
  omnibus$measure[
    (!is.na(omnibus$pFDR_source) & omnibus$pFDR_source < alpha) |
    (!is.na(omnibus$pFDR_interaction) & omnibus$pFDR_interaction < alpha)
  ]
}
