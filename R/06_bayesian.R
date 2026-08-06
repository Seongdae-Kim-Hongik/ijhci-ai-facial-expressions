## Exploratory Bayesian correlations between subjective ratings and the
## physiological indices, computed separately for each source x emotion cell
## with the default Jeffreys-Zellner-Siow prior.

SUBJECTIVE_MEASURES <- c("empathy", "discomfort")

## Oculomotor summaries carried into the cross-measure analysis. The set was
## fixed before the correlations were computed to limit the number of pairings.
BAYES_ET_MEASURES <- c("Average_pupil_diameter", "Average_eye_openness",
                       "Total_duration_of_fixations", "Number_of_fixations",
                       "Number_of_blinks", "Time_to_first_fixation")

## Reports participants that the crosswalk fails to place in all three channels.
## The joins below would otherwise drop them without any indication.
report_coverage <- function(q, fnirs, et) {
  unmapped <- sort(unique(q$questionnaire_name[is.na(q$device_key)]))
  if (length(unmapped) > 0) {
    warning("Questionnaire respondents absent from the crosswalk (excluded from ",
            "the cross-measure analysis): ", paste(unmapped, collapse = ", "),
            call. = FALSE)
  }

  mapped   <- stats::na.omit(unique(q$device_key))
  no_fnirs <- setdiff(mapped, unique(fnirs$device_key))
  no_et    <- setdiff(mapped, unique(et$device_key))
  orphans  <- setdiff(union(unique(fnirs$device_key), unique(et$device_key)), mapped)

  if (length(no_fnirs) > 0) {
    warning("Crosswalk device labels with no fNIRS record: ",
            paste(no_fnirs, collapse = ", "), call. = FALSE)
  }
  if (length(no_et) > 0) {
    warning("Crosswalk device labels with no eye-tracking record: ",
            paste(no_et, collapse = ", "), call. = FALSE)
  }
  if (length(orphans) > 0) {
    warning("Recorded device labels absent from the crosswalk: ",
            paste(orphans, collapse = ", "), call. = FALSE)
  }
  invisible(NULL)
}

build_master <- function(q, fnirs, et) {
  report_coverage(q, fnirs, et)

  survey <- q %>%
    dplyr::filter(!is.na(.data$device_key)) %>%
    dplyr::select("device_key", "emotion", "source",
                  dplyr::all_of(SUBJECTIVE_MEASURES))

  eye <- dplyr::select(et, "device_key", "emotion", "source",
                       dplyr::any_of(BAYES_ET_MEASURES))

  master <- survey %>%
    dplyr::inner_join(fnirs, by = c("device_key", "emotion", "source")) %>%
    dplyr::inner_join(eye, by = c("device_key", "emotion", "source"))

  message(sprintf("Cross-measure dataset: %d rows, %d participants.",
                  nrow(master), dplyr::n_distinct(master$device_key)))
  master
}

## Evidence categories, including the categories favouring the null so that
## null evidence can be counted rather than silently discarded.
interpret_bf <- function(bf) {
  dplyr::case_when(
    bf >= 100   ~ "decisive",
    bf >= 30    ~ "very strong",
    bf >= 10    ~ "strong",
    bf >= BF_THRESHOLD ~ "moderate",
    bf > 1      ~ "weak",
    bf > 1 / 3  ~ "weak (H0)",
    bf > 1 / 10 ~ "moderate (H0)",
    bf > 1 / 30 ~ "strong (H0)",
    TRUE        ~ "very strong (H0)"
  )
}

bayes_correlation <- function(x, y, iterations = MCMC_ITER) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 6) return(NULL)

  bf <- BayesFactor::correlationBF(x[ok], y[ok])
  ## posterior() reports the sampler acceptance rate on stdout; it is suppressed
  ## so that the console output stays readable.
  invisible(utils::capture.output(
    draws <- BayesFactor::posterior(bf, iterations = iterations, progress = FALSE)
  ))
  rho <- draws[, "rho"]

  tibble::tibble(
    n        = sum(ok),
    r_median = stats::median(rho),
    r_mean   = mean(rho),
    ci_lower = unname(stats::quantile(rho, 0.025)),
    ci_upper = unname(stats::quantile(rho, 0.975)),
    bf10     = as.numeric(BayesFactor::extractBF(bf)$bf)
  )
}

## Null distribution of the number of pairings reaching the evidence threshold.
##
## Permuting the participant labels of the two subjective ratings within each
## source x category cell removes any true rating-physiology association while
## leaving everything else intact: the physiological covariance structure, the
## reuse of the same participants across all fourteen cells, the missing-data
## pattern and the sample size of every cell. Simulating independent pairs
## instead would recover the same expected count -- expectation is linear, so
## dependence does not bias it -- but it badly understates the spread, and the
## spread is what decides whether an observed yield is remarkable.
null_yield_distribution <- function(master, physiological, subjective,
                                    n_perm = 500, seed = 42) {
  set.seed(seed)

  cells <- master %>%
    dplyr::group_by(.data$emotion, .data$source) %>%
    dplyr::group_split()

  vapply(seq_len(n_perm), function(i) {
    total <- 0L
    for (cell in cells) {
      shuffled <- cell
      order_i <- sample(nrow(cell))
      for (sv in subjective) shuffled[[sv]] <- cell[[sv]][order_i]

      for (pv in physiological) {
        for (sv in subjective) {
          x <- shuffled[[pv]]
          y <- shuffled[[sv]]
          ok <- is.finite(x) & is.finite(y)
          if (sum(ok) < 6) next
          bf <- BayesFactor::correlationBF(x[ok], y[ok])
          if (as.numeric(BayesFactor::extractBF(bf)$bf) >= BF_THRESHOLD) {
            total <- total + 1L
          }
        }
      }
    }
    total
  }, integer(1))
}

run_bayesian <- function(master) {
  physiological <- c(intersect(FNIRS_ROIS, names(master)),
                     intersect(BAYES_ET_MEASURES, names(master)))

  grid <- tidyr::expand_grid(
    emotion   = factor(EMOTIONS, levels = EMOTIONS),
    source    = factor(SOURCES, levels = SOURCES),
    physiological = physiological,
    subjective = SUBJECTIVE_MEASURES
  )

  res <- purrr::pmap_dfr(grid, function(emotion, source, physiological, subjective) {
    cell <- dplyr::filter(master, .data$emotion == !!emotion, .data$source == !!source)
    stats <- bayes_correlation(cell[[physiological]], cell[[subjective]])
    if (is.null(stats)) return(NULL)
    dplyr::bind_cols(
      tibble::tibble(emotion = emotion, source = source,
                     physiological = physiological, subjective = subjective),
      stats
    )
  }) %>%
    dplyr::mutate(evidence = interpret_bf(.data$bf10)) %>%
    dplyr::arrange(dplyr::desc(.data$bf10))

  substantial <- dplyr::filter(res, .data$bf10 >= BF_THRESHOLD)
  null_support <- dplyr::filter(res, .data$bf10 < 1 / BF_THRESHOLD)

  ## Because every source x category cell is screened against every
  ## physiological index, some pairings reach the threshold by chance alone.
  null_counts <- null_yield_distribution(master, physiological, SUBJECTIVE_MEASURES)

  benchmark <- tibble::tibble(
    n_pairings        = nrow(res),
    threshold         = BF_THRESHOLD,
    n_substantial     = nrow(substantial),
    null_mean         = mean(null_counts),
    null_sd           = stats::sd(null_counts),
    null_lower_95     = unname(stats::quantile(null_counts, 0.025)),
    null_upper_95     = unname(stats::quantile(null_counts, 0.975)),
    p_yield           = mean(null_counts >= nrow(substantial)),
    ## The smallest attainable BF10 under this prior is a function of the cell
    ## size. Where it exceeds 1/threshold, no pairing in that cell can express
    ## evidence for the null however uncorrelated the data are.
    min_bf10_observed = min(res$bf10),
    null_evidence_attainable = min(res$bf10) < 1 / BF_THRESHOLD,
    n_null_support    = nrow(null_support)
  )

  write_table(res, "bayesian_all_pairings")
  write_table(substantial, "table4_bayesian_substantial")
  write_table(benchmark, "table4b_bayesian_yield_benchmark")

  message(sprintf("Bayesian correlations: %d pairings; %d with BF10 >= %g. Permutation null: mean %.1f, 95%% interval [%g, %g], p = %.3f.",
                  nrow(res), nrow(substantial), BF_THRESHOLD,
                  benchmark$null_mean, benchmark$null_lower_95,
                  benchmark$null_upper_95, benchmark$p_yield))
  if (!benchmark$null_evidence_attainable) {
    message(sprintf("Smallest attainable BF10 at these cell sizes is %.3f, so evidence for the null (BF10 < 1/%g) cannot arise and is not reported.",
                    benchmark$min_bf10_observed, BF_THRESHOLD))
  }

  list(all = res, substantial = substantial, benchmark = benchmark)
}
