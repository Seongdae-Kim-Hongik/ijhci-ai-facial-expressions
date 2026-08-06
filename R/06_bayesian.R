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
    bf > 100    ~ "decisive",
    bf > 30     ~ "very strong",
    bf > 10     ~ "strong",
    bf > 3      ~ "moderate",
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

## Expected number of pairings reaching the evidence threshold when every
## association is truly null, obtained by simulation at each observed n.
expected_yield_under_null <- function(sample_sizes, n_sim = 2000) {
  rates <- vapply(sort(unique(sample_sizes)), function(n) {
    hits <- vapply(seq_len(n_sim), function(i) {
      bf <- BayesFactor::correlationBF(stats::rnorm(n), stats::rnorm(n))
      as.numeric(BayesFactor::extractBF(bf)$bf) >= BF_THRESHOLD
    }, logical(1))
    mean(hits)
  }, numeric(1))

  names(rates) <- as.character(sort(unique(sample_sizes)))
  sum(rates[as.character(sample_sizes)])
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
  ## physiological index, some pairings are expected to exceed the threshold by
  ## chance alone. The expected number is obtained by simulating uncorrelated
  ## data at each observed sample size, which is the only way to get the rate
  ## right: it depends on n and on the prior, and is nothing like 1 / BF.
  expected <- expected_yield_under_null(res$n)

  benchmark <- tibble::tibble(
    n_pairings           = nrow(res),
    threshold            = BF_THRESHOLD,
    n_substantial        = nrow(substantial),
    n_expected_by_chance = expected,
    ratio_observed_to_expected = nrow(substantial) / expected,
    n_null_support       = nrow(null_support)
  )

  write_table(res, "bayesian_all_pairings")
  write_table(substantial, "table4_bayesian_substantial")
  write_table(benchmark, "table4b_bayesian_yield_benchmark")

  message(sprintf("Bayesian correlations: %d pairings; %d with BF10 >= %g (about %.1f expected if all associations were null, ratio %.1f); %d support the null (BF10 < 1/%g).",
                  nrow(res), nrow(substantial), BF_THRESHOLD,
                  expected, nrow(substantial) / expected,
                  nrow(null_support), BF_THRESHOLD))

  list(all = res, substantial = substantial, benchmark = benchmark)
}
