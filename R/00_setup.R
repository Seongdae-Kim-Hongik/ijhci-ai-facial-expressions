## Evaluating AI-Generated Facial Expressions as Affective Interface Cues
## Shared configuration: packages, paths, and study constants.

suppressPackageStartupMessages({
  library(readxl)
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(car)
  library(BayesFactor)
  library(ggplot2)
})

## Input locations. Each can be overridden with an environment variable so that
## the raw files may live outside the repository.
DATA_DIR         <- Sys.getenv("IJHCI_DATA_DIR",    unset = "data")
QUESTIONNAIRE_PATH <- Sys.getenv("IJHCI_QUESTIONNAIRE", unset = file.path(DATA_DIR, "questionnaire.xlsx"))
EYETRACKING_PATH   <- Sys.getenv("IJHCI_EYETRACKING",   unset = file.path(DATA_DIR, "eyetracking.dta"))
FNIRS_DIR          <- Sys.getenv("IJHCI_FNIRS_DIR",     unset = file.path(DATA_DIR, "fnirs"))
CROSSWALK_PATH     <- Sys.getenv("IJHCI_CROSSWALK",     unset = file.path(DATA_DIR, "participant_crosswalk.csv"))

OUTPUT_DIR  <- Sys.getenv("IJHCI_OUTPUT_DIR", unset = "outputs")
TABLE_DIR   <- file.path(OUTPUT_DIR, "tables")
FIGURE_DIR  <- file.path(OUTPUT_DIR, "figures")

for (d in c(OUTPUT_DIR, TABLE_DIR, FIGURE_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

set.seed(42)

ALPHA          <- 0.05
BF_THRESHOLD   <- 3
MCMC_ITER      <- 5000

## Permutations for the Bayesian yield null. At 2,000 the Monte Carlo standard
## error of a p value near .09 is about .006.
PERMUTATIONS   <- 2000
P_ADJUST_METHOD <- "BH"

## A metric enters the eye-tracking family only if it is observed for at least
## this share of trials and actually varies.
ET_MIN_COVERAGE <- 0.6

## Measures correlating at or above this value are alternative summaries of the
## same underlying response and are collapsed to one representative.
REDUNDANCY_THRESHOLD <- 0.95

## Pre-specified core eye-tracking metrics for the main table; the full battery
## is reported as supplementary material.
ET_CORE_METRICS <- c("Average_pupil_diameter", "Average_wholefixation_pupil_dia",
                     "Average_eye_openness", "Total_duration_of_fixations",
                     "Average_duration_of_fixations", "Number_of_fixations",
                     "Time_to_first_fixation", "Number_of_saccades_in_AOI",
                     "Number_of_blinks")

EMOTIONS <- c("anger", "contempt", "disgust", "fear",
              "happiness", "sadness", "surprise")

SOURCES <- c("AI", "Human")

## Brodmann-based regions of interest exported by the NIRSIT Analysis Tool.
## "Left OFC" and "Right OFC" are bit-identical in every export, so the device
## reports a single combined orbitofrontal value under two column names. Only
## one is analysed; treating them as two regions would double-count one
## measurement in the correction family and in the cross-measure screen.
FNIRS_ROIS <- c("Left DLPFC", "Right DLPFC", "Left FPC", "Right FPC",
                "Left VLPFC", "Right VLPFC", "OFC")

FNIRS_DUPLICATE_ROIS <- c("Left OFC", "Right OFC")

## Forced-choice response options presented after every trial (Q1).
## Option letters follow the order used in the printed questionnaire.
ANSWER_KEY <- c(a = "happiness", b = "sadness", c = "anger", d = "contempt",
                e = "disgust",   f = "surprise", g = "fear")

## The questionnaire workbook labels emotions with abbreviated and, in two
## columns, misspelled tokens. They are mapped onto the canonical categories.
normalise_emotion <- function(x) {
  x <- tolower(trimws(x))
  dplyr::case_when(
    stringr::str_detect(x, "^(sup|sur)") ~ "surprise",
    stringr::str_detect(x, "^sad")       ~ "sadness",
    stringr::str_detect(x, "^happ")      ~ "happiness",
    stringr::str_detect(x, "^disgust")   ~ "disgust",
    stringr::str_detect(x, "^anger")     ~ "anger",
    stringr::str_detect(x, "^fear")      ~ "fear",
    stringr::str_detect(x, "^contempt")  ~ "contempt",
    TRUE ~ NA_character_
  )
}

## Mean and standard deviation formatted for the manuscript tables.
fmt_m_sd <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f ± %.", digits, "f"),
          mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
}

## Within-participant contrast of one dependent measure between the AI-generated
## and human stimulus of a single emotion category. Participants contributing
## only one of the two observations are dropped (available-case analysis).
paired_contrast <- function(df, value, id = "participant_key") {
  wide <- df %>%
    dplyr::select(dplyr::all_of(c(id, "source", value))) %>%
    tidyr::pivot_wider(names_from = "source", values_from = dplyr::all_of(value)) %>%
    tidyr::drop_na(dplyr::all_of(SOURCES))

  if (nrow(wide) < 3 || stats::sd(wide$AI - wide$Human) == 0) return(NULL)

  tt <- stats::t.test(wide$AI, wide$Human, paired = TRUE)

  tibble::tibble(
    n          = nrow(wide),
    ai_mean    = mean(wide$AI),
    ai_sd      = stats::sd(wide$AI),
    human_mean = mean(wide$Human),
    human_sd   = stats::sd(wide$Human),
    difference = unname(tt$estimate),
    ci_lower   = tt$conf.int[1],
    ci_upper   = tt$conf.int[2],
    t          = unname(tt$statistic),
    df         = unname(tt$parameter),
    p          = tt$p.value
  )
}

## Applies paired_contrast over every emotion x measure combination.
contrast_grid <- function(df, measures, id = "participant_key") {
  grid <- tidyr::expand_grid(
    emotion = factor(EMOTIONS, levels = EMOTIONS),
    measure = measures
  )

  purrr::pmap_dfr(grid, function(emotion, measure) {
    res <- paired_contrast(dplyr::filter(df, .data$emotion == !!emotion), measure, id)
    if (is.null(res)) return(NULL)
    dplyr::bind_cols(tibble::tibble(emotion = emotion, measure = measure), res)
  })
}

## Columns that are numerically identical to an earlier column contribute no
## independent test and are dropped before any correction is applied.
drop_duplicate_columns <- function(df, columns, label = "") {
  keep <- character(0)
  dropped <- character(0)
  for (m in columns) {
    if (any(vapply(keep, function(k) identical(df[[m]], df[[k]]), logical(1)))) {
      dropped <- c(dropped, m)
    } else {
      keep <- c(keep, m)
    }
  }
  if (length(dropped)) {
    message(label, "dropped ", length(dropped),
            " measure(s) identical to another measure: ",
            paste(dropped, collapse = ", "))
  }
  keep
}

## Measures correlating at or above the threshold are alternative summaries of
## the same response and are grouped into blocks, of which one representative is
## kept. Complete-linkage clustering at a height of 1 - threshold is used so
## that every pair inside a block clears the threshold and the grouping does not
## depend on the order the columns happen to arrive in.
collapse_redundant <- function(df, columns, threshold = REDUNDANCY_THRESHOLD,
                               label = "") {
  if (length(columns) < 2) return(list(keep = columns, blocks = list()))

  m <- as.matrix(df[, columns, drop = FALSE])
  storage.mode(m) <- "double"
  cm <- suppressWarnings(abs(stats::cor(m, use = "pairwise.complete.obs")))
  cm[!is.finite(cm)] <- 0

  clusters <- stats::cutree(
    stats::hclust(stats::as.dist(1 - cm), method = "complete"),
    h = 1 - threshold)

  keep <- character(0)
  blocks <- list()
  for (cl in unique(clusters[columns])) {
    members <- columns[clusters[columns] == cl]
    keep <- c(keep, members[1])
    if (length(members) > 1) blocks[[members[1]]] <- members[-1]
  }

  for (h in names(blocks)) {
    message(label, h, " represents ", paste(blocks[[h]], collapse = ", "),
            " (|r| >= ", threshold, ")")
  }
  list(keep = columns[columns %in% keep], blocks = blocks)
}

write_table <- function(x, name) {
  path <- file.path(TABLE_DIR, paste0(name, ".csv"))
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}
