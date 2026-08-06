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

set.seed(20240917)

ALPHA          <- 0.05
BF_THRESHOLD   <- 3
MCMC_ITER      <- 5000
P_ADJUST_METHOD <- "BH"

EMOTIONS <- c("anger", "contempt", "disgust", "fear",
              "happiness", "sadness", "surprise")

SOURCES <- c("AI", "Human")

## Brodmann-based regions of interest exported by the NIRSIT Analysis Tool.
FNIRS_ROIS <- c("Left DLPFC", "Right DLPFC", "Left FPC", "Right FPC",
                "Left VLPFC", "Right VLPFC", "Left OFC", "Right OFC")

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

write_table <- function(x, name) {
  path <- file.path(TABLE_DIR, paste0(name, ".csv"))
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}
