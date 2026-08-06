## Readers for the three measurement channels.
## Each reader returns a long-format table keyed by participant, emotion and
## stimulus source, so that the analysis scripts never touch the raw layouts.

## Crosswalk between the name used in the questionnaire workbook and the
## participant label written by the fNIRS and eye-tracking software. The file is
## not distributed with the code because it contains identifying information;
## see data/README.md for the expected columns.
load_crosswalk <- function(path = CROSSWALK_PATH) {
  if (!file.exists(path)) {
    stop("Participant crosswalk not found at '", path,
         "'. See data/README.md for the expected format.", call. = FALSE)
  }
  cw <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  stopifnot(all(c("questionnaire_name", "device_id", "participant_id") %in% names(cw)))
  cw$device_key <- tolower(trimws(cw$device_id))
  cw
}

load_questionnaire <- function(crosswalk,
                               path = QUESTIONNAIRE_PATH) {
  raw <- readxl::read_excel(path)
  name_col <- names(raw)[2]

  long <- raw %>%
    dplyr::rename(questionnaire_name = dplyr::all_of(name_col)) %>%
    dplyr::mutate(questionnaire_name = trimws(.data$questionnaire_name)) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("^Q[123]_"),
      names_to = "item",
      values_to = "response",
      values_transform = list(response = as.character)
    ) %>%
    dplyr::mutate(
      question = stringr::str_extract(.data$item, "^Q[123]"),
      stem     = stringr::str_remove(.data$item, "^Q[123]_"),
      source   = ifelse(stringr::str_detect(.data$stem, "_ai$"), "AI", "Human"),
      emotion  = normalise_emotion(stringr::str_remove(.data$stem, "_(ai|real)$"))
    ) %>%
    dplyr::filter(!is.na(.data$emotion))

  choice <- long %>%
    dplyr::filter(.data$question == "Q1") %>%
    dplyr::mutate(
      chosen    = unname(ANSWER_KEY[tolower(trimws(.data$response))]),
      recognised = as.integer(.data$chosen == .data$emotion)
    ) %>%
    dplyr::select("questionnaire_name", "emotion", "source", "chosen", "recognised")

  ratings <- long %>%
    dplyr::filter(.data$question %in% c("Q2", "Q3")) %>%
    dplyr::mutate(
      scale = ifelse(.data$question == "Q2", "empathy", "discomfort"),
      score = suppressWarnings(as.numeric(.data$response))
    ) %>%
    dplyr::select("questionnaire_name", "emotion", "source", "scale", "score") %>%
    tidyr::pivot_wider(names_from = "scale", values_from = "score")

  choice %>%
    dplyr::full_join(ratings, by = c("questionnaire_name", "emotion", "source")) %>%
    dplyr::left_join(
      dplyr::select(crosswalk, "questionnaire_name", "participant_id", "device_key"),
      by = "questionnaire_name"
    ) %>%
    dplyr::mutate(
      emotion = factor(.data$emotion, levels = EMOTIONS),
      source  = factor(.data$source, levels = SOURCES)
    ) %>%
    dplyr::arrange(.data$questionnaire_name, .data$emotion, .data$source)
}

load_fnirs <- function(dir = FNIRS_DIR, pattern = "^fnirs_.*\\.xlsx$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No fNIRS workbooks found in '", dir, "'.", call. = FALSE)
  }

  purrr::map_dfr(files, function(f) {
    raw <- readxl::read_excel(f)

    ## The export writes one combined orbitofrontal value into two identical
    ## columns; it is carried forward once, under a side-neutral name.
    present <- intersect(FNIRS_DUPLICATE_ROIS, names(raw))
    if (length(present) == 2 && !identical(raw[[present[1]]], raw[[present[2]]])) {
      warning("Left and Right OFC differ in '", basename(f),
              "'; the combined-column assumption no longer holds.", call. = FALSE)
    }
    if (length(present)) raw[["OFC"]] <- raw[[present[1]]]

    raw %>%
      dplyr::rename(device_id = "Subject Name") %>%
      dplyr::mutate(
        source  = ifelse(stringr::str_detect(.data$Contrast, "^AI"), "AI", "Human"),
        emotion = normalise_emotion(stringr::str_remove(.data$Contrast, "^(AI|Real)_Task_"))
      ) %>%
      dplyr::select("device_id", "emotion", "source", dplyr::any_of(FNIRS_ROIS))
  }) %>%
    dplyr::mutate(
      device_key = tolower(trimws(.data$device_id)),
      emotion    = factor(.data$emotion, levels = EMOTIONS),
      source     = factor(.data$source, levels = SOURCES),
      dplyr::across(dplyr::any_of(FNIRS_ROIS), as.numeric)
    ) %>%
    dplyr::select(-"device_id")
}

## Identifier columns in the Tobii Pro Lab export.
## Duration_of_interval and Start_of_interval are excluded as well: they record
## the length and the recording-clock position of the presentation window rather
## than a response of the participant. Because stimulus order was fixed within a
## block, Start_of_interval separates the two sources almost perfectly and would
## otherwise enter the family of comparisons as an artefact.
ET_ID_COLUMNS <- c("Media", "emotion", "group", "Recording",
                   "Participant", "AOI_at_interval_end",
                   "Duration_of_interval", "Start_of_interval")

load_eyetracking <- function(path = EYETRACKING_PATH) {
  raw <- haven::read_dta(path)

  metrics <- setdiff(names(raw), ET_ID_COLUMNS)

  raw %>%
    dplyr::mutate(
      device_key = tolower(trimws(.data$Participant)),
      emotion    = factor(normalise_emotion(.data$emotion), levels = EMOTIONS),
      source     = factor(ifelse(tolower(trimws(.data$group)) == "ai", "AI", "Human"),
                          levels = SOURCES),
      dplyr::across(dplyr::all_of(metrics), ~ as.numeric(haven::zap_labels(.x)))
    ) %>%
    dplyr::select("device_key", "emotion", "source", dplyr::all_of(metrics))
}
