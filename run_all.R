## Reproduces every table and figure reported in
## "Evaluating AI-Generated Facial Expressions as Affective Interface Cues:
##  A Multimodal Comparison with Human Expressions".
##
## Run from the repository root:  Rscript run_all.R

source("R/00_setup.R")
source("R/01_load_data.R")
source("R/02_questionnaire.R")
source("R/03_fnirs.R")
source("R/04_eyetracking.R")
source("R/05_bayesian.R")
source("R/06_figures.R")

crosswalk     <- load_crosswalk()
questionnaire <- load_questionnaire(crosswalk)
fnirs         <- load_fnirs()
eyetracking   <- load_eyetracking()

message(sprintf("Questionnaire: %d participants. fNIRS: %d participants. Eye-tracking: %d participants.",
                dplyr::n_distinct(questionnaire$questionnaire_name),
                dplyr::n_distinct(fnirs$device_key),
                dplyr::n_distinct(eyetracking$device_key)))

questionnaire_results <- run_questionnaire(questionnaire)
fnirs_results         <- run_fnirs(fnirs)
eyetracking_results   <- run_eyetracking(eyetracking)

master           <- build_master(questionnaire, fnirs, eyetracking)
bayesian_results <- run_bayesian(master)

invisible(figure_questionnaire(questionnaire, questionnaire_results$recognition))
invisible(figure_pupil(eyetracking))
invisible(figure_associations(master, bayesian_results$substantial))

writeLines(utils::capture.output(utils::sessionInfo()),
           file.path(OUTPUT_DIR, "sessionInfo.txt"))

message("Done. Tables in ", TABLE_DIR, ", figures in ", FIGURE_DIR, ".")
