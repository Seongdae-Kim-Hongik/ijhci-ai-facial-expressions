# Expected input files

None of these files are distributed with the repository. Place them here, or
point the corresponding environment variable at another location.

```
data/
├── questionnaire.xlsx              IJHCI_QUESTIONNAIRE
├── eyetracking.dta                 IJHCI_EYETRACKING
├── participant_crosswalk.csv       IJHCI_CROSSWALK
└── fnirs/                          IJHCI_FNIRS_DIR
    ├── fnirs_anger_....xlsx
    ├── fnirs_contempt_....xlsx
    ├── fnirs_disgust_....xlsx
    ├── fnirs_fear_....xlsx
    ├── fnirs_happiness_....xlsx
    ├── fnirs_sadness_....xlsx
    └── fnirs_surprise_....xlsx
```

## questionnaire.xlsx

One row per participant. The second column holds the participant identifier used
by the questionnaire. Response columns are named `Q<n>_<emotion>_<source>`, where
`<n>` is 1 for the forced-choice emotion label, 2 for the empathy rating and 3
for the perceived-discomfort rating; `<source>` is `ai` or `real`.

Q1 answers are the option letters `a`-`g`, mapped to emotion categories by
`ANSWER_KEY` in `R/00_setup.R`.

## eyetracking.dta

Interval-level Tobii Pro Lab export, one row per participant and stimulus.
Required columns: `Participant`, `emotion`, `group` (`AI` or `Real`), plus the
exported metrics. Columns listed in `ET_ID_COLUMNS` are treated as identifiers;
every other column is analysed as a dependent measure.

## fnirs/

One workbook per emotion category, as written by the NIRSIT Analysis Tool, with
an `HbO` sheet. Required columns: `Subject Name`, `Contrast` (of the form
`AI_Task_<Emotion>` or `Real_Task_<Emotion>`) and the eight Brodmann-based
regions listed in `FNIRS_ROIS`.

Files are matched by the pattern `^fnirs_.*\.xlsx$`.

## participant_crosswalk.csv

Links the questionnaire identifier to the label written by the fNIRS and
eye-tracking software. It is required only for the cross-measure Bayesian
analysis; the three single-channel analyses run without it.

| Column | Meaning |
| --- | --- |
| `questionnaire_name` | Identifier exactly as it appears in `questionnaire.xlsx` |
| `device_id` | `Subject Name` / `Participant` label in the fNIRS and eye-tracking exports |
| `participant_id` | Anonymous code used in reporting |

Matching on `device_id` is case-insensitive. See
`participant_crosswalk_TEMPLATE.csv` for the layout.

**Verify this file before running the analysis.** A single wrong row silently
pairs one participant's ratings with another participant's recordings, and the
inner joins in `build_master()` will quietly drop any participant whose name
does not match, without an error.
