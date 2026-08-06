# Evaluating AI-Generated Facial Expressions as Affective Interface Cues

Analysis code for the study *Evaluating AI-Generated Facial Expressions as
Affective Interface Cues: A Multimodal Comparison with Human Expressions*, in
which questionnaire, functional near-infrared spectroscopy (fNIRS) and
eye-tracking responses to selected AI-generated and human facial-expression
stimuli were compared across seven intended emotion categories.

The repository reproduces every table and figure in the manuscript from the raw
exports of the three recording systems.

## Requirements

R 4.4 or later with the following packages:

```r
install.packages(c("readxl", "haven", "dplyr", "tidyr", "stringr",
                   "purrr", "tibble", "BayesFactor", "ggplot2"))
```

## Running the analysis

Place the raw files as described in [`data/README.md`](data/README.md), then from
the repository root:

```bash
Rscript run_all.R
```

Tables are written to `outputs/tables/` and figures to `outputs/figures/`, along
with `outputs/sessionInfo.txt`. Input locations can be overridden without editing
any script by setting `IJHCI_QUESTIONNAIRE`, `IJHCI_EYETRACKING`,
`IJHCI_FNIRS_DIR`, `IJHCI_CROSSWALK` and `IJHCI_OUTPUT_DIR`.

## Repository layout

| Path | Contents |
| --- | --- |
| `R/00_setup.R` | Packages, paths, study constants, shared paired-contrast helpers |
| `R/01_load_data.R` | Readers that turn each raw export into a long-format table |
| `R/02_questionnaire.R` | Intended-label agreement, empathy and perceived discomfort |
| `R/03_fnirs.R` | HbO contrasts by Brodmann-based region of interest |
| `R/04_eyetracking.R` | Contrasts across the Tobii Pro Lab metrics |
| `R/05_bayesian.R` | Exploratory Bayesian correlations across measurement channels |
| `R/06_figures.R` | Figures 5, 7 and 8 |
| `run_all.R` | Entry point that runs the pipeline end to end |

## Statistical approach

All source comparisons are **within participant**, so every contrast is a
paired-samples *t* test of the AI-generated against the human stimulus of the
same intended emotion category. Participants missing one side of a pair are
dropped from that contrast only (available-case analysis), which is why the
degrees of freedom vary between measures.

The Benjamini-Hochberg false discovery rate procedure is applied to a different
family in each channel:

- **Questionnaire** — across the seven emotion categories, separately for the
  empathy and the perceived-discomfort scale.
- **fNIRS** — across the eight regions of interest within each emotion category.
- **Eye-tracking** — across the whole set of metric x category comparisons.

Both the unadjusted (`p`) and the adjusted (`p_fdr`) values are written to every
output table so that either can be reported.

`Duration_of_interval` and `Start_of_interval` are excluded from the
eye-tracking measures. They describe the length and the recording-clock position
of the presentation window rather than any response of the participant, and
because stimulus order was fixed within a block, `Start_of_interval` separates
the two sources almost perfectly. Leaving them in adds seven artefactual
comparisons to the family and distorts the correction.

Exploratory Bayesian Pearson correlations use the default Jeffreys-Zellner-Siow
prior with 5,000 posterior draws, computed separately for each source x category
cell. BF10 of 3 or greater is treated as substantial evidence.

## Data availability

No participant data are included. The questionnaire workbook and the participant
crosswalk contain directly identifying information, and the human face
photographs are licensed from a third party. De-identified data may be requested
from the corresponding author subject to the approval of the responsible
Institutional Review Board.

## Citation

If you use this code, please cite both the article and the archived release; see
`CITATION.cff`.

## License

Code is released under the MIT License (`LICENSE`).
