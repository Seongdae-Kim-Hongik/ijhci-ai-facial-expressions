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
install.packages(c("readxl", "haven", "dplyr", "tidyr", "stringr", "purrr",
                   "tibble", "lme4", "lmerTest", "emmeans", "car",
                   "BayesFactor", "ggplot2"))
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
| `R/00_setup.R` | Packages, paths, study constants, shared contrast helpers |
| `R/01_load_data.R` | Readers that turn each raw export into a long-format table |
| `R/02_models.R` | Repeated-measures ANOVA, effect sizes, sensitivity checks, post-hoc contrasts |
| `R/03_questionnaire.R` | Intended-label agreement, empathy and perceived discomfort |
| `R/04_fnirs.R` | HbO by Brodmann-based region of interest |
| `R/05_eyetracking.R` | Tobii Pro Lab metrics |
| `R/06_bayesian.R` | Exploratory Bayesian correlations across measurement channels |
| `R/07_figures.R` | Figures 5, 7 and 8 |
| `run_all.R` | Entry point that runs the pipeline end to end |

## Statistical approach

### Primary analysis

The design crosses intended emotion category (7 levels) with stimulus source
(2 levels), fully within participant. Each hypothesis states that the source
difference *varies across categories*, which is the Emotion × Source
interaction, so the primary analysis is a two-way repeated-measures ANOVA
per dependent measure:

```r
aov(DV ~ emotion * source + Error(participant / (emotion * source)))
```

Every effect is tested against its own within-participant error stratum.
Participants missing any design cell are excluded from that measure, so the
number of participants is reported per measure. Effect size is partial eta
squared, computed as `F * df1 / (F * df1 + df2)`.

### Multiple comparisons

The Benjamini-Hochberg false discovery rate is controlled **across the
dependent measures within a modality, separately for each effect** — one
adjustment for the emotion effect, one for the source effect, one for the
interaction. Families are the two rating scales, the eight regions of interest,
and the eye-tracking battery.

Metrics that are numerically identical to another metric are dropped before the
correction, since they contribute no independent test; the script reports which
ones. Eye-tracking metrics also have to be observed on at least 60 per cent of
trials and to vary.

### Sensitivity and post-hoc analyses

Greenhouse-Geisser corrected p values are reported for the effects involving the
seven-level emotion factor. Source contrasts within each emotion category are
estimated post hoc from the corresponding mixed model with `emmeans` and
Bonferroni adjustment, and are computed only for measures whose source or
interaction effect survives correction.

Normality is not tested and no nonparametric fallback is used; the
repeated-measures ANOVA with the Greenhouse-Geisser check is the reported
analysis.

### Eye-tracking measures

`Duration_of_interval` and `Start_of_interval` are excluded. They describe the
length and the recording-clock position of the presentation window rather than
any response of the participant, and because stimulus order was fixed within a
block, `Start_of_interval` separates the two sources almost perfectly. A
pre-specified core set of metrics is reported in the main table and the full
battery as supplementary material, with the correction computed over the full
battery.

### Exploratory Bayesian correlations

Bayesian Pearson correlations between the subjective ratings and the
physiological indices use the default Jeffreys-Zellner-Siow prior with 5,000
posterior draws, computed separately for each source × category cell. The
posterior median correlation is reported with a 95 per cent credible interval,
and BF₁₀ of 3 or greater is treated as substantial evidence.

Because every cell is screened against every index, the yield is reported
against a chance benchmark: the expected number of pairings reaching the
threshold when all associations are null, obtained by simulating uncorrelated
data at each observed sample size. Pairings with BF₁₀ below 1/3, which support
the null, are counted as well. This section is exploratory.

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
