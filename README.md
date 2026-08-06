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
| `R/07_figures.R` | Figures 5 and 6 |
| `R/08_stimulus_confound.R` | Whether image luminance explains the pupil result |
| `python/08_stimulus_properties.py` | Low-level properties of the stimulus images |
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
number of participants is reported per measure. Two effect sizes accompany each
effect: partial eta squared, `F * df1 / (F * df1 + df2)`, and generalized eta
squared, which divides the effect sum of squares by itself plus every error
stratum. Generalized eta squared is the comparable one in a fully
within-participant design; partial eta squared is markedly upward biased at
these sample sizes and is reported only for continuity.

For fNIRS, where the complete-case rule is costly, the omnibus result is
repeated on all available observations with a mixed model whose random terms
mirror the ANOVA error strata (`tableS5`), so a null result can be checked
against the missing-data rule.

### Multiple comparisons

The Benjamini-Hochberg false discovery rate is controlled **across the
dependent measures within a modality, separately for each effect** — one
adjustment for the emotion effect, one for the source effect, one for the
interaction. Families are the two rating scales, the seven regions of interest, and the
eye-tracking battery.

Metrics that are numerically identical to another metric are dropped before the
correction, since they contribute no independent test; the script reports which
ones. Eye-tracking metrics also have to be observed on at least 60 per cent of
trials and to vary.

### Sensitivity and post-hoc analyses

Greenhouse-Geisser corrected p values are reported for the effects involving the
seven-level emotion factor.

Source contrasts within each emotion category are paired t tests on the same
complete-case sample as the omnibus model, Bonferroni-corrected across the seven
categories, and computed only for measures whose source or interaction effect
survives correction. They are deliberately not contrasts from a random-intercept
mixed model: a random intercept alone imposes compound symmetry across all
fourteen cells, pools a single error term across categories and counts residual
degrees of freedom as though the observations were independent, which at twenty
participants inflates df from 19 to 247. It would also contradict the sphericity
correction reported for the omnibus model. A richer random structure is not
available, because with one observation per cell a model with random slopes for
emotion and source is saturated.

Note that requesting Bonferroni from `emmeans` with `~ source | emotion` would
silently do nothing: each by-group holds one contrast and the adjustment is
applied within groups.

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
against a null distribution obtained by permuting the participant labels of the
two ratings within each cell. This removes any true rating-physiology
association while preserving the physiological covariance structure, the reuse
of the same participants across cells, the missing-data pattern and every cell
size. Simulating independent pairs would recover the same expected count --
expectation is linear, so dependence does not bias it -- but it understates the
spread, and the spread is what decides whether a yield is remarkable.

The smallest BF₁₀ attainable under this prior is a function of cell size; at
these sample sizes it stays above 1/3, so evidence *for* the null cannot arise
and is not interpreted. This section is exploratory.

### Measures that are not independent

`Left OFC` and `Right OFC` are bit-identical in every fNIRS export, so the
device reports one combined orbitofrontal value under two names; it is analysed
once. Among the eye-tracking metrics, the Visit and Glance families duplicate
the fixation family when a single AOI is defined, and the fixation-linked pupil
and eye-openness summaries track their trial-level counterparts at r > .99. One
representative per correlated block enters the family and the mapping is written
to `tableS6`. This does not affect error control -- Benjamini-Hochberg remains
valid under positive dependence, and redundancy makes it conservative rather
than liberal -- but it prevents one finding from being presented as several.

### The stimulus confound

Pupil diameter follows display luminance through the pupillary light reflex.
With one exemplar per cell, a luminance difference between the two images of a
category is confounded with the source manipulation, and a luminance difference
that varies across categories reproduces an Emotion × Source interaction with no
affective process involved. `python/08_stimulus_properties.py` measures the
images and `R/08_stimulus_confound.R` tests whether the pupil differences follow
them. Run the Python step first; it writes the properties table the R step
reads.

```bash
python3 python/08_stimulus_properties.py <stimulus_dir> outputs/tables/tableS7_stimulus_properties.csv
```

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
