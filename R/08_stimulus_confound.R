## Is the pupil interaction explained by the images rather than by the faces?
##
## Pupil diameter follows display luminance through the pupillary light reflex.
## With one exemplar per cell, a source difference in luminance that varies by
## category would reproduce the observed Emotion x Source interaction with no
## affective process involved. The test is whether the luminance difference
## between the two images of a category predicts the pupil difference for that
## category, and whether it does so in the direction the light reflex requires:
## a brighter image should give a smaller pupil.
##
## Seven categories give seven points, so this is descriptive. It is reported
## because a null confound check is far weaker evidence than a specific
## exception that survives it.

STIMULUS_PROPERTIES_PATH <- Sys.getenv(
  "IJHCI_STIMULUS_PROPERTIES",
  unset = file.path(TABLE_DIR, "tableS7_stimulus_properties.csv"))

run_stimulus_confound <- function(eyetracking_posthoc,
                                  path = STIMULUS_PROPERTIES_PATH,
                                  measure = "Average_pupil_diameter") {
  if (!file.exists(path)) {
    message("Stimulus properties not found at '", path,
            "'; run python/08_stimulus_properties.py first. Skipping.")
    return(NULL)
  }

  props <- utils::read.csv(path, fileEncoding = "UTF-8")
  image_measures <- c("mean_luminance", "rms_contrast", "edge_energy")

  differences <- props %>%
    dplyr::select("emotion", "source", dplyr::all_of(image_measures)) %>%
    tidyr::pivot_longer(dplyr::all_of(image_measures),
                        names_to = "property", values_to = "value") %>%
    tidyr::pivot_wider(names_from = "source", values_from = "value") %>%
    dplyr::mutate(image_difference = .data$AI - .data$Human) %>%
    dplyr::select("emotion", "property", "image_difference")

  pupil <- eyetracking_posthoc %>%
    dplyr::filter(.data$measure == !!measure) %>%
    dplyr::transmute(emotion = as.character(.data$emotion),
                     pupil_difference = .data$difference,
                     p_bonf = .data$p_bonf)

  paired <- dplyr::inner_join(differences, pupil, by = "emotion")

  ## A brighter AI image should give a smaller AI pupil, so a luminance
  ## difference and a pupil difference of opposite sign is what the light
  ## reflex predicts. Categories whose differences are negligible carry no
  ## information about sign, so the flag is descriptive only.
  agreement <- paired %>%
    dplyr::filter(.data$property == "mean_luminance") %>%
    dplyr::mutate(consistent_with_light_reflex =
                    sign(.data$image_difference) != sign(.data$pupil_difference)) %>%
    dplyr::select("emotion", "image_difference", "pupil_difference",
                  "consistent_with_light_reflex", "p_bonf")

  correlations <- purrr::map_dfr(image_measures, function(prop) {
    d <- dplyr::filter(paired, .data$property == prop)
    all_rows <- stats::cor.test(d$image_difference, d$pupil_difference)
    without <- dplyr::filter(d, .data$emotion != "disgust")
    excl <- stats::cor.test(without$image_difference, without$pupil_difference)
    tibble::tibble(
      property        = prop,
      n              = nrow(d),
      r              = unname(all_rows$estimate),
      ci_lower       = all_rows$conf.int[1],
      ci_upper       = all_rows$conf.int[2],
      p              = all_rows$p.value,
      n_excl_disgust = nrow(without),
      r_excl_disgust = unname(excl$estimate),
      p_excl_disgust = excl$p.value
    )
  })

  write_table(agreement, "tableS8_luminance_pupil_agreement")
  write_table(correlations, "tableS9_luminance_pupil_correlation")

  message(sprintf("Stimulus confound: luminance difference vs pupil difference r = %.2f (p = %.3f) over %d categories; r = %.2f (p = %.3f) with disgust excluded. Light-reflex direction holds in %d of %d categories.",
                  correlations$r[1], correlations$p[1], correlations$n[1],
                  correlations$r_excl_disgust[1], correlations$p_excl_disgust[1],
                  sum(agreement$consistent_with_light_reflex),
                  nrow(agreement)))

  list(agreement = agreement, correlations = correlations)
}
