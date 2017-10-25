examples_files <- data.frame(
  model = c("sir", "predator-prey"),
  description = c("An SIR epidemiological model.",
                  paste("A predator-prey model that includes vegetation growth",
                        "and grazing by the prey.")),
  gaml = c("sir.gaml", "predator_prey/models/predator_prey.gaml"),
  copy = c("/sir.gaml", "/predator_prey"),
  stringsAsFactors = FALSE
)
devtools::use_data(examples_files, internal = TRUE, overwrite = TRUE)
