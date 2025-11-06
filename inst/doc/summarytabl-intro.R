## ----setup--------------------------------------------------------------------
library(summarytabl)

## -----------------------------------------------------------------------------
cat_tbl(data = nlsy, var = "race")

## -----------------------------------------------------------------------------
cat_tbl(data = nlsy, 
        var = "race",
        ignore = "Hispanic",
        na.rm = TRUE)

## -----------------------------------------------------------------------------
nlsy_cross_tab <- 
  nlsy |>
  dplyr::select(c(race, bthwht)) |>
  dplyr::mutate(bthwht = ifelse(bthwht == 0, "regular_bithweight", "low_birthweight")) 

cat_group_tbl(data = nlsy_cross_tab,
              row_var = "race",
              col_var = "bthwht")

## -----------------------------------------------------------------------------
cat_group_tbl(data = nlsy_cross_tab,
              row_var = "race",
              col_var = "bthwht",
              pivot = "wider")

## -----------------------------------------------------------------------------
# Default: percentages across the full table sum to one
cat_group_tbl(data = nlsy_cross_tab,
              row_var = "race",
              col_var = "bthwht",
              pivot = "wider",
              only = "percent")

# Rowwise: percentages sum to one across columns within each row
cat_group_tbl(data = nlsy_cross_tab,
              row_var = "race",
              col_var = "bthwht",
              margins = "rows",
              pivot = "wider",
              only = "percent")

# Columnwise: percentages within each column sum to one
cat_group_tbl(data = nlsy_cross_tab,
              row_var = "race",
              col_var = "bthwht",
              margins = "columns",
              pivot = "wider",
              only = "percent")

## -----------------------------------------------------------------------------
cat_group_tbl(data = nlsy_cross_tab,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE,
              ignore = c(race = "Non-Black,Non-Hispanic"))

## -----------------------------------------------------------------------------
cat_group_tbl(data = nlsy_cross_tab,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE,
              ignore = list(race = c("Non-Black,Non-Hispanic", "Hispanic")))

## -----------------------------------------------------------------------------
names(depressive)

## -----------------------------------------------------------------------------
select_tbl(data = depressive, var_stem = "dep")

## -----------------------------------------------------------------------------
select_tbl(data = depressive, 
           var_stem = c("dep_1", "dep_4", "dep_6"),
           var_input = "name")

## -----------------------------------------------------------------------------
select_tbl(data = depressive, 
           var_stem = "dep",
           na_removal = "pairwise")

## -----------------------------------------------------------------------------
select_tbl(data = depressive, 
           var_stem = "dep",
           na_removal = "pairwise",
           pivot = "wider")

## -----------------------------------------------------------------------------
dep_recoded <- 
  depressive |>
  dplyr::mutate(
    race = dplyr::case_match(.x = race,
                             1 ~ "Hispanic", 
                             2 ~ "Black", 
                             3 ~ "Non-Black/Non-Hispanic",
                             .default = NA)
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::starts_with("dep"),
      .fns = ~ dplyr::case_when(.x == 1 ~ "often", 
                                .x == 2 ~ "sometimes", 
                                .x == 3 ~ "hardly ever")
    ))

## -----------------------------------------------------------------------------
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race")

## -----------------------------------------------------------------------------
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 na_removal = "pairwise",
                 pivot = "wider")

## -----------------------------------------------------------------------------
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 na_removal = "pairwise",
                 pivot = "wider",
                 ignore = c(dep = "often", race = "Non-Black/Non-Hispanic"))

## -----------------------------------------------------------------------------
# Default: percentages across each variable sum to one
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 na_removal = "pairwise",
                 pivot = "wider")

# Rowwise: for each value of the variable, the percentages 
# across all levels of the grouping variable sum to one
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 margins = "rows",
                 na_removal = "pairwise",
                 pivot = "wider")

# Columnwise: for each level of the grouping variable, 
# the percentages across all values of the variable sum 
# to one.
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 margins = "columns",
                 na_removal = "pairwise",
                 pivot = "wider")

## -----------------------------------------------------------------------------
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern")

## -----------------------------------------------------------------------------
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave")

## -----------------------------------------------------------------------------
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave",
                 var_labels = c(
                   belong_belongStem_w1 = "I feel like I belong in STEM (wave 1)",
                   belong_belongStem_w2 = "I feel like I belong in STEM (wave 2)"
                 ))

## -----------------------------------------------------------------------------
# Default: counts and percentages
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave")

# Counts only
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave",
                 only = "count")

# Percentages only
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave",
                 only = "percent")

## -----------------------------------------------------------------------------
mean_tbl(data = sdoh, var_stem = "HHC_PCT")

## -----------------------------------------------------------------------------
mean_tbl(
  data = sdoh,
  var_stem = c("HHC_PCT_HHA_PHYS_THERAPY",
               "HHC_PCT_HHA_OCC_THERAPY",
               "HHC_PCT_HHA_SPEECH"),
  var_input = "name"
)

## -----------------------------------------------------------------------------
# Default listwise removal
mean_tbl(data = sdoh, var_stem = "HHC_PCT")

# Pairwise removal
mean_tbl(data = sdoh, 
         var_stem = "HHC_PCT",
         na_removal = "pairwise")

## -----------------------------------------------------------------------------
mean_tbl(data = sdoh, 
         var_stem = "HHC_PCT",
         na_removal = "pairwise",
         var_labels = c(
           HHC_PCT_HHA_NURSING="% agencies offering nursing care services",
           HHC_PCT_HHA_PHYS_THERAPY="% agencies offering physical therapy services",
           HHC_PCT_HHA_OCC_THERAPY="% agencies offering occupational therapy services",
           HHC_PCT_HHA_SPEECH="% agencies offering speech pathology services",
           HHC_PCT_HHA_MEDICAL="% agencies offering medical social services",
           HHC_PCT_HHA_AIDE="% agencies offering home health aide services"
         ))

## -----------------------------------------------------------------------------
mean_group_tbl(data = sdoh, 
               var_stem = "HHC_PCT",
               group = "REGION",
               group_type = "variable")

## -----------------------------------------------------------------------------
# Default listwise removal
mean_group_tbl(data = sdoh, 
               var_stem = "HHC_PCT",
               group = "REGION",
               ignore = c(HHC_PCT = 0, REGION = "Northeast"))

# Pairwise removal
mean_group_tbl(data = sdoh, 
               var_stem = "HHC_PCT",
               group = "REGION",
               na_removal = "pairwise",
               ignore = c(HHC_PCT = 0, REGION = "Northeast"))

# Pairwise removal excluding several values from the same stem 
# or group variable.
mean_group_tbl(data = sdoh, 
               var_stem = "HHC_PCT",
               group = "REGION",
               na_removal = "pairwise",
               ignore = list(HHC_PCT = 0, REGION = c("Northeast", "South")))

## -----------------------------------------------------------------------------
set.seed(0803)
symptoms_data <-
  data.frame(
    symptoms_t1 = sample(c(0:10, -999), replace = TRUE, size = 50),
    symptoms_t2 = sample(c(NA, 0:10, -999), replace = TRUE, size = 50),
    symptoms_t3 = sample(c(NA, 0:10, -999), replace = TRUE, size = 50)
  )

mean_group_tbl(data = symptoms_data, 
               var_stem = "symptoms",
               group = "_t\\d",
               group_type = "pattern",
               ignore = c(symptoms = -999))

## -----------------------------------------------------------------------------
mean_group_tbl(data = symptoms_data, 
               var_stem = "symptoms",
               group = "_t\\d",
               group_type = "pattern",
               group_name = "time_point",
               ignore = c(symptoms = -999), 
               var_labels = c(symptoms_t1 = "# of symptoms at baseline",
                              symptoms_t2 = "# of symptoms at 6 months follow up",
                              symptoms_t3 = "# of symptoms at one-year follow up"))

## -----------------------------------------------------------------------------
# Default: all summary statistics returned
# (mean, sd, min, max, nobs)
mean_group_tbl(data = symptoms_data, 
               var_stem = "symptoms",
               group = "_t\\d",
               group_type = "pattern",
               group_name = "time_point",
               ignore = c(symptoms = -999))

# Means and non-missing observations only
mean_group_tbl(data = symptoms_data, 
               var_stem = "symptoms",
               group = "_t\\d",
               group_type = "pattern",
               group_name = "time_point",
               ignore = c(symptoms = -999),
               only = c("mean", "nobs"))

# Means and standard deviations only
mean_group_tbl(data = symptoms_data, 
               var_stem = "symptoms",
               group = "_t\\d",
               group_type = "pattern",
               group_name = "time_point",
               ignore = c(symptoms = -999),
               only = c("mean", "sd"))

