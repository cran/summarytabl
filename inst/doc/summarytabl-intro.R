## ----setup--------------------------------------------------------------------
library(summarytabl)

## -----------------------------------------------------------------------------
cat_tbl(data = nlsy, var = "race")

## -----------------------------------------------------------------------------
cat_tbl(data = nlsy, 
        var = "race",
        ignore = "Black",
        na.rm = TRUE)

## -----------------------------------------------------------------------------
# Default: counts and percentages
cat_group_tbl(data = nlsy,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE)

# Counts only
cat_tbl(data = nlsy, 
        var = "race",
        ignore = "Black",
        na.rm = TRUE,
        only = "count")

# Percents only
cat_group_tbl(data = nlsy,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE,
              only = "percent")

## -----------------------------------------------------------------------------
cat_group_tbl(data = nlsy,
              row_var = "gender",
              col_var = "bthwht")

## -----------------------------------------------------------------------------
cat_group_tbl(data = nlsy,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE,
              ignore = c(race = "Non-Black,Non-Hispanic"),
              pivot = "wider")

## -----------------------------------------------------------------------------
cat_group_tbl(data = nlsy,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE,
              ignore = list(race = c("Non-Black,Non-Hispanic", "Hispanic")),
              pivot = "wider")

## -----------------------------------------------------------------------------
# Default: counts and percentages
cat_group_tbl(data = nlsy,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE)

# Counts only
cat_group_tbl(data = nlsy,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE,
              only = "count")

# Percents only
cat_group_tbl(data = nlsy,
              row_var = "race",
              col_var = "bthwht",
              na.rm.row_var = TRUE,
              only = "percent")

## -----------------------------------------------------------------------------
names(depressive)

## -----------------------------------------------------------------------------
select_tbl(data = depressive, var_stem = "dep")

## -----------------------------------------------------------------------------
# Default listwise removal, value '3' removed from data
select_tbl(data = depressive, 
           var_stem = "dep", 
           ignore = 3)

# Pairwise removal, value '3' removed from data
select_tbl(data = depressive, 
           var_stem = "dep", 
           ignore = 3,
           na_removal = "pairwise")

## -----------------------------------------------------------------------------

# Default longer format
select_tbl(data = depressive, 
           var_stem = "dep")

# Wider format
select_tbl(data = depressive, 
           var_stem = "dep",
           pivot = "wider")

## -----------------------------------------------------------------------------
select_tbl(data = depressive, 
           var_stem = "dep",
           pivot = "wider",
           var_labels = c(
             dep_1="how often child feels sad and blue",
             dep_2="how often child feels nervous, tense, or on edge",
             dep_3="how often child feels happy",
             dep_4="how often child feels bored",
             dep_5="how often child feels lonely",
             dep_6="how often child feels tired or worn out",
             dep_7="how often child feels excited about something",
             dep_8="how often child feels too busy to get everything"
           )
)

## -----------------------------------------------------------------------------
# Default: counts and percentages
select_tbl(data = depressive, 
           var_stem = "dep",
           pivot = "wider")

# Counts only
select_tbl(data = depressive, 
           var_stem = "dep",
           pivot = "wider",
           only = "count")

# Percents only
select_tbl(data = depressive, 
           var_stem = "dep",
           pivot = "wider",
           only = "percent")

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
    )
  )

# longer format
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 pivot = "longer")

# wider format
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 pivot = "wider")

## -----------------------------------------------------------------------------

# Default listwise removal: 'often' value removed from all
# dep_ variables, and 'Non-Black/Non-Hispanic' value removed
# from race variable
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 pivot = "longer",
                 ignore = c(dep = "often", race = "Non-Black/Non-Hispanic"))

# Pairwise removal: 'often' value removed from all
# dep_ variables, and 'Non-Black/Non-Hispanic' value removed
# from race variable
select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 pivot = "longer",
                 ignore = c(dep = "often", race = "Non-Black/Non-Hispanic"),
                 na_removal = "pairwise")

## -----------------------------------------------------------------------------

select_group_tbl(data = dep_recoded, 
                 var_stem = "dep",
                 group = "race",
                 pivot = "longer",
                 ignore = list(race = c("Hispanic", "Non-Black/Non-Hispanic")))

## -----------------------------------------------------------------------------
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 pivot = "longer")

## -----------------------------------------------------------------------------
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave",
                 pivot = "longer")

## -----------------------------------------------------------------------------
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave",
                 pivot = "longer",
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
                 group_name = "wave",
                 pivot = "longer",
                 only = "count")

# Counts only
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave",
                 pivot = "longer",
                 only = "count")

# Percents only
select_group_tbl(data = stem_social_psych, 
                 var_stem = "belong_belong",
                 group = "_w\\d",
                 group_type = "pattern",
                 group_name = "wave",
                 pivot = "longer",
                 only = "percent")

## -----------------------------------------------------------------------------
mean_tbl(data = social_psy_data, 
         var_stem = "belong")

## -----------------------------------------------------------------------------
mean_tbl(data = social_psy_data, 
         var_stem = "belong",
         ignore = 5)

## -----------------------------------------------------------------------------

# Default listwise removal
mean_tbl(data = social_psy_data, 
         var_stem = "belong",
         ignore = 5)

# Pairwise removal
mean_tbl(data = social_psy_data, 
         var_stem = "belong",
         na_removal = "pairwise", 
         ignore = 5)

## -----------------------------------------------------------------------------
mean_tbl(data = social_psy_data, 
         var_stem = "belong",
         na_removal = "pairwise",
         var_labels = c(
           belong_1 = "I feel like I belong at this institution",
           belong_2 = "I feel like part of the community",
           belong_3 = "I feel valued by this institution")
)

## -----------------------------------------------------------------------------
# Default: all summary statistics returned
# (mean, sd, min, max, nobs)
mean_tbl(data = social_psy_data, 
         var_stem = "belong",
         na_removal = "pairwise")

# Means and non-missing observations returned
mean_tbl(data = social_psy_data, 
         var_stem = "belong",
         na_removal = "pairwise",
         only = c("mean", "nobs"))

# Means and standard deviations  returned
mean_tbl(data = social_psy_data, 
         var_stem = "belong",
         na_removal = "pairwise",
         only = c("mean", "sd"))

## -----------------------------------------------------------------------------
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "urm",
               group_type = "variable")

## -----------------------------------------------------------------------------
# Default listwise removal
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "urm",
               ignore = c(belong_belong = 5, urm = 0)
               )

# Pairwise removal
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "urm",
               na_removal = "pairwise",
               ignore = c(belong_belong = 5, urm = 0)
               )

## -----------------------------------------------------------------------------
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "urm",
               ignore = list(belong_belong = c(4,5), urm = 0)
               )

## -----------------------------------------------------------------------------
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "_w\\d",
               group_type = "pattern")

## -----------------------------------------------------------------------------
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "_w\\d",
               group_type = "pattern",
               group_name = "wave",
               var_labels = c(
                 belong_belongStem_w1 = "I feel like I belong in computing",
                 belong_belongStem_w2 = "I feel like I belong in computing")
)

## -----------------------------------------------------------------------------
# Default: all summary statistics returned
# (mean, sd, min, max, nobs)
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "_w\\d",
               group_type = "pattern",
               group_name = "wave",
               var_labels = c(
                 belong_belongStem_w1 = "I feel like I belong in computing",
                 belong_belongStem_w2 = "I feel like I belong in computing")
)

# Means and non-missing observations only
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "_w\\d",
               group_type = "pattern",
               group_name = "wave",
               var_labels = c(
                 belong_belongStem_w1 = "I feel like I belong in computing",
                 belong_belongStem_w2 = "I feel like I belong in computing"),
               only = c("mean", "nobs")
)

# Means and standard deviations only
mean_group_tbl(data = stem_social_psych, 
               var_stem = "belong_belong",
               group = "_w\\d",
               group_type = "pattern",
               group_name = "wave",
               var_labels = c(
                 belong_belongStem_w1 = "I feel like I belong in computing",
                 belong_belongStem_w2 = "I feel like I belong in computing"),
               only = c("mean", "sd")
)

