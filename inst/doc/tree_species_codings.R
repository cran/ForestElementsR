## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(ForestElementsR)
library(tibble)
library(dplyr)
library(ggplot2)

## ----view_species_master_table------------------------------------------------
species_master_table

# Also show the tail of the table
species_master_table |> tail(10)

## ----view_species_codings-----------------------------------------------------
species_codings

## ----get_coding_table---------------------------------------------------------
fe_species_get_coding_table("tum_wwk_short")

## ----view_coding_compact------------------------------------------------------
fe_species_get_coding_table("tum_wwk_short") |>
  select(species_id, name_eng) |> # English names only for clarity
  distinct()

## ----view_species_numbers_in_coding-------------------------------------------
fe_species_get_coding_table("tum_wwk_short") |>
  group_by(species_id, name_eng) |>
  summarise(n_species = n()) |>
  arrange(as.numeric(species_id)) # not required, but output is nicely sorted

## ----view_quercus_group-------------------------------------------------------
fe_species_get_coding_table("tum_wwk_short") |>
  select(species_id, genus, species_no, name_eng) |>
  filter(species_id == "6")

## ----get_quercus_group_names--------------------------------------------------
species_master_table |>
  filter(genus == "quercus" & species_no %in% c("001", "002")) |>
  select(-deciduous_conifer)

## ----create_species_code_vectors----------------------------------------------
spec_ids_1 <- fe_species_tum_wwk_short(c(1, 1, 1, 5, 5, 5, 5, 3, 3, 8, 9, 8))
spec_ids_2 <- fe_species_ger_nfi_2012(
  c(10, 10, 10, 100, 100, 100, 100, 20, 20, 190, 290, 190)
)

spec_ids_1
spec_ids_2

## ----bad_code_input, error = TRUE---------------------------------------------
try({
fe_species_tum_wwk_short(c(1, 321, 1, 9999))
fe_species_ger_nfi_2012(c("100", "290", "Peter", "Paul", "Mary"))
})

## ----check_object_if_species_codes--------------------------------------------
spec_ids <- c(1:10)
is_fe_species_tum_wwk_short(spec_ids)
spec_ids <- fe_species_tum_wwk_short(c(1:10))
is_fe_species_tum_wwk_short(spec_ids)
is_fe_species_bavrn_state(spec_ids)

## ----option_spec_lang---------------------------------------------------------
spec_ids_1 <- fe_species_tum_wwk_short(c(1, 1, 5, 5, 5, 5, 3, 3))
spec_ids_2 <- fe_species_ger_nfi_2012(c(100, 100, 20, 20, 30, 110))
spec_ids_3 <- fe_species_bavrn_state(c(60, 60, 30, 30, 84, 86))
spec_ids_4 <- fe_species_master(c("abies_001", "tilia_002", "ulmus_001"))

## ----reset_spec_lang_option, echo = FALSE-------------------------------------
op_help <- options(fe_spec_lang = NULL) # catch user's actual setting

## ----display_default_codes, echo = FALSE--------------------------------------
spec_ids_1
spec_ids_2
spec_ids_3
spec_ids_4

## ----display_scientific_names-------------------------------------------------
options(fe_spec_lang = "sci") # Display scientific species names

spec_ids_1
spec_ids_2
spec_ids_3
spec_ids_4

## ----display_english_names----------------------------------------------------
options(fe_spec_lang = "eng") # Display English species names

spec_ids_1
spec_ids_2
spec_ids_3
spec_ids_4

## ----set_spec_lang_option_to_original, echo = FALSE---------------------------
options(fe_spec_lang = op_help)

## ----format_example-----------------------------------------------------------
format(spec_ids_1, spec_lang = "eng")
format(spec_ids_2, spec_lang = "sci")
format(spec_ids_3, spec_lang = "code")
format(spec_ids_4, spec_lang = "ger")

## ----do_not_try_to_generate_from_names, error = TRUE--------------------------
try({
spec_names <- c("Abies alba", "Picea abies")
fe_species_ger_nfi_2012(spec_names)
})

## ----assigning_species_codes, error = TRUE------------------------------------
try({
spec_vec <- fe_species_bavrn_state(c("10", "10", "10", "50", "50", "50"))
format(spec_vec, "eng")

# Safest way, same class on both sides of the '<-'
spec_vec[3] <- fe_species_bavrn_state("40")
is_fe_species_bavrn_state(spec_vec)
format(spec_vec, "eng")

# Character vector is converted
spec_vec[3:4] <- c("40", "70")
is_fe_species_bavrn_state(spec_vec)
format(spec_vec, "eng")

# Numerical vector is converted
spec_vec[3:4] <- c(60, 87)
is_fe_species_bavrn_state(spec_vec)
format(spec_vec, "eng")

# Species code not supported by goal coding - no assignment and error
spec_vec[1:2] <- c("3333", "12")
is_fe_species_bavrn_state(spec_vec)
format(spec_vec, "eng")

# Vectors of other species codings are converted, if possible
spec_vec[5:6] <- fe_species_tum_wwk_short(c("3", "3")) # "3" Scots pine in rhs
# coding
is_fe_species_bavrn_state(spec_vec)
format(spec_vec, "code") # "3" becomes "20" ...
format(spec_vec, "eng") # ... which is Scots pine in the goal coding
})

## ----unproblematic_conversion-------------------------------------------------
spec_ids <- as_fe_species_tum_wwk_short(c("1", "3", "5"))
as_fe_species_ger_nfi_2012(spec_ids) |> format("eng")

## ----conversion_with_information_loss-----------------------------------------
spec_ids_1 <- as_fe_species_ger_nfi_2012(c("170", "150", "140"))
spec_ids_1 |> format("eng")

# Backward ambiguous cast (possibly, but with information loss)
spec_ids_2 <- as_fe_species_tum_wwk_short(spec_ids_1)
spec_ids_2 |> format("eng")

## ----impossible_conversion_no_match, error = TRUE-----------------------------
try({
spec_ids <- as_fe_species_bavrn_state(c("11", "11", "11"))
spec_ids |> format("eng")

# No Serbian spruce in the tum_wwk_long coding
spec_ids |> as_fe_species_tum_wwk_long()
})

## ----forward_ambiguous_cast, error = TRUE-------------------------------------
try({
# Each of these codes comprises many single species
spec_ids <- fe_species_tum_wwk_short(c("8", "9", "10"))
spec_ids |> format("eng")

# Conversion attempt terminates with error
spec_ids |> as_fe_species_ger_nfi_2012()

# Similar
as_fe_species_master(fe_species_ger_nfi_2012("90"))
})

## ----good_and_bad_conversions_between_same_types, error = TRUE----------------
try({
# Conversion from tum_wwk_short to ger_nfi_2012 - works
spec_ids_1 <- fe_species_tum_wwk_short(c("1", "3", "5"))
spec_ids_1 |> format("eng")

spec_ids_2 <- as_fe_species_ger_nfi_2012(spec_ids_1)
spec_ids_2 |> format("eng")

# Conversion from tum_wwk_short to ger_nfi_2012 - fails
spec_ids_1 <- fe_species_tum_wwk_short(c("8", "9", "10"))
spec_ids_1 |> format("eng")

spec_ids_2 <- as_fe_species_ger_nfi_2012(spec_ids_1)
})

## ----option_2a, echo = FALSE--------------------------------------------------
opt_help <- getOption("fe_spec_lang")
options(fe_spec_lang = "code")

## ----get_the_char_vector------------------------------------------------------
spec_ids <- fe_species_ger_nfi_2012(c("10", "10", "100", "170"))
spec_ids

chars_1 <- unclass(spec_ids)
chars_1
chars_2 <- vctrs::vec_data(spec_ids)
chars_2

is_fe_species_ger_nfi_2012(chars_1)
is_fe_species_ger_nfi_2012(chars_2)

is.character(chars_1)
is.character(chars_2)

## ----option_2b, echo = FALSE--------------------------------------------------
options(fe_spec_lang = opt_help)

## ----option_3a, echo = FALSE--------------------------------------------------
opt_help <- getOption("fe_spec_lang")
options(fe_spec_lang = "code")

## ----demo_with_selection_forest_1---------------------------------------------
dat <- selection_forest_1_fe_stand$trees |> select(
  tree_id, species_id, time_yr, dbh_cm, height_m
)
dat

## ----option_3b, echo = FALSE--------------------------------------------------
options(fe_spec_lang = opt_help)

## ----option_4a, echo = FALSE--------------------------------------------------
opt_help <- getOption("fe_spec_lang")
options(fe_spec_lang = "code")

## ----demo_with_selection_forest_2---------------------------------------------
dat |> summary()

## ----option_4b, echo = FALSE--------------------------------------------------
options(fe_spec_lang = opt_help)

## ----option_5a, echo = FALSE--------------------------------------------------
opt_help <- getOption("fe_spec_lang")
options(fe_spec_lang = "code")

## ----demo_with_selection_forest_3---------------------------------------------
# Set option to display colloquial English species names, and store the
# previous setting in opt_prev
opt_prev <- getOption("fe_spec_lang")
options(fe_spec_lang = "eng")

# Display dat
dat

# Display a summary of dat
dat |> summary()

# Reset option to previous value
options(fe_spec_lang = opt_prev)

## ----option_5b, echo = FALSE--------------------------------------------------
options(fe_spec_lang = opt_help)

## ----option_6a, echo = FALSE--------------------------------------------------
opt_help <- getOption("fe_spec_lang")
options(fe_spec_lang = "code")

## ----single_tree_volumes------------------------------------------------------
opt_prev <- getOption("fe_spec_lang")
options(fe_spec_lang = "eng")

dat <- dat |>
  mutate(v_cbm = v_gri(species_id, dbh_cm, height_m))

# Note that the summary of species_id does not preserve the original order of
# the codes (species are alphabetically sorted, dependent on language setting)
dat |> summary()

options(fe_spec_lang = opt_prev)

## ----option_6b, echo = FALSE--------------------------------------------------
options(fe_spec_lang = opt_help)

## ----option_7a, echo = FALSE--------------------------------------------------
opt_help <- getOption("fe_spec_lang")
options(fe_spec_lang = "code")

## ----mean_volumes-------------------------------------------------------------
# Set option for displaying scientific species names
opt_prev <- getOption("fe_spec_lang")
options(fe_spec_lang = "sci")

dat |>
  group_by(species_id) |>
  summarise(
    mean_stem_volume_cbm = mean(v_cbm),
    sd_stem_volume_cbm = sd(v_cbm)
  )
# In contrast to summary, summarise keeps the original order of the species
# codes, no matter the language setting

options(fe_spec_lang = opt_prev)

## ----option_7b, echo = FALSE--------------------------------------------------
options(fe_spec_lang = opt_help)

## ----plot_1, fig.cap=, fig.dim=c(4.9, 3.5), fig.align = 'center', fig.cap = 'Stem volume over diameter by species in log-log display'----
# Note: Using simply 'format(species_id)' below would use the current setting
# of the option fe_spec_lang
dat |>
  ggplot() +
  geom_point(aes(x = dbh_cm, y = v_cbm, col = format(species_id, "eng"))) +
  scale_color_discrete("Species") +
  scale_x_log10() +
  scale_y_log10()

## ----all_inherit_from_vctrs---------------------------------------------------
fe_species_bavrn_state("30") |> class()
fe_species_ger_nfi_2012("20") |> class()
fe_species_tum_wwk_long("87") |> class()
fe_species_tum_wwk_short("7") |> class()
fe_species_master("abies_004") |> class()

