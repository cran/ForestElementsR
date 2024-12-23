## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(ForestElementsR) # Attach ForestElementsR

# Other packages required in the code examples below
library(dplyr)
library(ggplot2)
library(purrr)

## -----------------------------------------------------------------------------
# spruce_beech_1_fe_stand is one of the included example objects, 
# for an overview, type "?example_data"
spruce_beech_1_fe_stand
                        

## ----fig.dim=c(4.9, 2.8), fig.align="center"----------------------------------
oo <- options(fe_spec_lang = "eng")  # display species names in English

spruce_beech_1_fe_stand |> summary() # Give a summary of stand-level values
spruce_beech_1_fe_stand |> plot()    # Make an appropriate plot
                                     
options(oo)                          # Reset species name display option

## ----fig.dim=c(6.3, 3.6), fig.align="center"----------------------------------
oo <- options(fe_spec_lang = "sci")       # display scientific species names

# Example: A mixed mountain forest plot in the Bavarian Alps;
# for an overview, type "?example_data"
mm_forest_1_fe_stand_spatial |> summary() # Give a summary of stand-level values
mm_forest_1_fe_stand_spatial |> plot()    # Make an appropriate plot
                                     
options(oo)                               # Reset species name display option

## ----fig.dim=c(4.9, 3.6), fig.align="center"----------------------------------
mm_forest_1_fe_stand_spatial$outline |> 
  ggplot() +
  geom_sf(fill = NA) +
  geom_sf(data = mm_forest_1_fe_stand_spatial$tree_positions) +
  xlab("x (m)") + ylab("y (m)")


## ----fig.dim=(c(4.9, 4.9)), fig.align="center"--------------------------------
oo <- options(fe_spec_lang = "ger") # display German species names

spruce_pine_ccircle_spatial |> summary()
spruce_pine_ccircle_spatial |> plot(dbh_scale = 3) # oversize dbh x3

options(oo)        # Reset species name display option

## -----------------------------------------------------------------------------
# Define a vector of species ids compatible with the coding used by the 
# Bavarian Forest Service
species_ids <- as_fe_species_bavrn_state(c(10, 20, 21, 24, 68, 62, 60))
# How they are displayed depends on the current setting of the option
# fe_spec_lang
species_ids
# Check the option
options("fe_spec_lang") # if NULL or "code" the number codes are displayed
# Set to English (and keep the prevous option settings in oo)
oo <- options(fe_spec_lang = "eng")
species_ids
# Set to scientific names
options(fe_spec_lang = "sci")
species_ids
# Set to German
options(fe_spec_lang = "ger")
species_ids
# Set to codes
options(fe_spec_lang = "code")
species_ids
# Reset to the previous setting before enforcing English
options(oo)

## ----fig.dim = c(4.9, 4.2), fig.align="center"--------------------------------
fe_ytable_spruce_wiedemann_moderate_1936_42 |> plot()


## -----------------------------------------------------------------------------
# Get all variables available in the yield table of interest
vars <- names(fe_ytable_spruce_wiedemann_moderate_1936_42$values)
vars

## ----eval=FALSE---------------------------------------------------------------
# # select standing volume ...
# fe_ytable_spruce_wiedemann_moderate_1936_42 |> plot(variable = "v_m3_ha")

## ----fig.dim = c(4.9, 3.5), fig.align="center"--------------------------------
# ... and the periodic annual incement for plotting
fe_ytable_spruce_wiedemann_moderate_1936_42 |> plot(variable = "pai_m3_ha_yr")

## -----------------------------------------------------------------------------
dbh <- c(30.5, 30.9, 33.3, 35.1, 19.4, 19.5, 28.1, 25.5)


## -----------------------------------------------------------------------------
# Calculate each tree's basal area in m²
ba <- (dbh / 100)^2 * pi/4
# Angle count factor 4/ba gives each tree's individual representation number
# per ha
nrep_ha <- 4 / ba
nrep_ha

## -----------------------------------------------------------------------------
# Quadratic mean diameter
dq <- d_q(dbh, nrep_ha)
dq

# Note that the quadratic mean diameter is always greater than the arithmetic
# mean as calculated here
stats::weighted.mean(dbh, w = nrep_ha)

# Assmann's dominant diameter (quadratic mean diameter of the 100 thickest stems
# per ha)
d_100(dbh, nrep_ha)

# Weise's dominant diameter (quadratic mean diameter of 20% thickest stems)
d_dom_weise(dbh, nrep_ha)

## ----fig.dim=c(7, 4), dpi=96--------------------------------------------------
# Make a vector of 8 species ids = 5 and transform it into a tum_wwk_short
# species coding vector (see vignette about Species Codings in ForestElementsR),
# where code 5 stands for European beech.
# For our monospecific sample, we could make things even simpler (i.e. providing
# only one species id value for all trees together to the functions for height
# and volume estimates below), but we prefer to show the more generic approach
# here, where each tree has its own corresponding species id.
species_id <- rep(5, 8) |> as_fe_species_tum_wwk_short()
species_id

oo <- options(fe_spec_lang = "eng") # display species names in English
species_id

hq  <- 23.6 # Height value corresponding to the quadratic mean diameter dq
age <- 72   # Stand age

# Estimate a height for each tree in the stand based on the information above
# with the Bavarian standard height curve system
h <- h_standard_bv(species_id, dbh, age, dq, hq)
h    # Vector of height estimates corresponding to the dbh vector

options(oo) # Switch back options to previous values

## ----fig.dim=c(7, 4), dpi=96--------------------------------------------------
v <- v_gri(species_id, dbh, h)
v


## -----------------------------------------------------------------------------
v_ha <- sum(v * nrep_ha)
v_ha


## -----------------------------------------------------------------------------
si <- site_index(
  age    = age,
  size   = hq,
  ytable = fe_ytable_beech_wiedemann_moderate_1931,
  si_variable = "h_q_m"
)

si


## -----------------------------------------------------------------------------
inc <- ytable_lookup(age = age, si = si, variable = "pai_m3_ha_yr",
                     ytable = fe_ytable_beech_wiedemann_moderate_1931)
inc


## -----------------------------------------------------------------------------
# Basal area per hectare from angle count sample
ba_ha <- length(dbh) * 4
ba_ha

# Stocking level 
stl <- stocking_level(ba = ba_ha, age = age, si = si,
                      ytable = fe_ytable_beech_wiedemann_moderate_1931)

stl


## -----------------------------------------------------------------------------
inc * min(1, stl) # correct the increment only if stocking level < 1


## -----------------------------------------------------------------------------
# Current volume of the quadratic mean diameter tree (dbh = dq, height = hq)
species_id  <- as_fe_species_tum_wwk_short(5) # Beech in tum_wwk_short coding
vol_current <- v_gri(species_id, dq, hq)

# future (+5 years) dq and hq estimated with German NFI3 functions
dq_future   <- d_age_gnfi3(species_id, age + 5, dq, age)
hq_future   <- h_age_gnfi3(species_id, age + 5, hq, age)

# future volume of the quadratic mean diameter tree (dq-tree)
vol_future  <- v_gri(species_id, dq_future, hq_future)

# Estimated annual volume increment of the dq-tree
# Division by 5 years, because we want an annual value
iv_mean_tree <- (vol_future - vol_current) / 5 

# The dq-tree currently represents so many trees per ha ...
n_ha <- sum(nrep_ha)

# ... therefore, we estimate the stand's increment as
iv_stand <- iv_mean_tree * n_ha

iv_stand


## -----------------------------------------------------------------------------
# Monospecific stand
#   1. Extract the tree data frame frame from an fe_stand_object
trees <- norway_spruce_1_fe_stand$trees 
#   2. Run shannon_index() on its species_id column (technically a vector)
shannon_index(trees$species_id)

# Two-species mixed stand
trees <- spruce_beech_1_fe_stand$trees
shannon_index(trees$species_id)

# Selection forest
trees <- selection_forest_1_fe_stand$trees
shannon_index(trees$species_id)

## -----------------------------------------------------------------------------
 # Monospecific stand
trees <- norway_spruce_1_fe_stand$trees
species_profile(trees$species_id, trees$height_m)

# Two-species mixed stand
trees <- spruce_beech_1_fe_stand$trees
species_profile(trees$species_id, trees$height_m)

# Selection forest
trees <- selection_forest_1_fe_stand$trees
species_profile(trees$species_id, trees$height_m)
  

## -----------------------------------------------------------------------------
oo <- options(fe_spec_lang = "eng")

# Include all trees in evaluation with stand_sums_static
mm_forest_1_fe_stand_spatial |> stand_sums_static()

# For applying a filter, we must refer to the column names of the "trees" data 
# frame inside the fe_stand object. These are
mm_forest_1_fe_stand_spatial$trees |> names()

# Assume, we want to include only the trees that were not removed ...
mm_forest_1_fe_stand_spatial |> stand_sums_static(tree_filter = !removal)

# ... and now only the removal trees
mm_forest_1_fe_stand_spatial |> stand_sums_static(tree_filter = removal)

# Focus on Norway spruce and the surveys after 2000 only. Note, for filtering
# on species names you must use the format() function
# 1. Remaining trees only
mm_forest_1_fe_stand_spatial |> 
  stand_sums_static(
    tree_filter = 
      format(species_id, "eng") == "Norway spruce" & time_yr > 2000 & !removal
  )

# 2. Removal trees only
mm_forest_1_fe_stand_spatial |> 
  stand_sums_static(
    tree_filter = 
      format(species_id, "eng") == "Norway spruce" & time_yr > 2000 & removal
  )

options(oo)

## -----------------------------------------------------------------------------
oo <- options(fe_spec_lang = "eng")

# Calculate basal area and volume increments for an fe_stand object that covers
# more than one survey
ssd <- stand_sums_dynamic(mm_forest_1_fe_stand_spatial)
ssd |> print(n = Inf)

options(oo)

## -----------------------------------------------------------------------------
# Species-overarching increments
stand_sums_dynamic(mm_forest_1_fe_stand_spatial) |>
  group_by(time_yr) |>
  summarise(
    iba_m2_ha_yr = sum(iba_m2_ha_yr, na.rm = TRUE),
    iv_m3_ha_yr  = sum(iv_m3_ha_yr,  na.rm = TRUE)         
  )

# Zero increments for 1975 are obtained, because no previous survey is 
# available, so no meaningful increment can be calculated there


## -----------------------------------------------------------------------------
# fe_stand
get_area_ha(norway_spruce_1_fe_stand)

# fe_stand_spatial
get_area_ha(mm_forest_1_fe_stand_spatial)

# fe_ccircle_spatial (the area of the outermost circle is returned)
get_area_ha(spruce_pine_ccircle_spatial)


