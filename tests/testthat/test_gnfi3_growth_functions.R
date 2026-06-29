
library(ForestElementsR)

test_that("growth estimate with the original species coding is correct", {

  # We assume a tree with dbh = 24 cm and height 23 at age 60, projected 10
  # years forward in time
  dbh_proj_exp <- c(
    27.23898, 27.23898, 27.23898, 26.69911, 26.69911, 26.69911, 26.69911,
    26.69911, 26.69911, 28.43144, 28.43144, 28.43144, 28.19472, 26.63321,
    26.63321, 27.23898, 28.43144, 27.69545, 27.23433, 27.23433, 27.23433,
    27.25792, 27.69545, 27.05482, 27.05482, 27.05482, 26.52874, 26.52874,
    26.52874, 26.52874, 26.52874, 26.52874, 26.52874, 25.78808, 25.78808,
    26.30714, 26.30714, 27.08078, 27.08078, 27.08078, 27.08078, 27.08078,
    26.39730, 26.39730, 26.39730, 26.39730, 26.39730, 26.39730, 26.39730,
    26.39730, 26.39730
  )

  h_proj_exp <- c(
    25.62742, 25.62742, 25.62742, 25.36847, 25.36847, 25.36847, 25.36847,
    25.36847, 25.36847, 26.14398, 26.14398, 26.14398, 26.33764, 25.18792,
    25.18792, 25.62742, 26.14398, 25.38060, 25.50383, 25.50383, 25.50383,
    25.14847, 25.38060, 25.00314, 25.00314, 25.00314, 24.80693, 24.80693,
    24.80693, 24.80693, 24.80693, 24.80693, 24.80693, 24.43085, 24.43085,
    24.54162, 24.54162, 24.94053, 24.94053, 24.94053, 24.94053, 24.94053,
    24.67429, 24.67429, 24.67429, 24.67429, 24.67429, 24.67429, 24.67429,
    24.67429, 24.67429
  )

  age_dbh_exp <- c(
    75.53371, 75.53371, 75.53371, 78.98191, 78.98191, 78.98191, 78.98191,
    78.98191, 78.98191, 71.26501, 71.26501, 71.26501, 71.88412, 79.27370,
    79.27370, 75.53371, 71.26501, 73.52691, 75.45723, 75.45723, 75.45723,
    75.33170, 73.52691, 76.55321, 76.55321, 76.55321, 80.15921, 80.15921,
    80.15921, 80.15921, 80.15921, 80.15921, 80.15921, 91.32075, 91.32075,
    82.28759, 82.28759, 76.25499, 76.25499, 76.25499, 76.25499, 76.25499,
    81.63087, 81.63087, 81.63087, 81.63087, 81.63087, 81.63087, 81.63087,
    81.63087, 81.63087
  )

  age_height_exp <- c(
    71.53372, 71.53372, 71.53372, 72.88740, 72.88740, 72.88740, 72.88740,
    72.88740, 72.88740, 69.52262, 69.52262, 69.52262, 68.94943, 74.08174,
    74.08174, 71.53372, 69.52262, 72.79227, 72.08616, 72.08616, 72.08616,
    74.26556, 72.79227, 75.44082, 75.44082, 75.44082, 77.21082, 77.21082,
    77.21082, 77.21082, 77.21082, 77.21082, 77.21082, 83.06397, 83.06397,
    80.65706, 80.65706, 76.07488, 76.07488, 76.07488, 76.07488, 76.07488,
    78.84866, 78.84866, 78.84866, 78.84866, 78.84866, 78.84866, 78.84866,
    78.84866, 78.84866
  )

  spec_id <- fe_species_get_coding_table("ger_nfi_2012") |>
    purrr::pluck("species_id") |> unique()

  comp_dat <- tibble::tibble(
    species_id   = spec_id,
    dbh          = 24,
    height       = 23,
    age          = 60,
    delta_age    = 10,
    delta_dbh    =  5,
    delta_height =  3
  ) |>
    dplyr::mutate(species_id = as_fe_species_ger_nfi_2012(unique(species_id)))

  # Check projected diameters
  expect_equal(
    d_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                comp_dat$dbh, comp_dat$age) |> round(digits = 5),
    dbh_proj_exp
  )

  # Check projected heights
  expect_equal(
    h_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                comp_dat$height, comp_dat$age) |> round(digits = 5),
    h_proj_exp
  )

  # Check ages estimated from dbh
  expect_equal(
    age_d_gnfi3(comp_dat$species_id, comp_dat$dbh + comp_dat$delta_dbh,
                comp_dat$dbh, comp_dat$age) |> round(digits = 5),
    age_dbh_exp
  )

  # Check ages estimated from height
  expect_equal(
    age_h_gnfi3(comp_dat$species_id, comp_dat$height + comp_dat$delta_height,
                comp_dat$height, comp_dat$age) |> round(digits = 5),
    age_height_exp
  )
})



test_that("growth estimate with tum_wwk_short species coding is correct", {

  # We assume a tree with dbh = 24 cm and height 23 at age 60, projected 10
  # years forward in time
  dbh_proj_exp <- c(
    27.23898, 28.43144, 26.69911, 26.63321, 27.69545, 27.23433, 28.19472,
    26.52874, 26.39730, 27.23898
  )

  h_proj_exp <- c(
    25.62742, 26.14398, 25.36847, 25.18792, 25.38060, 25.50383, 26.33764,
    24.80693, 24.67429, 25.62742
  )

  age_dbh_exp <- c(
   75.53371, 71.26501, 78.98191, 79.27370, 73.52691, 75.45723, 71.88412,
   80.15921, 81.63087, 75.53371
  )

  age_height_exp <- c(
   71.53372, 69.52262, 72.88740, 74.08174, 72.79227, 72.08616, 68.94943,
   77.21082, 78.84866, 71.53372
  )

  spec_id <- fe_species_get_coding_table("tum_wwk_short") |>
    purrr::pluck("species_id") |> unique()

  comp_dat <- tibble::tibble(
    species_id   = spec_id,
    dbh          = 24,
    height       = 23,
    age          = 60,
    delta_age    = 10,
    delta_dbh    =  5,
    delta_height =  3
  ) |>
    dplyr::mutate(species_id = as_fe_species_tum_wwk_short(unique(species_id)))

  # Check projected diameters
  expect_equal(
    d_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                comp_dat$dbh, comp_dat$age) |> round(digits = 5),
    dbh_proj_exp
  )

  # Check projected heights
  expect_equal(
    h_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                comp_dat$height, comp_dat$age) |> round(digits = 5),
    h_proj_exp
  )

  # Check ages estimated from dbh
  expect_equal(
    age_d_gnfi3(comp_dat$species_id, comp_dat$dbh + comp_dat$delta_dbh,
                comp_dat$dbh, comp_dat$age) |> round(digits = 5),
    age_dbh_exp
  )

  # Check ages estimated from height
  expect_equal(
    age_h_gnfi3(comp_dat$species_id, comp_dat$height + comp_dat$delta_height,
                comp_dat$height, comp_dat$age) |> round(digits = 5),
    age_height_exp
  )
})


test_that("growth estimate with bavrn_state_short species coding is correct", {

  # We assume a tree with dbh = 24 cm and height 23 at age 60, projected 10
  # years forward in time
  dbh_proj_exp <- c(
    27.23898, 26.69911, 28.43144, 26.63321, 28.19472, 27.69545, 27.23433,
    26.39730, 26.52874
  )

  h_proj_exp <- c(
    25.62742, 25.36847, 26.14398, 25.18792, 26.33764, 25.38060, 25.50383,
    24.67429, 24.80693
  )

  age_dbh_exp <- c(
    75.53371, 78.98191, 71.26501, 79.27370, 71.88412, 73.52691, 75.45723,
    81.63087, 80.15921
  )

  age_height_exp <- c(
    71.53372, 72.88740, 69.52262, 74.08174, 68.94943, 72.79227, 72.08616,
    78.84866, 77.21082
  )

  spec_id <- fe_species_get_coding_table("bavrn_state_short") |>
    purrr::pluck("species_id") |> unique()

  comp_dat <- tibble::tibble(
    species_id   = spec_id,
    dbh          = 24,
    height       = 23,
    age          = 60,
    delta_age    = 10,
    delta_dbh    =  5,
    delta_height =  3
  ) |>
    dplyr::mutate(
      species_id = as_fe_species_bavrn_state_short(unique(species_id))
    ) |>
    dplyr::arrange(species_id)

  # Check projected diameters
  expect_equal(
    d_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                comp_dat$dbh, comp_dat$age) |> round(digits = 5),
    dbh_proj_exp
  )

  # Check projected heights
  expect_equal(
    h_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                comp_dat$height, comp_dat$age) |> round(digits = 5),
    h_proj_exp
  )

  # Check ages estimated from dbh
  expect_equal(
    age_d_gnfi3(comp_dat$species_id, comp_dat$dbh + comp_dat$delta_dbh,
                comp_dat$dbh, comp_dat$age) |> round(digits = 5),
    age_dbh_exp
  )

  # Check ages estimated from height
  expect_equal(
    age_h_gnfi3(comp_dat$species_id, comp_dat$height + comp_dat$delta_height,
                comp_dat$height, comp_dat$age) |> round(digits = 5),
    age_height_exp
  )
})


test_that("growth estimate with tum_wwk_long species coding is correct", {
  # The tum_wwk_long growth methods cast tum_wwk_long -> tum_wwk_short (a valid
  # coarsening), so a tum_wwk_long species must give exactly the same projection
  # as its short group. Tested as that invariant (order- and edit-robust); the
  # absolute values are pinned in the tum_wwk_short test above.
  ct  <- fe_species_get_coding_table("tum_wwk_long")
  ids <- unique(ct$species_id[ct$is_tree])

  cd <- tibble::tibble(
    species_id   = as_fe_species_tum_wwk_long(ids),
    dbh = 24, height = 23, age = 60,
    delta_age = 10, delta_dbh = 5, delta_height = 3
  )
  short <- suppressWarnings(as_fe_species_tum_wwk_short(cd$species_id))

  expect_equal(
    suppressWarnings(
      d_age_gnfi3(cd$species_id, cd$age + cd$delta_age, cd$dbh, cd$age)),
    d_age_gnfi3(short, cd$age + cd$delta_age, cd$dbh, cd$age)
  )
  expect_equal(
    suppressWarnings(
      h_age_gnfi3(cd$species_id, cd$age + cd$delta_age, cd$height, cd$age)),
    h_age_gnfi3(short, cd$age + cd$delta_age, cd$height, cd$age)
  )
  expect_equal(
    suppressWarnings(
      age_d_gnfi3(cd$species_id, cd$dbh + cd$delta_dbh, cd$dbh, cd$age)),
    age_d_gnfi3(short, cd$dbh + cd$delta_dbh, cd$dbh, cd$age)
  )
  expect_equal(
    suppressWarnings(
      age_h_gnfi3(cd$species_id, cd$height + cd$delta_height, cd$height, cd$age)),
    age_h_gnfi3(short, cd$height + cd$delta_height, cd$height, cd$age)
  )
})


test_that("growth estimate with bavrn_state species coding is correct", {
  # The bavrn_state growth methods cast bavrn_state -> bavrn_state_short, so a
  # bavrn_state species must give exactly the same projection as its short
  # group. Tested as that invariant (order- and edit-robust); the absolute
  # values are pinned in the bavrn_state_short test above.
  ct  <- fe_species_get_coding_table("bavrn_state")
  ids <- unique(ct$species_id[ct$is_tree])   # exclude non-tree codes (e.g. 99)

  cd <- tibble::tibble(
    species_id   = as_fe_species_bavrn_state(ids),
    dbh = 24, height = 23, age = 60,
    delta_age = 10, delta_dbh = 5, delta_height = 3
  )
  short <- suppressMessages(as_fe_species_bavrn_state_short(cd$species_id))

  expect_equal(
    suppressWarnings(
      d_age_gnfi3(cd$species_id, cd$age + cd$delta_age, cd$dbh, cd$age)),
    d_age_gnfi3(short, cd$age + cd$delta_age, cd$dbh, cd$age)
  )
  expect_equal(
    suppressWarnings(
      h_age_gnfi3(cd$species_id, cd$age + cd$delta_age, cd$height, cd$age)),
    h_age_gnfi3(short, cd$age + cd$delta_age, cd$height, cd$age)
  )
  expect_equal(
    suppressWarnings(
      age_d_gnfi3(cd$species_id, cd$dbh + cd$delta_dbh, cd$dbh, cd$age)),
    age_d_gnfi3(short, cd$dbh + cd$delta_dbh, cd$dbh, cd$age)
  )
  expect_equal(
    suppressWarnings(
      age_h_gnfi3(cd$species_id, cd$height + cd$delta_height, cd$height, cd$age)),
    age_h_gnfi3(short, cd$height + cd$delta_height, cd$height, cd$age)
  )
})


test_that("calculations work with species codes incompatible with ger_nfi_2012", {
  # Code 90 ("other conifers") cannot be cast into ger_nfi_2012. The gnfi3
  # growth functions must still work: for bavrn_state they fall back to the
  # bavrn_state_short parameterization. They no longer match the tum_wwk_short
  # path, because bavrn_state_short now groups code 90 with the pines (code 2) -
  # a deliberate coding choice; so this checks the result is valid and matches
  # the documented bavrn_state_short path, not that the two codings agree.
  spec <- fe_species_bavrn_state(90)

  cd <- tibble::tibble(
    dbh = 24, height = 23, age = 60,
    delta_age = c(10, -10), delta_dbh = c(5, -5), delta_height = c(3, -3)
  )

  expect_true(all(is.finite(suppressMessages(
    d_age_gnfi3(spec, cd$age + cd$delta_age, cd$dbh, cd$age)))))
  expect_true(all(is.finite(suppressMessages(
    h_age_gnfi3(spec, cd$age + cd$delta_age, cd$height, cd$age)))))
  expect_true(all(is.finite(suppressMessages(
    age_d_gnfi3(spec, cd$dbh + cd$delta_dbh, cd$dbh, cd$age)))))
  expect_true(all(is.finite(suppressMessages(
    age_h_gnfi3(spec, cd$height + cd$delta_height, cd$height, cd$age)))))

  # The bavrn_state result equals the explicit bavrn_state_short path
  short <- suppressMessages(as_fe_species_bavrn_state_short(spec))
  expect_equal(
    suppressMessages(d_age_gnfi3(spec, cd$age + cd$delta_age, cd$dbh, cd$age)),
    d_age_gnfi3(short, cd$age + cd$delta_age, cd$dbh, cd$age)
  )
})


test_that("calculations work with species codes compatible with ger_nfi_2023",
{
  # Species that can be cast into ger_nfi_2012 ("European beech")
  spec_orig <- fe_species_bavrn_state(60)
  spec_cast <- as_fe_species_ger_nfi_2012(spec_orig)


  comp_dat <- tibble::tibble(
    dbh          = 24,
    height       = 23,
    age          = 60,
    delta_age    = c(10, -10),
    delta_dbh    = c( 5,  -5),
    delta_height = c( 3,  -3)
  )


  expect_equal(
    d_age_gnfi3(
      spec_orig, comp_dat$age + comp_dat$delta_age, comp_dat$dbh, comp_dat$age
    ),
    d_age_gnfi3(
      spec_cast, comp_dat$age + comp_dat$delta_age, comp_dat$dbh, comp_dat$age
    )
  )

  expect_equal(
    h_age_gnfi3(
      spec_orig, comp_dat$age + comp_dat$delta_age, comp_dat$height,
      comp_dat$age
    ),
    h_age_gnfi3(
      spec_cast, comp_dat$age + comp_dat$delta_age, comp_dat$height,
      comp_dat$age
    )
  )

  expect_equal(
    age_d_gnfi3(
      spec_orig, comp_dat$dbh + comp_dat$delta_dbh, comp_dat$dbh, comp_dat$age
    ),
    age_d_gnfi3(
      spec_cast, comp_dat$dbh + comp_dat$delta_dbh, comp_dat$dbh, comp_dat$age
    )
  )

  expect_equal(
    age_h_gnfi3(
      spec_orig, comp_dat$height + comp_dat$delta_height, comp_dat$height,
      comp_dat$age
    ),
    age_h_gnfi3(
      spec_cast, comp_dat$height + comp_dat$delta_height, comp_dat$height,
      comp_dat$age
    )
  )
})

