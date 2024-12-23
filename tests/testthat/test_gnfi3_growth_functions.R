
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

  # We assume a tree with dbh = 24 cm and height 23 at age 60, projected 10
  # years forward in time
  dbh_proj_exp <- c(
    27.23898, 28.43144, 26.69911, 27.23898, 27.23898, 26.63321, 27.23898,
    27.23898, 27.69545, 26.52874, 27.23433, 26.52874, 28.19472, 26.52874,
    26.52874, 26.52874, 26.52874, 26.52874, 26.52874, 26.39730, 26.39730,
    26.39730, 26.52874, 26.52874, 26.52874, 26.52874, 26.52874, 26.52874,
    26.52874, 26.52874, 26.52874, 26.52874, 26.52874, 26.52874, 26.52874,
    26.52874, 26.39730, 26.39730, 26.52874, 26.52874, 26.52874, 26.52874,
    26.52874, 27.23898
  )

  h_proj_exp <- c(
    25.62742, 26.14398, 25.36847, 25.62742, 25.62742, 25.18792, 25.62742,
    25.62742, 25.38060, 24.80693, 25.50383, 24.80693, 26.33764, 24.80693,
    24.80693, 24.80693, 24.80693, 24.80693, 24.80693, 24.67429, 24.67429,
    24.67429, 24.80693, 24.80693, 24.80693, 24.80693, 24.80693, 24.80693,
    24.80693, 24.80693, 24.80693, 24.80693, 24.80693, 24.80693, 24.80693,
    24.80693, 24.67429, 24.67429, 24.80693, 24.80693, 24.80693, 24.80693,
    24.80693, 25.62742
  )

  age_dbh_exp <- c(
    75.53371, 71.26501, 78.98191, 75.53371, 75.53371, 79.27370, 75.53371,
    75.53371, 73.52691, 80.15921, 75.45723, 80.15921, 71.88412, 80.15921,
    80.15921, 80.15921, 80.15921, 80.15921, 80.15921, 81.63087, 81.63087,
    81.63087, 80.15921, 80.15921, 80.15921, 80.15921, 80.15921, 80.15921,
    80.15921, 80.15921, 80.15921, 80.15921, 80.15921, 80.15921, 80.15921,
    80.15921, 81.63087, 81.63087, 80.15921, 80.15921, 80.15921, 80.15921,
    80.15921, 75.53371
  )

  age_height_exp <- c(
    71.53372, 69.52262, 72.88740, 71.53372, 71.53372, 74.08174, 71.53372,
    71.53372, 72.79227, 77.21082, 72.08616, 77.21082, 68.94943, 77.21082,
    77.21082, 77.21082, 77.21082, 77.21082, 77.21082, 78.84866, 78.84866,
    78.84866, 77.21082, 77.21082, 77.21082, 77.21082, 77.21082, 77.21082,
    77.21082, 77.21082, 77.21082, 77.21082, 77.21082, 77.21082, 77.21082,
    77.21082, 78.84866, 78.84866, 77.21082, 77.21082, 77.21082, 77.21082,
    77.21082, 71.53372
  )

  spec_id <- fe_species_get_coding_table("tum_wwk_long") |>
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
      species_id = as_fe_species_tum_wwk_long(unique(species_id))
    ) |>
    dplyr::arrange(species_id)

  # Check projected diameters
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
      d_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                  comp_dat$dbh, comp_dat$age)
    ) |> round(digits = 5),
    dbh_proj_exp
  )

  # Check projected heights
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
      h_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                  comp_dat$height, comp_dat$age)
    ) |> round(digits = 5),
    h_proj_exp
  )

  # Check ages estimated from dbh
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
      age_d_gnfi3(comp_dat$species_id, comp_dat$dbh + comp_dat$delta_dbh,
                  comp_dat$dbh, comp_dat$age)
    ) |> round(digits = 5),
    age_dbh_exp
  )

  # Check ages estimated from height
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
      age_h_gnfi3(comp_dat$species_id, comp_dat$height + comp_dat$delta_height,
                  comp_dat$height, comp_dat$age)
    ) |> round(digits = 5),
    age_height_exp
  )
})


test_that("growth estimate with bavrn_state species coding is correct", {

  # We assume a tree with dbh = 24 cm and height 23 at age 60, projected 10
  # years forward in time
  dbh_proj_exp <- c(
    27.23898, 27.23898, 27.23898, 26.69911, 26.69911, 26.69911, 26.69911,
    26.69911, 26.69911, 28.43144, 28.43144, 26.63321, 26.63321, 28.19472,
    27.69545, 26.39730, 26.52874, 26.52874, 26.52874, 26.39730, 26.39730,
    26.39730, 26.52874, 26.52874, 27.23433, 27.23433, 26.52874, 26.52874,
    26.39730, 26.39730, 26.39730, 26.52874, 26.52874, 26.52874, 26.39730,
    26.39730, 26.39730, 26.39730, 26.39730, 26.39730, 26.39730, 26.52874,
    26.52874, 26.39730, 27.23898
  )

  h_proj_exp <- c(
    25.62742, 25.62742, 25.62742, 25.36847, 25.36847, 25.36847, 25.36847,
    25.36847, 25.36847, 26.14398, 26.14398, 25.18792, 25.18792, 26.33764,
    25.38060, 24.67429, 24.80693, 24.80693, 24.80693, 24.67429, 24.67429,
    24.67429, 24.80693, 24.80693, 25.50383, 25.50383, 24.80693, 24.80693,
    24.67429, 24.67429, 24.67429, 24.80693, 24.80693, 24.80693, 24.67429,
    24.67429, 24.67429, 24.67429, 24.67429, 24.67429, 24.67429, 24.80693,
    24.80693, 24.67429, 25.62742
  )

  age_dbh_exp <- c(
    75.53371, 75.53371, 75.53371, 78.98191, 78.98191, 78.98191, 78.98191,
    78.98191, 78.98191, 71.26501, 71.26501, 79.27370, 79.27370, 71.88412,
    73.52691, 81.63087, 80.15921, 80.15921, 80.15921, 81.63087, 81.63087,
    81.63087, 80.15921, 80.15921, 75.45723, 75.45723, 80.15921, 80.15921,
    81.63087, 81.63087, 81.63087, 80.15921, 80.15921, 80.15921, 81.63087,
    81.63087, 81.63087, 81.63087, 81.63087, 81.63087, 81.63087, 80.15921,
    80.15921, 81.63087, 75.53371
  )

  age_height_exp <- c(
    71.53372, 71.53372, 71.53372, 72.88740, 72.88740, 72.88740, 72.88740,
    72.88740, 72.88740, 69.52262, 69.52262, 74.08174, 74.08174, 68.94943,
    72.79227, 78.84866, 77.21082, 77.21082, 77.21082, 78.84866, 78.84866,
    78.84866, 77.21082, 77.21082, 72.08616, 72.08616, 77.21082, 77.21082,
    78.84866, 78.84866, 78.84866, 77.21082, 77.21082, 77.21082, 78.84866,
    78.84866, 78.84866, 78.84866, 78.84866, 78.84866, 78.84866, 77.21082,
    77.21082, 78.84866, 71.53372
  )

  spec_id <- fe_species_get_coding_table("bavrn_state") |>
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
      species_id = as_fe_species_bavrn_state(unique(species_id))
    ) |>
    dplyr::arrange(species_id)

  # Check projected diameters
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
      d_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                  comp_dat$dbh, comp_dat$age)
    ) |> round(digits = 5),
    dbh_proj_exp
  )

  # Check projected heights
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
        h_age_gnfi3(comp_dat$species_id, comp_dat$age + comp_dat$delta_age,
                    comp_dat$height, comp_dat$age)
    ) |> round(digits = 5),
    h_proj_exp
  )

  # Check ages estimated from dbh
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
      age_d_gnfi3(comp_dat$species_id, comp_dat$dbh + comp_dat$delta_dbh,
                  comp_dat$dbh, comp_dat$age)
    ) |> round(digits = 5),
    age_dbh_exp
  )

  # Check ages estimated from height
  expect_equal(
    suppressWarnings( # species cast warnings are no problem
      age_h_gnfi3(comp_dat$species_id, comp_dat$height + comp_dat$delta_height,
                  comp_dat$height, comp_dat$age)
    ) |> round(digits = 5),
    age_height_exp
  )
})


test_that("calculations work with species codes incompatible with ger_nfi_2023",
  {
  # Group that cannot be cast into ger_nfi_2012 ("other conifers")
  spec_orig <- fe_species_bavrn_state(90)
  # But it can be converted into tum wwk short
  spec_cast <- as_fe_species_tum_wwk_short(spec_orig)


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

