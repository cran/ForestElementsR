
# ForestElementsR

README by Peter Biber

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/ForestElementsR)](https://cran.r-project.org/package=ForestElementsR)
[![License: GPL (\>=
3)](https://img.shields.io/badge/license-GPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](http://cranlogs.r-pkg.org/badges/grand-total/ForestElementsR)](https://cran.r-project.org/package=ForestElementsR)
<!-- badges: end -->

Many methods from the fields of forest growth and yield, as well as
forest inventory, that belong to the core contents of forest related
education were not widely available in a contemporary form so far. To
cover this deficit, and to continue doing so in the long run is the
driving idea behind the creation and development of the package
*ForestElementsR*. The concept of the package is mainly based on
experiences and procedures made and developed at the former Chair of
Forest Growth and Yield at the Technical University of Munich. With
*ForestElementsR*, we want to provide generic algorithms and data
structures for use with forest mensurational data within a consistent
framework. The functions and objects included are a collection of widely
usable tools; more specialized applications should be implemented in
separate packages that build on this foundation.

With *ForestElementsR* we would like to address a broad audience.
**Practitioners** might want to apply it for many purposes from using
single functions in a “forest pocket calculator” style to evaluate
forest stand surveys by arranging functions in a workchain way.
**Scientists** will hopefully find the package useful for carrying out
routine evaluations of forest data (like volume estimates, site
indexing) that are regular prerequisites for in-dephth scientific
analyzes. **Students** could profit from applying *ForestElementR*’s
functionality to the extensive set of example data that comes with the
package. We are confident that this would help them to develop an
understanding and intuition about quantitative key methods in forestry
as well as about the quantities themselves.

<div style="border: 2px solid #0073e6; padding: 10px; border-radius: 5px; 
background-color: #f9f9f9;">

Before we proceed, let us put out an **Important Warning**: While the
functions and objects we provide with *ForestElemensR* behave exactly as
described in the documentation, professional training in the fields of
forestry, forest management, forest science is required to apply them
correctly and to understand what their output actually means. If you are
a hobby forester, you are more than welcome to make use of this package,
but be sure to consult a professional before you draw important
conclusions from what you get out of your evaluations. We also would
like to point out another trivial wisdom that, however, seems to be
ignored on a regular basis: *If your data are bad, don’t expect to get
good results.*

</div>

## Installation

In order to work with ForestElementsR you require an installation of R
(version \>= 4.4.0) on your system. The installation files for R itself
are available [here](https://cran.r-project.org/). The released version
of ForestElementsR can be installed from CRAN with:

``` r
install.packages("ForestElementsR")
```

## Example

Within this README file, we can only provide a limited set of examples
with brief explanations of how to use *ForestElementsR*. The package,
however, comes with three vignettes that provide a detailed introduction
and information about important features. See the section “More
Information” at the bottom of this file for how to access these
vignettes.

One of the ways the package can be used is a “pocket calculator
workstyle”. Assume, we have rough information about a Norway spruce
forest stand as follows: The number of trees is about 820 per ha, the
mean diameter is 25 cm, and the mean height is 22.5 m at a stand age of
60 years. An estimate of the stand volume is easily obtained as follows:

``` r
library(ForestElementsR)
```

    ## Lade nötiges Paket: sf

    ## Linking to GEOS 3.13.1, GDAL 3.11.0, PROJ 9.6.0; sf_use_s2() is TRUE

``` r
# Estimate the mean tree's merchantable wood volume over bark in m³ with the
# function v_gri; species_id = 1 relates to Norway spruce (see vignette about 
# tree species codings)
v_mean <- v_gri(species_id = 1, dbh_cm = 25, height_m = 22.5) 
v_mean
```

    ## [1] 0.5392409

``` r
v_mean * 820 # Stand volume per ha (820 trees per ha) 
```

    ## [1] 442.1775

As we also know the stand’s age and its mean height, we can determine
its site index according to an appropriate yield table:

``` r
si <- site_index(
  age = 60, 
  size = 22.5, 
  ytable = fe_ytable_spruce_gehrhardt_moderate_1921,
  si_variable = "h_q_m"
)

si
```

    ## [1] 1.486842

Using this site index, we can estimate the stand’s current annual
increment by looking it up in the yield table:

``` r
# Current annual volume increment in m³/ha/yr
iv <- ytable_lookup(age = 60, si = si, variable = "pai_m3_ha_yr",
              ytable = fe_ytable_spruce_gehrhardt_moderate_1921)
iv
```

    ## [1] 16.84474

As the information above also allows to calculate the stand’s basal
area, the stocking level (i.e. a measure of stand density related to the
yield table of interest) can be determined. This in turn is useful for
adjusting the increment estimate above:

``` r
# Calculate the stand's basal area in m²/ha (dbh^2 * pi/4 * stem number)
ba <- (25/100)^2 * pi/4 * 820
ba
```

    ## [1] 40.25166

``` r
sl <- stocking_level(ba, age = 60, si = si, 
                     ytable = fe_ytable_spruce_gehrhardt_moderate_1921)
sl
```

    ## [1] 0.9094797

``` r
# As the stocking level < 1, we correct the increment estimate above
iv_corr <- iv * sl
iv_corr
```

    ## [1] 15.31995

Assume, we have measured the stem diameters at breast height of some
trees in the stand we were dealing with above, and we would like to have
estimates for their individual heights. As we have information about the
stand’s mean height, mean diameter and its age, we can use a standard
height curve system for this task:

``` r
# Measured stem diameters at breast height (cm)
dbh <- c(19.1, 22.4, 21.3, 27.8, 23.5, 26.6, 31.2, 25.1, 33.7, 17.2)

# Height estimates (m) with a standard height curve system, species_id = 1 
# refers to Norway spruce (check documentation of the function h_standard_bv and
# the vignette about tree species codings)
h_est <- h_standard_bv(species_id = 1,
                       dbh_cm     = dbh,
                       age_yr     = 60,
                       d_q_cm     = 25,
                       h_q_m      = 22.5)
h_est
```

    ##  [1] 19.83220 21.44096 20.94121 23.47997 21.90867 23.07813 24.49230 22.53766
    ##  [9] 25.13652 18.73660

With the height estimates being available, the single tree volumes can
be easily obtained by using the function `v_gri`:

``` r
# Standing merchantable wood volumes over bark (m³)
v <- v_gri(species_id = 1, dbh_cm = dbh, height_m = h_est)
v
```

    ##  [1] 0.2747285 0.4122321 0.3633952 0.6935922 0.4640041 0.6252607 0.9043768
    ##  [8] 0.5444442 1.0749596 0.2080623

While the volume values above represent standing volumes over bark,
practitioners often prefer to work with under bark values that are, in
addition, reduced by harvest losses. For obtaining these, the function
`v_red_harvest_ubark` can be applied to the original volume values:

``` r
# Reduced volumes to under bark values and accounting for harvest losses
v_red_harvest_ubark(species_id = 1, v_orig_m3 = v)
```

    ##  [1] 0.2225301 0.3339080 0.2943501 0.5618097 0.3758433 0.5064612 0.7325452
    ##  [8] 0.4409998 0.8707173 0.1685304

Besides the “pocket calculator style”, another way of working with
*ForestElementsR* is using the classes it provides for representing data
and running evaluations on these. One example data set that comes with
the package is `mm_forest_1_fe_stand_spatial`; it represents a plot in a
mixed mountain forest that has been subsequently surveyed several times
on a single tree basis. It belongs to the family of `fe_stand` objects
(see vignette *The Package ForestElementsR*). For such objects, one
single function (`stand_sums_static`) calculates the most important
stand level sum and mean variables in one pass:

``` r
# Display species names in scientific notation
po <- options(fe_spec_lang = "sci")

# mm_forest_1_fe_stand_spatial is an example stand that comes with 
# ForestElementsR. Its single trees can be accessed and viewed as follows:
mm_forest_1_fe_stand_spatial$trees
```

    ## # A tibble: 353 × 12
    ##    tree_id species_id   layer_key time_yr age_yr dbh_cm height_m
    ##    <chr>   <tm_wwk_lng>     <dbl>   <int>  <dbl>  <dbl>    <dbl>
    ##  1 3       Picea abies          1    1975     NA   44.1     30.9
    ##  2 6       Picea abies          1    1975     NA   60.7     33.6
    ##  3 8       Picea abies          1    1975     NA   38.4     29.2
    ##  4 12      Picea abies          1    1975     NA   34.6     27.8
    ##  5 17      Picea abies          1    1975     NA   36.9     28.7
    ##  6 18      Picea abies          1    1975     NA   28.5     24.9
    ##  7 22      Picea abies          1    1975     NA   52.1     32.5
    ##  8 25      Picea abies          1    1975     NA   54.1     32.8
    ##  9 35      Picea abies          1    1975     NA   37.8     29.0
    ## 10 41      Picea abies          1    1975     NA   62.3     33.7
    ## # ℹ 343 more rows
    ## # ℹ 5 more variables: crown_base_height_m <dbl>, crown_radius_m <dbl>,
    ## #   removal <lgl>, ingrowth <lgl>, n_rep_ha <dbl>

``` r
# Stand sum and mean values on species level
sts_sta <- mm_forest_1_fe_stand_spatial |> stand_sums_static() 
sts_sta |> print(n = Inf)
```

    ## # A tibble: 22 × 9
    ## # Groups:   time_yr [5]
    ##    time_yr species_id          stem_number_ha basal_area_m2_ha d_q_cm d_dom_cm
    ##      <int> <tm_wwk_lng>                 <dbl>            <dbl>  <dbl>    <dbl>
    ##  1    1975 Picea abies                  143.            21.4    43.6     60.3 
    ##  2    1975 Abies alba                   101.             6.16   27.8     42.5 
    ##  3    1975 Fagus sylvatica              161.             3.83   17.4     29.5 
    ##  4    1975 Acer pseudoplatanus          101.             3.55   21.1     27.2 
    ##  5    1984 Picea abies                   71.6           14.1    50.2     64.0 
    ##  6    1984 Abies alba                    53.7            3.47   28.7     46.6 
    ##  7    1984 Fagus sylvatica               89.5            3.22   21.4     36.7 
    ##  8    1984 Acer pseudoplatanus           59.7            2.54   23.3     31.4 
    ##  9    1995 Picea abies                   71.6           15.8    52.9     67.6 
    ## 10    1995 Abies alba                    41.8            3.87   34.4     50.2 
    ## 11    1995 Fagus sylvatica               89.5            4.39   25.0     41.4 
    ## 12    1995 Acer pseudoplatanus           53.7            2.72   25.4     34.2 
    ## 13    2004 Picea abies                   77.6           16.4    51.9     68.6 
    ## 14    2004 Abies alba                    47.7            4.62   35.1     53.5 
    ## 15    2004 Fagus sylvatica              101.             5.26   25.7     42.8 
    ## 16    2004 Fraxinus excelsior            41.8            0.196   7.73     8.94
    ## 17    2004 Acer pseudoplatanus           71.6            3.05   23.3     35.4 
    ## 18    2015 Picea abies                   41.8            6.65   45.0     71.5 
    ## 19    2015 Abies alba                    59.7            5.70   34.9     57.2 
    ## 20    2015 Fagus sylvatica              149.             5.41   21.5     39.7 
    ## 21    2015 Fraxinus excelsior           101.             0.634   8.92    11.5 
    ## 22    2015 Acer pseudoplatanus           95.5            3.25   20.8     36.4 
    ## # ℹ 3 more variables: h_q_m <dbl>, h_dom_m <dbl>, v_m3_ha <dbl>

``` r
# Stand volume sums of all species
sts_sta |> dplyr::group_by(time_yr) |> dplyr::summarise(v_m3_ha = sum(v_m3_ha))
```

    ## # A tibble: 5 × 2
    ##   time_yr v_m3_ha
    ##     <int>   <dbl>
    ## 1    1975    444.
    ## 2    1984    327.
    ## 3    1995    363.
    ## 4    2004    409.
    ## 5    2015    282.

Similarly, the stand level volume and basal area increments can be
obtained in one go when subsequent surveys are represented in the
`fe_stand` object of interest:

``` r
# Stand level increments on species level
sts_dyn <- mm_forest_1_fe_stand_spatial |> stand_sums_dynamic()
```

    ## Joining with `by = join_by(species_id, time_yr)`
    ## Joining with `by = join_by(time_yr, species_id)`
    ## Joining with `by = join_by(time_yr, species_id)`

``` r
sts_dyn |> print(n = Inf)
```

    ## # A tibble: 22 × 4
    ##    time_yr species_id          iba_m2_ha_yr iv_m3_ha_yr
    ##      <int> <tm_wwk_lng>               <dbl>       <dbl>
    ##  1    1975 Picea abies              NA          NA     
    ##  2    1984 Picea abies               0.176       3.94  
    ##  3    1995 Picea abies               0.146       1.52  
    ##  4    2004 Picea abies               0.0745      1.58  
    ##  5    2015 Picea abies               0.0337      0.311 
    ##  6    1975 Abies alba               NA          NA     
    ##  7    1984 Abies alba                0.0598      1.10  
    ##  8    1995 Abies alba                0.0532      0.574 
    ##  9    2004 Abies alba                0.0825      1.49  
    ## 10    2015 Abies alba                0.0981      1.59  
    ## 11    1975 Fagus sylvatica          NA          NA     
    ## 12    1984 Fagus sylvatica           0.101       1.72  
    ## 13    1995 Fagus sylvatica           0.107       1.33  
    ## 14    2004 Fagus sylvatica           0.0964      1.32  
    ## 15    2015 Fagus sylvatica           0.170       2.13  
    ## 16    1975 Acer pseudoplatanus      NA          NA     
    ## 17    1984 Acer pseudoplatanus       0.0354      0.794 
    ## 18    1995 Acer pseudoplatanus       0.0289      0.0709
    ## 19    2004 Acer pseudoplatanus       0.0369      0.671 
    ## 20    2015 Acer pseudoplatanus       0.0577      0.462 
    ## 21    2004 Fraxinus excelsior       NA          NA     
    ## 22    2015 Fraxinus excelsior        0.0399      0.175

``` r
# Stand level increment sums of all species
sts_dyn |> 
  dplyr::group_by(time_yr) |> 
  dplyr::summarise(iv_m3_ha_yr = sum(iv_m3_ha_yr, na.rm = TRUE))
```

    ## # A tibble: 5 × 2
    ##   time_yr iv_m3_ha_yr
    ##     <int>       <dbl>
    ## 1    1975        0   
    ## 2    1984        7.56
    ## 3    1995        3.49
    ## 4    2004        5.06
    ## 5    2015        4.67

``` r
# Reset species display language to previous setting
options(po)
```

## More Information

For more information and details, we currently provide three vignettes
that come with the package. We suggest to read the main vignette, *The
Package ForestElementsR*, first. The other two vignettes *Tree Species
Codings in ForestElementsR*, and *Yield Tables in ForestElementsR*
provide in-detail information about two of the package’s major features.
With`browseVignettes("ForestElementsR")` you obtain a link to each
available vignette. You can also call each vignette directly with one of
the following commands:

``` r
vignette("forestelementsr_package")
vignette("tree_species_codings")
vignette("yield_tables")
```

## Acknowledgments

The authors would like to thank the Bavarian Ministry for Nutrition,
Agriculture, Forestry, and Tourism for funding the projects *FeNEU: Ein
innovatives Instrument* *für die forstliche Planung in Bayern* (E062)
and *Ertragskundliche Betreuung der* *langfristigen Versuche* (W007).
