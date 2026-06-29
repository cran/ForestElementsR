# ForestElementsR 3.0.0

Major overhaul of the species coding system.

* Species codings may now be **hierarchical**: a species can appear both as an
  individual code and inside one or more coarser group codes of the same coding
  (a laminar family of species sets). Coding tables gained a `level` column
  (0 = finest leaf, higher numbers = coarser groups). `bavrn_state` and
  `tum_wwk_long` are now hierarchical.
* Codings may carry **non-tree codes** — categories without a tree-species
  equivalent, such as the Bavarian state code `99` ("Strauch", shrub). Whether a
  code is a tree code is **derived** from its link to the
  `species_master_table`, so there is no separate flag to keep in sync. New
  exported helpers `fe_species_is_tree()` and `fe_species_non_tree_codes()`
  report this.
  - `fe_stand()` and its spatial children now **reject** non-tree species codes
    with a clear error.
  - Casting a non-tree code into another coding yields `NA` (with a message)
    instead of raising an error.
* **Casting** into a hierarchical coding resolves each species to the finest
  code that contains it. A cast that loses information (aggregating finer codes
  into a coarser group) is now signalled with a **message** instead of a
  **warning**; forward-ambiguous casts still raise an error. A small documented
  dataset, `species_cast_overrides`, declares deliberate target codes for
  otherwise forward-ambiguous casts.
* New exported, CSV-driven builders for maintaining the codings and the master
  table: `master_template_csv()`, `master_table_from_csv()`,
  `coding_template_from_master()`, `coding_table_from_template()`, and
  `cast_overrides_from_csv()`. Coding tables are now stored in a canonical row
  order (level, then code, then master key), and the builder rejects
  inconsistent or conflicting entries early.
* Extended the species data: new Bavarian state forest species and codes,
  `tum_wwk_long` filled out with grouping levels and now covering all master
  species, and additional master species (e.g. the genera *Cedrus* and
  *Corylus*).
* Added two cast overrides for `bavrn_state` -> `tum_wwk_short`: the oak group
  code `70` ("Eiche (Gruppe)") now resolves to `6` (oak), and the
  "other hardwood" code `80` ("Sonstige Laubhölzer") to `8` (other hardwood).
  Both casts were forward-ambiguous (`70` straddles `{6, 8}` via red oak, `80`
  straddles `{8, 9}` hard/soft broadleaf) and therefore errored. This unblocks
  downstream casting of BaySF inventory data carrying these group codes.
* New exported helper `fe_species_get_field_table()` returns a compact,
  field-usable lookup table for a coding: each code exactly once, with the
  coding's own species/group names (all three languages), plus the `level`
  and `is_tree` columns, in canonical order. Rendering it into a printable
  document is left to downstream packages.
* The "Tree Species Codings" vignette was updated to cover the overhauled
  system: hierarchical codings and the `level` column, non-tree codes and
  `is_tree`, the finest-node cast and the cast-override mechanism, the cast
  information-loss message (formerly a warning), the new field table, and a
  rewritten "Information for developers" part documenting the CSV-driven
  builder workflow.

Other news  
  
* Exported the object validation helpers `has_required_names`,
  `has_required_types_or_classes`, `has_no_missing_values`, and `is_distinct`
  (previously internal). They remain marked with `@keywords internal`, but are
  now part of the public interface so that dependent packages (e.g. `FeNEU`)
  can call them with `::` instead of relying on the non-exported `:::`
  mechanism, which is not permitted on CRAN.
* Promoted `wood_density_tum_wwk_short` from an internal object (in
  `sysdata.rda`) to a documented, exported dataset. It is now accessible as
  `ForestElementsR::wood_density_tum_wwk_short`.

***
# ForestElementsR 2.2.0

* Implemented S3 function get_center() that retrieves the coordinates of a 
  meaningful center point for fe_stand childs that carry spatial information.
* The yield table related functions `ytable_age_slice`, `ytable_max_slice`,
  `site_index`, `ytable_lookup`, `si_to_mai_age`, `si_to_mai_max`, and
  `stocking_level` were made more safe. These functions are not designed for
  working with input variable vectors of length > 1. However, under certain
  circumstances such input would trigger a warning, but produce dubious results.
  Now, any attempt to provide input vectors with more than one element to these 
  functions results in an error.
* Edited all three vignettes.
  
***
# ForestElementsR 2.1.0

* Implemented automated tests for stand_sums_static()
* Revised function stand_sums_static(); new version of v_gri() allowed to 
  simplify the code.
* Modified the important function v_gri() to make it more tolerant. In case of 
  missing height values, it does not stop anymore but returns NA and issues a 
  warning.

***
# ForestElementsR 2.0.1

Revised CRAN submission

We have addressed the issues raised by Konstanze Lauseker in her review of the 
initial submission:

* Added references to the description field of the `DESCRIPTION` file.  
* Removed the use of `:::` even in internal documentation.  
* Replaced `\dontrun{}` with `try()` for examples that intentionally generate 
  errors.  
* Substituted `options(warn = -1)` with `suppressWarnings()` in the automated 
  tests.  

***
# ForestElementsR 2.0.0

Initial CRAN submission.
