# license GPL-3
# This file is part of the R-package ForestElementsR.
#
# ForestElementsR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ForestElementsR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ForestElementsR.  If not, see <https://www.gnu.org/licenses/>.




# Tools for building species coding tables from a human-editable template
# ("species-indexed + parent_code" format, see coding_template_from_master).


# Columns of the template / builder input, in canonical order
.coding_template_cols <- c(
  "species_id", "parent_code", "genus", "species_no",
  "name_sci", "name_eng", "name_ger"
)


# Trim whitespace and turn "" into NA (character)
.blank_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}


# Declarative registry of "aggregation" pairs: a coarser ("short") coding that
# must be a valid aggregation (coarsening) of a finer parent coding, so that the
# cast finer -> short always works. Keyed by the aggregating coding. Add a row
# here when a new short/long pair is introduced.
.coding_aggregations <- function() {
  tibble::tribble(
    ~coding,             ~aggregate_of,
    "bavrn_state_short", "bavrn_state",
    "tum_wwk_short",     "tum_wwk_long"
  )
}


# Look up the parent (finer) coding a given coding aggregates, or NA if none.
.aggregate_parent_of <- function(coding) {
  if (is.null(coding) || is.na(coding)) return(NA_character_)
  reg <- .coding_aggregations()
  hit <- reg$aggregate_of[reg$coding == coding]
  if (length(hit) == 1L) hit else NA_character_
}


# For each master species, the coarsest (highest-level) code in `parent_table`
# that contains it, plus that code's German name. Species sharing a value here
# belong to one group in the parent coding and therefore MUST share a single
# code in the aggregating ("short") coding - the editing hint of stage 1.
.coarsest_code_per_species <- function(parent_table) {
  link <- !is.na(parent_table$genus) & !is.na(parent_table$species_no)
  pt   <- parent_table[link, , drop = FALSE]
  lvl  <- if ("level" %in% names(pt)) pt$level else rep(0L, nrow(pt))
  ord  <- order(lvl, decreasing = TRUE) # coarsest (highest level) first
  pt   <- pt[ord, , drop = FALSE]
  key  <- paste(pt$genus, pt$species_no, sep = "\r")
  keep <- !duplicated(key)              # first occurrence = coarsest per species
  tibble::tibble(
    genus        = pt$genus[keep],
    species_no   = pt$species_no[keep],
    agg_from_id  = pt$species_id[keep],
    agg_from_ger = pt$name_ger[keep]
  )
}


# Stage-2 aggregation guard: assert that `child_table` is a valid coarsening of
# `parent_table` - every code (group) in the parent maps to a single code in the
# child, and every parent species is covered. This is what guarantees the cast
# parent -> child never fails. Non-tree codes (no master link) are excluded;
# they cast to NA by design.
.assert_coding_coarsens <- function(child_table, parent_table,
                                    coding_name = NULL, parent_name = NULL) {
  cl <- if (is.null(coding_name)) "" else paste0(" '", coding_name, "'")
  pl <- if (is.null(parent_name)) "its parent coding" else
    paste0("'", parent_name, "'")

  # finest child code per master species
  child <- .finest_per_species(child_table)
  clink <- !is.na(child$genus) & !is.na(child$species_no)
  child <- child[clink, , drop = FALSE]
  child_code <- stats::setNames(
    child$species_id, paste(child$genus, child$species_no, sep = "\r")
  )

  # parent species rows (groups carry their members; non-tree codes excluded)
  plink <- !is.na(parent_table$genus) & !is.na(parent_table$species_no)
  pt    <- parent_table[plink, , drop = FALSE]
  pt$child_code <- unname(
    child_code[paste(pt$genus, pt$species_no, sep = "\r")]
  )

  msg <- character(0)

  # (a) coverage: every parent species must reach a child code
  miss <- is.na(pt$child_code)
  if (any(miss)) {
    sp <- unique(paste(pt$genus[miss], pt$species_no[miss]))
    msg <- c(msg, sprintf("species not covered: %s",
                          paste(sp, collapse = ", ")))
  }

  # (b) no split: all members of a parent code must share one child code
  split <- tapply(pt$child_code, pt$species_id, function(z) {
    length(unique(z[!is.na(z)])) > 1L
  })
  bad <- names(split)[!is.na(split) & split]
  if (length(bad)) {
    detail <- vapply(bad, function(code) {
      cc <- sort(unique(
        pt$child_code[pt$species_id == code & !is.na(pt$child_code)]
      ))
      sprintf("%s -> {%s}", code, paste(cc, collapse = ", "))
    }, character(1))
    msg <- c(msg, sprintf("parent group(s) split across several codes: %s",
                          paste(detail, collapse = "; ")))
  }

  if (length(msg)) {
    stop(sprintf("Coding%s is not a valid aggregation of %s: %s.",
                 cl, pl, paste(msg, collapse = "; ")), call. = FALSE)
  }
  invisible(child_table)
}



#' Create a Template for Building a Species Coding Table
#'
#' Produces a template (in the "species-indexed + parent_code" format) that can
#' be filled in (e.g. in a spreadsheet) and turned into a coding table with
#' \code{\link{coding_table_from_template}}. It always lists \emph{every} master
#' species (see \code{\link{species_master_table}}), in master order, carrying
#' the spelled-out master names for orientation. When prefilled from an existing
#' coding, each species also carries that coding's \code{species_id} and names;
#' species the coding does \emph{not} cover are left with a blank
#' \code{species_id}, so missing coverage is visible at a glance (such rows are
#' dropped when the coding table is built).
#'
#' Columns:
#' \describe{
#'   \item{species_id}{the code a species (or group) gets}
#'   \item{parent_code}{the next coarser code this code rolls up into (blank if
#'     none); expresses the grouping hierarchy}
#'   \item{genus, species_no}{the master species key (blank on a group-only
#'     declaration row)}
#'   \item{master_name_sci, master_name_ger}{the spelled-out names of the
#'     individual master species, for orientation while editing.
#'     \strong{Reference only - ignored by \code{coding_table_from_template}};
#'     to change a coding's display names edit \code{name_*} instead}
#'   \item{agg_from_id, agg_from_ger}{only present when \code{aggregate_of} is in
#'     play: the coarsest code (and its German name) the species has in the
#'     finer parent coding. \strong{Reference only}; species sharing an
#'     \code{agg_from_id} must get the same \code{species_id} here}
#'   \item{name_sci, name_eng, name_ger}{the coding-specific names (the species'
#'     name for a single-species code, the group name on a group's rows). Blank
#'     is allowed and then inherits the master name for a single species; a
#'     group code needs a name on at least one of its rows}
#' }
#'
#' @param coding Optional name of an existing coding (see
#'   \code{\link{species_codings}}). If given, the template is prefilled with
#'   that coding's current assignment, which is convenient for editing an
#'   existing coding. If \code{NULL} (default), an empty template with one row
#'   per master species is returned.
#'
#' @param file Optional path. If given, the template is written there as a CSV
#'   (blank for \code{NA}); the template is always returned (invisibly when
#'   written to a file). The CSV is written as UTF-8 with a byte-order mark (so
#'   umlauts display correctly in a spreadsheet) using \code{sep} as the field
#'   separator.
#'
#' @param sep Field separator used when writing to \code{file}. Defaults to
#'   \code{";"} (what a German-locale Excel expects); use \code{","} for a
#'   classic comma-separated file.
#'
#' @param aggregate_of Optional name of a finer "parent" coding that the coding
#'   to be built is meant to aggregate (coarsen). If given - or known from the
#'   built-in registry of aggregation pairs, e.g. \code{"bavrn_state_short"} is
#'   recognised as an aggregation of \code{"bavrn_state"} - the template gains
#'   two \strong{reference-only} columns, \code{agg_from_id} and
#'   \code{agg_from_ger}: the coarsest code (and its German name) each master
#'   species has in the parent coding. The template is sorted by these so that
#'   species belonging to one parent group sit together. \strong{All species
#'   sharing an \code{agg_from_id} must get the same \code{species_id}} in the
#'   coding being built, otherwise the cast parent -> this coding would break
#'   (\code{\link{coding_table_from_template}} enforces this when given the
#'   parent table).
#'
#' @return A \code{tibble} in the template format described above
#'
#' @seealso \code{\link{coding_table_from_template}}
#'
#' @export
#'
#' @examples
#' # Empty template (one row per master species)
#' coding_template_from_master() |> head()
#'
#' # Prefilled from an existing coding (good starting point for edits)
#' coding_template_from_master("bavrn_state") |> head()
#'
#' # Template for an aggregating coding, with parent-group hint columns
#' coding_template_from_master("bavrn_state_short") |> head()
#'
coding_template_from_master <- function(coding = NULL, file = NULL, sep = ";",
                                        aggregate_of = NULL) {
  master <- ForestElementsR::species_master_table

  # One row per master species (in master order). A coding that does not cover
  # a species leaves its species_id blank - so missing coverage is visible.
  tmpl <- tibble::tibble(
    species_id      = NA_character_,
    parent_code     = NA_character_,
    genus           = master$genus,
    species_no      = master$species_no,
    master_name_sci = master$name_sci,
    master_name_ger = master$name_ger,
    name_sci        = NA_character_,
    name_eng        = NA_character_,
    name_ger        = NA_character_
  )

  if (!is.null(coding)) {
    ct      <- fe_species_get_coding_table(coding)
    sp      <- !is.na(ct$genus) & !is.na(ct$species_no)
    ct_sp   <- ct[sp, , drop = FALSE]
    # Match each master species to its row in the coding (NA = not covered)
    idx <- match(
      paste(master$genus, master$species_no, sep = "\r"),
      paste(ct_sp$genus, ct_sp$species_no, sep = "\r")
    )
    tmpl$species_id <- ct_sp$species_id[idx]
    tmpl$name_sci   <- ct_sp$name_sci[idx]
    tmpl$name_eng   <- ct_sp$name_eng[idx]
    tmpl$name_ger   <- ct_sp$name_ger[idx]

    # Coding rows not tied to a master species (e.g. non-tree codes)
    decl <- ct[!sp, , drop = FALSE]
    if (nrow(decl) > 0) {
      tmpl <- dplyr::bind_rows(
        tmpl,
        tibble::tibble(
          species_id      = decl$species_id,
          parent_code     = NA_character_,
          genus           = NA_character_,
          species_no      = NA_character_,
          master_name_sci = NA_character_,
          master_name_ger = NA_character_,
          name_sci        = decl$name_sci,
          name_eng        = decl$name_eng,
          name_ger        = decl$name_ger
        )
      )
    }

    # Sort by this coding's own order (code, then species) so members of a code
    # sit together; species the coding does not cover (blank species_id) go last.
    tmpl <- tmpl[order(
      is.na(tmpl$species_id),
      suppressWarnings(as.numeric(tmpl$species_id)),
      tmpl$genus, tmpl$species_no
    ), , drop = FALSE]
  }

  # Stage-1 aggregation hint: if the coding to be built aggregates a finer
  # parent coding, show each species' coarsest parent group and cluster by it.
  if (is.null(aggregate_of)) aggregate_of <- .aggregate_parent_of(coding)
  if (!is.na(aggregate_of)) {
    agg <- .coarsest_code_per_species(fe_species_get_coding_table(aggregate_of))
    tmpl <- dplyr::left_join(tmpl, agg, by = c("genus", "species_no"))
    tmpl <- dplyr::relocate(tmpl, "agg_from_id", "agg_from_ger",
                            .after = "master_name_ger")
    # Cluster species of one parent group together; uncovered rows go last.
    tmpl <- tmpl[order(
      is.na(tmpl$agg_from_id),
      suppressWarnings(as.numeric(tmpl$agg_from_id)),
      tmpl$genus, tmpl$species_no
    ), , drop = FALSE]
  }

  if (!is.null(file)) {
    .write_csv_excel(tmpl, file, sep = sep)
    return(invisible(tmpl))
  }
  tmpl
}



#' Build a Species Coding Table from a Filled Template
#'
#' Turns a filled template (see \code{\link{coding_template_from_master}}, the
#' "species-indexed + parent_code" format) into a complete coding table:
#' resolves the grouping hierarchy from the \code{parent_code} links, derives
#' the \code{level} column, fills missing species names from the
#' \code{\link{species_master_table}}, attaches \code{deciduous_conifer}, and
#' validates laminarity.
#'
#' The \code{is_tree} column is \strong{derived}, not declared: a code is a tree
#' code if and only if it resolves to at least one master species. A code that
#' resolves to no master species (e.g. a "shrub" category) is treated as a
#' non-tree code (\code{is_tree = FALSE}, no master link) and reported with a
#' message, so the master table stays the single source of truth for what is a
#' tree. A row naming a concrete species that is \emph{not} in the master table
#' remains a hard error.
#'
#' Naming rule: a code's names are taken from any non-blank name in its rows
#' (a group's declaration row, or an overriding species row); if none are given,
#' a single-species code inherits the master name, while a group code without a
#' name is an error.
#'
#' @param template A template \code{data.frame}/\code{tibble} in the format of
#'   \code{\link{coding_template_from_master}}, or a path to a CSV file holding
#'   such a template. A CSV is read with automatic detection of the field
#'   separator (\code{";"} vs \code{","}) and of the encoding (UTF-8 with or
#'   without BOM, falling back to Windows-1252).
#'
#' @param coding_name Optional coding name used in validation error messages.
#'
#' @param sep Field separator to assume when \code{template} is a CSV file. By
#'   default (\code{NULL}) it is detected automatically from the file.
#'
#' @param parent_table Optional coding table of a finer "parent" coding that the
#'   coding being built is meant to aggregate (coarsen). When given, the result
#'   is checked to be a valid aggregation: every group of the parent must map to
#'   a single code here and every parent species must be covered, so that the
#'   cast parent -> this coding always works. Use this when (re)building a
#'   "short" coding (see \code{\link{coding_template_from_master}}'s
#'   \code{aggregate_of}).
#'
#' @param parent_name Optional name of the parent coding, used only in the
#'   aggregation error message.
#'
#' @return A \code{tibble} coding table with columns \code{species_id},
#'   \code{genus}, \code{species_no}, \code{deciduous_conifer}, \code{name_sci},
#'   \code{name_eng}, \code{name_ger}, \code{level}, \code{is_tree}.
#'
#' @seealso \code{\link{coding_template_from_master}}
#'
#' @export
#'
#' @examples
#' # Round trip: build the current bavrn_state coding from its own template
#' tmpl <- coding_template_from_master("bavrn_state")
#' ct   <- coding_table_from_template(tmpl, "bavrn_state")
#' head(ct)
#'
coding_table_from_template <- function(template, coding_name = NULL,
                                        sep = NULL, parent_table = NULL,
                                        parent_name = NULL) {
  if (is.character(template) && length(template) == 1L) {
    template <- .read_csv_flexible(template, sep = sep)
  }
  tmpl <- tibble::as_tibble(template)
  cn   <- if (is.null(coding_name)) "" else paste0(" '", coding_name, "'")

  # --- normalise ----------------------------------------------------------
  for (cc in c("species_id", "parent_code", "genus", "species_no",
               "name_sci", "name_eng", "name_ger")) {
    if (!cc %in% names(tmpl)) tmpl[[cc]] <- NA_character_
    tmpl[[cc]] <- .blank_to_na(tmpl[[cc]])
  }

  # A blank species_id marks a master species that this coding does not cover;
  # such rows are dropped (the convertibility invariant tests catch unintended
  # gaps, e.g. a species that should reach tum_wwk_short).
  tmpl <- tmpl[!is.na(tmpl$species_id), , drop = FALSE]

  master <- ForestElementsR::species_master_table

  # Normalise species_no to three digits with leading zeros (a spreadsheet
  # turning "001" into 1 is harmless); only where a species_no is given.
  has_no <- !is.na(tmpl$species_no)
  bad_no <- has_no & !grepl("^[0-9]{1,3}$", tmpl$species_no)
  if (any(bad_no)) {
    stop(sprintf("Coding%s: species_no must be one to three digits; offending: %s",
                 cn, paste(unique(tmpl$species_no[bad_no]), collapse = ", ")),
         call. = FALSE)
  }
  tmpl$species_no[has_no] <- sprintf("%03d", as.integer(tmpl$species_no[has_no]))

  # --- parent map (one parent per code) -----------------------------------
  parent_map <- tapply(tmpl$parent_code, tmpl$species_id, function(z) {
    u <- unique(stats::na.omit(z))
    if (length(u) > 1L) {
      stop(sprintf("Coding%s: code has conflicting parent_code values: %s",
                   cn, paste(u, collapse = ", ")), call. = FALSE)
    }
    if (length(u) == 0L) NA_character_ else u
  })
  parent_map <- stats::setNames(as.character(parent_map), names(parent_map))

  # ancestors of a code along the parent chain (cycle-safe)
  ancestors <- function(code) {
    out <- character(0)
    seen <- code
    cur <- parent_map[[code]]
    while (!is.na(cur)) {
      if (cur %in% seen) {
        stop(sprintf("Coding%s: parent_code cycle at '%s'.", cn, cur),
             call. = FALSE)
      }
      out  <- c(out, cur)
      seen <- c(seen, cur)
      cur  <- if (cur %in% names(parent_map)) parent_map[[cur]] else NA_character_
    }
    out
  }

  # level = nesting depth (leaf = 0, parent = 1 + max child level)
  level_of <- function(code) {
    kids <- names(parent_map)[!is.na(parent_map) & parent_map == code]
    if (length(kids) == 0L) return(0L)
    1L + max(vapply(kids, level_of, integer(1)))
  }

  # --- species rows vs. declaration rows ----------------------------------
  is_spec <- !is.na(tmpl$genus) & !is.na(tmpl$species_no)
  spec    <- tmpl[is_spec, , drop = FALSE]

  # every master key referenced must exist
  mkey   <- paste(master$genus, master$species_no, sep = "\r")
  s_key  <- paste(spec$genus, spec$species_no, sep = "\r")
  bad    <- setdiff(unique(s_key), mkey)
  if (length(bad) > 0L) {
    bad_disp <- gsub("\r", " ", bad)
    stop(sprintf("Coding%s: species not in master table: %s",
                 cn, paste(bad_disp, collapse = "; ")), call. = FALSE)
  }

  # --- build (code x species) membership including ancestors --------------
  out_list <- vector("list", nrow(spec))
  for (i in seq_len(nrow(spec))) {
    code  <- spec$species_id[i]
    codes <- c(code, ancestors(code))
    out_list[[i]] <- tibble::tibble(
      species_id = codes,
      genus      = spec$genus[i],
      species_no = spec$species_no[i]
    )
  }
  body <- if (length(out_list)) dplyr::bind_rows(out_list) else
    tibble::tibble(species_id = character(), genus = character(),
                   species_no = character())
  body <- dplyr::distinct(body)

  # --- names per code -----------------------------------------------------
  # A code's rows must not disagree on a name: more than one distinct non-blank
  # value in a language is an editing error (the builder would otherwise silently
  # keep the first). Fail early and name the offending code(s).
  for (lang in c("name_sci", "name_eng", "name_ger")) {
    vals_per_code <- split(tmpl[[lang]], tmpl$species_id)
    n_distinct <- vapply(vals_per_code,
                         function(z) length(unique(z[!is.na(z)])), integer(1))
    bad <- names(n_distinct)[n_distinct > 1L]
    if (length(bad)) {
      detail <- vapply(bad, function(code) {
        v <- unique(tmpl[[lang]][tmpl$species_id == code & !is.na(tmpl[[lang]])])
        sprintf("'%s' {%s}", code, paste(v, collapse = " / "))
      }, character(1))
      stop(sprintf("Coding%s: conflicting %s for code(s): %s",
                   cn, lang, paste(detail, collapse = "; ")), call. = FALSE)
    }
  }

  first_non_na <- function(z) {
    z <- z[!is.na(z)]
    if (length(z)) z[[1]] else NA_character_
  }
  name_tab <- tmpl |>
    dplyr::group_by(.data$species_id) |>
    dplyr::summarise(
      name_sci = first_non_na(.data$name_sci),
      name_eng = first_non_na(.data$name_eng),
      name_ger = first_non_na(.data$name_ger),
      .groups  = "drop"
    )

  # master names for inheritance
  m_sci <- stats::setNames(master$name_sci, mkey)
  m_eng <- stats::setNames(master$name_eng, mkey)
  m_ger <- stats::setNames(master$name_ger, mkey)
  m_dc  <- stats::setNames(master$deciduous_conifer, mkey)

  # set size per code (number of master species)
  set_size <- table(body$species_id)

  # resolve a code's display names. For a single-species code, any language
  # left blank is inherited from the master table (per language). For group
  # codes there is nothing to inherit; missing languages stay NA and are
  # reported by the completeness check below.
  resolve_names <- function(code) {
    nm   <- name_tab[name_tab$species_id == code, ]
    vals <- c(nm$name_sci, nm$name_eng, nm$name_ger)
    sz   <- if (code %in% names(set_size)) as.integer(set_size[[code]]) else 0L
    if (sz == 1L) {
      k <- paste(body$genus[body$species_id == code],
                 body$species_no[body$species_id == code], sep = "\r")
      master_vals <- c(m_sci[[k]], m_eng[[k]], m_ger[[k]])
      vals[is.na(vals)] <- master_vals[is.na(vals)]
    }
    vals
  }

  # --- assemble tree-code rows -------------------------------------------
  body$deciduous_conifer <- unname(
    m_dc[paste(body$genus, body$species_no, sep = "\r")]
  )
  nm_codes <- unique(body$species_id)
  nm_mat   <- t(vapply(nm_codes, resolve_names, character(3)))
  nm_df    <- tibble::tibble(
    species_id = nm_codes,
    name_sci   = unname(nm_mat[, 1]),
    name_eng   = unname(nm_mat[, 2]),
    name_ger   = unname(nm_mat[, 3])
  )
  body <- dplyr::left_join(body, nm_df, by = "species_id")

  # --- non-tree codes: declared codes that resolve to no master species ---
  # is_tree is not declared but derived: a code that never reaches a master
  # species (e.g. BaySF 99 "Strauch") is a non-tree category. It stays usable
  # as a label but carries no master link. A concrete species missing from the
  # master is a hard error above, so anything landing here is a deliberate
  # container code; the message lets the user add it to the master table
  # instead, should the code actually denote a tree.
  nontree_ids <- setdiff(unique(tmpl$species_id), unique(body$species_id))
  if (length(nontree_ids)) {
    nt <- name_tab[name_tab$species_id %in% nontree_ids, ]
    nontree <- tibble::tibble(
      species_id        = nt$species_id,
      genus             = NA_character_,
      species_no        = NA_character_,
      deciduous_conifer = NA_character_,
      name_sci          = nt$name_sci,
      name_eng          = nt$name_eng,
      name_ger          = nt$name_ger
    )
    body <- dplyr::bind_rows(body, nontree)
    labels <- ifelse(is.na(nt$name_ger), nt$species_id,
                     sprintf("%s (%s)", nt$species_id, nt$name_ger))
    message(sprintf(
      "Coding%s: species code(s) without a master link, treated as non-tree: %s",
      cn, paste(labels, collapse = ", ")
    ))
  }

  # --- every code needs names in all three languages ---------------------
  # (otherwise format() would later return NA for the missing language)
  miss <- is.na(body$name_sci) | is.na(body$name_eng) | is.na(body$name_ger)
  if (any(miss)) {
    detail <- vapply(unique(body$species_id[miss]), function(code) {
      r     <- body[body$species_id == code, ][1, ]
      langs <- c("name_sci", "name_eng", "name_ger")[
        is.na(c(r$name_sci, r$name_eng, r$name_ger))
      ]
      sprintf("'%s' (%s)", code, paste(langs, collapse = ", "))
    }, character(1))
    stop(sprintf(
      paste0("Coding%s: every code needs a name in all three languages ",
             "(sci/eng/ger). Missing for: %s."),
      cn, paste(detail, collapse = "; ")
    ), call. = FALSE)
  }

  # --- level and is_tree per code ----------------------------------------
  # is_tree is derived from the master link: a code is a tree code iff it
  # resolves to at least one master species. Non-tree codes (added above) have
  # no master link and so are FALSE. This makes the master table the single
  # source of truth and removes any separately maintained is_tree column.
  body$level   <- vapply(body$species_id, level_of, integer(1))
  body$is_tree <- !is.na(body$genus) & !is.na(body$species_no)

  # Canonical row order: level ascending (leaves before the groups that contain
  # them), then the code, then the master key. Master is unaffected (all level 0
  # and non-numeric species_id, so it stays in genus/species_no order).
  body <- body |>
    dplyr::select("species_id", "genus", "species_no", "deciduous_conifer",
                  "name_sci", "name_eng", "name_ger", "level", "is_tree") |>
    dplyr::arrange(.data$level,
                   suppressWarnings(as.numeric(.data$species_id)),
                   .data$genus, .data$species_no)

  # --- validate -----------------------------------------------------------
  .validate_coding_laminar(body, coding_name)
  if (!is.null(parent_table)) {
    .assert_coding_coarsens(body, parent_table, coding_name, parent_name)
  }

  body
}


# Canonical columns of a cast-override table
.cast_override_cols <- c("coding_from", "coding_to",
                         "species_id_from", "species_id_to")


# Validate that each declared override is both necessary and valid: the codings
# exist, the codes exist in them, the source -> goal cast for that source code
# is genuinely forward-ambiguous (more than one goal code among its species),
# and the chosen target is one of those goal codes. Reads the involved coding
# tables (they must already be installed).
.validate_cast_overrides <- function(ov) {
  known <- ForestElementsR::species_codings$species_coding

  for (i in seq_len(nrow(ov))) {
    cf <- ov$coding_from[i]; ctg <- ov$coding_to[i]
    sf <- ov$species_id_from[i]; st <- ov$species_id_to[i]
    tag <- sprintf("override %s/%s '%s' -> '%s'", cf, ctg, sf, st)

    if (!cf %in% known || !ctg %in% known) {
      stop(sprintf("%s: unknown coding name(s).", tag), call. = FALSE)
    }
    tab_from <- fe_species_get_coding_table(cf)
    tab_to   <- .finest_per_species(fe_species_get_coding_table(ctg))
    if (!sf %in% tab_from$species_id) {
      stop(sprintf("%s: source code not in '%s'.", tag, cf), call. = FALSE)
    }
    if (!st %in% tab_to$species_id) {
      stop(sprintf("%s: target code not in '%s'.", tag, ctg), call. = FALSE)
    }

    # goal codes the source code's species map to
    spp <- tab_from[tab_from$species_id == sf &
                      !is.na(tab_from$genus) & !is.na(tab_from$species_no), ]
    tgt <- tab_to$species_id[match(
      paste(spp$genus, spp$species_no, sep = "\r"),
      paste(tab_to$genus, tab_to$species_no, sep = "\r")
    )]
    targets <- unique(tgt[!is.na(tgt)])
    if (anyNA(tgt)) {
      stop(sprintf("%s: some species of '%s' have no match in '%s'.",
                   tag, sf, ctg), call. = FALSE)
    }
    if (length(targets) < 2L) {
      stop(sprintf(paste0("%s: cast is not forward-ambiguous (source maps to ",
                          "the single code '%s') - no override needed."),
                   tag, targets[[1]]), call. = FALSE)
    }
    if (!st %in% targets) {
      stop(sprintf(paste0("%s: target is not one of the straddled goal codes ",
                          "{%s}."), tag, paste(targets, collapse = ", ")),
           call. = FALSE)
    }
  }
  invisible(ov)
}


#' Build a Cast-Override Table from a CSV
#'
#' Reads the declarative registry of species-coding cast overrides (see the
#' \emph{Details}) from an editable CSV and validates it. A cast override
#' resolves a genuinely forward-ambiguous cast - a source \emph{group} code
#' whose species straddle several groups in the goal coding, so there is no
#' single matching target - to a deliberately chosen target code (the cast then
#' succeeds lossily, with a message, instead of erroring).
#'
#' The CSV has the four columns \code{coding_from}, \code{coding_to},
#' \code{species_id_from}, \code{species_id_to} and one row per override, keyed
#' by \code{(coding_from, coding_to, species_id_from)}. Validation (unless
#' \code{validate = FALSE}) checks that the codings and codes exist, that the
#' cast really is forward-ambiguous, and that the chosen target is one of the
#' straddled goal codes; it therefore needs the involved codings installed.
#'
#' @param file A path to a CSV file (read with automatic separator/encoding
#'   detection) or a \code{data.frame}/\code{tibble} already in the four-column
#'   format.
#'
#' @param sep Field separator to assume when \code{file} is a CSV. By default
#'   (\code{NULL}) it is detected automatically.
#'
#' @param validate If \code{TRUE} (default), run the semantic checks described
#'   above (they read the involved coding tables).
#'
#' @return A \code{tibble} with the columns \code{coding_from},
#'   \code{coding_to}, \code{species_id_from}, \code{species_id_to}.
#'
#' @seealso \code{\link{coding_table_from_template}}
#'
#' @export
#'
#' @examples
#' # A single override resolving the forward-ambiguous ger_nfi_2012 code "290"
#' # (its species straddle tum_wwk_short "8" and "9"; "8" is the chosen target)
#' cast_overrides_from_csv(tibble::tibble(
#'   coding_from     = "ger_nfi_2012",
#'   coding_to       = "tum_wwk_short",
#'   species_id_from = "290",
#'   species_id_to   = "8"
#' ))
#'
cast_overrides_from_csv <- function(file, sep = NULL, validate = TRUE) {
  ov <- if (is.character(file) && length(file) == 1L) {
    .read_csv_flexible(file, sep = sep)
  } else {
    file
  }
  ov <- tibble::as_tibble(ov)

  miss <- setdiff(.cast_override_cols, names(ov))
  if (length(miss)) {
    stop(sprintf("cast overrides: missing column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  ov <- ov[, .cast_override_cols, drop = FALSE]
  for (cc in .cast_override_cols) ov[[cc]] <- .blank_to_na(ov[[cc]])

  if (anyNA(ov)) {
    stop("cast overrides: blank cells are not allowed.", call. = FALSE)
  }
  key <- paste(ov$coding_from, ov$coding_to, ov$species_id_from, sep = "\r")
  if (any(duplicated(key))) {
    dup <- gsub("\r", "/", unique(key[duplicated(key)]))
    stop(sprintf("cast overrides: duplicate (coding_from, coding_to, %s",
                 paste0("species_id_from): ", paste(dup, collapse = "; "))),
         call. = FALSE)
  }

  if (validate) .validate_cast_overrides(ov)
  ov
}
