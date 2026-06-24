#' Compute earnings tier shares from LODES RAC or WAC data
#'
#' @param lodes_df A data frame (tibble) of LODES RAC or WAC data returned
#'   by [grab_lodes()] with `segment = "S000"` (the total count segment,
#'   which includes all three earnings columns). Must contain columns
#'   `CE01`, `CE02`, and `CE03` (for WAC) or `CR01`, `CR02`, `CR03` (for
#'   RAC). See Details.
#' @param type One of `"wac"` (workplace area characteristics, default) or
#'   `"rac"` (residential area characteristics). Controls which earnings
#'   columns are used.
#' @param geo_col The name of the geography column to group by, e.g.
#'   `"w_tract"` or `"h_county"`. Defaults to `NULL`, which auto-detects
#'   the first geography column in `lodes_df`.
#' @param output One of `"wide"` (default) or `"long"`. Wide format appends
#'   three share columns. Long format returns one row per geography-tier
#'   combination.
#'
#' @return A tibble with earnings tier counts and shares. In `"wide"` format,
#'   columns are added for `share_low`, `share_mid`, and `share_high`. In
#'   `"long"` format, columns are `tier`, `label`, `count`, and `share`.
#'
#' @description Computes the share of jobs (or workers) in each of the three
#'   LODES monthly earnings tiers:
#'   \describe{
#'     \item{Low (`CE01`/`CR01`)}{Earnings up to $1,250/month.}
#'     \item{Mid (`CE02`/`CR02`)}{Earnings $1,251-$3,333/month.}
#'     \item{High (`CE03`/`CR03`)}{Earnings above $3,333/month.}
#'   }
#'
#'   Earnings shares are useful for tracking wage polarization, identifying
#'   low-wage job concentration, and examining how the earnings structure of
#'   a labor market has shifted over time, especially when combined with
#'   [compute_lodes_change()].
#'
#'   Note: For WAC data (`type = "wac"`), earnings columns are prefixed `CE`.
#'   For RAC data (`type = "rac"`), they are prefixed `CR`. The total
#'   denominator used is the sum of the three tiers (i.e., `C000`/`CA01`
#'   earnings sub-total), not the overall job count. This ensures shares sum
#'   to 1 within rounding.
#'
#' @importFrom dplyr group_by summarise mutate select rename all_of any_of
#'   across where
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#'
#' @examples
#' \donttest{
#'   wac <- grab_lodes(
#'     state = "md", year = 2019,
#'     lodes_type = "wac", job_type = "JT00",
#'     segment = "S000", agg_geo = "county"
#'   )
#'   compute_earnings_share(wac, type = "wac", geo_col = "w_county")
#'
#'   # Long format, suitable for ggplot2
#'   compute_earnings_share(
#'     wac, type = "wac", geo_col = "w_county", output = "long"
#'   )
#' }
#' @export
compute_earnings_share <- function(
  lodes_df,
  type    = c("wac", "rac"),
  geo_col = NULL,
  output  = c("wide", "long")
) {
  type   <- match.arg(type)
  output <- match.arg(output)

  # Determine earnings column prefix based on data type
  earnings_cols <- if (type == "wac") {
    c(CE01 = "CE01", CE02 = "CE02", CE03 = "CE03")
  } else {
    c(CR01 = "CR01", CR02 = "CR02", CR03 = "CR03")
  }

  missing_cols <- setdiff(earnings_cols, names(lodes_df))
  if (length(missing_cols) > 0) {
    rlang::abort(c(
      glue::glue(
        "Earnings columns not found: {paste(missing_cols, collapse = ', ')}."
      ),
      "i" = glue::glue(
        "Retrieve data with `grab_lodes(..., lodes_type = \"{type}\", ",
        "segment = \"S000\")` to include earnings tier columns."
      )
    ))
  }

  # Auto-detect geography column
  if (is.null(geo_col)) {
    geo_col <- .detect_geo_col(lodes_df)
    rlang::inform(glue::glue("Using `{geo_col}` as the geography column."))
  }
  if (!geo_col %in% names(lodes_df)) {
    rlang::abort(glue::glue("Column `{geo_col}` not found in `lodes_df`."))
  }

  grp_extras <- intersect(c("year", "state"), names(lodes_df))
  grp_cols   <- c(geo_col, grp_extras)

  low_col  <- earnings_cols[1]
  mid_col  <- earnings_cols[2]
  high_col <- earnings_cols[3]

  result <- lodes_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>%
    dplyr::summarise(
      count_low  = sum(.data[[low_col]],  na.rm = TRUE),
      count_mid  = sum(.data[[mid_col]],  na.rm = TRUE),
      count_high = sum(.data[[high_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      total_earnings = .data$count_low + .data$count_mid + .data$count_high,
      share_low  = dplyr::if_else(
        .data$total_earnings > 0,
        .data$count_low  / .data$total_earnings,
        NA_real_
      ),
      share_mid  = dplyr::if_else(
        .data$total_earnings > 0,
        .data$count_mid  / .data$total_earnings,
        NA_real_
      ),
      share_high = dplyr::if_else(
        .data$total_earnings > 0,
        .data$count_high / .data$total_earnings,
        NA_real_
      )
    )

  if (output == "wide") {
    return(result)
  }

  # Long format
  tier_labels <- c(
    low  = glue::glue("{low_col}: up to $1,250/month"),
    mid  = glue::glue("{mid_col}: $1,251-$3,333/month"),
    high = glue::glue("{high_col}: above $3,333/month")
  )

  long_rows <- lapply(c("low", "mid", "high"), function(t) {
    dplyr::tibble(
      !!geo_col      := result[[geo_col]],
      dplyr::across(dplyr::all_of(grp_extras), ~ result[[dplyr::cur_column()]]),
      tier            = t,
      label           = tier_labels[[t]],
      count           = result[[glue::glue("count_{t}")]],
      share           = result[[glue::glue("share_{t}")]]
    )
  })

  dplyr::bind_rows(long_rows)
}
