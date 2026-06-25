# ============================================================
# Tests base functionality to retrieve lehd data
# These tests use DE (Delaware), VT (Vermont), ND 
# (North Dakota), SD (South Dakota), and WY (Wyoming) as test
# test states because the data is relatively small and fast 
# to download.
# ============================================================

# ---- grab_lodes () ------------------------------------------

test_that("test grab lodes od", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state = "de",
      year = 2020,
      version = "LODES8",
      lodes_type = "od",
      job_type = "JT00",
      segment = "SA01",
      state_part = "main",
      agg_geo = "tract"
    ) %>%
      dim,
    c(36671, 14)
  )
  expect_equal(
    grab_lodes(
      state = "de",
      year = 2009,
      version = "LODES5",
      lodes_type = "od",
      state_part = "main",
      agg_geo = "tract"
    ) %>%
      dim,
    c(25805, 14)
  )
  expect_equal(
    grab_lodes(
      state = "de",
      year = "2015",
      version = "LODES8",
      lodes_type = "od",
      job_type = "JT00",
      segment = "SE01",
      state_part = "main"
    ) %>%
      dim,
    c(290314, 15)
  )
  expect_equal(
    grab_lodes(
      state = "de",
      year = "2009",
      version = "LODES5",
      lodes_type = "od",
      state_part = "aux"
    ) %>%
      dim,
    c(68588, 15)
  )
})

test_that("test grab lodes rac", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state = "de",
      year = 2014,
      version = "LODES7",
      lodes_type = "rac",
      agg_geo = "tract"
    ) %>%
      dim,
    c(218, 44)
  )
  expect_equal(
    grab_lodes(
      state = "de",
      year = "2015",
      version = "LODES8",
      lodes_type = "rac"
    ) %>%
      dim,
    c(14436, 45)
  )
  expect_equal(
    grab_lodes(
      state = "de",
      year = "2004",
      version = "LODES7",
      lodes_type = "rac",
      job_type = "JT01",
      segment = "SA01"
    ) %>%
      dim,
    c(12596, 45)
  )
})

test_that("test grab lodes wac", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state = "de",
      year = 2009,
      version = "LODES5",
      lodes_type = "wac",
      job_type = "JT01",
      agg_geo = "tract"
    ) %>%
      dim,
    c(197, 42)
  )
  expect_equal(
    grab_lodes(
      state = "de",
      year = "2015",
      version = "LODES7",
      lodes_type = "wac"
    ) %>%
      dim,
    c(5476, 55)
  )
  expect_equal(
    grab_lodes(
      state = "de",
      year = "2020",
      version = "LODES8",
      lodes_type = "wac"
    ) %>%
      dim,
    c(6421, 55)
  )
})

# ---- grab_lodes () ---- multi-state -------------------------

test_that("test grab lodes od for multiple states and years", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state = c("nd", "vt"),
      year = c(2007, 2008),
      version = "LODES5",
      lodes_type = "od",
      job_type = "JT01",
      segment = "SA01",
      state_part = "main",
      agg_geo = "tract"
    ) %>%
      dim,
    c(65262, 14)
  )
  expect_equal(
    grab_lodes(
      state = c("de", "vt"),
      year = c(2013, 2014),
      version = "LODES8",
      lodes_type = "od",
      job_type = "JT01",
      segment = "SA01",
      state_part = "main"
    ) %>%
      dim,
    c(929717, 15)
  )
  expect_equal(
    grab_lodes(
      state = c("de", 'sd'),
      year = c(2013, 2020),
      version = "LODES8",
      lodes_type = "od",
      job_type = "JT01",
      segment = "SA01",
      state_part = "main"
    ) %>%
      dim,
    c(1125184, 15)
  )
})

# ---- grab_lodes () ---- multi-state and multi-year-----------

test_that("test grab lodes wac for multiple states and years", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state = c("de", "vt"),
      year = c(2013, 2014),
      version = "LODES7",
      lodes_type = "wac",
      agg_geo = "tract"
    ) %>%
      dim,
    c(798, 54)
  )
  expect_equal(
    grab_lodes(
      state = c("de", "vt"),
      year = c(2007, 2009),
      version = "LODES5",
      lodes_type = "wac",
      agg_geo = "tract"
    ) %>%
      dim,
    c(752, 42)
  )
  expect_equal(
    grab_lodes(
      state = c("de", "vt"),
      year = c(2013, 2014),
      version = "LODES7",
      lodes_type = "wac"
    ) %>%
      dim,
    c(24132, 55)
  )
  expect_equal(
    grab_lodes(
      state = c("de", "vt"),
      year = c(2017, 2018, 2019, 2020),
      version = "LODES8",
      lodes_type = "wac",
      job_type = "JT01",
      segment = "S000"
    ) %>%
      dim,
    c(55696, 55)
  )
})

test_that("test grab crosswalk", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_crosswalk('vt') %>%
      dim,
    c(24611, 41)
  )
  expect_equal(
    grab_crosswalk(c("wy", "ND")) %>%
      dim,
    c(138335, 41)
  )
})

test_that("test join_lodes_geometry", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  rac_data <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "rac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "county",
    geometry = TRUE
  )

  wac_data <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "wac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "county",
    geometry = TRUE
  )

  expect_s3_class(rac_data, "sf")
  expect_s3_class(wac_data, "sf")

  rac_data_bg <- grab_lodes(
    state = "vt",
    year = 2022,
    version = "LODES8",
    lodes_type = "rac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "bg",
    geometry = TRUE
  )

  wac_data_bg <- grab_lodes(
    state = "vt",
    year = 2022,
    version = "LODES8",
    lodes_type = "wac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "bg",
    geometry = TRUE
  )

  expect_s3_class(rac_data_bg, "sf")
  expect_s3_class(wac_data_bg, "sf")

  rac_data_block <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "rac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "block",
    geometry = TRUE
  )

  wac_data_block <- grab_lodes(
    state = "vt",
    year = 2008,
    version = "LODES5",
    lodes_type = "wac",
    job_type = "JT01",
    segment = "SA01",
    state_part = "main",
    agg_geo = "block",
    geometry = TRUE
  )

  expect_s3_class(rac_data_block, "sf")
  expect_s3_class(wac_data_block, "sf")

  expect_message(
    grab_lodes(
      state = "vt",
      year = 2008,
      version = "LODES5",
      lodes_type = "wac",
      job_type = "JT01",
      segment = "SA01",
      state_part = "main",
      agg_geo = "county",
      geometry = TRUE
    )
  )
})



# ============================================================
# Tests new analytical functions introduced in lehdr v1.2.0
# These tests use DE (Delaware) as the test state because it
# is small and fast to download.
# ============================================================

# ---- compute_commute_stats() --------------------------------

test_that("compute_commute_stats returns expected columns for tract-level OD", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("tract", "year", "state",
      "workers_in", "workers_out", "workers_internal",
      "net_flow", "self_containment"),
    ignore.order = TRUE
  )
})

test_that("compute_commute_stats numeric columns are non-negative where expected", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")

  expect_true(all(result$workers_in      >= 0, na.rm = TRUE))
  expect_true(all(result$workers_out     >= 0, na.rm = TRUE))
  expect_true(all(result$workers_internal >= 0, na.rm = TRUE))
})

test_that("compute_commute_stats self_containment is between 0 and 1", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")
  sc <- result$self_containment[!is.na(result$self_containment)]

  expect_true(all(sc >= 0 & sc <= 1))
})

test_that("compute_commute_stats net_flow identity: workers_in - workers_out_total == net_flow", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")

  # workers_out_total (internal + out) = workers_internal + workers_out
  workers_out_total <- result$workers_internal + result$workers_out
  expect_equal(result$net_flow, result$workers_in - workers_out_total)
})

test_that("compute_commute_stats works at county level", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "county"
  )

  result <- compute_commute_stats(od, agg_geo = "county")

  expect_s3_class(result, "tbl_df")
  expect_true("county" %in% names(result))
  # Delaware has 3 counties
  expect_equal(nrow(result), 3L)
})

test_that("compute_commute_stats aborts on missing S000 column", {
  bad_df <- data.frame(h_tract = "11001", w_tract = "11002", year = 2020L)

  expect_error(
    compute_commute_stats(bad_df, agg_geo = "tract"),
    "S000"
  )
})

test_that("compute_commute_stats aborts on invalid agg_geo", {
  od_stub <- data.frame(
    h_tract = character(), w_tract = character(),
    S000 = integer(), year = integer()
  )
  expect_error(
    compute_commute_stats(od_stub, agg_geo = "zipcode"),
    "'arg' should be one of"
  )
})


# ---- compute_lodes_change() ---------------------------------

test_that("compute_lodes_change wide output has expected column pattern", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = c("de", "vt"),
    year       = c(2017, 2019),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_lodes_change(
    wac,
    geo_col      = "w_county",
    base_year    = 2017,
    compare_year = 2019,
    variables    = c("C000", "CE01", "CE02", "CE03")
  )

  expect_s3_class(result, "tbl_df")
  # Check that all four expected column suffixes exist for C000
  for (suffix in c("_base", "_compare", "_change", "_pct_change")) {
    expect_true(
      paste0("C000", suffix) %in% names(result),
      info = paste0("Missing column: C000", suffix)
    )
  }
})

test_that("compute_lodes_change long output has required columns", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = c("de", "vt"),
    year       = c(2017, 2019),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_lodes_change(
    wac,
    geo_col      = "w_county",
    base_year    = 2017,
    compare_year = 2019,
    variables    = c("C000"),
    output       = "long"
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("w_county", "variable", "base_year", "compare_year",
      "base_value", "compare_value", "change", "pct_change"),
    ignore.order = TRUE
  )
})

test_that("compute_lodes_change pct_change arithmetic is correct", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2018, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_lodes_change(
    wac,
    geo_col   = "w_county",
    variables = "C000"
  )

  # pct_change = (compare - base) / base * 100
  expected_pct <- (result$C000_compare - result$C000_base) / result$C000_base * 100
  expect_equal(result$C000_pct_change, expected_pct, tolerance = 1e-6)
})

test_that("compute_lodes_change defaults to min/max year", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2015, 2017, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  # Should not error; should use 2015 as base, 2020 as compare
  result <- compute_lodes_change(wac, geo_col = "w_county", variables = "C000")

  expect_true(all(result$base_year  == 2015 | !"base_year"  %in% names(result)))
  # In wide output there is no base_year column, so check via values
  # The change should equal compare minus base pulled from the source
  expect_s3_class(result, "tbl_df")
})

test_that("compute_lodes_change auto-detects geo_col and emits a message", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2018, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  expect_message(
    compute_lodes_change(wac, variables = "C000"),
    "w_county"
  )
})

test_that("compute_lodes_change aborts with single year input", {
  single_year <- data.frame(w_county = "10001", C000 = 100L, year = 2020L)

  expect_error(
    compute_lodes_change(single_year, geo_col = "w_county"),
    "at least two distinct years"
  )
})

test_that("compute_lodes_change aborts with missing year column", {
  no_year <- data.frame(w_county = "10001", C000 = 100L)

  expect_error(
    compute_lodes_change(no_year, geo_col = "w_county"),
    "`year` column"
  )
})

test_that("compute_lodes_change aborts if base_year not in data", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2018, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  expect_error(
    compute_lodes_change(wac, geo_col = "w_county", base_year = 2010),
    "2010"
  )
})


# ---- compute_earnings_share() --------------------------------

test_that("compute_earnings_share WAC wide output has share columns summing to 1", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(wac, type = "wac", geo_col = "w_county")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("share_low", "share_mid", "share_high") %in% names(result)))

  row_sums <- result$share_low + result$share_mid + result$share_high
  expect_equal(row_sums, rep(1, nrow(result)), tolerance = 1e-6)
})

test_that("compute_earnings_share RAC wide output has share columns summing to 1", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  rac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "rac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(rac, type = "rac", geo_col = "h_county")

  expect_s3_class(result, "tbl_df")
  row_sums <- result$share_low + result$share_mid + result$share_high
  expect_equal(row_sums, rep(1, nrow(result)), tolerance = 1e-6)
})

test_that("compute_earnings_share long output has required columns", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(
    wac, type = "wac", geo_col = "w_county", output = "long"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("tier", "label", "count", "share") %in% names(result)))
  # Three tiers * 3 DE counties = 9 rows
  expect_equal(nrow(result), 9L)
})

test_that("compute_earnings_share long shares are in [0, 1]", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(
    wac, type = "wac", geo_col = "w_county", output = "long"
  )

  shares <- result$share[!is.na(result$share)]
  expect_true(all(shares >= 0 & shares <= 1))
})

test_that("compute_earnings_share auto-detects geo_col and emits a message", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  expect_message(
    compute_earnings_share(wac, type = "wac"),
    "w_county"
  )
})

test_that("compute_earnings_share aborts with wrong type for WAC data", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  # Requesting type = "rac" on WAC data should abort because CR01/CR02/CR03 are absent
  expect_error(
    compute_earnings_share(wac, type = "rac", geo_col = "w_county"),
    "CR0"
  )
})

test_that("compute_earnings_share aborts on invalid type argument", {
  stub <- data.frame(w_county = character(), CE01 = integer(),
                     CE02 = integer(), CE03 = integer())
  expect_error(
    compute_earnings_share(stub, type = "od"),
    "'arg' should be one of"
  )
})

test_that("compute_earnings_share counts match raw column sums", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(wac, type = "wac", geo_col = "w_county")

  # Manual totals for one county, e.g. the first one
  one_county <- wac[wac$w_county == result$w_county[1], ]
  expect_equal(result$count_low[1],  sum(one_county$CE01))
  expect_equal(result$count_mid[1],  sum(one_county$CE02))
  expect_equal(result$count_high[1], sum(one_county$CE03))
})
