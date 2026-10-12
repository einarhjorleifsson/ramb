# logbook_mfri.R — MFRI logbook reader and the logbook curation helpers
# (effort capping, mesh-size standardisation, gear-width filling, gear
# lumping). Iceland-specific: gear ids (gid) are the Icelandic ones.

#' Read MFRI logbook records
#'
#' Towed ("mobile") and static-gear records from the MFRI Oracle database for
#' given vessels and years, as one table, with each record's catch summed
#' from the catch table. Iceland-specific: the gear ids (`gid`) are the
#' Icelandic ones. Reads with `omar::lb_mobile()`, `omar::lb_static()` and
#' `omar::lb_catch()`.
#'
#' @param con An Oracle connection (see the `omar` package).
#' @param vids Vessel ids; if missing, all vessels.
#' @param years The years to read.
#'
#' @return A tibble of mobile and static records, `source` saying which.
#' @export
rb_read_logbook_mfri <- function(con, vids, years) {
  mb <-
    omar::lb_mobile(con)
  if (!missing(vids)) {
    mb <-
      mb |>
      dplyr::filter(vid %in% vids)
  }
  mb <-
    mb |>
    dplyr::filter(year %in% years) |>
    dplyr::left_join(
      omar::lb_catch(con) |>
        dplyr::group_by(visir) |>
        # check unit here, should be in kg
        dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop"),
      by = "visir"
    ) |>
    dplyr::collect(n = Inf)
  st <- omar::lb_static(con)
  if (!missing(vids)) {
    st <-
      st |>
      dplyr::filter(vid %in% vids)
  }

  st <-
    st |>
    dplyr::filter(year %in% years) |>
    dplyr::left_join(
      omar::lb_catch(con) |>
        dplyr::group_by(visir) |>
        dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop"),
      by = "visir"
    ) |>
    dplyr::collect(n = Inf)

  LGS <-
    dplyr::bind_rows(
      mb |> dplyr::mutate(source = "mobile"),
      st |> dplyr::mutate(source = "static")
    ) |>
    dplyr::mutate(
      date = lubridate::as_date(date),
      datel = lubridate::as_date(datel),
      gid = as.integer(gid)
    )
  return(LGS)
}


#' Cap and fill logbook effort
#'
#' Icelandic gear ids. A missing `effort` takes the gear's median; an effort
#' above the gear's cap is cut to it (12 hours for gid 6, 24 for gid 7, 15
#' for gid 14). Where a haul's end (`t2`) passes the next setting's start
#' (`t1`), it is cut back to one minute before; the effort is not adjusted
#' to match.
#'
#' @param lb Logbook records as from [rb_read_logbook_mfri()], with `vid`,
#'   `gid`, `effort`, `t1` and `t2`.
#'
#' @return `lb` with `effort` and `t2` adjusted; the same rows.
#' @export
rb_cap_effort <- function(lb) {
  median.effort <-
    lb |>
    dplyr::group_by(gid) |>
    dplyr::summarise(
      median = stats::median(effort, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::drop_na()
  lb <-
    lb |>
    dplyr::left_join(median.effort, by = "gid") |>
    dplyr::mutate(effort = ifelse(!is.na(effort), effort, median)) |>
    dplyr::select(-median) |>
    # cap effort hours
    dplyr::mutate(
      effort = dplyr::case_when(
        effort > 12 & gid == 6 ~ 12,
        effort > 24 & gid == 7 ~ 24,
        effort > 15 & gid == 14 ~ 15,
        TRUE ~ effort
      )
    ) |>
    # Cap on the t2 so not overlapping with next setting
    #    NOTE: Effort not adjusted accordingly
    dplyr::arrange(vid, t1) |>
    dplyr::group_by(vid) |>
    dplyr::mutate(
      overlap = dplyr::if_else(t2 > dplyr::lead(t1), TRUE, FALSE, NA),
      t22 = dplyr::if_else(
        overlap,
        dplyr::lead(t1) - lubridate::minutes(1), # need to subtract 1 minute but get the format right
        t2,
        as.POSIXct(NA)
      ),
      t22 = lubridate::ymd_hms(format(as.POSIXct(
        t22,
        origin = "1970-01-01",
        tz = "UTC"
      ))),
      t2 = dplyr::if_else(overlap & !is.na(t22), t22, t2, as.POSIXct(NA))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-t22)

  return(lb)
}

#' Standardise logbook mesh sizes
#'
#' Icelandic gear ids. Adds `mesh.std`, a mesh size on a common scale: fixed
#' classes for most gears, the recorded mesh classed at 147 mm for trawls
#' (gid 5, 6; for gid 7 `mesh_min` is used), and gillnet (gid 2) mesh
#' converted from its recorded unit and classed, defaulting to 203.
#'
#' @param lb Logbook records as from [rb_read_logbook_mfri()], with `gid`,
#'   `mesh` and `mesh_min`.
#'
#' @return `lb` with `mesh.std` added.
#' @export
rb_standardise_gear_meshsize <- function(lb) {
  lb <-
    lb |>
    # "correct" mesh size
    dplyr::mutate(
      mesh = ifelse(gid == 7, mesh_min, mesh),
      mesh.std = dplyr::case_when(
        gid == 9 ~ 80,
        gid %in% c(7, 10, 12, 14) ~ 40,
        gid %in% c(5, 6) & (mesh <= 147 | is.na(mesh)) ~ 135,
        gid %in% c(5, 6) & mesh > 147 ~ 155,
        gid %in% c(15, 38, 40) ~ 100,
        TRUE ~ NA_real_
      )
    ) |>
    # gill net stuff
    dplyr::mutate(
      tommur = ifelse(gid == 2, round(mesh / 2.54), NA),
      tommur = dplyr::case_when(
        tommur <= 66 ~ 60,
        tommur <= 76 ~ 70,
        tommur <= 87 ~ 80,
        tommur <= 100000 ~ 90,
        TRUE ~ NA_real_
      ),
      mesh.std = ifelse(gid == 2, round(tommur * 2.54), mesh.std),
      mesh.std = ifelse(gid == 2 & is.na(mesh.std), 203, mesh.std)
    ) |>
    dplyr::select(-tommur)

  return(lb)
}


#' Fill logbook gear widths
#'
#' Icelandic gear ids. Adds `gear.width`: the sweep length for trawls and
#' seines (gid 6, 7, 9, 14), the plough width for dredges (gid 15, 38, 40),
#' capped per gear; a missing width takes the gear's median, and gid 5 a
#' placeholder 500.
#'
#' @param lb Logbook records as from [rb_read_logbook_mfri()], with `gid`,
#'   `sweeps` and `plow_width`.
#'
#' @return `lb` with `gear.width` added.
#' @export
rb_fill_gear_width <- function(lb) {
  lb <-
    lb |>
    dplyr::mutate(
      gear.width = dplyr::case_when(
        gid %in% c(6L, 7L, 9L, 14L) ~ as.numeric(sweeps),
        gid %in% c(15L, 38L, 40L) ~ as.numeric(plow_width),
        TRUE ~ NA_real_
      ),
      # cap gear width
      gear.width = dplyr::case_when(
        gid == 6L & gear.width > 250 ~ 250,
        gid == 7L & gear.width > 250 ~ 250,
        gid == 9L & gear.width > 75 ~ 75,
        gid == 14L & gear.width > 55 ~ 55,
        TRUE ~ gear.width
      )
    )
  gear.width <-
    lb |>
    dplyr::filter(gid %in% c(6, 7, 9, 14, 15, 37, 38, 40)) |>
    dplyr::group_by(gid) |>
    dplyr::summarise(
      gear.width.median = stats::median(gear.width, na.rm = TRUE),
      .groups = "drop"
    )
  # use median gear width by gear, if gear width is missing
  #   could also try to do this by vessels. time scale (year) may also matter here
  lb <-
    lb |>
    dplyr::left_join(gear.width, by = "gid") |>
    dplyr::mutate(
      gear.width = ifelse(
        gid %in% c(6, 7, 9, 14, 15, 37, 38, 40) & is.na(gear.width),
        gear.width.median,
        gear.width
      )
    ) |>
    dplyr::select(-gear.width.median)

  # Put gear width of 5 as 500 - this is taken from thin air
  #   TODO: What gear width should be used
  lb <-
    lb |>
    dplyr::mutate(gear.width = ifelse(gid == 5, 500, gear.width))

  return(lb)
}

# Lump Icelandic gear ids into broader groups. Currently unused; kept for the
# MFRI logbook curation flow it was written for.
.rb_lump_gears <- function(lb) {
  lb |>
    dplyr::mutate(
      gid = dplyr::case_when(
        gid %in% c(10L, 12L) ~ 4L, # purse seines
        gid %in% c(18L, 39L) ~ 18L, # traps
        TRUE ~ gid
      )
    ) |>
    # make the rest a negative number
    dplyr::mutate(gid = ifelse(is.na(gid), -666L, gid)) |>
    # "skip" these also in downstream processing
    dplyr::mutate(gid = ifelse(gid %in% c(4, 12, 42), -666, gid)) |>
    # lump dredges into one single gear
    dplyr::mutate(gid = ifelse(gid %in% c(15, 37, 38, 40), 15, gid))
}
