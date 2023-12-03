get_waiting_list_avg_change_data <- function(provider_successors, rtt_specialties) {
  con <- get_con("master")

  dplyr::tbl(
    con,
    dbplyr::sql(
      "
        SELECT *
        FROM   [FD_USERDB].[central_midlands_csu_UserDB].[RTT].[Incomplete_Pathways_Provider]
      "
    )
  ) |>
    dplyr::filter(
      .data[["Organisation_Code"]] %LIKE% "R%", # nolint
      .data[["Effective_Snapshot_Date"]] %in% (!!paste0(seq(2011, 2020), "-03-31")),
      .data[["Treatment_Function_Code"]] != "999"
    ) |>
    dplyr::count(
      .data[["Organisation_Code"]],
      .data[["Treatment_Function_Code"]],
      .data[["Effective_Snapshot_Date"]],
      wt = .data[["Number_Of_Incomplete_Pathways"]]
    ) |>
    dplyr::collect() |>
    dplyr::inner_join(
      provider_successors,
      by = dplyr::join_by("Organisation_Code" == "old_code")
    ) |>
    dplyr::mutate(
      dplyr::across(
        "Treatment_Function_Code",
        \(.x) dplyr::case_when(
          .x == "X01" ~ "Other",
          .x == "X02" ~ "Other (Medical)",
          .x == "X05" ~ "Other (Surgical)",
          .x %in% rtt_specialties ~ .x,
          .default =  "Other"
        )
      )
    ) |>
    dplyr::count(
      procode = .data[["new_code"]],
      tretspef = .data[["Treatment_Function_Code"]],
      date = .data[["Effective_Snapshot_Date"]],
      wt = .data[["n"]]
    ) |>
    dplyr::mutate(
      dplyr::across("date", lubridate::ymd)
    ) |>
    dplyr::arrange(.data[["procode"]], .data[["date"]], .data[["tretspef"]]) |>
    tidyr::complete(
      .data[["procode"]],
      .data[["tretspef"]],
      .data[["date"]],
      fill = list(n = 0)
    ) |>
    dplyr::group_by(.data[["procode"]], .data[["tretspef"]]) |>
    dplyr::summarise(
      avg_change = mean(diff(.data[["n"]])),
      .groups = "drop"
    ) |>
    dplyr::filter(.data[["avg_change"]] != 0)
}
