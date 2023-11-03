mod_waiting_list_imbalances_table <- function() {
  rtt_specialties() |>
    dplyr::mutate(
      ip_id = paste0("wli_ip_", sanitize_input_name(.data$code)),
      op_id = paste0("wli_op_", sanitize_input_name(.data$code))
    )
}
