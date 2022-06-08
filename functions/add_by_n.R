add_by_n <- function(data, variable, by, tbl, ...) {
  data %>%
    select(all_of(c(variable, by))) %>%
    dplyr::group_by(.data[[by]]) %>%
    dplyr::summarise_all(~sum(!is.na(.))) %>%
    rlang::set_names(c("by", "variable")) %>%
    dplyr::left_join(
      tbl$df_by %>% select(by, by_col),
      by = "by"
    ) %>%
    mutate(
      by_col = paste0("add_n_", by_col),
      variable = style_number(variable)
    ) %>%
    select(-by) %>%
    tidyr::pivot_wider(names_from = by_col, 
                       values_from = variable)
}