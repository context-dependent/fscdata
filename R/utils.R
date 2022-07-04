fix_survey_date <- function(survey_data, date_stem) {
    date_frame <- survey_data |>
        dplyr::select(dplyr::matches(date_stem))

    date_string <- glue::glue("{date_frame[[3]]}-{date_frame[[1]]}-{date_frame[[2]]}")
    date_vec <- lubridate::ymd(date_string)

    res <- survey_data |>
        dplyr::mutate(
            !!date_stem := date_vec
        )

    res
}


lable <- function(survey_data) {
    
    res <- tibble(
        name = names(survey_data), 
        label = sjlabelled::get_labels(survey_data)
    )

    return(res)
}
