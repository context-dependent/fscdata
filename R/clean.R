clean_sdle_demos <- function(baseline_responses, program_slug) {
    d <- baseline_responses |>
        fix_survey_date("demo_date_of_birth") |>
        clean_indigenous(program_slug) |>
        clean_newcomer(program_slug) |>
        clean_lang_vars(program_slug) |>
        clean_family_vars(program_slug) |>
        dplyr::select(
            enrollment_id, 
            lnk_date_of_birth = demo_date_of_birth, 
            lnk_sex = demo_sex,
            dem_indigenous, 
            dem_newcomer, 
            dem_esl, 
            dem_fols, 
            dem_educ_highest = education_highest,
            dem_educ_highest_advanced = education_advanced, 
            dem_educ_in_canada = education_domestic, 
            dem_marital = demo_marital, 
            dem_parent, 
            dem_single_parent, 
        )
}

calc_age_at_program_start <- function(date_of_birth, enrollment_start_date) {

}

clean_education <- function(baseline_responses, program_slug) {

}

clean_indigenous <- function(baseline_responses, program_slug) {
    
}

clean_newcomer <- function(baseline_responses, program_slug) {

}

clean_lang_vars <- function(baseline_responses, program_slug) {

}

clean_family_vars <- function(baseline_responses, program_slug) {

}