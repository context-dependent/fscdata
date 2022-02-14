#' Import Sf / Qx records one or more FSC projects
#' 
#' This function returns a list of tibbles that comprise the research-relevant 
#' contents of the FSC Evaluation Database for one or more programs. 
#' If no programs are specified, records for all programs are returned. 
#' 
#' @param programs A vector of program names to include in the query to salesforce
#' @return A list of the following tibbles: 
#'   - enrollments: tombstone record for each participant
#'   - surveys: raw qualtrics exports, linked logically to enrollments
#'   - components: program component completion status 
#' @export 
#' @examples
#' all_records <- sf_read_fsc()
#' fast_data <- sf_read_fsc(programs = "FAST")
#' fast_and_climb <- sf_read_fsc(programs = "CLIMB") 

sf_read_fsc <- function(programs = NULL) {
    d_enrollments <- sf_read_enrollments(programs)
    d_surveys <- sf_read_program_surveys(programs)
    d_components <- sf_read_program_components(programs)

    ls_program_data <- list(
        enrollments = d_enrollments, 
        surveys = d_surveys, 
        components = d_components
    )

    return(ls_program_data)
}

sf_read_enrollments <- function(programs = NULL) {

    q_where <- gen_where_program(programs, program_name_key = "Cohorts__r.Program__r.Name")

    q <- glue::glue(
        "
            SELECT
                Id, 
                Participant_Contact__c, 
                Participant_Contact__r.Name, 
                Participant_Contact__r.External_Reference_ID__c, 
                Participant_Contact__r.Gender__c, 
                Participant_Contact__r.Birthdate, 
                Cohorts__r.Name, 
                Cohorts__r.Start_Date__c, 
                Cohorts__r.End_Date__c,
                Cohorts__r.Program__r.Name, 
                Experiment_Assignment_Group__c, 
                AssignmentID__c
            FROM

                Enrollment__c
            {q_where}
        "
    ) 

    enrollments_bad_names <- salesforcer::sf_query(q)
    enrollments_good_names <- enrollments_bad_names |>
        dplyr::select(
            enrollment_id = Id, 
            contact_id = Participant_Contact__c, 
            external_reference_id = Participant_Contact__r.External_Reference_ID__c,
            name = Participant_Contact__r.Name, 
            gender_sf = Participant_Contact__r.Gender__c, 
            date_of_birth_sf = Participant_Contact__r.Birthdate,
            cohort_name = Cohorts__r.Name, 
            cohort_start_date = Cohorts__r.Start_Date__c, 
            cohort_end_date = Cohorts__r.End_Date__c,
            program = Cohorts__r.Program__r.Name, 
            assignment_group = Experiment_Assignment_Group__c, 
            assignment_id = AssignmentID__c
        )

    return(enrollments_good_names)
}

sf_read_program_surveys <- function(programs = NULL) {

    q_where <- gen_where_program(programs, "Program_SurveyAssessment__r.Program__r.Name")

    q <- glue::glue(
        "
            SELECT
                Enrollment__r.Id,
                Program_SurveyAssessment__r.Program__r.Name, 
                Program_SurveyAssessment__r.Program_Survey_ID__c,
                Program_SurveyAssessment__r.Name, 
                Due_Date__c, 
                Status__c, 
                Date_Completed__c,
                Survey_Response_ID__c
            FROM
                Participant_SurveyAssessment_Response__c
            {q_where}
        "
    )

    d_responses_empty <- salesforcer::sf_query(q)  |>
        dplyr::select(
            enrollment_id = Enrollment__r.Id, 
            program = Program_SurveyAssessment__r.Program__r.Name,
            sf_survey_name = Program_SurveyAssessment__r.Name,
            sf_due_date = Due_Date__c,
            sf_status = Status__c, 
            sf_completed_date = Date_Completed__c,
            survey_id   = Program_SurveyAssessment__r.Program_Survey_ID__c, 
            response_id = Survey_Response_ID__c
        ) |>
        dplyr::filter(!is.na(survey_id))
    
    d_responses <- d_responses_empty |>
        dplyr::group_nest(program, sf_survey_name, survey_id, .key = "enrollment_response_link") |>
        dplyr::mutate(
            response_data = survey_id |>
                purrr::map(
                    qualtr::get_responses_v2
                )  |>
                purrr::map2(
                    enrollment_response_link, 
                    function(x, y) {
                        dplyr::left_join(y, x) 
                    }
                )
        ) |>
        dplyr::select(-enrollment_response_link)

    return(d_responses)

}

sf_read_program_components <- function(programs = NULL) {
    q_where <- gen_where_program(programs, program_name_key = "Program_Component__r.Program__r.Name")

    q <- glue::glue(
        "SELECT
            Enrollment__c, 
            Component_Type__c,
            Program_Component__r.Name,
            Status__c, 
            Date_Completed__c
        FROM 
            Participant_Program_Component_Status__c
        {q_where}
        "
    )

    d_components <- salesforcer::sf_query(q) |>
        dplyr::select(
            enrollment_id = Enrollment__c, 
            type = Component_Type__c, 
            name = Program_Component__r.Name, 
            status = Status__c, 
            date_completed = Date_Completed__c
        )

    return(d_components)
}

gen_where_program <- function(programs = NULL, program_name_key = NULL) {
    if(is.null(programs)) {
        q_where <- ""
    } else {

        q_conditions <- 
            glue::glue(
                "{program_name_key} = '{programs}' "
            ) |>
            stringr::str_c(
                collapse = "OR\n"
            )

        q_where <- glue::glue("WHERE \n {q_conditions}")

    }

    return(q_where)
    
}

