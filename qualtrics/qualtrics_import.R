qualtrics_import <- function(root, token, survey_id = NA){
        
        # initialization
        require(qualtRics)
        registerOptions(api_token=token, root_url=root)
        
        # list surveys and select
        if(is.na(survey_id)){
                print(getSurveys())
                print("These surveys are avaiable. Enter the required survey ID")
                survey_id <- readline("[survey ID] : ") 
        }
        
        # list questions
        print(getSurveyQuestions(surveyID = survey_id)[,1])
        
        # select PSAP questions
        print("Please enter the question IDs for PSAP1, PSAP2 & PSAP3.")
        PSAP1_var <- readline("ID PSAP1 : ") 
        PSAP2_var <- readline("ID PSAP2 : ") 
        PSAP3_var <- readline("ID PSAP3 : ")
        
        # obtain data from Qualtrics
        surveydata <- getSurvey(surveyID = survey_id,
                          save_dir = tempdir(),
                          includedQuestionIds = c(PSAP1_var, PSAP2_var, PSAP3_var),
                          verbose=TRUE, force_request = T)
        
        # push data to environment
        outputdata <- surveydata[,12:14]
        names(outputdata) <- c("PSAP1", "PSAP2", "PSAP3") 
        PSAP_data <<- outputdata
        print("the PSAP data is now available as [PSAP_data] in your environment")
        
}






