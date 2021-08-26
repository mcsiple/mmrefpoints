#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
 ### BEGIN COPY PASTE FROM APP
 shinyUI(fluidPage(
   shiny.i18n::usei18n(i18n),
   theme = shinythemes::shinytheme("flatly"),
   tags$head(tags$style(".shiny-output-error{color: darkblue;}")), # dark blue error messages
   fluidRow(
     column(
       8,
       titlePanel(
         title = i18n$t("Marine Mammal Bycatch Impacts Exploration Tool"),
         windowTitle = "MMBIET"
       )
     ),
     column(
       4,
       radioButtons(
         inputId = "selected_language",
         label = "Language",
         choiceNames = c("English", "Español", "Français"),
         choiceValues = i18n$get_languages(), # c("en","es","fr"),
         selected = i18n$get_key_translation(),
         inline = FALSE
       )
     ) # /column
   ), # /fluidRow
   uiOutput("page_content")
 ))
 ###
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'mmrefpoints'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

