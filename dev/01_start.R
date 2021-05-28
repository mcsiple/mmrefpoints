# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "mmrefpoints", # The Name of the package containing the App 
  pkg_title = "Project Marine Mammal Populations and Calculate Reference Points", # The Title of the package containing the App 
  pkg_description = "Tools for projecting marine mammal populations into the future under different bycatch scenarios. It uses an age-structured population model to project numbers at age, based on life history parameters and some reference points entered by the user. Its primary purpose is to allow users to explore the long term impacts of different bycatch rates, based on some basic life history information. It is also built to help users understand and estimate reference points for bycatch such as Potential Biological Removal (PBR).", # The Description of the package containing the App 
  author_first_name = "Margaret", # Your First Name
  author_last_name = "Siple", # Your Last Name
  author_email = "margaret.siple@noaa.gov", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional) 
) #done

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license(copyright_holder = "Margaret C. Siple" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Use git ----
# I ended up doing this manually thru terminal
#usethis::use_git_config(user.name = "mcsiple", user.email = "mcsiple@gmail.com")
#usethis::use_git() # doesn't point to the right directory!

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## MCS: All the packages I use in the shiny app - so many.
usethis::use_package("plotly")
#usethis::use_package(shiny)
usethis::use_package("gapminder")
usethis::use_package("gganimate")
usethis::use_package("scales")
usethis::use_package("xtable")
usethis::use_package("tidyverse", type = "depends")
usethis::use_package("reshape2")
usethis::use_package("DT")
usethis::use_package("RColorBrewer")
usethis::use_package("popbio") # betaval
usethis::use_package("shinydashboard")
usethis::use_package("tableHTML")
usethis::use_package("formattable")
usethis::use_package("kableExtra")
usethis::use_package("shinythemes")
usethis::use_package("shinyBS")
usethis::use_package("shinyLP")
usethis::use_package("extrafont")
#usethis::use_package("gifski")
#usethis::use_package("magick")
usethis::use_package("gridExtra")
usethis::use_package("xts")
usethis::use_package("shiny.i18n")


## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon() # path = "path/to/ico". Can be an online file. 

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

