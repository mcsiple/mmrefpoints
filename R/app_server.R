#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  # output$R_working_dir <- renderText({
  #   print(here::here())
  # })
  
  # TRANSLATOR --------------------------------------------------------------
  
  observeEvent(input$selected_language, {
    print(paste("Language change:", input$selected_language))
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
  # HOMEPAGE ----------------------------------------------------------------
  output$splash <- renderUI({
    htmltools::HTML(switch(input$selected_language,
                           "en" = splash_en,
                           "es" = splash_es,
                           "fr" = splash_fr
    ))
  })
  
  output$tn_simple <- renderUI({
    htmltools::HTML(switch(input$selected_language,
                           "en" = tn_en,
                           "es" = tn_es,
                           "fr" = tn_fr
    ))
  })
  
  output$tn_advanced <- renderUI({
    htmltools::HTML(switch(input$selected_language,
                           "en" = tn_en_adv,
                           "es" = tn_es_adv,
                           "fr" = tn_fr_adv
    ))
  })
  
  observeEvent(input$jumpToSimple, {
    updateTabsetPanel(session, "inTabset",
                      selected = "simple"
    )
  })
  
  observeEvent(input$jumpToAdv, {
    updateTabsetPanel(session, "inTabset",
                      selected = "advanced"
    )
  })
  
  
  # SIMPLE ------------------------------------------------------------------
  # translate radio buttons and dropdown menus
  observe({
    updateSelectInput(session,
                      inputId = "type_simple",
                      label = i18n$t("Mammal life history type (choose one):"),
                      choices = stats::setNames(paste0(lh$Code, "_simple"), i18n$t(lh$Type))
    ) # / update SelectInput
  })
  
  observeEvent(input$type, {
    x <- input$type
    updateNumericInput(session, "global.S0", value = c(lh.table()[paste(x), "S0"]))
    updateNumericInput(session, "global.S1plus", value = c(lh.table()[paste(x), "S1plus"]))
    updateNumericInput(session, "global.AgeMat", value = c(lh.table()[paste(x), "AgeMat"]))
  })
  
  observeEvent(input$popsize_usr_simple, {
    updateSliderInput(session, "constantcatch_simple",
                      value = c(50, 100),
                      min = 0,
                      max = input$popsize_usr_simple
    )
  })
  
  initdepl_simple <- reactive(ifelse(
    input$vdepln_simple == "lo", 0.25,
    ifelse(input$vdepln_simple == "med", 0.5,
           0.75
    )
  ))
  
  K1plus_simple <- reactive(input$popsize_usr_simple / initdepl_simple()) # new
  
  lh.table_simple <- reactive(lh %>% # keep this the same
                                mutate(K1plus = K1plus_simple()) %>%
                                mutate(add.as.code = paste(Code, "_simple", sep = "")) %>% # have to rename for shiny...clunky
                                tibble::column_to_rownames("add.as.code") %>%
                                select(S0, S1plus, AgeMat, PlusGroupAge, fmax, z, lambdaMax, K1plus) %>%
                                as.data.frame())
  
  # lookup lh type
  lh.params2_simple <- reactive(as.list(lh.table_simple()[paste(input$type_simple), ]))
  
  # Vector of depletion levels so that low, med, high depletion are a vector
  InitDepl.vec_simple <- reactive(c(
    min(1, initdepl_simple() * 1.25),
    initdepl_simple(),
    initdepl_simple() * .75
  ))
  
  # Constant catch (simple scenario)
  high.simple <- reactive(projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = input$constantcatch_simple[2],
      CV = input$cvcatch_simple
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  med.simple <- reactive(projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = stats::median(input$constantcatch_simple),
      CV = input$cvcatch_simple
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  low.simple <- reactive(projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = stats::median(input$constantcatch_simple[1]),
      CV = input$cvcatch_simple
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  zero.simple <- reactive(projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = 0,
      CV = 0
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  # Constant catch rate (simple tab)
  high.simple.rate <- reactive(projections(
    NOut = 50,
    ConstantRateBycatch = list(
      Rate = input$bycatchrate_simple[2],
      CV = input$cvcatchrate_simple
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  med.simple.rate <- reactive(projections(
    NOut = 50,
    ConstantRateBycatch = list(
      Rate = stats::median(input$bycatchrate_simple),
      CV = input$cvcatchrate_simple
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  low.simple.rate <- reactive(projections(
    NOut = 50,
    ConstantRateBycatch = list(
      Rate = input$bycatchrate_simple[1],
      CV = input$cvcatchrate_simple
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  zero.simple.rate <- reactive(projections(
    NOut = 50,
    ConstantRateBycatch = list(
      Rate = 0,
      CV = 0
    ),
    InitDepl = initdepl_simple(),
    lh.params = lh.params2_simple(),
    nyears = nyears.simple
  ))
  
  observeEvent(eventExpr = input$bycatchrate_simple[2], {
    # Important: this controls the input for sd of betaval so that sd is restricted to where it is defined. May be clunky for small means:
    val1 <- input$bycatchrate_simple[2]
    updateSliderInput(session, "cvcatchrate_simple",
                      value = val1,
                      min = floor(val1),
                      max = round((sqrt((1 - val1) * val1)) / val1, digits = 2),
                      step = 0.01
    )
    updateSliderInput(session, "cvcatchrate_simple",
                      value = val1,
                      min = floor(val1),
                      max = round((sqrt((1 - val1) * val1)) / val1, digits = 2),
                      step = 0.01
    )
  })
  
  
  output$projPlotSimple <- renderPlot({
    if (input$crad_simple == "n_yr_simple") {
      high.list <- high.simple()
      med.list <- med.simple()
      low.list <- low.simple()
      zero.list <- zero.simple()
    }
    if (input$crad_simple == "rate2_simple") {
      high.list <- high.simple.rate()
      med.list <- med.simple.rate()
      low.list <- low.simple.rate()
      zero.list <- zero.simple.rate()
    }
    
    print(input$selected_language)
    # print(translator$get_languages())
    
    
    stf <- input$spag_simple
    nspag <- 50
    spaghetti <- ifelse(stf, nspag, FALSE)
    lh.params <- lh.params2_simple()
    # Only one depletion level in the simple case
    plot_proj(
      high = high.list,
      med = med.list,
      low = low.list,
      spaghetti = spaghetti,
      ylims = c(0, lh.params$K1plus),
      InitDepl = initdepl_simple(),
      K1plus = lh.params$K1plus,
      years.plot = plotyears,
      color.palette = colorscheme,
      lang = input$selected_language
    )
  })
  
  # ADVANCED ----------------------------------------------------------------
  # translate life history type options
  observe({
    updateSelectInput(session,
                      inputId = "type",
                      label = i18n$t("Mammal life history type (choose one):"),
                      choices = stats::setNames(lh$Code, i18n$t(lh$Type))
    ) # / update SelectInput -- this works for translating options but not for the calculations
  })
  
  observeEvent(input$type, {
    # Update default lambdaMax based on whether a cetacean or a pinniped is selected
    x <- input$type
    cetaceans <- c("bowhead", "bdolphin", "humpback", "porpoise", "minke", "falsetruekw", "pilot", "right")
    pinnipeds <- c("phocid", "furseal", "sealion")
    if (x %in% cetaceans) {
      updateNumericInput(session, "lambdaMax", value = 1.04)
    }
    if (x %in% pinnipeds) {
      updateNumericInput(session, "lambdaMax", value = 1.12)
    }
  })
  
  # Depletion, based on radio button selection:
  initdepl_adv <- reactive(ifelse(input$vdepln_adv == "lo", 0.25,
                                  ifelse(input$vdepln_adv == "med", 0.5,
                                         0.75
                                  )
  ))
  # Depletion vector, for later use:
  InitDepl.vec <- reactive(c(
    min(1, initdepl_adv() * 1.25),
    initdepl_adv(),
    initdepl_adv() * .75
  ))
  
  # K1p, based on user depletion and population size:
  K1plus_adv <- reactive(input$popsize_usr / initdepl_adv()) # new
  
  lh.table <- reactive(lh %>% # default life history setting
                         mutate(
                           K1plus = ifelse(input$crad == "n_yr",
                                           K1plus_adv(),
                                           lh$K1plus[1]
                           ),
                           lambdaMax = input$lambdaMax
                         ) %>%
                         column_to_rownames("Code") %>%
                         select(S0, S1plus, AgeMat, PlusGroupAge, fmax, z, lambdaMax, K1plus) %>%
                         as.data.frame())
  
  
  lh.paramspre2 <- reactive(list(
    S0 = input$global.S0,
    S1plus = input$global.S1plus,
    AgeMat = input$global.AgeMat,
    PlusGroupAge = input$global.AgeMat + 2,
    nages = input$global.AgeMat + 2,
    fmax = 0.29,
    z = 2.39, # I think I can remove this
    lambdaMax = input$lambdaMax,
    K1plus = K1plus_adv()
  ))
  
  # Calculate z based on user MNPL selection
  z.usr <- eventReactive(input$go, {
    calc_z(
      MNPL_in = input$MNPL.usr,
      lh.params_in = lh.paramspre2()
    )
  })
  
  # This is the functioning list of lh params
  lh.params2 <- reactive(list(
    S0 = input$global.S0,
    S1plus = input$global.S1plus,
    AgeMat = input$global.AgeMat,
    PlusGroupAge = input$global.AgeMat + 2,
    nages = input$global.AgeMat + 2,
    fmax = 0.29,
    z = z.usr(),
    lambdaMax = input$lambdaMax,
    K1plus = K1plus_adv()
  ))
  
  
  # Simulations - eventReactive()  ---------------------------------------------
  high.list.const <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantBycatch = list(Catch = input$constantcatch[2], CV = input$cvcatch),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  med.list.const <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantBycatch = list(Catch = stats::median(input$constantcatch), CV = input$cvcatch),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  low.list.const <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantBycatch = list(Catch = input$constantcatch[1], CV = input$cvcatch),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  zero.list.const <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantBycatch = list(Catch = 0, CV = 0),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  high.list.rate <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantRateBycatch = list(Rate = input$bycatchrate[2], CV = input$cvcatchrate),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  med.list.rate <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantRateBycatch = list(Rate = stats::median(input$bycatchrate), CV = input$cvcatchrate),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  low.list.rate <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantRateBycatch = list(Rate = input$bycatchrate[1], CV = input$cvcatchrate),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  zero.list.rate <- eventReactive(input$go, {
    lapply(
      X = InitDepl.vec(),
      function(x) {
        projections(
          NOut = as.numeric(input$nproj),
          ConstantRateBycatch = list(Rate = 0, CV = 0),
          InitDepl = x,
          lh.params = lh.params2(),
          nyears = nyears,
          obs_CV = input$obs_cv
        )
      }
    )
  })
  
  performance.table <- eventReactive(input$go, {
    make_ptable(
      traj.list = switch(input$crad,
                         "n_yr" = {
                           list(
                             high.list.const(),
                             med.list.const(),
                             low.list.const(),
                             zero.list.const()
                           )
                         },
                         "rate2" = {
                           list(
                             high.list.rate(),
                             med.list.rate(),
                             low.list.rate(),
                             zero.list.rate()
                           )
                         }
      ),
      depletion = InitDepl.vec(),
      lh.params = lh.params2(),
      mnpl = input$MNPL.usr
    )
  })
  
  # Observe() values --------------------------------------------------------
  
  
  output$BycatchIn <- renderPlot({
    bycatch <- vector()
    if (input$crad == "n_yr") {
      bycatch <- input$constantcatch
      cvb <- input$cvcatch
    }
    if (input$crad == "rate2") {
      bycatch <- input$bycatchrate
      cvb <- input$cvcatchrate
    }
    
    p <- plot_bycatch_guesses(
      lowval = bycatch[1],
      medval = stats::median(bycatch),
      highval = bycatch[2],
      cv = cvb,
      color.palette = colorscheme,
      lang = input$selected_language
    )
    plot(p)
  })
  
  output$z.out <- renderText({
    z <- z.usr()
    print(c("Calculated z:", round(z, digits = 2)))
  })
  
  output$MultiDeplNote <- renderText({
    paste("Results will still be based on the abundance and depletion you entered.")
  })
  
  # Projection plots (advanced) ---------------------------------------------
  
  output$projPlot1 <- renderPlot({
    # input$go
    # isolate({
    if (input$crad == "n_yr") {
      high.list <- high.list.const()
      med.list <- med.list.const()
      low.list <- low.list.const()
      zero.list <- zero.list.const()
    }
    
    if (input$crad == "rate2") {
      high.list <- high.list.rate()
      med.list <- med.list.rate()
      low.list <- low.list.rate()
      zero.list <- zero.list.rate()
    }
    
    stf <- input$spag
    nspag <- as.numeric(input$nproj)
    spaghetti <- ifelse(stf, nspag, FALSE)
    lh.params <- lh.params2()
    
    if (input$multipledepl) {
      multiplot_proj(
        high.d1 = high.list[[1]]$trajectories, # d1 is the lowest depletion
        med.d1 = med.list[[1]]$trajectories,
        low.d1 = low.list[[1]]$trajectories,
        
        high.d2 = high.list[[2]]$trajectories,
        med.d2 = med.list[[2]]$trajectories,
        low.d2 = low.list[[2]]$trajectories,
        
        high.d3 = high.list[[3]]$trajectories,
        med.d3 = med.list[[3]]$trajectories,
        low.d3 = low.list[[3]]$trajectories,
        ylims = c(0, lh.params$K1plus),
        spaghetti = spaghetti,
        years.plot = plotyears,
        K1plus = lh.params$K1plus,
        InitDepls = InitDepl.vec(),
        color.palette = colorscheme,
        lang = input$selected_language
      )
    } else {
      i <- 2 # Plots only the depletion level the user entered
      plot_proj(
        high = high.list[[i]],
        med = med.list[[i]],
        low = low.list[[i]],
        spaghetti = spaghetti,
        ylims = c(0, lh.params$K1plus),
        InitDepl = initdepl_adv(),
        K1plus = lh.params$K1plus,
        years.plot = plotyears,
        color.palette = colorscheme,
        lang = input$selected_language
      )
    }
    # }) # end "isolate" with go button
  })
  
  output$testPlot <- renderPlot({
    plot(1:20, 1:20)
  })
  
  output$yieldPlot <- renderPlot({
    plot_yield_curve(
      lh.params = lh.params2(),
      z = z.usr(),
      MNPL_in = input$MNPL.usr,
      lang = input$selected_language
    )
  })
  
  
  
  
  # LH table to show in "simple" tab -------------------------------------------
  tableReact <- reactive(lh %>%
                           select(Type, S0, S1plus, AgeMat) %>%
                           mutate(
                             S0 = round(S0, digits = 2),
                             S1plus = round(S1plus, digits = 2)
                           ) %>%
                           rename(
                             "S<sub>0</sub>" = S0,
                             "S<sub>1+</sub>" = S1plus,
                             "Age at maturity" = AgeMat
                           ) %>%
                           kableExtra::kable(escape = F) %>%
                           kableExtra::kable_styling("hover", full_width = F) %>%
                           kableExtra::column_spec(4, width = "3cm"))
  
  output$pinnipedplot <- plotly::renderPlotly({
    plot_pinnipeds(dat = dat, central = central, set_size = 10)
  })
  
  output$Later <- renderText({
    "Under construction"
  })
  
  
  # Violin plot of relative abundance ---------------------------------------
  
  output$relAbund <- renderPlot({
    if (input$crad == "n_yr") {
      high.list <- high.list.const() # elements of high.list are projections at different depletion levels
      med.list <- med.list.const()
      low.list <- low.list.const()
      zero.list <- zero.list.const()
    }
    if (input$crad == "rate2") {
      high.list <- high.list.rate() #***
      med.list <- med.list.rate()
      low.list <- low.list.rate()
      zero.list <- zero.list.rate()
    }
    
    withProgress(message = "Plotting performance", value = 0, {
      Kfig <- high.list[[1]]$params[1, "K1plus"]
      tp.all <- vector()
      
      level_names <- switch(input$selected_language,
                            "en" = c("Low","Med","High"),
                            "es" = c("Bajo","Medio","Alto"),
                            "fr" = c("Inférieure","Médian","Supérieure"))
      
      for (i in 1:3) { # Loop though different depletion levels
        combs <- data.frame(BycatchLvl = level_names)
        nsims <- as.numeric(input$nproj)
        tp <- expand(combs, BycatchLvl, sim = 1:nsims, Depl = InitDepl.vec()[i]) %>%
          as.data.frame()
        
        high.vec <- high.list[[i]]$trajectories[, 10] / Kfig
        med.vec <- med.list[[i]]$trajectories[, 10] / Kfig
        low.vec <- low.list[[i]]$trajectories[, 10] / Kfig
        tp$abund10 <- c(high.vec, low.vec, med.vec)
        
        high.vec <- high.list[[i]]$trajectories[, 20] / Kfig
        med.vec <- med.list[[i]]$trajectories[, 20] / Kfig
        low.vec <- low.list[[i]]$trajectories[, 20] / Kfig
        tp$abund20 <- c(high.vec, low.vec, med.vec)
        
        high.vec <- high.list[[i]]$trajectories[, 50] / Kfig
        med.vec <- med.list[[i]]$trajectories[, 50] / Kfig
        low.vec <- low.list[[i]]$trajectories[, 50] / Kfig
        tp$abund50 <- c(high.vec, low.vec, med.vec)
        
        tp.all <- rbind(tp.all, tp)
      }
      
      df <- reshape2::melt(tp.all, id.vars = c("BycatchLvl", "sim", "Depl"))
      
      df <- df %>%
        subset(Depl == initdepl_adv()) %>%
        mutate(variable = fct_recode(variable,
                                     "10" = "abund10",
                                     "20" = "abund20",
                                     "50" = "abund50"
        ))
      
      xlab_translated <- switch(input$selected_language,
                                "en" = "Years after projections start",
                                "es" = "Años después del inicio de las proyecciones",
                                "fr" = "nombre d'années après le début des projections"
      )
      ylab_translated <- switch(input$selected_language,
                                "en" = "Abundance relative to K",
                                "es" = "Abundancia relativa a K",
                                "fr" = "Abondance relative à K"
      )
      fill_translated <- switch(input$selected_language,
                                "en" = "Bycatch level",
                                "es" = "Nivel de mortalidad",
                                "fr" = "Niveau de la gamme de prises"
      )
      relK <- df %>%
        ggplot(aes(
          x = variable,
          y = value,
          fill = BycatchLvl,
          outlier.fill = BycatchLvl
        )) +
        scale_fill_manual(fill_translated,
                          values = colorscheme[c(3, 1, 2)]
        ) +
        geom_violin(scale = "width") +
        theme_classic(base_size = 16) +
        theme(legend.position = "none") +
        xlab(xlab_translated) +
        ylab(ylab_translated)
      
      relK
    }) # end "withProgress"
  })
  
  
  
  # Relative to unfished
  output$relUnfished <- renderPlot({
    # input$PMButton
    # isolate({
    if (input$crad == "n_yr") {
      high.list <- high.list.const()
      med.list <- med.list.const()
      low.list <- low.list.const()
      zero.list <- zero.list.const()
    }
    if (input$crad == "rate2") {
      high.list <- high.list.rate()
      med.list <- med.list.rate()
      low.list <- low.list.rate()
      zero.list <- zero.list.rate()
    }
    
    Kfig <- high.list[[1]]$params[1, "K1plus"] #
    tp.all <- vector()
    level_names <- switch(input$selected_language,
                          "en" = c("Low","Med","High"),
                          "es" = c("Bajo","Medio","Alto"),
                          "fr" = c("Inférieure","Médian","Supérieure"))
    
    withProgress(message = "Plotting performance", value = 0, {
      for (i in 1:3) {
        incProgress(1 / 3, detail = paste("Plotting bycatch level", i))
        nsims <- input$nproj
        combs <- data.frame(BycatchLvl = level_names)
        tp <- expand(combs,
                     BycatchLvl,
                     sim = 1:nsims,
                     Depl = InitDepl.vec()[i]
        ) %>%
          as.data.frame()
        
        zero.scen <- zero.list[[i]]$trajectories
        high.vec <- high.list[[i]]$trajectories[, 10] / zero.scen[, 10]
        med.vec <- med.list[[i]]$trajectories[, 10] / zero.scen[, 10]
        low.vec <- low.list[[i]]$trajectories[, 10] / zero.scen[, 10]
        tp$abund10 <- c(high.vec, low.vec, med.vec)
        
        high.vec <- high.list[[i]]$trajectories[, 20] / zero.scen[, 20]
        med.vec <- med.list[[i]]$trajectories[, 20] / zero.scen[, 20]
        low.vec <- low.list[[i]]$trajectories[, 20] / zero.scen[, 20]
        tp$abund20 <- c(high.vec, low.vec, med.vec)
        
        high.vec <- high.list[[i]]$trajectories[, 50] / zero.scen[, 50]
        med.vec <- med.list[[i]]$trajectories[, 50] / zero.scen[, 50]
        low.vec <- low.list[[i]]$trajectories[, 50] / zero.scen[, 50]
        
        tp$abund50 <- c(high.vec, low.vec, med.vec)
        tp.all <- rbind(tp.all, tp)
      }
      
      df <- reshape2::melt(tp.all, id.vars = c("BycatchLvl", "sim", "Depl"))
      
      xlab <- switch(input$selected_language,
                     "en" = "Years after projections start",
                     "es" = "Años después del inicio de las proyecciones",
                     "fr" = "nombre d'années après le début des projections")
      ylab <- switch(input$selected_language,
                     "en" = "Abundance relative to a \n no bycatch scenario",
                     "es" = "Abundancia relativa a \n un escenario sin captura incidental",
                     "fr" = "Abondance par rapport à \n un scénario sans prises accessoires")
      lvl_name <- switch (input$selected_language,
                          "en" = "Bycatch level",
                          "es" = "Nivel de mortalidad",
                          "fr" = "Niveau de la gamme de prises")
      
      df <- df %>%
        subset(Depl == initdepl_adv()) %>% # Only show mid depletion level
        mutate(variable = fct_recode(variable,
                                     "10" = "abund10",
                                     "20" = "abund20",
                                     "50" = "abund50"))
      #print(str(df))
      
      relunfished <- ggplot(df, 
                            aes(x = variable, y = value, 
                                fill = BycatchLvl,
                                outlier.fill = BycatchLvl)) +
        scale_fill_manual(lvl_name,
                          values = colorscheme[c(3, 1, 2)]) +
        geom_violin(scale = "width") +
        theme_classic(base_size = 16) +
        xlab(xlab) +
        ylab(ylab)
      relunfished
      #  }) #add when you need a progress bar
    })
  })
  
  
  output$PMtable <- function() {
    textvalues <- colorscheme
    values <- add_trans(textvalues, trans = 100)
    
    pmkey <- switch(input$selected_language,
                    "en" = pmkey_en,
                    "es" = pmkey_es,
                    "fr" = pmkey_fr
    )
    newranges <- switch(input$selected_language,
                        "en" = bylevels_en,
                        "es" = bylevels_es,
                        "fr" =  bylevels_fr
    )
    
    PM <- performance.table() %>%
      pivot_longer(cols = prebuild50:abundrel50) %>%
      pivot_wider(names_from = bycatch) %>%
      mutate(name = recode(name, !!!pmkey)) %>%
      filter(depletion == initdepl_adv()) %>%
      select(-depletion, -zero) %>%
      rename_with(~newranges, c("name", "low", "med", "high"))
    
    #print(PM)
    #dput(performance.table())
    kableExtra::kable(PM) %>%
      kableExtra::column_spec(1, width = "10em") %>%
      kableExtra::column_spec(2, color = textvalues[3], width = "7em") %>%
      kableExtra::column_spec(3, color = textvalues[2], width = "7em") %>%
      kableExtra::column_spec(4, color = textvalues[1], width = "7em") %>%
      kableExtra::kable_styling("striped", full_width = F, position = "center")
  }
  
  
  output$PMkite <- renderPlot({
    pt <- performance.table() %>%
      filter(depletion == initdepl_adv())
    nicenames <- data.frame(
      original = colnames(pt)[-c(1, 2)],
      polished = switch(input$selected_language,
                        "en" = polished_en,
                        "es" = polished_es,
                        "fr" = polished_fr
      )
    )
    
    axis.ind <- match(
      x = colnames(pt)[-c(1, 2)],
      table = nicenames$original
    )
    axis.labels <- nicenames$polished[axis.ind]
    
    rangenames <- switch(input$selected_language,
                         "en" = rangenames_en,
                         "es" = rangenames_es,
                         "fr" = rangenames_fr
    )
    PM.try2 <- pt %>%
      select(-depletion) %>%
      filter(bycatch != "zero") %>%
      mutate(bycatch = recode_factor(bycatch,
                                     "low" = rangenames[1],
                                     "med" = rangenames[2],
                                     "high" = rangenames[3]
      ))
    kiteplot <- ggradar(PM.try2,
                        axis.labels = axis.labels,
                        grid.label.size = 3,
                        axis.label.size = 4,
                        legend.text.size = 6,
                        plot.legend = T,
                        palette.vec = colorscheme[c(3, 1, 2)]
    )
    plot(kiteplot)
  })
  
  # Estimating PBR and other parameters -------------------------------------
  Nbest <- reactive(input$popsize_usr)
  
  Ndist <- reactive(0.842 * sqrt(log(1 + input$obs_cv^2))) # 0.842 = -1*qnorm(0.2,0,1)
  
  Nmin <- reactive(Nbest() / exp(Ndist()))
  
  PBR.metrics <- reactive(list(
    depl = initdepl_adv(),
    lh.params = lh.params2(),
    Nbest = Nbest(), # user-defined
    Ndist = Ndist(),
    Nmin = Nmin(),
    Rmax = input$lambdaMax - 1
  ))
  
  PBRDefinitions <- reactive({
    switch(input$selected_language,
           "en" = pbrdefs_en,
           "es" = pbrdefs_es,
           "fr" = pbrdefs_fr
    )
  })
  
  output$PBR.table <- function() { # kableExtra table
    PB <- data.frame(
      "." = c("N<sub>BEST</sub>", "N<sub>MIN</sub>", "R<sub>MAX</sub>"),
      Definition = PBRDefinitions(),
      Value = c(
        PBR.metrics()$Nbest,
        PBR.metrics()$Nmin,
        round(PBR.metrics()$Rmax, digits = 2)
      )
    )
    kableExtra::kable(PB, escape = FALSE) %>%
      kableExtra::kable_styling("striped")
  }
  
  # PBR CALCULATOR ----------------------------------------------------------
  Nbest <- reactive(input$popsize_usr)
  
  Ndist <- reactive(0.842 * sqrt(log(1 + input$obs_cv^2))) # 0.842 = -1*qnorm(0.2,0,1)
  
  Nmin <- reactive(Nbest() / exp(Ndist()))
  
  PBR.metrics <- reactive(list(
    depl = initdepl_adv(),
    lh.params = lh.params2(),
    Nbest = Nbest(), # user-defined
    Ndist = Ndist(),
    Nmin = Nmin(),
    Rmax = input$lambdaMax - 1
  ))
  
  output$PBRprint <- renderText({
    P <- input$Nmin.usr * 0.5 * input$Rmax.usr * input$fr.usr
    paste0("PBR = ", P)
  })
  
  
  # SOLVE FOR PERFORMANCE ---------------------------------------------------
  recovery.goal2 <- reactive({
    input$solveButton1
    isolate({
      withProgress(
        message = switch(input$selected_language,
                         "en" = "Calculation in progress",
                         "es" = "Cálculo en curso",
                         "fr" = "Calcul en cours"
        ),
        detail = switch(input$selected_language,
                        "en" = "This may take a while...",
                        "es" = "Esto puede tardar un rato...",
                        "fr" = "Cela peut prendre un peu de temps"
        ),
        value = 0,
        {
          lh.params <- lh.params2()
          rg <- rebuild_by_x(
            needf.start = 0.5,
            init.depl.w = initdepl_adv(),
            goal.w = input$recover.abund * lh.params$K1plus, # abund, as proportion of K1+
            desired.prob.w = input$recover.prob, # test value: 0.6
            when.w = input$recover.year, # test value: 50
            lh.params.w = lh.params,
            fixed.cv.catch.w = input$cvcatchrate # test value: 0.3
          )
          rg
        }
      )
    })
  })
  
  output$solvePMs <- function() { # kableExtra table
    goal_label <- switch(input$selected_language,
                         "en" = "Max bycatch rate to reach goal",
                         "es" = "Máxima captura incidental para alcanzar el objetivo",
                         "fr" = "Taux maximal de prises accessoires pour atteindre l'objectif"
    )
    rg <- recovery.goal2()
    RG <- data.frame(recovery.goal = ifelse(length(rg) > 1, round(rg$f, digits = 3), "--"))
    colnames(RG) <- goal_label
    kableExtra::kable(RG) %>% kableExtra::kable_styling("striped")
  }
  
  output$solvePMsPlot <- renderImage(
    {
      if (length(recovery.goal2()) > 1) {
        outfile <- tempfile(fileext = ".gif")
        guesses <- recovery.goal2()$mat
        guesses <- guesses[which(guesses[, 1] > 0), ]
        gdf <- data.frame(
          Attempt = 1:nrow(guesses),
          fishing.rate = guesses[, 1],
          probability = guesses[, 2]
        )
        xlab_translated <- switch(input$selected_language,
                                  "en" = "Bycatch rate",
                                  "es" = "Tasa de captura incidental",
                                  "fr" = "Taux de prises accessoires"
        )
        ylab_translated <- switch(input$selected_language,
                                  "en" = "Recovery probability",
                                  "es" = "Probabilidad de recuperación",
                                  "fr" = "Probabilité de reconstitution"
        )
        
        p <- ggplot(gdf, aes(x = fishing.rate, y = probability)) +
          xlab(xlab_translated) +
          ylab(ylab_translated) +
          geom_point(size = 4, aes(colour = Attempt)) +
          scale_colour_viridis_c() +
          gganimate::transition_states(Attempt, 1, 1) +
          gganimate::shadow_mark(past = TRUE) +
          geom_hline(yintercept = input$recover.prob, colour = "lightblue") +
          theme_minimal(base_size = 24) +
          theme(legend.position = "none")
        
        gganimate::anim_save(
          "outfile.gif",
          gganimate::animate(p, renderer = gganimate::magick_renderer(loop = F))
        ) # New
        
        # Return a list containing the filename
        list(
          src = "outfile.gif",
          contentType = "image/gif",
          width = 350,
          height = 350
        )
      } else {
        return(list(
          src = "www/NoRecovery.png",
          contentType = "image/png",
          alt = "NoRecoveryPossible"
        ))
      }
    },
    deleteFile = TRUE
  ) # end of renderImage
  
  
  # DOWNLOAD REPORT ---------------------------------------------------------
  
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(from = app_sys("report","report.Rmd"), tempReport, overwrite = TRUE)
      
      # Life history params and MNPL
      lh.params <- as.data.frame(lh.params2())
      mnpl <- input$MNPL.usr
      
      # Performance table
      xx <- performance.table()
      
      if (input$crad == "n_yr") {
        catchlevels <- input$constantcatch
      } else {
        catchlevels <- input$bycatchrate
      }
      
      tm <- xx %>%
        mutate(`Bycatch` = ifelse(bycatch == "low", catchlevels[1], 
                                  ifelse(bycatch == "high", catchlevels[2], 
                                         ifelse(bycatch == "zero", 0, 
                                                stats::median(catchlevels))))) %>%
        select(depletion, `Bycatch`, prebuild50, prebuild100, abundrel10, abundrel20, abundrel50) %>%
        rename(
          `Initial depletion (N/K)` = depletion,
          `Probability (Above MNPL in 50 years)` = prebuild50,
          `Probability (Above MNPL in 100 years` = prebuild100,
          `Abundance relative to K after 10 years` = abundrel10,
          `Abundance relative to K after 20 years` = abundrel20,
          `Abundance relative to K after 50 years` = abundrel50
        )
      
      
      # PBR part
      Nbest <- input$Nbest.usr
      Nmin <- input$Nmin.usr
      Rmax <- input$Rmax.usr
      Fr <- input$fr.usr
      PBR <- Nmin * 0.5 * Rmax * Fr
      PB <- data.frame(
        Parameter = c("Nbest", "Nmin", "Rmax", "Fr", "PBR"),
        Definition = c(
          "Estimate of total population size (this is the population size that corresponds to the initial depletion you entered in the Advanced Projections tab",
          "Minimum population estimate (this will be the same as the estimate of the population size if you have not included an observation error in the Advanced Projections tab)",
          "Maximum theoretical or estimated productivity rate of the stock",
          "Recovery factor (see below)",
          "Potential Biological Removal (PBR)"
        ),
        Estimate = c(Nbest, Nmin, Rmax, Fr, PBR)
      )
      #
      #
      
      params <- list(lh.params = lh.params, performance = tm, pbr = PB)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) #/ downloadHandler
  
  
  # DOCUMENTATION -----------------------------------------------------------
  dpath <- reactive(app_sys("Documentation", 
    paste0("ProjectionModel_", input$selected_language, ".html")
  ))
  
  
  # PAGE CONTENT ------------------------------------------------------------
  output$page_content <- renderUI({
    tagList(
      navbarPage(
        title = "",
        id = "inTabset",
        # Home page ------------------------------------------------------------
        tabPanel(
          title = i18n$t("Home"),
          uiOutput("splash"),
          hr(),
          fluidRow(
            column(
              6,
              uiOutput("tn_simple")
            ),
            column(
              6,
              uiOutput("tn_advanced")
            )
          ) # /fluidRow
        ),
        
        # Simple projections ------------------------------------------------------------
        tabPanel(
          title = i18n$t("Simple"),
          value = "simple",
          h3(i18n$t("Simple Projections")),
          br(),
          br(),
          p(i18n$t("This tab shows model projections of abundance, given very limited data. To change the projections to fit your case:")),
          tags$ol(
            tags$li(i18n$t("Choose the life history type that most closely resembles your marine mammal of interest.")),
            tags$li(i18n$t("Enter the current population size.")),
            tags$li(i18n$t("Configure the projection to use one of the following:")),
            tags$ul(
              tags$li(i18n$t("Numbers per year - the number of mammals killed each year")),
              tags$li(i18n$t("Mortality rate - mammals killed each year as a proportion of the population"))
            )
          ), # /tags$ol
          br(),
          br(),
          fluidRow(
            column(
              4,
              selectInput("type_simple",
                          label = i18n$t("Mammal life history type (choose one):"),
                          stats::setNames(paste0(lh$Code, "_simple"), lh$Type)
              ), # quickly put dataframe elements as options! :)
              radioButtons(
                inputId = "vdepln_simple",
                label = i18n$t("History of human-caused mortality:"),
                choiceNames = i18n$t(c(
                  "The population has been reduced by 75%",
                  "The population has been reduced by 50%",
                  "The population has been reduced by 25%"
                )),
                choiceValues = c("lo", "med", "hi")
              ),
              h4(i18n$t("Specify bycatch")),
              
              radioButtons(
                inputId = "crad_simple",
                label = i18n$t("Project using:"),
                choiceNames = i18n$t(c("Numbers per year", "Mortality rate")),
                choiceValues = c("n_yr_simple", "rate2_simple")
              ), # UPDATE CHOICE
              numericInput("popsize_usr_simple",
                           label = i18n$t("Current population size:"),
                           min = 1, max = 1e9,
                           value = 1000
              ),
              
              conditionalPanel(
                condition = "input.crad_simple == 'n_yr_simple'",
                sliderInput("constantcatch_simple",
                            label = i18n$t("Bycatch mortality range to explore:"),
                            min = 0, max = 1000,
                            value = c(50, 100)
                ),
                numericInput("cvcatch_simple",
                             label = i18n$t("Bycatch coefficient of variation (CV):"),
                             min = 0, max = 1,
                             value = 0.5
                )
              ),
              
              conditionalPanel(
                condition = "input.crad_simple == 'rate2_simple'",
                sliderInput("bycatchrate_simple",
                            label = i18n$t("Bycatch mortality range to explore:"),
                            min = 0, max = 0.5, value = c(0.05, 0.5)
                ),
                sliderInput("cvcatchrate_simple",
                            label = i18n$t("Bycatch coefficient of variation (CV)"),
                            min = 0, max = 0.8, value = 0.5
                )
              ),
              
              checkboxInput(
                "spag_simple",
                i18n$t("Show individual projections")
              )
            ),
            column(
              8,
              plotOutput("projPlotSimple")
            )
          )
        ), # end simple tab
        
        # Advanced projections ----------------------------------------------------
        
        tabPanel(
          title = i18n$t("Advanced"),
          value = "advanced",
          withMathJax(),
          h3(i18n$t("Advanced Projections")),
          br(),
          br(),
          p(i18n$t("The graphs below are model projections of abundance, based on options you select. To change the projections to fit your case:")),
          tags$ol(
            tags$li(i18n$t("Choose the life history type that most closely resembles your marine mammal of interest or type in parameter values in the boxes provided.")),
            tags$li(i18n$t("Enter an estimate of current abundance and the uncertainty around that estimate.")),
            tags$li(i18n$t("Select the history of human-caused mortality that best fits the population.")),
            tags$li(i18n$t("Configure the projection to use one of the following:")),
            tags$ul(
              tags$li(i18n$t("Numbers per year - the number of mammals killed each year")),
              tags$li(i18n$t("Mortality rate - mammals killed each year as a proportion of the size of the population"))
            ),
            tags$li(i18n$t("Enter a value for the Maximum Net Productivity Level (MNPL) as a proportion of carrying capacity (K)."))
          ),
          p(
            i18n$t("Once you have selected your options, click"),
            strong(i18n$t("Run Projections"))
          ),
          br(),
          p(em(i18n$t("Performance measures below will be based on what you enter here."))),
          br(),
          br(),
          fluidRow(
            column(
              5,
              h4(i18n$t("Specify life history and abundance")),
              selectInput("type",
                          label = i18n$t("Life history type (choose one):"),
                          c(stats::setNames(paste(lh$Code), lh$Type))
              ),
              fluidRow(
                column(3, numericInput("global.S0", "\u0053\u2080",
                                       0.9,
                                       min = 0.01, max = 0.99,
                                       step = 0.01
                )),
                column(3, numericInput("global.S1plus", "\u0053\u2081\u208a",
                                       0.9,
                                       min = 0.01, max = 0.99,
                                       step = 0.01
                )),
                column(3, numericInput("global.AgeMat", "AgeMat", 8,
                                       min = 2, max = 200
                ))
              ),
              numericInput("lambdaMax",
                           label = i18n$t("Maximum steady rate of population growth (\\( \\lambda_{max} \\))"),
                           value = 1.04,
                           min = 0.01, max = 1.5,
                           step = 0.01
              ),
              shinyBS::bsTooltip(
                id = "lambdaMax",
                title = i18n$t("This is the maximum steady rate of population increase. The default values are 1.04 for cetaceans and 1.12 for pinnipeds")
              ),
              numericInput("popsize_usr",
                           label = i18n$t("Current population size:"),
                           value = 1000,
                           min = 1, max = 1e9
              ),
              sliderInput("obs_cv",
                          label = i18n$t("Coefficient of variation (CV) of abundance:"),
                          min = 0, max = 3,
                          value = 0, step = 0.1
              ),
              shinyBS::bsTooltip(
                id = "obs_cv",
                title = i18n$t("This is the survey CV of abundance")
              ),
              radioButtons(
                inputId = "vdepln_adv",
                label = i18n$t("History of human-caused mortality:"),
                choiceNames = i18n$t(
                  c(
                    "The population has been reduced by 75%",
                    "The population has been reduced by 50%",
                    "The population has been reduced by 25%"
                  )
                ),
                choiceValues = c("lo", "med", "hi")
              ),
              h4(i18n$t("Specify bycatch")),
              radioButtons(
                inputId = "crad",
                label = i18n$t("Project using:"),
                choiceNames = i18n$t(c("Numbers per year", "Mortality rate")),
                choiceValues = c("n_yr", "rate2")
              ),
              conditionalPanel(
                condition = "input.crad == 'n_yr'",
                sliderInput("constantcatch",
                            label = i18n$t("Bycatch mortality range to explore"),
                            min = 0, max = 2000,
                            value = c(10, 500)
                ),
                sliderInput("cvcatch",
                            label = i18n$t("CV of bycatch mortality:"),
                            min = 0, max = 0.8,
                            value = 0.5,
                            step = 0.1
                ),
                shinyBS::bsTooltip(
                  id = "cvcatch",
                  title = i18n$t("This is the CV of the number of animals caught per year")
                )
              ),
              
              conditionalPanel(
                condition = "input.crad == 'rate2'",
                sliderInput("bycatchrate",
                            label = i18n$t("Bycatch mortality range to explore:"),
                            min = 0, max = 0.5,
                            value = c(0.05, 0.5)
                ),
                sliderInput("cvcatchrate",
                            label = i18n$t("CV of bycatch mortality rate:"),
                            min = 0, max = 1,
                            value = 0.5
                )
              ),
              shinyBS::bsTooltip(
                id = "cvcatchrate",
                title = i18n$t("This is the CV of bycatch mortality from year to year")
              ),
              
              selectInput("nproj",
                          label = i18n$t("Number of projections:"),
                          c(
                            "10" = 10,
                            "50" = 50,
                            "100" = 100
                          ),
                          selected = "50"
              ),
              
              checkboxInput("spag", i18n$t("Show individual projections")),
              checkboxInput("multipledepl", i18n$t("Show multiple depletion levels")),
              conditionalPanel(
                condition = "input.multipledepl==true",
                textOutput("MultiDeplNote")
              ),
              br(),
              sliderInput(
                inputId = "MNPL.usr",
                label = i18n$t("Max net productivity level (MNPL)"),
                min = 0.4, max = 0.9,
                value = 0.5, step = 0.1,
                round = -1
              ),
              textOutput(outputId = "z.out"),
              br(),
              actionButton("go",
                           label = i18n$t("Run projections"),
                           icon("cog", lib = "glyphicon", "fa-2x"), # Button to make projections go
                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              p(i18n$t("Scroll down to see more outputs"))
            ),
            column(
              7,
              plotOutput("projPlot1"),
              br(),
              br(),
              plotOutput("yieldPlot"),
              br(),
              br(),
              plotOutput("BycatchIn")
            )
          ) # end fluidRow
          , # Performance measures
          h4(i18n$t("Performance")),
          p(i18n$t("This table shows median performance from all projections. If you entered multiple depletion levels in the Advanced Projections tab, this table is showing just the intermediate depletion level (the one you entered manually).")),
          fluidRow(column(
            12,
            tableOutput("PMtable")
          )),
          h4(i18n$t("Graphics")),
          p(i18n$t("This is a kite plot showing the same performance measures above.")),
          plotOutput("PMkite"),
          shinyBS::bsTooltip(
            id = "PMkite",
            title = i18n$t("Outer edge = best performance. All performance measures are on a 0-1 scale.")
          ),
          p(i18n$t("These plots show projected marine mammal population abundance relative to their carrying capacity K and an 'unfished' (no bycatch) state. The x axis represents the number of years after the start of the projections, and the violin plots show probability distributions.")),
          fluidRow(
            column(6, plotOutput("relAbund")),
            column(6, plotOutput("relUnfished"))
          ),
          shinyBS::bsTooltip(
            id = "relAbund",
            title = i18n$t("This is the distribution of the abundance relative to K, based on bycatch variation and observation error")
          ),
          br(),
          h3(i18n$t("Values needed to calculate PBR")),
          column(
            12,
            tableOutput("PBR.table"),
            h4(i18n$t("Notes")),
            tags$ol(
              tags$li(i18n$t("If you selected 'Show multiple depletion levels', these results are still based on the starting abundance you entered above and not the lower and upper values.")),
              # tags$li("If NMIN and Nbest are the same, you must enter a survey CV of abundance."), #txt73
              tags$div(HTML(i18n$t("<li>If N<sub>MIN</sub> and N<sub>BEST</sub> are the same, you must enter a survey CV of abundance.</li>"))),
              tags$li(i18n$t("For this table to give the correct values, the user has to provide an estimate of the population size. Otherwise, the numbers below will not be based on a real population size.")),
              tags$li(i18n$t("You will need to enter these values in the PBR calculator tab in order to get a value for PBR."))
            ) # txt75
          )
        ), # end advanced tab
        
        # PBR calculator ----------------------------------------------------------
        tabPanel(
          i18n$t("PBR & \n PBR calculator"),
          withMathJax(),
          h3(i18n$t("Calculate PBR")),
          h4(i18n$t("The PBR approach")),
          helpText(i18n$t("Potential Biological Removal (PBR) is the maximum number of animals, excluding natural mortalities, that may be removed from a marine mammal stock while allowing that stock to be at its optimum sustainable population (MMPA, sec. 3(20)).")), # txt79
          helpText(i18n$t("PBR is the product of the minimum population estimate, half of the maximum theoretical or estimated productivity rate of the stock, and a recovery factor between 0.1 and 1.0:")),
          helpText("$$ \\huge N_{MIN} \\cdot \\frac{1}{2}R_{MAX}\\cdot F_R$$"),
          tags$div(HTML(i18n$t("<span class='help-block'>Note: To use this feature you need to have run projections in the Advanced tab. The values you need in order to calculate PBR are available in that tab. While the Advanced tab automatically calculates N<sub>MIN</sub> from the numbers you entered, you can use your own abundance estimates here if you want. Knowing carrying capacity and depletion is not required, only the abundance estimate and the CV of that estimate.</span>"))),
          br(),
          fluidRow(
            column(
              6,
              numericInput("Nbest.usr", label = "\\( N_{BEST} \\)", value = NA),
              numericInput("Nmin.usr", label = "\\( N_{MIN} \\)", value = NA)
            ),
            column(
              6,
              numericInput("Rmax.usr", label = "\\( R_{MAX} \\)", value = NA),
              numericInput("fr.usr", label = "\\( F_R \\)", value = 0.5, step = 0.1, max = 1, min = 0)
            )
          ),
          shinyBS::bsTooltip(
            id = "Nbest.usr",
            title = i18n$t("Enter the best available estimate of abundance")
          ),
          shinyBS::bsTooltip(
            id = "Rmax.usr",
            title = i18n$t("RMAX is equivalent to lambdamax - 1")
          ),
          fluidRow(
            style = "border: 2px solid #2c3e50;",
            
            column(12, align = "center", br(), textOutput("PBRprint"), br())
          ),
          tags$head(tags$style("#PBRprint{color: #2c3e50;
                                 font-size: 30px;
                                            font-style: bold;
                                            }")),
          br(),
          h4(i18n$t("What recovery factor should you use?")),
          p(
            i18n$t("The recovery factor is set in the U.S. based on the status of the stock. See the"),
            a(href = "https://www.fisheries.noaa.gov/national/marine-mammal-protection/marine-mammal-protection-act", "MMPA"), i18n$t("or the most recent"),
            a(href = "https://www.fisheries.noaa.gov/webdam/download/64669267", "GAMMS"),
            i18n$t("for more detail.")
          ),
          p(strong("0.1-0.3"), i18n$t(" for endangered species or stocks known to be declining")),
          p(strong("0.4-0.5"), i18n$t(" for threatened or depleted species, and for stocks of unknown status")),
          p(strong("up to 1.0"), i18n$t(" for stocks known to be at their optimum sustainable population (OSP) level, or of unknown status but known to be increasing")),
          br(),
          h4(i18n$t("What if I don't have a good abundance or bycatch estimate?")),
          p(
            i18n$t("The Ocean Modeling Forum's Marine Mammal Bycatch Working Group is in the process of synthesizing estimation tools to help with the abundance estimates that are needed for estimating PBR. These tools have not yet been published but they will be available"),
            a(href = "http://oceanmodelingforum.org/working-groups/marine-mammal-bycatch-working-group/", i18n$t("here."))
          ),
          br(),
          h3(i18n$t("Create a report")),
          p(i18n$t("Would you like to download a report of your inputs and outputs? The button below will write an html file that you can save and share.")),
          br(),
          p(i18n$t("Make sure you have clicked"), em(i18n$t("Run Projections."))),
          downloadButton(outputId = "report", i18n$t("Generate report"))
        ), # end PBR tab
        tabPanel(
          i18n$t("Solve for Bycatch"),
          h4(i18n$t("Calculate the bycatch rate necessary to achieve a recovery goal")),
          p(i18n$t("If you have a specific management goal for your marine mammal population of interest, put in the desired recovery goal and timeline below. This part of the app will calculate an approximate bycatch rate that will allow the population to reach that goal. This page uses information from the 'Advanced Projections' tab to make this calculation. Once the plot appears, the animation will show the bycatch rates that the solver has attempted in its search for the max bycatch rate that will allow you to achieve your goal.")),
          br(),
          p(
            strong(i18n$t("NOTE:")),
            i18n$t("This will take a while! Make sure you are sure of your goals before clicking"), em(i18n$t("Get max bycatch rate"))
          ),
          column(
            6,
            sliderInput("recover.abund",
                        label = i18n$t("Recovery goal as a proportion of K:"),
                        min = 0, max = 1, value = 0.5
            ),
            sliderInput("recover.year",
                        label = i18n$t("Year by which you want to reach the recovery goal:"),
                        min = 1, max = 100, value = 50
            ),
            sliderInput("recover.prob",
                        label = i18n$t("Desired probability of rebuilding:"),
                        min = 0.01, max = 1, step = 0.01, value = 0.6
            ),
            actionButton("solveButton1",
                         label = i18n$t("Get max bycatch rate"),
                         icon("cog", lib = "glyphicon", "fa-2x"),
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          ),
          column(
            6, tableOutput("solvePMs"),
            imageOutput("solvePMsPlot")
          )
        ), # end solve for bycatch tab
        tabPanel(
          i18n$t("About the Model"),
          includeHTML(dpath()),
          h2(i18n$t("Life history parameters for pinnipeds")),
          p(i18n$t("Pinniped life histories are diverse and to our knowledge, the 'best' values for simulation have not been published. These are published life history parameters for pinniped species.")),
          fluidRow(
            column(12, 
                   plotly::plotlyOutput("pinnipedplot"))
          ),
          br(),
          h4(i18n$t("Where do these values come from?")),
          p(i18n$t("The values used for generic life history types shown here are a combination of literature values and estimates from a hierarchical analysis. Each life history type is represented by one species, which has survival rates and an age at maturity close to the type-level mean. Humpback whale, bottlenose dolphin, phocid seal, fur seal, and sea lion values are the same as in Punt et al. (2018).  For cetaceans not in Punt et al. (2018), the default survival rates and age at maturity are based on a hierarchical analysis following the methods in Dillingham et al. (2016). All references are listed at the bottom of this tab.")),
          includeHTML(app_sys("Documentation", "Citations_ed.html"))
        ) # end About the Model tab
      ), # end navbarpage
      tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    "))
    ) # end taglist
  }) # end renderUI
}
