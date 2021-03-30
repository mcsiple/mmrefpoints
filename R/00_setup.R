# 00_constants and setup

# 00 constants to load

plot <- FALSE
nyears <- 100
nyears.simple <- 100
plotyears <- 50
cat <- message
options(shiny.reactlog = TRUE)
i18n <- shiny.i18n::Translator$new(translation_json_path = system.file(package = "mmrefpoints","Documentation",
  "translation_maritza.json"
))

# Splash text -------------------------------------------------------------

# Splash page jumbotron
splash_en <- "<div class='jumbotron'>
  <h2>Welcome to the Marine Mammal Bycatch Impacts Exploration Tool</h2>
  <p>This tool shows population projections under different bycatch mortality levels, based on information you provide. You can also use it to calculate the parameters for Potential Biological Removal (PBR).</p>
  </div>"

splash_es <- "<div class='jumbotron'>
  <h2>Bienvenido a la herramienta de exploración de impactos de la captura incidental de mamíferos marinos</h2>
  <p>Esta herramienta muestra las proyecciones de poblaciones de mamíferos marinos bajo diferentes niveles de mortalidad por captura incidental, según la información que proporcione el usuario. También puede usarlo para calcular los parámetros para Remoción Biológica Potencial (Potential Biological Removal; PBR).</p>
</div>"

splash_fr <- "<div class='jumbotron'>
  <h2>Bienvenue dans l'outil d'exploration des impacts des prises accessoires de mammifères marins</h2>
  <p>Ce logiciel permet de visualiser les projections démographiques selon différents niveaux de mortalité par prises accessoires, en fonction des informations que vous fournissez. Vous pouvez également l'utiliser pour calculer les paramètres définissant le 'Potential Biological Removal' (PBR).</p>
  </div>"

# simple thumbnails
tn_en <- "<div class='row'>
  <div class='col-sm-14 col-md-12'>
    <div class='thumbnail'>
      <img src='www/1.png' alt='...'>
        <div class='caption'>
          <h3>Simple Projections</h3>
          <p>If you have minimal information about abundance and bycatch, go here.</p>
          <button id='jumpToSimple' type='button' class='btn btn-default action-button'>Simple Projections</button>
        </div>
      </img>
    </div>
  </div>
</div>"

tn_es <- "<div class='row'>
  <div class='col-sm-14 col-md-12'>
    <div class='thumbnail'>
      <img src='www/1.png' alt='...'>
        <div class='caption'>
          <h3>Proyecciones simples</h3>
          <p>Si tiene información mínima sobre la abundancia y la captura incidental, vaya aquí.</p>
          <button id='jumpToSimple' type='button' class='btn btn-default action-button'>Proyecciones simples</button>
        </div>
      </img>
    </div>
  </div>
</div>"

tn_fr <- "<div class='row'>
  <div class='col-sm-14 col-md-12'>
    <div class='thumbnail'>
      <img src='www/1.png' alt='...'>
        <div class='caption'>
          <h3>Projections simples</h3>
          <p>Si vous ne possédez que peu d'informations sur les abondances et les prises accessoires, cliquez ici.</p>
          <button id='jumpToSimple' type='button' class='btn btn-default action-button'>Projections simples</button>
        </div>
      </img>
    </div>
  </div>
</div>"

# advanced thumbnails
tn_en_adv <- "<div class='row'>
  <div class='col-sm-14 col-md-12'>
    <div class='thumbnail'>
      <img src='www/2.png' alt='...'>
        <div class='caption'>
          <h3>Advanced Projections</h3>
          <p>If you have information about abundance and bycatch, and want to evaluate performance or calculate PBR, go here.</p>
          <button id='jumpToAdv' type='button' class='btn btn-default action-button'>Advanced Projections</button>
        </div>
      </img>
    </div>
  </div>
</div>"

tn_es_adv <- "<div class='row'>
  <div class='col-sm-14 col-md-12'>
    <div class='thumbnail'>
      <img src='www/2.png' alt='...'>
        <div class='caption'>
          <h3>Proyecciones Avanzadas</h3>
          <p>Si tiene información sobre la abundancia y la captura incidental, y desea evaluar el rendimiento o calcular el PBR, vaya aquí.</p>
          <button id='jumpToAdv' type='button' class='btn btn-default action-button'>Proyecciones Avanzadas</button>
        </div>
      </img>
    </div>
  </div>
</div>"

tn_fr_adv <- "<div class='row'>
  <div class='col-sm-14 col-md-12'>
    <div class='thumbnail'>
      <img src='www/2.png' alt='...'>
        <div class='caption'>
          <h3>Projections avancées</h3>
          <p>Si vous possédez des informations à la fois sur les abondances et les prises accessoires et que vous souhaitez évaluer les performances de reconstitution du stock ou calculer le PBR, cliquez ici.</p>
          <button id='jumpToAdv' type='button' class='btn btn-default action-button'>Projections avancées</button>
        </div>
      </img>
    </div>
  </div>
</div>"


# -------------------------------------------------------------------------

# Plot labels -------------------------------------------------------------

# kiteplot

polished_en <- c(
  "P(above MNPL \n in 50 yr)",
  "P(above MNPL \n in 100 yr)",
  "Relative \n abundance \n in 10 yr",
  "Relative \n abundance \n in 20 yr",
  "Relative \n abundance \n in 50 yr"
)

polished_es <- c(
  "P(sobre MNPL en 50 años)",
  "P(sobre MNPL en 100 años)",
  "Abundancia \n en relación a su capacidad de carga K \n en 10 años",
  "Abundancia \n en relación a su capacidad de carga K \n en 20 años",
  "Abundancia \n en relación a su capacidad de carga K \n en 50 años"
)

polished_fr <- c(
  "Probabilité (au-dessus du MNPL dans 50 ans)",
  "Probabilité (au-dessus du MNPL dans 100 ans)",
  "Abondance relative à K \n après 10 ans",
  "Abondance relative à K \n après 20 ans",
  "Abondance relative à K \n après 50 ans"
)

rangenames_en <- c(
  "Lower end of bycatch range",
  "Midpoint of bycatch range",
  "Higher end of bycatch range"
)
rangenames_es <- c(
  "Más bajo fin del rango de captura incidental",
  "Punto medio del rango de captura incidental",
  "Más alto fin del rango de captura incidental"
)
rangenames_fr <- c(
  "Limite inférieure de la gamme de prises accessoires",
  "Point médian de la gamme de prises accessoires",
  "Limite supérieure de la gamme de prises accessoires"
)


# Table elements ----------------------------------------------------------

# Performance table
pmkey_en <- c(
  prebuild50 = "Probability (Above MNPL in 50 years)",
  prebuild100 = "Probability (Above MNPL in 100 years)",
  abundrel10 = "Abundance relative to K after 10 years",
  abundrel20 = "Abundance relative to K after 20 years",
  abundrel50 = "Abundance relative to K after 50 years"
)
pmkey_es <- c(
  prebuild50 = "Probabilidad (sobre MNPL en 50 años)",
  prebuild100 = "Probabilidad (sobre MNPL en 100 años)",
  abundrel10 = "Abundancia relativa a K después de 10 años",
  abundrel20 = "Abundancia relativa a K después de 20 años",
  abundrel50 = "Abundancia relativa a K después de 50 años"
)
pmkey_fr <- c(
  prebuild50 = "Probabilité (au-dessus du MNPL dans 50 ans)",
  prebuild100 = "Probabilité (au-dessus du MNPL dans 100 ans)",
  abundrel10 = "Abondance relative à K après 10 ans",
  abundrel20 = "Abondance relative à K après 20 ans",
  abundrel50 = "Abondance relative à K après 50 ans"
)

bylevels_en <- c("Value", "Low end of bycatch range", "Midpoint of bycatch range", "High end of bycatch range")
bylevels_es <- c("Valor", "Más bajo fin del rango de captura incidental", "Punto medio del rango de captura incidental", "Más alto fin del rango de captura incidental")
bylevels_fr <- c("Valeur", "Limite inférieure de la gamme de prises accessoires", "Point médian de la gamme de prises accessoires", "Limite supérieure de la gamme de prises accessoires")

# PBR definitions table
pbrdefs_en <- c(
  "Estimate of total population size (this is the population size that corresponds to the initial depletion you entered)",
  "Minimum population estimate (this will be the same as the estimate of the population size if you have not included an observation error in the Advanced Projections tab)",
  "Maximum theoretical or estimated productivity rate of the stock"
)
pbrdefs_es <- c(
  "Estimación del tamaño total de la población (este es el tamaño de la población que corresponde al agotamiento inicial que ingresó)",
  "La estimación mínima de la población (será la misma que la estimación del tamaño de la población si no ha incluido un error de observación en la pestaña de Proyecciones avanzadas)",
  "Máximo teorico o tasa de productividad estimada del stock"
)
pbrdefs_fr <- c(
  "Estimation de la taille de la population totale (c'est la taille de la population qui correspond à l'épuisement initial que vous avez indiqué)",
  "Estimation de la population minimale (elle sera identique à l'estimation de la taille de la population si vous n'avez pas inclus une erreur d'observation dans l'onglet 'Projections avancées')",
  "Taux de productivité maximum théorique ou estimé du stock"
)


# ui functions -------------------------------------------------------------
# Function for making a nice thumbnail label (adapted from shinyLP)
thumbnail_label2 <- function (image, label, content, button_label,button_text){
  div(class = "row",
      div(class = "col-sm-14 col-md-12",
          div(class = "thumbnail",
              img(src = image, alt = "...", div(class = "caption",
                                                h3(label), p(content),
                                                actionButton(button_label, button_text))))))
}

jumbotron2 <- function (header, content, button = TRUE){
  if (button) {
    div(class = "jumbotron", 
        h2(header), 
        p(content), 
        p(a(class = "btn btn-primary btn-lg button",
            id = "tabBut",
            button_label)))
  }
  else {
    div(class = "jumbotron",
        h2(header),
        p(content))
  }
}


# Documentation ----------------------------------------------
# see inst/Documentation


# Colors etc -------------------------------------------------
central <- FALSE
colorscheme <- c("#7bbcb0", "#3a7c89", "#123f5a")
i18n$set_translation_language("en")
