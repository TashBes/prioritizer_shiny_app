

##explore neiss data

library(shiny)

## -----------------------------------------------------------------------------------------------------------------------
pacman::p_load(tidyverse, sf, rnaturalearth, patchwork, prioritizr, viridis)
# IMCC7 Workshop:
# Conservation Planning for the marine environment: An introduction to tools and techniques
# Helper functions adapted from spatialplanr R package (mathmarecol.github.io/spatialplanr/)
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

splnr_plot_Solution <- function(soln, colorVals = c("#c6dbef", "#3182bd"),
                                showLegend = TRUE, legendLabels = c("Not selected", "Selected"),
                                plotTitle = "Solution", legendTitle = "Planning Units",
                                zones = FALSE) {
  if (zones == FALSE) {
    soln <- soln %>%
      dplyr::select("solution_1") %>%
      dplyr::mutate(solution = as.factor(.data$solution_1))
    nrows <- 2
  } else if (zones == TRUE) {
    oldName <- soln %>%
      dplyr::select(tidyselect::starts_with(c("solution"))) %>%
      sf::st_drop_geometry() %>%
      tibble::as_tibble() %>%
      names()
    
    newName <- gsub("1_zone", "", oldName) # to make data a bit nicer to work with
    nrows <- (length(newName) + 1)
    
    solnNewNames <- soln %>%
      dplyr::rename_at(dplyr::vars(tidyselect::all_of(oldName)), ~newName) %>%
      dplyr::select(tidyselect::starts_with(c("solution")))
    
    for (i in 2:(length(newName))) {
      solnNewNames <- solnNewNames %>%
        dplyr::mutate(
          !!rlang::sym(newName[i]) := dplyr::case_when(
            !!rlang::sym(newName[i]) == 1 ~ i,
            !!rlang::sym(newName[i]) == 0 ~ 0
          )
        )
    }
    
    soln <- solnNewNames %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        solution = sum(dplyr::c_across(cols = tidyselect::starts_with("solution_"))),
        solution = factor(.data$solution, levels = 0:(length(newName)))
      )
  }
  
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution), colour = NA, size = 0.1, show.legend = showLegend) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::scale_fill_manual(
      name = legendTitle,
      values = colorVals,
      labels = legendLabels,
      aesthetics = c("colour", "fill"),
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = nrows,
        order = 1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    ggplot2::labs(subtitle = plotTitle)
}

splnr_get_featureRep <- function(soln, pDat, targetsDF = NA,
                                 solnCol = "solution_1",
                                 maxUtility = FALSE) {
  s_cols <- pDat$data$features[[1]]
  
  # Get data for features not chosen
  not_selected <- soln %>%
    dplyr::select(
      -tidyselect::starts_with(c("Cost", "solution_")),
      -tidyselect::any_of(c("metric", "cellID")),
      -tidyselect::any_of(s_cols)
    ) %>%
    sf::st_drop_geometry()
  
  ns_cols <- not_selected %>%
    colnames()
  
  if (length(ns_cols) > 0) {
    ns1 <- not_selected %>%
      dplyr::select(c(tidyselect::all_of(ns_cols))) %>%
      dplyr::mutate(solution = dplyr::pull(soln, !!rlang::sym(solnCol)))
    
    area_feature <- ns1 %>%
      dplyr::select(-c("solution")) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "total_amount") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(total_amount = sum(.data$total_amount))
    
    selected_feature <- ns1 %>%
      dplyr::filter(.data$solution == 1) %>%
      dplyr::select(-c("solution")) %>%
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "absolute_held") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(absolute_held = sum(.data$absolute_held))
    
    ns1 <- dplyr::left_join(area_feature, selected_feature, by = "feature") %>%
      dplyr::mutate(
        relative_held = (.data$absolute_held / .data$total_amount),
        incidental = TRUE
      )
  } else {
    ns1 <- tibble::tibble(
      feature = "DummyVar",
      total_amount = 0,
      absolute_held = 0,
      relative_held = 0,
      incidental = TRUE
    )
  }
  
  ## Now do the selected features
  
  s1 <- soln %>%
    dplyr::rename(solution = !!rlang::sym(solnCol)) %>%
    tibble::as_tibble()
  
  if (maxUtility) {
    s1 <- prioritizr::eval_feature_representation_summary(pDat, s1[, "solution"]) %>%
      dplyr::select(-"summary") %>%
      # dplyr::left_join(pDat$targets$data[["targets"]], by = "feature") %>% # Add targets to df
      #  dplyr::select(-"type") %>%
      dplyr::mutate(
        relative_held = .data$relative_held,
        incidental = FALSE
      ) %>%
      stats::na.omit()
    
  } else {
    s1 <- prioritizr::eval_feature_representation_summary(pDat, s1[, "solution"]) %>%
      dplyr::select(-"summary") %>%
      dplyr::left_join(pDat$targets$data[["targets"]], by = "feature") %>% # Add targets to df
      dplyr::select(-"type") %>%
      dplyr::mutate(
        relative_held = .data$relative_held,
        incidental = FALSE
      ) %>%
      stats::na.omit()
  }
  
  
  # Now join the selected and non-selected values
  if ((length(ns_cols) > 0)) { # Only if there are values in ns1
    df <- dplyr::bind_rows(s1, ns1)
  } else {
    df <- s1
  }
  
  return(df)
}

splnr_plot_featureRep <- function(df, category = NA,
                                  categoryFeatureCol = NA,
                                  renameFeatures = FALSE,
                                  namesToReplace = NA,
                                  nr = 1, showTarget = TRUE,
                                  plotTitle = "",
                                  maxUtility = FALSE) {
  
  
  if(inherits(category, c("df", "tbl_df")) & !("feature" %in% colnames(category))) {
    if (!(inherits(categoryFeatureCol, "character"))) {
      cat("There is no column called 'feature' in your category data frame. Please provide a column name that should be renamed to 'feature'.");
    } else {
      category <- category %>%
        dplyr::rename(feature = categoryFeatureCol)
    }}
  
  if (renameFeatures == TRUE) {
    
    assertthat::assert_that(is.data.frame(namesToReplace)) #sanity check
    
    rpl <- namesToReplace %>%
      dplyr::filter(.data$nameVariable %in% df$feature) %>%
      dplyr::select("nameVariable", "nameCommon") %>%
      tibble::deframe()
    
    df <- df %>%
      dplyr::mutate(feature = stringr::str_replace_all(.data$feature, rpl))
    
    category <- category %>%
      dplyr::mutate(feature = stringr::str_replace_all(.data$feature, rpl))
    
  }
  
  
  if (inherits(category, c("df", "tbl_df")) & ("feature" %in% colnames(category))) {
    df <- df %>%
      dplyr::left_join(category, by = "feature") %>%
      dplyr::arrange(.data$category, .data$feature) %>%
      dplyr::mutate(feature = factor(.data$feature, levels = .data$feature))
  }
  
  if (maxUtility) {
    if (max(df$relative_held < 1)) {
      df <- df %>%
        dplyr::mutate(
          relative_held = .data$relative_held * 100
        )
    }
    
  } else {
    if (max(df$relative_held < 1)) {
      df <- df %>%
        dplyr::mutate(
          relative_held = .data$relative_held * 100,
          target = .data$target * 100
        )
    }
    
  }
  
  uniqueCat <- unique(df$category[!is.na(df$category)])
  
  colr <- tibble::tibble(
    Category = uniqueCat,
    Colour = viridis::viridis(length(uniqueCat))
  ) %>%
    tibble::deframe()
  
  gg_target <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = df, stat = "identity",
                      ggplot2::aes(x = .data$feature, y = .data$relative_held, fill = .data$category), na.rm = TRUE) +
    ggplot2::geom_bar(data = df %>% dplyr::filter(.data$incidental == TRUE),
                      stat = "identity", ggplot2::aes(x = .data$feature, y = .data$relative_held),
                      na.rm = TRUE, fill = "NA", colour = "black") +
    ggplot2::labs(title = plotTitle, x = "Feature", y = "Representation of features \nin total selected area (%)") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = c(0, ymax <- max(df$relative_held, na.rm = TRUE) + 10),
                                expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      values = colr
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "NA")
    )
  
  if (showTarget) {
    gg_target <- gg_target +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data$feature, y = .data$target), shape = 3, size = 10, na.rm = TRUE)
  }
  
  return(gg_target)
}




## ----filepath1----------------------------------------------------------------------------------------------------------
inputDat <- file.path("Input") # Define file paths


## -----------------------------------------------------------------------------------------------------------------------
cCRS <- 4326 


## -----------------------------------------------------------------------------------------------------------------------
PUs <- st_read(file.path(inputDat, "PUs", "Galapagos_Planning_Units.shp")) %>%
  st_transform(cCRS) %>%
  select(-"cost") %>%
  rename(cellID = puid)


## -----------------------------------------------------------------------------------------------------------------------
features <- readRDS(file.path(inputDat, "Features", "Features_Combined.rds"))

## -----------------------------------------------------------------------------------------------------------------------
cost <- st_read(file.path(inputDat, "Cost", "cost_surface.shp")) %>%
  st_transform(cCRS) %>%
  rename(cellID = puid)



## -----------------------------------------------------------------------------------------------------------------------
out_sf <- features %>%
  left_join(cost %>% sf::st_drop_geometry(), by = "cellID")


## -----------------------------------------------------------------------------------------------------------------------
landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)


## -----------------------------------------------------------------------------------------------------------------------
# Extract feature names
col_name <- features %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()


## -----------------------------------------------------------------------------------------------------------------------
# Same target for all
set_target <- c(0.1, 0.2, 0.3, 0.4, 0.5, 
                0.6, 0.7, 0.8, 0.9)

#---------------------------------------------------------------------
# if needed, install HiGHS solver
#install.packages("highs", repos = "https://cran.rstudio.com/")



ui <- fluidPage(

#---------------------------------------------------------
####inputs
###row 1
fluidRow(

##1
column(6, 
selectInput(
  "feature", "What feature would you like to include?", col_name,
  multiple = TRUE, selected = "fans")),
  
##2
column(4,
numericInput(inputId = "n",
     "Protection target", value = 0.3),

numericInput(inputId = "penalty",
         "Boarder penalty", value = 0),
actionButton("simulate", "Solve!"))


# checkboxGroupInput(inputId = "feature",
#                    label = "What feature would you like to include?",
#                    choices = col_name, 
#                    selected = "fans"),
),
#----------------------------------------------------------
#outputs

##1
  plotOutput(outputId = "map"),

##2 
  tableOutput(outputId = "SummaryTable"),

##3
# textOutput(outputId = "test")

)



server <- function(input, output, session) {
  

  out_sf_selected <- eventReactive(input$simulate,{
    
    out_sf %>% 
      dplyr::select(input$feature, cost)
    
  })
  
  col_name <- reactive({ 
    
    out_sf_selected() %>%
      select(-c(cost)) %>%
    st_drop_geometry() %>%
    colnames()
  })
  
  problem <- reactive({
    prioritizr::problem(out_sf_selected(),
                       features = col_name(),
                       cost_column = "cost") %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_relative_targets(input$n) %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_default_solver(verbose = FALSE) %>% 
    prioritizr::add_boundary_penalties(penalty = input$penalty, edge_factor = 0.5)
  })
  
  soln <- reactive({
    problem() %>%
    solve.ConservationProblem()
  })
  
  
  
  output$map <- renderPlot({

  
     (splnr_plot_Solution(soln()))
    
  
  }, res = 96)
  
  
  output$SummaryTable <- renderTable({
    
  eval_target_coverage_summary(
    problem(), 
    dplyr::select(soln(), solution_1))
    

  })


}




shinyApp(ui, server)
















