library(shiny)

### Required for functional shinylive
if (FALSE) {
  library("munsell")
}

library(RColorBrewer)
library(bslib)
library(ggplot2)

# TODO - Fix Use of custom font for ggplot - not currently working

data_file <- "./data/movie_reviews.csv"
#data_file <- "./data/movie_reviews.csv"

## Set up common values so they can be easily altered
small_plot_point <- 16
point_size_small <- 2
point_size_large <- point_size_small * 1.5
graph_font_size <- 14
graph_font <- "sans"

## Load Data
test_data_csv <- read.csv(data_file)


# Get current date - set to 01/01/2025 for demonstration purposes
# cur_date <- Sys.Date()
cur_date <- as.Date("2025-01-01")


## Vectors of characteristics column names for simplicity later

reviewer_characteristic_names <- c(
  "gender_identity",
  "age_group",
  "sexuality"
)


### Verify data is in expected format
### This should be extended to all required columns

verify_data <- function(df) {
  # TODO Check all used columns are in this list
  # Check that required columns are all present
  if (any(!c("score", "review_id", "movie", "genre") %in% names(df))) {
    stop("Column names are not as expected - check data file")
  }
  
  # Check review sources
  
  df[!df[, "review_type"] %in% c("Regular Viewer", 
                                 "Paid Reviewer", 
                                 "Critic",
                                 "Influencer"), ] <- "Unknown"
  
  if (any(df[, "review_type"] == "Unknown")) {
    message("Not all review sources could be processed - marked as Unknown")
  }
  
  
  df[, "completed_at"] <- as.Date(df[, "completed_at"], format = "%Y-%m-%d %H:%M:%S")
  
  
  # Replace Missing or NA character values with 'Unknown'
  
  char_subset <- df[, reviewer_characteristic_names]
  
  char_subset[is.na(char_subset) | char_subset == "" | char_subset == "Prefer not to say"] <- "Unknown"
  
  df[,  reviewer_characteristic_names] <- unname(char_subset)
  
  
  # Convert scores to numeric and flag if any scores are in unexpected format
  null_values_idx <- which(df[, "score"] == "null")
  df[null_values_idx, "score"] <- NA
  
  withCallingHandlers(
    {
      df[, "score"] <- as.numeric(df[, "score"])
    },
    warning = function(w) {
      if (grepl("NAs introduced by coercion", w$message)) {
        message("Scores that were not numeric or 'null' were unexpected and removed")
        invokeRestart("muffleWarning")
      } else {
        message(w$message)
      }
    }
  )
  
  df <- df[!is.na(df[, "score"]),]
  
  df[,"review_id"] <- as.character(df[, "review_id"])
  
  return(df)
}


## Run verified function

test_data_csv_verified <- verify_data(df = test_data_csv)
rm(test_data_csv)

test_data_csv_verified_no_paid <- test_data_csv_verified[test_data_csv_verified$review_type != "Paid Reviewer", ]

test_data_csv_verified$scored_cat_short <- paste0(substr(test_data_csv_verified$scored_cat, 
                                                         1, 15), "...(",test_data_csv_verified$scored_cat_id, ")")

score_cat_key <- unique(test_data_csv_verified[, c("scored_cat_id", 
                                                   "scored_cat_short", 
                                                   "scored_cat")])


### Average Scores for multiple Scores of the same category score in same review

av_score_cat_review <- aggregate(score ~ review_id + scored_cat_id,  
                                 test_data_csv_verified, 
                                 mean, 
                                 na.rm=TRUE)

#  And add other columns back

test_data_csv_verified <- data.frame(
  av_score_cat_review,
  test_data_csv_verified[
    match(
      paste0(av_score_cat_review$review_id, av_score_cat_review$scored_cat_id, sep="_"),
      paste0(test_data_csv_verified$review_id, test_data_csv_verified$scored_cat_id, sep="_")
    ),
    !names(test_data_csv_verified) %in% c(
      "scored_cat_id",
      "score",
      "review_id"
    )
  ]
)


all_movies <- unique(test_data_csv_verified[, c("movie", "genre"), drop=FALSE])

## Calculate the average score per review ID

average_score_per_review <- aggregate(score ~ review_id, test_data_csv_verified, mean)

### Add back information about review session

average_score_per_review <- data.frame(
  average_score_per_review,
  test_data_csv_verified[
    match(
      average_score_per_review$review_id,
      test_data_csv_verified$review_id
    ),
    !names(test_data_csv_verified) %in% c(
      "scored_cat",
      "scored_cat_id",
      "score",
      "review_id"
    )
  ]
)


## Create reviewer info dataframe

reviewer_info <- unique(average_score_per_review[, c("reviewer_id")])
reviewer_info <- data.frame(
  reviewer_info,
  average_score_per_review[
    match(
      reviewer_info,
      average_score_per_review$reviewer_id
    ),
    reviewer_characteristic_names
  ]
)

## Order based on date

average_score_per_review <- average_score_per_review[order(average_score_per_review$completed_at,
                                                           decreasing = TRUE
), ]



average_score_per_review[, "genre"] <- gsub("/.*","",average_score_per_review[, "genre"])
average_score_per_review_no_paid <- average_score_per_review[average_score_per_review$review_type != "Paid Reviewer", ]

## Get ordered list of current genres

current_genres <- sort(unique(average_score_per_review[, "genre"]))

num_genres <- length(current_genres)

# Select colours from colour-blind friendly palette
# NOTE: This might need adjusted if number of genres is over 12
# minimum number of colours is three so [1:num_genres] ensures correct number
# if less than 3

col_pal <- brewer.pal(n = num_genres, name = "Paired")[1:num_genres]


# Darken Yellow Colour so is visible
if(length(col_pal) > 10){
  col_pal[11] <- adjustcolor(col_pal[11] ,
                             alpha.f = 1,
                             red.f = 0.85,
                             green.f = 0.85,
                             blue.f = 0.85
  )
}

col_pal_dark <- adjustcolor(col_pal,
                            alpha.f = 1,
                            red.f = 0.75,
                            green.f = 0.75,
                            blue.f = 0.75
)

#### Create html code so that genres are coloured in drop down list

current_genres  <- setNames(
  current_genres ,
  paste0(
    "<span style='color:", col_pal,
    "';>",
    current_genres , "</span>"
  )
)

# TODO - This doesn't necessarily scale well as movie number increases, might require more thought or limiting
# selected number of genres!


### Function below creates dummy data for stacked dotplot
### This is necessary to allow the reactive plot

# assign score groups
create_stacked_dot_plot_data <- function(data){
  value_groups <- cut(
    data$score,
    seq(0, 10, 0.25),
    seq(0, 9.75, 0.25)
  )
  
  # extract actual value for each group (not factor)
  data[, "value_group"] <- as.numeric(as.character(value_groups))
  
  
  # blank data frame for when a group has no data
  blank_df <- data.frame(matrix(ncol = ncol(data) + 1, nrow = 0))
  colnames(blank_df) <- c(names(data), "plot_height")
  
  
  # split into groups, order and assign cumulative value
  data_ls <- lapply(split(data, value_groups), function(x) {
    x <- x[order(x[["genre"]], x[["score"]], decreasing = TRUE), ]
    if (nrow(x) > 0) {
      x["plot_height"] <- seq(1, nrow(x), 1)
      return(x)
    } else {
      return(blank_df)
    }
  })
  
  return(do.call(rbind, data_ls))
}


### Compare Movies Initial and Latest Review Values

movie_list <- split(average_score_per_review_no_paid, average_score_per_review_no_paid$movie)

first_last_list <- lapply(movie_list , function(x) {
  x <- x[order(x[, "completed_at"]), c("reviewer_id", "completed_at", "score")]
  list(x[1, ], x[nrow(x), ])
})

first_review <- do.call(rbind, lapply(first_last_list, "[[", 1))
second_review <- do.call(rbind, lapply(first_last_list, "[[", 2))

first_last_df <- data.frame(first_review, second_review[, c("completed_at", "score")])

names(first_last_df) <- c(
  "movie",
  "inital_review_date",
  "initial_review_value",
  "last_review_date",
  "last_review_value"
)


# Keep only movies with multiple dates
first_last_df <- first_last_df[first_last_df[, "inital_review_date"] != first_last_df[, "last_review_date"], ]
first_last_df[, "score_diff"] <- first_last_df$last_review_value - first_last_df$initial_review_value


### Static values for data overview

prop_reduced_review_score <- round(prop.table(table(first_last_df$score_diff < 0))[2] * 100, 1)

num_movies <- nrow(all_movies)
num_paid_reviews <- sum(average_score_per_review[, "review_type"] == "Paid Reviewer")
num_reviews <- nrow(average_score_per_review)

# Select only reviewers for which the full set of characteristics is available
characteristics_known <- rowSums(reviewer_info[, reviewer_characteristic_names] == "Unknown") == 0

reviewer_info_char_known <- reviewer_info[characteristics_known, reviewer_characteristic_names]

count_character_sets <- as.data.frame(table(reviewer_info_char_known))
count_character_sets <- count_character_sets[count_character_sets$Freq > 0, ]
count_character_sets <- count_character_sets[order(count_character_sets$Freq,
                                                   decreasing = TRUE
), ]

count_character_sets[, "idx"] <- 1:nrow(count_character_sets)

num_char_set <- nrow(count_character_sets)

num_unique_char_set <- sum(count_character_sets[, "Freq"] == 1)

# Set up initial values



# Default is that paid reviews are not shown
exclude_paid <- TRUE

sel_genre <- current_genres[[1]]

relevant_reviews <- average_score_per_review[average_score_per_review$genre == sel_genre, ]


if (exclude_paid) {
  relevant_reviews <- relevant_reviews[relevant_reviews$review_type != "Paid Reviewer", ]
} else {
}

latest_average_scores <- average_score_per_review[!duplicated(average_score_per_review$movie), ]
latest_average_scores_no_paid <- average_score_per_review_no_paid[!duplicated(average_score_per_review_no_paid$movie), ]


init_movies <- sort(unique(relevant_reviews$movie))

init_movie <- init_movies[1]

movie_reviews <- relevant_reviews[relevant_reviews$movie == init_movie, 
                                  c("completed_at","review_id", "review_type")]
movie_reviews <- movie_reviews[order(movie_reviews$completed_at, decreasing = TRUE), ]

review_types <- sort(unique(movie_reviews[, "review_type"]))

review_type_sel <- movie_reviews[1, "review_type"]

format_review_date <- function(df){
  paste0(df[, "review_id"], " (", df[, "completed_at"], ")")
}

extract_review_id <- function(entry){
  gsub("([0-9]+) .*", "\\1", entry)
}


review_dates <- format_review_date(movie_reviews[movie_reviews[,"review_type"]==review_type_sel,])

review_date_sel <- review_dates[1]

test_data_list_no_paid <- split(test_data_csv_verified_no_paid, 
                                test_data_csv_verified_no_paid$movie)

test_data_list_no_paid <- lapply(test_data_list_no_paid , function(x){
  split(x, x$review_id)
})


test_data_list <- split(test_data_csv_verified, 
                        test_data_csv_verified$movie)

test_data_list <- lapply(test_data_list, function(x){
  split(x, x$review_id)
})


source("./create_static_plots.R", local = TRUE)

# Initialize a reactive values
selected_movie <- reactiveVal(init_movie)

if (exclude_paid) {
  av_review_set <- reactiveVal(average_score_per_review_no_paid)
  full_data_set <- reactiveVal(test_data_list_no_paid)
  latest_average_scores_sel_movie <- latest_average_scores_no_paid[latest_average_scores_no_paid$genre  == sel_genre, ]
  
} else {
  av_review_set <- reactiveVal(average_score_per_review)
  full_data_set <- reactiveVal(test_data_list)
  latest_average_scores_sel_movie <- latest_average_scores[latest_average_scores$genre  == sel_genre, ]
  
}

cur_data_set <- reactiveVal(create_stacked_dot_plot_data(latest_average_scores_sel_movie ))
selected_review <- reactiveVal(review_date_sel)
selected_type <- reactiveVal(review_type_sel)
time_series_click <- reactiveVal(FALSE)
ignore_update <- reactiveVal(FALSE)
init_setup <- reactiveVal(TRUE)
# Define UI
ui <- page_sidebar(
  title = "Movie Review Tracker",
  
  
  # TODO This should be moved to a separate file - but can't get it to link properly
  # When using shiny live
  ###### Custom CSS and theme setup #####
  tags$style(HTML("
    .card-title {
      font-size: 13px;
      font-weight: bold;
      margin-bottom: -10px;
    }
    .card-content {
      font-size: 13px; 
      margin-bottom: -10px;
    }
    .card-full-page {
      width: 100%; /* Set your desired width */
      max-height: 200px; /* Set height to auto or a specific value */
      min-height: 200px;
    }
    .card {
      margin-bottom: 0px;  /* Adjust this value to control space between cards */
      margin-top: 0px;
    }
    .tab-content {
      max-height: 200px; 
      overflow-y: auto;
          }
    .card-body {
      padding: 5px;  /* Adjust this value to control inner padding of the cards */
    }
    .navbar {
      height: 40px;  /* You can adjust this value to make the navbar smaller */
      padding: 0;    /* Remove extra padding */
    }
    
    /* Vertically center the title */
    .navbar-brand {
      display: flex;
      align-items: center;
      height: 100%;
    }
    .nav-tabs > li > a {
      padding: 5px 10px; /* Adjust padding for smaller tabs */
      font-size: 12px;   /* Adjust font size for smaller text */
    }
    body, title, .content-wrapper, .main-header, .sidebar, .navbar, h1, h2, h3, h4, h5, h6, p, div,
    .nav-link.active, .table, .table th, .table td { 
      font-family: 'Trebuchet MS', sans-serif !important; 
      color: #000000 !important; 
    } 
    .table {
      min-height: 200px;
      width: 300px;  /* Set table width to 100% */
      }
      th:nth-child(1) {
        width: 100px;  
      }
      th:nth-child(2) {
        width: 100px;  
      }
      th:nth-child(3) {
        width: 100px;  
      }
  ")),
  ######
  
  theme = bs_theme(
    bslib_spacer = "10px",
    preset = "flatly",
    fg = "rgb(110, 224, 192)",
    bg = "rgb(255,255,255)",
    primary = "rgb(121, 245, 210)",
    success = "rgb(137, 145, 151)"
  ),
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Work+Sans:wght@400&display=swap"
  ),
  sidebar = sidebar(
    checkboxInput(
      inputId = "exclude_paid",
      label = "Exclude Paid Reviews",
      value = TRUE
    ),
    
    # Set up Genre Selection
    selectizeInput(
      inputId = "genre_sel",
      label = "Genre:",
      choices = current_genres,
      selected = sel_genre,
      multiple = TRUE,
      options = list(render = I("
  {
    item: function(item, escape) { return '<div>' + item.label + '</div>'; },
    option: function(item, escape) { return '<div>' + item.label + '</div>'; }
  }"))
    ),
    # Set up ID Selection
    selectizeInput(
      inputId = "movies_sel",
      label = "Movie (Type to search):",
      choices = init_movies,
      selected = init_movie,
      multiple = FALSE
    ),
    # Set up Review Type Selection
    selectInput(
      inputId = "review_type_sel",
      label = "Review Type:",
      choices = review_types,
      selected = review_type_sel,
      multiple = FALSE
    ),
    # Set up Review Date Selection
    selectInput(
      inputId = "review_date_sel",
      label = "Review:",
      choices = review_dates,
      selected = review_date_sel,
      multiple = FALSE
    ),
    # Radio buttons for page selection
    radioButtons("page_selection", "Select Page:",
                 choices = c(
                   "Interactive Dashboard" = "int_dash",
                   "Data Overview" = "data_overview"
                 ),
                 selected = "int_dash"
    )
  ),
  
  # Placeholder for dynamic content
  uiOutput("dynamic_content")
)

# Define server logic
server <- function(input, output, session) {
  # Render the dynamic content based on the selected page
  output$dynamic_content <- renderUI({
    if (input$page_selection == "int_dash") {
      tagList(
        tabsetPanel(
          id = "tabs", # Optional: give an ID to the tabset
          tabPanel(
            "Most Recent Scores",
            # Content for Tab 1
            card(
              class = "card-full-page",
              plotOutput(outputId = "stack_genre", click = "dot_plot_click")
            )
          ),
          tabPanel(
            "Movies Without Reviews",
            # Content for Tab 2
            card(
              class = "table",
              tableOutput("missing_movie_table")
            )
          )
        ),
        layout_columns(
          col_widths = c(5, 7),
          layout_columns(
            col_widths = c(12, 12), # Full width for the layout
            row_heights = c(3, 1), # Height proportions
            card(
              style = "max-height: 300px; height: auto",
              plotOutput(outputId = "behave_bar", hover = "behave_bar_hover")
            ),
            card(
              style = "max-height: 100px; height: auto",
              tags$h3("Category:", class = "card-title"),
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              tags$div(
                class = "card-content",
                textOutput("hover_info")
              )
            )
          ),
          layout_columns(
            col_widths = c(7, 5, 12),
            card(
              style = "max-height: 200px; height: auto",
              plotOutput(outputId = "overall_graphic")
            ),
            card(
              style = "max-height: 200px; height: auto",
              width = 12,
              status = "primary",
              tags$h3("Movie:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_id")),
              tags$h3("Genre:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_genre")),
              tags$h3("Review ID:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_review")),
              tags$h3("Reviewer:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_reviewer")),
              tags$h3("Days Since Review:", class = "card-title"),
              tags$div(class = "card-content", textOutput("info_box_days"))
            ),
            card(
              style = "max-height: 200px; height: auto",
              plotOutput(outputId = "time_series", click = "time_series_click")
            )
          ),
        )
      )
    } else {
      tagList(
        layout_columns(
          col_widths = c(6, 6, 12, 6, 6, 12, 8, 4),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "movies_per_genre")
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "reviews_per_genre")
          ),
          card(
            class = "card-content",
            (paste(
              "There are", num_movies, "movies across", num_genres, "genres. A total of",
              num_reviews, "reviews have been completed,", num_paid_reviews, "of these are paid reviews."
            ))
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "review_relationship_dist")
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "review_diff")
          ),
          card(
            class = "card-content",
            (paste(
              "The average score value for 'Paid Reviewer' reviews is higher than any other review type.", "
                              Reviews by critics are lowest.  For movies which have more than one review, ignoring paid reviews, many do not show a change in average score over time, ",
              paste0(prop_reduced_review_score, "%"), "show a decrease in score between intial and last review."
            ))
          ),
          card(
            style = "max-height: 200px; height: auto",
            plotOutput(outputId = "character_set_dist")
          ),
          card(
            class = "card-content",
            (paste(
              "For reviewers with a complete set of known characteristics there are", num_char_set, "different characteristic sets with",
              num_unique_char_set, "reviewers having a combination of characters unique to them."
            ))
          )
        )
      )
    }
  })
  
  ### reactive data
  
  ### Genrelevel Reactive Data
  stack_plot_data <- reactive({
    list(
      new_point = cur_data_set()[cur_data_set()$movie == selected_movie(), ],
      cur_data = cur_data_set()
    )
  })
  
  ind_plot_data <- reactive({
    sel_id <- selected_movie()
    sel_type <- selected_type()
    sel_review <- selected_review()
    current_full_data <- full_data_set()
    current_selected_movie <- selected_movie()
    av_review_set <- av_review_set()
    
    review_type_sel <- {
      if (sel_type == "All") {
        c("Critic", "Influencer", "Paid Reviewer", "Regular Viewer")
      } else {
        sel_type
      }
    }
    
    
    
    # Subset dataset based on currently selected values
    data_subset_all_reviews <- current_full_data[[current_selected_movie]]
    data_subset <- data_subset_all_reviews[[sel_review]]
    
    ind_data_subset <- av_review_set[av_review_set$movie == current_selected_movie, ]
    
    genre_col <- col_pal[match(data_subset$genre[1], current_genres)]
    
    # Darker color is used for paid reviews
    
    dark_genre_col <- col_pal_dark[match(data_subset$genre[1], current_genres)]
    
    list(
      ind_data_subset = ind_data_subset,
      data_subset = data_subset,
      data_subset_all_reviews = data_subset_all_reviews,
      genre_col = c(light = genre_col, dark = dark_genre_col)
    )
  })
  
  
  
  observeEvent(input$exclude_paid, {
    if (input$exclude_paid) {
      full_data_set(test_data_list_no_paid )
      av_review_set(average_score_per_review_no_paid)
    } else {
      full_data_set(test_data_list)
      av_review_set(average_score_per_review)
    }
  })
  
  observeEvent(c(input$genre_sel, input$exclude_paid), {
    if (input$exclude_paid) {
      data_subset <- latest_average_scores_no_paid
    } else {
      data_subset <- latest_average_scores
    }
    
    data_subset <- data_subset[data_subset$genre %in% input$genre_sel, ]
    movies_sub <- sort(unique(data_subset$movie))
    
    
    # convert list back into dataframe
    cur_data_set(create_stacked_dot_plot_data(data_subset))
    
    updateSelectizeInput(session, "movies_sel",
                         choices = movies_sub,
                         selected = movies_sub[1], server = TRUE
    ) # Reset selection
  })
  
  
  # When new ID is selected update review_type_sel & review_date_sel to latest
  observeEvent(input$movies_sel, {
    if (!is.null(input$movies_sel) && input$movies_sel != "") {
      data_subset_all_reviews <- av_review_set()
      
      review_date_sub <- data_subset_all_reviews[data_subset_all_reviews$movie == input$movies_sel,  
                                                 c("review_type", "completed_at", "review_id")]
      
      
      updateSelectInput(session, "review_type_sel",
                        choices = c(review_date_sub$review_type, "All"),
                        selected = review_date_sub$review_type[1]
      )
      
      review_dates <- review_date_sub[review_date_sub[,"review_type"]==review_date_sub$review_type[1], 
                                      c("completed_at", "review_id")]
      cur_reviews <- format_review_date(review_dates)
      
      updateSelectInput(session, "review_date_sel",
                        choices =  cur_reviews,
                        selected =  cur_reviews[1]
      ) # Reset selection
      if(!init_setup()){
        time_series_click(TRUE)
      }
      selected_type(review_date_sub$review_type[1])
      selected_review(review_dates[1, "review_id"])
      selected_movie(input$movies_sel)
    }
  })
  
  # When new review type is selected update review_date_sel to latest
  observeEvent(input$review_type_sel, {
    # TODO this is bit of a hack to stop updating if you click on the time series plot
    # Need to improve reactivity so that this sort of thing doesn't happen
    if(!time_series_click() | init_setup() ){
      if (input$review_type_sel == "All" | input$review_type_sel == "") {
        review_type_sel <- c("Critic", "Influencer", "Paid Reviewer", "Regular Viewer")
      } else {
        review_type_sel <- input$review_type_sel
      }
      data_subset_all_reviews <- isolate(av_review_set())
      review_date_sub <- unique(data_subset_all_reviews[data_subset_all_reviews$movie == input$movies_sel &
                                                          data_subset_all_reviews$review_type  %in% review_type_sel,  
                                                        c("review_type", "completed_at", "review_id")])
      
      cur_review_list <- format_review_date(review_date_sub)
      updateSelectInput(session, "review_date_sel",
                        choices = cur_review_list,
                        selected = cur_review_list[1]
      ) 
      
      # Reset selection
      selected_review(review_date_sub[1, "review_id"])
      selected_type(input$review_type_sel)
    }
  })
  
  
  # When new review date is selected updated selected_date() value
  observeEvent(input$review_date_sel, {
    if(time_series_click()){
      time_series_click(FALSE)
      
    } else {
      selected_review(extract_review_id(input$review_date_sel))
    }
  })
  
  # When when point is clicked on dot plot change to the relevant ID
  # Also save point value so it can be enlarged
  observeEvent(input$dot_plot_click, {
    new_point <- nearPoints(cur_data_set(),
                            input$dot_plot_click,
                            threshold = 5,
                            maxpoints = 1
    )
    
    if (nrow(new_point) > 0 && "movie" %in% colnames(new_point)) {
      # Update the SelectizeInput only if the point is valid
      updateSelectizeInput(session, "movies_sel",
                           choices = cur_data_set()$movie,
                           selected = new_point$movie, server = TRUE
      )
    }
  })
  
  # When when point is clicked on time-series change to correct review date & type
  # Also save point value so it can be enlarged
  observeEvent(input$time_series_click, {
    ind_data_subset <- ind_plot_data()[["ind_data_subset"]]
    new_point <- nearPoints(ind_data_subset,
                            input$time_series_click,
                            threshold = 5,
                            maxpoints = 1
    )
    
    if (nrow(new_point) > 0 && "completed_at" %in% colnames(new_point)) {
      updateSelectInput(session, "review_type_sel",
                        selected = new_point$review_type[1]
      )
      
      
      data_subset_all_reviews <- isolate(av_review_set())
      
      review_date_sub <- unique(data_subset_all_reviews[data_subset_all_reviews$review_id == new_point$review_id,  
                                                        c("review_type", "completed_at", "review_id")])
      cur_reviews <-  format_review_date(review_date_sub)
      
      sel_review <- new_point[,c("completed_at", "review_id")]
      
      updateSelectInput(session, "review_date_sel",
                        choices =  cur_reviews,
                        selected = cur_reviews[1]
      ) # Reset selection
      
      selected_type(new_point$review_type[1])
      selected_review(sel_review[, "review_id"])
      time_series_click(TRUE)
      init_setup(FALSE)
    }
  })
  
  
  
  ### Create Basic ID Summary Graphic
  output$overall_graphic <- renderPlot({
    data_subset <- ind_plot_data()[["data_subset"]]
    genre_col <- ind_plot_data()[["genre_col"]][["light"]]
    dark_genre_col <-  ind_plot_data()[["genre_col"]][["dark"]]
    
    average_value <- round(mean(data_subset$score, na.rm = TRUE), 2)
    
    # Create dummy data
    data <- data.frame(
      category = c("A", "B"),
      count = c(10 - average_value, average_value)
    )
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n = -1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, "\n score: ", data$count)
    
    point_col <- if (data_subset$review_type[1] == "Paid Reviewer") dark_genre_col else genre_col
    
    ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      geom_rect() +
      geom_label(
        size = 5, family = graph_font,
        x = 2, y = 0.25, label = paste0("Overall:\n ", average_value),
        label.padding = unit(0.1, "lines"),
        lineheight = 0.75
      ) +
      scale_fill_manual(values = c(
        "B" = point_col,
        "A" = alpha(point_col, 0.5)
      )) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(
        text = element_text(family = graph_font),
        legend.position = "none"
      )
  })
  
  ### Get Text Summaries
  output$info_box_id <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    return(as.character(data_subset[1, c("movie")]))
  })
  
  output$info_box_genre <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    return(as.character(data_subset[1, c("genre")]))
  })
  
  output$info_box_review <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    return(as.numeric(data_subset[1, c("review_id")]))
  })
  
  output$info_box_reviewer <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    return(as.numeric(data_subset[1, c("reviewer_id")]))
  })
  
  output$info_box_days <- renderText({
    data_subset <- isolate(ind_plot_data()[["data_subset"]])
    sel_review <- selected_review()
    review_date <- data_subset[data_subset$review_id == sel_review, "completed_at"]
    return(as.numeric(cur_date - as.Date(review_date[1])))
  })
  
  
  ### Create Time Series Plot
  output$time_series <- renderPlot({
    cur_data <- stack_plot_data()[["cur_data"]]
    ind_data_subset <- ind_plot_data()[["ind_data_subset"]]
    genre_col <- ind_plot_data()[["genre_col"]][["light"]]
    genre_col_dark <- ind_plot_data()[["genre_col"]][["dark"]]
    data_subset <- ind_plot_data()[["data_subset"]]
    
    
    
    # Subset the data
    ind_data_subset_self <- ind_data_subset[ind_data_subset$review_type == "Paid Reviewer", ]
    ind_data_subset_other <- ind_data_subset[ind_data_subset$review_type != "Paid Reviewer", ]
    
    # Create a sequence of x values for predictions
    
    date_range <- seq(min(ind_data_subset$completed_at),
                      max(ind_data_subset$completed_at),
                      length.out = 100
    )
    
    
    #  Function to get best attempt at a spline depending on the number of points
    create_spline_data <- function(df, date_range) {
      num_dates <- length(unique(df$completed_at))
      if (num_dates > 3) {
        spline_predict <- smooth.spline(df$completed_at,
                                        df$score,
                                        all.knots = TRUE,
                                        spar = 0.5
        )
        return(data.frame(
          predict(spline_predict, newdata = data.frame(completed_at = date_range))
        ))
      } else if (num_dates > 1) {
        spline_predict <- lm(score ~ completed_at, df[, c("completed_at", "score")])
        return(data.frame(
          x = date_range,
          y = predict(spline_predict,
                      newdata = data.frame(completed_at = date_range, score = NA)
          )
        ))
      } else if (num_dates == 1) {
        return(data.frame(x = date_range, y = df$score))
      } else {
        blank_df <- data.frame(matrix(ncol = 2, nrow = 0))
        names(blank_df) <- c("x", "y")
        return(blank_df)
      }
    }
    
    # We want to plot the self review and other reviews separately
    
    spline_int_self <- create_spline_data(ind_data_subset_self, date_range)
    spline_int_other <- create_spline_data(ind_data_subset_other, date_range)
    
    review_displayed <- data_subset[1, c("review_id")]
    
    new_point <- ind_data_subset[ind_data_subset$review_id == review_displayed, ]
    
    high_point_col <- if (new_point$review_type == "Paid Reviewer") genre_col_dark else genre_col
    
    
    ggplot() +
      geom_point(data = ind_data_subset_self, aes(x = completed_at, y = score), color = genre_col_dark, size = point_size_small, shape = small_plot_point) +
      geom_point(data = ind_data_subset_other, aes(x = completed_at, y = score), color = genre_col, size = point_size_small, shape = small_plot_point) +
      geom_point(data = new_point, aes(x = completed_at, y = score), color = high_point_col, size = point_size_large, shape = small_plot_point) +
      geom_line(data = spline_int_other, aes(x = as.Date(x), y = y), color = genre_col) +
      geom_line(data = spline_int_self, aes(x = as.Date(x), y = y), color = genre_col_dark) +
      labs(x = "Review Date", y = "Score") +
      xlim(min(date_range) - 30, max(date_range) + 30) +
      ylim(0, 10) +
      theme_minimal() +
      theme(text = element_text(family = graph_font, size = graph_font_size))
  })
  
  ### Create The Catergory Score Bar plot
  
  
  output$behave_bar <- renderPlot({
    data_subset <- ind_plot_data()[["data_subset"]]
    genre_col <- ind_plot_data()[["genre_col"]][["light"]]
    dark_genre_col <- ind_plot_data()[["genre_col"]][["dark"]]
    #av_score_per_behave <- ind_plot_data()[["av_score_per_behave"]]
    point_col <- if (data_subset$review_type[1] == "Paid Reviewer") dark_genre_col else genre_col
    ggplot(data = data_subset , aes(
      x = factor(scored_cat_id,
                 levels = scored_cat_id
      ),
      y = score
    ), ) +
      geom_bar(stat = "identity", fill = point_col) +
      coord_flip() +
      ylim(c(0, 10)) +
      theme_minimal() +
      labs(y = "Score", x = "Category") +
      theme(
        text = element_text(family = graph_font, size = graph_font_size),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  ### Set up display of category on hover
  output$hover_info <- renderText({
    data_subset <- ind_plot_data()[["data_subset"]]
    genre_col <- ind_plot_data()[["genre_col"]]
    #av_score_per_behave <- ind_plot_data()[["av_score_per_behave"]]
    
    hover <- input$behave_bar_hover
    if (is.null(hover)) {
      return("Hover over a bar to see the scored category.")
    }
    # Calculate which bar is being hovered over
    bar_index <- round(hover$y)
    if (bar_index >= 1 && bar_index <= nrow(data_subset)) {
      value <- data_subset$score[bar_index]
      category <- data_subset$scored_cat[bar_index]
      return(category)
    } else {
      return("Hover over a bar to see the scored category.")
    }
  })
  
  
  ### Create The Stacked Dot Plot
  output$stack_genre <- renderPlot({
    new_point <- stack_plot_data()[["new_point"]]
    isolate({
      cur_data <- stack_plot_data()[["cur_data"]]
    })
    sel_col <- col_pal[match(new_point$genre[1], current_genres)]
    
    
    ggplot(cur_data, aes(x = value_group, y = plot_height, color = factor(genre, levels = input$genre_sel))) +
      geom_point(size = point_size_small, shape = small_plot_point) +
      geom_point(data = new_point, aes(x = value_group, y = plot_height), colour = sel_col, size = point_size_large, shape = small_plot_point) +
      labs(x = "Scores", y = "") +
      xlim(0, 10) +
      ylim(0, 30) +
      scale_color_manual(values = col_pal[match(input$genre_sel, current_genres)]) +
      theme_minimal() +
      theme(
        text = element_text(family = graph_font, size = graph_font_size),
        legend.position = "none",
        axis.text.y = element_blank(), # Remove y-axis text
        axis.ticks.y = element_blank(), # Remove y-axis ticks
        axis.title.y = element_blank(), # Remove y-axis title
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        plot.title = element_text(size = 14)
      )
  })
  
  ## Create Movies Per Genre Barchart
  output$movies_per_genre <- renderPlot({
    movies_per_genre_barchart
  })
  
  ## Create Reviews Per Genre Barchart
  output$reviews_per_genre <- renderPlot({
    review_per_genre_barchart
  })
  
  ## Create Review Diff Histogram
  output$review_diff <- renderPlot({
    review_diff_hist
  })
  
  ## Create Review Type Histogram
  output$review_relationship_dist <- renderPlot({
    review_type_hist
  })
  
  
  ## Create Character Set Barchart
  output$character_set_dist <- renderPlot({
    char_set_barplot
  })
  
  # Find any movies that are in the selected genres that don't have any review data
  output$missing_movie_table <- renderTable({
    cur_data <- stack_plot_data()[["cur_data"]]
    
    selected_genres <- unique(cur_data[, "genre"])
    
    all_movies_sel_genres <- all_movies[all_movies[, "genre"] %in% selected_genres, ]
    all_movies_sel_genres[, "score"] <- NA
    missing_table <- all_movies_sel_genres[!all_movies_sel_genres[, "movie"] %in% cur_data[, "movie"], , drop = FALSE]
    names(missing_table) <- c("Movie", "Genre", "Score")
    
    return(missing_table)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
