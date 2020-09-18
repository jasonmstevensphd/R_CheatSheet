

# Add Libraries -----------------------------------------------------------

library(tidyverse)



# Plots -------------------------------------------------------------------

### Favorite style for a geom_point

Plot <- ggplot(DF, aes(x = x)) +
  geom_histogram() +
  ggtitle("This is my title") +
  labs(subtitle = "This is my subtitle",
       x = "text for x-axis",
       y = "text for y-axis") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(panel.background = element_rect(fill = "white", color = "grey50")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 18,
                                  color = "Navy")) +
  theme(axis.title = element_text(face = "bold",
                                  size = 16)) +
  theme(axis.text.x = element_text(vjust = 1,
                                   hjust = 1,
                                   size = 12,
                                   angle = 60)) +
  theme(axis.text.y = element_text(size = 12))
  

### Favorite style for a histogram

Plot <- ggplot(DF, aes(x = x, y = y)) +
  geom_point()  +
  theme(plot.background = element_rect(fill = "white")) +
  theme(panel.background = element_rect(fill = "white", color = "grey50")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 18,
                                  color = "Navy")) +
  theme(axis.title = element_text(face = "bold",
                                  size = 16))

# Tidyverse ---------------------------------------------------------------

### Using column names in a fuction

My_Function <- function(Data, x, y) {
  
  Data <- Data %>%
    group_by(.data[[x]]) %>%
    summarise(Counts = n(), Average = mean(.data[[y]]))
  
}

### Remove all columns that are only NA



# Domino API --------------------------------------------------------------

library(jsonlite)
library(httr)

Retrieve_File <- function(User_ID, Project, Filename) {
  
  x <- User_ID
  y <- Project
  z <- Filename
  
  # Get Commits -------------------------------------------------------------
  
  url <- paste0("https://api.domino.com/projects/", x, "/", y, "/runs")
  
  raw.result <- GET(url = url,
                    config = add_headers(`X-Domino-Api-Key` = Sys.getenv("DOMINO_USER_API_KEY")))
  
  this.raw.content <- rawToChar(raw.result$content)
  
  substr(this.raw.content, 1, 100)
  
  this.content <- fromJSON(this.raw.content)
  
  Commits <- this.content$data %>% filter(!is.na(outputCommitId)) %>% select(outputCommitId)
  Commits <- Commits$outputCommitId[[1]]
  
  # Get Files ---------------------------------------------------------------
  
  url_1 <- paste0("https://api.domino.com/projects/", x, "/", y, "/files/", Commits, "//")
  
  raw.result_1 <- GET(url = url_1,
                      config = add_headers(`X-Domino-Api-Key` = Sys.getenv("DOMINO_USER_API_KEY")))
  
  this.raw.content_1 <- rawToChar(raw.result_1$content)
  
  substr(this.raw.content_1, 1, 100)
  
  this.content_1 <- fromJSON(this.raw.content_1)
  
  Blob <- this.content_1$data
  Blob <- Blob %>% select(path, key) %>% filter(path == z)
  blob_id <- Blob$key[1]
  
  # Get File ----------------------------------------------------------------
  
  url_2 <- paste0("https://api.domino.com/projects/", x, "/", y, "/blobs/", blob_id, "/")
  
  raw.result_2 <- GET(url = url_2,
                      config = add_headers(`X-Domino-Api-Key` = Sys.getenv("DOMINO_USER_API_KEY")))
  
  data <- content(raw.result_2, type = "text/csv")
  
  data
  
}


# PubChem API -------------------------------------------------------------

PUG_Rest_FUN <- function(object){
  
  Molecule <- object
  
  Link <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",
                 Molecule,
                 "/property/",
                 "MolecularFormula,MolecularWeight,ExactMass,InChIKey,InChI,IsomericSMILES,CanonicalSmiles,IUPACName",
                 "/JSON")
  
  Fetch <- GET(Link)
  
  Content <- content(Fetch, "text", encoding = "UTF-8") %>% fromJSON()
  
  df <- data.frame(matrix(unlist(Content), nrow = 1, byrow = T), stringsAsFactors = FALSE) %>%
    rename(cid = X1) %>%
    rename(molecular_formula = X2) %>%
    rename(molecular_weight = X3) %>%
    rename(canonical_smiles = X4) %>%
    rename(isomeric_smiles = X5) %>%
    rename(inchi = X6) %>%
    rename(inchi_key = X7) %>%
    rename(iupac_name = X8) %>%
    rename(exact_mass = X9)
  
  df$molecular_weight <- as.numeric(df$molecular_weight)
  df$cid <- as.numeric(df$cid)
  df$exact_mass <- as.numeric(df$exact_mass)
  
  df
  
}
