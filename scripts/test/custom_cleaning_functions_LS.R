# Custom Text Cleaning Functions
# Written by Lily Smith

#===============================================================================
# Function for preserving abbreviations
preserve_abbreviations <- function(text) {
  matches <- gregexpr("\\b([a-z](?:\\.[a-z])+)\\.?", text, ignore.case = TRUE)
  regmatches(text, matches) <- lapply(regmatches(text, matches), function (abbrev) {
    gsub("\\.", "DOTDOTDOT", abbrev)
  })
  return(text)
}

#===============================================================================
# Function to convert possessive forms
convert_possessives <- function(text) {
  text <- gsub("\\b(\\w+)'s\\b", "\\1", text, ignore.case = TRUE)
  text <- gsub("\\b(\\w+)'\\b", "\\1", text, ignore.case = TRUE)
  
  return(text)
}
