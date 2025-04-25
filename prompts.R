#' Construct LLM prompt
#'
#' Construct a LLM prompt based on user input
#'
#' @param blog_link URL of source material
#' @param platforms Social media platform to create prompts for
#' @param n Number of prompts to create for each platform
#' @param emojis Use emojis in post?
#' @param tone Desired tone of the post
#' @param hashtags Hashtags to include in the post
#' @importFrom ellmer interpolate_file
get_prompt <- function(blog_link, platforms, n, emojis, tone, hashtags) {
  # retrieve post contents from GitHub
  post_contents <- fetch_github_markdown(blog_link)
 
  platform_specific_advice <- get_platform_specific_advice(platforms)

  # combine components
  ellmer::interpolate_file(
    file.path("prompts", "prompt-main.md"),
    platforms = paste(platforms, collapse = ", "),
    n = n,
    tone = tone,
    hashtags = hashtags,
    emojis = emojis,
    post_contents = post_contents,
    platform_specific_advice = platform_specific_advice
  )
}

#' Retrieve post-writing advice unique to specific platforms
#'
#' @param platforms Which platforms to get advice for
get_platform_specific_advice <- function(platforms){
  prompt_files <- paste0("prompt-", tolower(platforms), ".md")
  file_paths <- file.path("prompts", prompt_files)

  contents <- lapply(file_paths, readLines)
  paste(unlist(contents), collapse = "\n")
}

#' Fetch markdown file from GitHub
#'
#' Fetches a markdown file from GitHub, converting it to the raw content URL if it's not already one.
#'
#' @param url URL of file
#' @importFrom httr GET content status_code
fetch_github_markdown <- function(url) {
  # Convert to raw content URL if it's a GitHub repository URL
  if (grepl("github.com", url) && !grepl("raw.githubusercontent.com", url)) {
    url <- sub("github.com", "raw.githubusercontent.com", url)
    url <- sub("/blob/", "/", url)
  }

  # Fetch content
  response <- GET(url)

  # Check for successful retrieval
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    return(content)
  } else {
    stop("Failed to retrieve the file. Check the URL and try again.")
  }
}

#' Call the LLM API with the prompt
#'
#' @param prompt Prompt to use as input
#' @importFrom ellmer chat_gemini
call_llm_api <- function(prompt) {
  chat <- chat_gemini(echo = "none")
  out <- chat$chat(prompt)
}
