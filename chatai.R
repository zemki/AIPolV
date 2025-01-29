library(httr)
library(jsonlite)

chatai <- function(x = "", system_prompt = "You are a helpful assistant", model = "meta-llama-3.1-8b-instruct", temperature = 0, key = Sys.getenv("chatai_key")) {
  url <- "https://chat-ai.academiccloud.de/v1/chat/completions"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", key),
    "Content-Type" = "application/json"
  )
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = x)
    ),
    temperature = temperature
  )
  response <- POST(url, add_headers(.headers = headers), body = toJSON(body, auto_unbox = TRUE))
  response_parsed <- try(fromJSON(content(response, as = "text", encoding = "UTF-8")), outFile = "chatai_error.log")
  if (class(response_parsed) == "try-error") return("API error :-(")
  else return(response_parsed$choices$message$content)
}

chatai_models <- function(key = Sys.getenv("chatai_key")) {
  url <- "https://chat-ai.academiccloud.de/v1/models"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", key),
    "Content-Type" = "application/json"
  )
  response <- POST(url, add_headers(.headers = headers))
  response_parsed <- try(fromJSON(content(response, as = "text", encoding = "UTF-8")), outFile = "chatai_error.log")
  if (class(response_parsed) == "try-error") return("API error :-(")
  else return(response_parsed$data)
}
