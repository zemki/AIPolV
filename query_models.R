# query models via the chatAI API

# load libraries
library(readr)
library(dplyr)
library(tidyllm)
source("chatai.R")

# load settings
system_prompt <- "You are a helpful AI assistant."
instructions <- "Provide a clear and concise answer (four to five sentences)."
models <- read_csv("models.csv", comment = "#")
questions <- read_csv("questions.csv") %>% 
  mutate(q = paste(q, instructions))
cases <- nrow(models) * nrow(questions) * 1 # >1 for repeated querying 

# prepare results df 
results <- data.frame(id = rep(questions$id, times = nrow(models)),
                      category = rep(questions$category, times = nrow(models)),
                      user_prompt = rep(questions$q, times = nrow(models)),
                      system_prompt = system_prompt,
                      model = rep(models$model, each = nrow(questions)),
                      type = rep(models$type, each = nrow(questions)),
                      provider = rep(models$provider, each = nrow(questions)),
                      temperature = 0, 
                      response = "")

# loop over models
for (i in 1:cases) 
{
  if (results$provider[i] == "tidyllm")
  {
    if (results$model[i] == "gpt-4o-2024-08-06")
      conversation <- llm_message(results$user_prompt[i]) |> chat(openai())
    if (results$model[i] == "gemini-1.5-flash")
      conversation <- llm_message(results$user_prompt[i]) |> chat(gemini())
    if (results$model[i] == "claude-3-5-sonnet-20241022")
      conversation <- llm_message(results$user_prompt[i]) |> chat(claude())
    results$response[i] <- conversation@message_history[[3]]$content
    #model <- conversation@message_history[[3]]$meta$model
  }
  else if (results$provider[i] == "chatai")
  {
    result <- try(chatai(results$user_prompt[i],
                         system_prompt = results$system_prompt[i],
                         model = results$model[i],
                         temperature = results$temperature[i]), silent = TRUE)
    if (length(result) > 0 & class(result) != "try-error") results$response[i] <- result
    else results$response[i] <- ""
  }
 message(glue::glue("({i}/{cases})\nModel:  {results$model[i]}\nUser: {results$user_prompt[i]}\nSystem: {results$system_prompt[i]}\nTemp.: {results$temperature[i]}\nResponse: {results$response[i]}\n\n\n"))
 #Sys.sleep(5)
}

results_wide <- results %>% 
  select(model, user_prompt, response) %>% 
  tidyr::pivot_wider(names_from = user_prompt, values_from = response) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "question")

write_csv2(results, file = paste0("outputs/responses_", Sys.Date(), ".csv"))
write_csv2(results_wide, file = paste0("outputs/responses_", Sys.Date(), "-wide.csv"))

           