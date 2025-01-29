# query models via the chatAI API

# load libraries
library(readr)
library(dplyr)
source("chatai.R")

# load settings
models_list <- read_lines("models.txt", skip_empty_rows = T)
models <- models_list[str_detect(models_list, pattern = "#", negate = T)] %>% trimws()
system_prompt <- "You are a helpful AI assistant."
instructions <- "Provide a clear and concise answer (four to five sentences)."
questions <- read_csv("questions.csv") %>% 
  mutate(q = paste(q, instructions))
cases <- length(models) * nrow(questions) * 1 # >1 for repeated querying 

# prepare results df 
results <- data.frame(id = rep(questions$id, times = length(models)),
                      category = rep(questions$category, times = length(models)),
                      user_prompt = rep(questions$q, times = length(models)),
                      system_prompt = system_prompt,
                      model = rep(models, each = nrow(questions)),
                      temperature = 0, 
                      response = "")

# loop over models
for (i in 1:cases) 
{
 result <- try(chatai(results$user_prompt[i],
                      #system_prompt = results$system_prompt[i],
                      model = results$model[i],
                      temperature = results$temperature[i]), silent = TRUE)
 if (length(result) > 0 & class(result) != "try-error") results$response[i] <- result
 else results$response[i] <- ""
 message(glue::glue("({i}/{cases})\nModel:  {results$model[i]}\nUser: {results$user_prompt[i]}\nSystem: {results$system_prompt[i]}\nTemp.: {results$temperature[i]}\nResponse: {results$response[i]}\n\n\n"))
 #Sys.sleep(5)
}
