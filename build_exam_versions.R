library(readxl)
library(dplyr)
library(rmarkdown)
library(tidyr)

set.seed(123)

setwd("C:/Users/Kyla Dahlin/Dropbox/MSU_GEO201/2026/exam_2/")
exam <- read_excel("quizzes_combined.xlsx", sheet = "ExamBuild")

shuffle_answers <- function(row) {
  
  answers <- c(row$A, row$B, row$C, row$D, row$E)
  letters <- c("A", "B", "C", "D", "E")
  
  # Drop NA choices (if some questions have no E)
  keep <- !is.na(answers)
  answers <- answers[keep]
  letters <- letters[keep]
  
  correct_text <- answers[letters == row$Correct]
  
  perm <- sample(seq_along(answers))
  answers_shuffled <- answers[perm]
  
  new_correct <- letters[which(answers_shuffled == correct_text)]
  
  tibble(
    A = answers_shuffled[1],
    B = answers_shuffled[2],
    C = answers_shuffled[3],
    D = answers_shuffled[4],
    E = if (length(answers_shuffled) == 5) answers_shuffled[5] else NA,
    Correct = new_correct
  )
}

make_version <- function(version_letter) {
  
  exam_tf <- exam %>%
    filter(Type == "TF") %>%
    sample_frac(1)
  
  exam_mc <- exam %>%
    filter(Type == "MC") %>%
    sample_frac(1)
  
  exam_v <- bind_rows(exam_tf, exam_mc) %>%
    mutate(ExamQNum = row_number()) %>%
    rowwise() %>%
    mutate(
      shuffled = if (Type == "MC") {
        list(shuffle_answers(cur_data()))
      } else {
        list(tibble(
          A = A,
          B = B,
          C = NA,
          D = NA,
          E = NA,
          Correct = Correct
        ))
      }
    ) %>%
    ungroup() %>%
    select(-A, -B, -C, -D, -E, -Correct) %>%
    unnest(shuffled)
  
  # Save answer key (Scantron-ready)
  writeLines(
    paste(exam_v$ExamQNum, exam_v$Correct, sep = ": "),
    paste0("Key_Version_", version_letter, ".txt")
  )
  
  # Render PDF
  render(
    input = "exam_template.Rmd",
    output_file = paste0("Exam_Version_", version_letter, ".pdf"),
    params = list(
      version = version_letter,
      exam_data = exam_v
    ),
    quiet = TRUE
  )
}

versions <- c("A", "B")
lapply(versions, make_version)

