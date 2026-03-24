# reminder- to export quizzes in D2L go to Quizzes > Click on the down arrow
# next to the quiz > Select "Statistics" > Select the "Question Details" tab >
# select "Export to CSV"

library(tidyr)

setwd("C:/Users/Kyla Dahlin/Dropbox/MSU_GEO201/2026/exam_2/")

files <- list.files("./quizzes")

in.1 <- read.csv(file.path("quizzes", files[1]))
in.1 <- in.1[c(2,3,5,8,9)]
names(in.1)[1] <- "Q.num"

wide.1 <- in.1 %>%
  group_by(Q.num) %>%
  mutate(
    answer_col = LETTERS[row_number()],
    Correct = answer_col[Answer.Match == "Checked"]) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(Q.num, Q.Type, Q.Text, Correct),
    names_from = answer_col,
    values_from = Answer
  )
wide.1$Quiz <- paste0("Quiz_", 1)

out.all <- wide.1

for (i in 2:length(files)) {
  in.x <- read.csv(file.path("quizzes", files[i]))
  in.x <- in.x[c(2,3,5,8,9)]
  names(in.x)[1] <- "Q.num"
  
  wide.x <- in.x %>%
    group_by(Q.num) %>%
    mutate(
      answer_col = LETTERS[row_number()],
      Correct = answer_col[Answer.Match == "Checked"]) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c(Q.num, Q.Type, Q.Text, Correct),
      names_from = answer_col,
      values_from = Answer
    )
  wide.x$Quiz <- paste0("Quiz_", i)
  out.all <- rbind(out.all, wide.x)
}

write.csv("./quizzes/quizzes_combined.csv")

# NOW CLEAN UP BY HAND IN EXCEL TO CHECK STUFF

