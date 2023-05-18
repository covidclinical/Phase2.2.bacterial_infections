count_sequences_hospitalisation <- function(df, ...) {
  seq_hospitalisation_df <- data.frame(total_span = seq(min(df$days_since_admission),
                                                        max(df$days_since_admission))
  ) %>%
    left_join(df, by = c("total_span" = "days_since_admission")) %>%
    replace_na(list(in_hospital = 0))
  count_sequences <- rle(seq_hospitalisation_df$in_hospital)
  count_sequences_1 <- lapply(count_sequences, function(x) x[count_sequences$values == 1])
  n_sequences <- seq_along(count_sequences_1$lengths)
  sequences <- rep.int(n_sequences, count_sequences_1$lengths)
  sequences_len <- rep.int(count_sequences_1$lengths, count_sequences_1$lengths)
  stopifnot(length(df$days_since_admission) == length(sequences))
  data.frame(days_since_admission = df$days_since_admission,
             n_hospitalisation = sequences,
             len_hospitalisation = sequences_len)
}