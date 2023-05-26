t.test2 <- function(m1, m2, s1, s2, n1, n2) {
  # t.test computed from sample statistics, ie using mean, standard deviation and count value of the samples
  # useful in case we can only retrieve aggregated stats from the sites
  # ref: https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
  # and https://en.wikipedia.org/wiki/Student%27s_t-test?oldformat=true#Equal_or_unequal_sample_sizes,_unequal_variances_(sX1_%3E_2sX2_or_sX2_%3E_2sX1)
  se <- sqrt( (s1^2/n1) + (s2^2/n2) )
  # welch-satterthwaite df
  df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  t <- (m1-m2)/se
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat)
}