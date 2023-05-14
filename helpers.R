get_journal_meta <- function(journal_name) {
  journal_info <- cr_journals(query = journal_name, works = FALSE) %>% pluck("data")
  if (nrow(journal_info) > 0) {
    if (nrow(journal_info) > 1) {
      journal_info_sel <- journal_info %>% filter(title == journal_name)
      if (nrow(journal_info_sel) > 0) journal_info <- journal_info_sel
    }
    if (!length(unique(journal_info$issn))==1) {
      browser()
      warning("Multiple ISSN found for ", journal_name, ": ",
                                                       glue::glue_collapse(na.omit(journal_info$issn), ", ", last = " & "),
                                                       ". Returning the first one, but check!")
    }
    journal_info %>% select(publisher, issn) %>% .[1, ]
  } else {
    return(tibble(publisher = NA, issn = NA))
  }
}

p_from_d <- function(d, n1, n2) {
  t <- .5 * d * sqrt(n1 + n2)
  2 * pt(abs(t), n1 + n2 - 2, lower.tail = FALSE)
}


p_from_r <- function(r, n) {
   t <- r * sqrt(n - 2) / sqrt(1 - r^2)
   p_value <- 2 * pt(abs(t), df = n - 2, lower.tail = FALSE)
   p_value
   }
