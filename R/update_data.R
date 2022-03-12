temp <- tempfile()
download.file("https://datasets.imdbws.com/name.basics.tsv.gz",temp)
data <- read_tsv(gzfile(temp))

update_all <- function(exclude_table = "none") {
  n <- c("name.basics",
         "title.akas",
         "title.basics",
         "title.crew",
         "title.episode",
         "title.principals",
         "title.ratings")
  
  n_only <- n[!stringr::str_detect(n, exclude_table)]
  
  links <- glue::glue("https://datasets.imdbws.com/{n}.tsv.gz")

  purr::walk2(links, n_only, function(x, y) {
    temp <- tempfile()
    download.file(x, temp)
    data <- read_tsv(gzfile(temp))
    saveRDS(data, glue("data/{y}.rda"))
  })
}

update_all()
