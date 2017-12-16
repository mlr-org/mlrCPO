## ---- results = "asis", echo = FALSE-------------------------------------

cat("hi", file = "/tmp/wri", append = TRUE)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

path = names(knitr::opts_knit$get("encoding"))[1]

rpath = gsub("\\.[^.]*$", ".R", path)

knitr::knit_hooks$set(document = function(x) {
  lines = readLines(rpath)
  lines = gsub(" *(\n|$)", "\\1", lines)
  cat(lines, file = rpath, sep = "\n", append = FALSE)
  x
})



base = dirname(path)
file = basename(path)

allfiles = list.files(path = base, pattern = ".*\\.Rmd$")

stopifnot(file %in% allfiles)

fileinfolist = list()

for (cf in allfiles) {
  ismain = TRUE
  if (grepl("^z_", cf)) {
    infoslot = gsub("^z_", "", cf)
    infoslot = gsub("_terse\\.Rmd$", "", infoslot)
    subslot = "compact"
  } else {
    infoslot = gsub("^a_", "", cf)
    infoslot = gsub("\\.Rmd$", "", infoslot)
    subslot = "main"
  }

  content = scan(paste(base, cf, sep = "/"), what = "character", quiet = TRUE)
  pos = min(c(which(content == "title:"), Inf))
  if (is.infinite(pos)) {
    stop(sprintf("parsing error: %s", cf))
  }
  infolist = list(title = content[pos + 1], url = cf, iscurrent = cf == file)

  applist = list(infolist)
  names(applist) = subslot
  fileinfolist[[infoslot]] = c(fileinfolist[[infoslot]], applist)
}

linkify = function(info, title) {
  if (info$iscurrent) {
    title
  } else {
    sprintf("[%s](%s)", title, gsub("\\.Rmd$", ".html", info$url))
  }
}

for (idx in seq_along(fileinfolist)) {

  content = fileinfolist[[sort(names(fileinfolist))[idx]]]
  if (!is.null(content$compact)) {
    if (paste(content$main$title, "(No Output)") != content$compact$title) {
      stop(sprintf("File %s and its compact version %s have incompatible titles\nThe compact version must be paste(main_title, \"(No Output)\")",
        content$main$url, content$compact$url))
    }
    line = sprintf("%s (%s)", linkify(content$main, content$main$title), linkify(content$compact, "compact version"))
  } else {
    line = linkify(content$main, content$main$title)
  }
  cat(sprintf("%s. %s\n", idx, line))
}




## ---- echo = FALSE-------------------------------------------------------
library("mlrCPO")

## ------------------------------------------------------------------------
names(formals(makeCPO))  # see help(makeCPO) for explanation of arguments

## ------------------------------------------------------------------------
constFeatRem = makeCPO("constFeatRem",  # nolint
  dataformat = "df.features",
  cpo.train = function(data, target) {
    names(Filter(function(x) {  # names of columns to keep
        length(unique(x)) > 1
      }, data))
    }, cpo.retrafo = function(data, control) {
    data[control]
  })
head(iris) %>>% constFeatRem()
print(constFeatRem, verbose = TRUE)

## ------------------------------------------------------------------------
constFeatRem = makeCPO("constFeatRem",  # nolint
  dataformat = "df.features",
  cpo.train = function(data, target) {
    cols.keep = names(Filter(function(x) {
    length(unique(x)) > 1
      }, data))
    # the following function will do both the trafo and retrafo
    result = function(data) {
      data[cols.keep]
    }
    result
  }, cpo.retrafo = NULL)
head(iris) %>>% constFeatRem()
print(constFeatRem, verbose = TRUE)

