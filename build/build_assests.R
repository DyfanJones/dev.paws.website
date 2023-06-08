# basic logger
log_info <- function(msg) {
  on.exit(flush.console())
  date_time <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  log_msg <- sprintf("INFO %s: %s", date_time, msg)
  writeLines(log_msg)
}

log_info("Build site assests")

fs::dir_create("build/mkdocs/docs")

# copy assests from vendor
fs::file_copy("vendor/paws/README.md", "build/mkdocs/docs", overwrite = TRUE)
fs::file_copy("vendor/paws/docs/logo.png", "build/mkdocs/docs", overwrite = TRUE)

# create site directory structure
dirs <- fs::path("build/mkdocs/docs", c("examples", "articles", "img", "docs"))
if (all(file.exists(dirs))) fs::dir_delete(dirs)
fs::dir_create(c("build/mkdocs/docs/articles/img", dirs), recurse = TRUE)

# copy assests examples from vendor
fs::dir_copy("vendor/paws/examples", "build/mkdocs/docs/examples", overwrite = TRUE)

# copy articles from vendor
for (f in c("credentials.md", "cheat_sheet.pdf")) {
  fs::file_copy(
    file.path("vendor/paws/docs", f),
    file.path("build/mkdocs/docs/articles", f),
    overwrite = T
  )
}

# copy read assests
for (f in c("code_completion.gif")) {
  fs::file_copy(
    file.path("vendor/paws/docs", f),
    file.path("build/mkdocs/docs/img", f),
    overwrite = TRUE
  )
}

edit_readme <- function(file = "build/mkdocs/docs/README.md") {
  readme <- readLines(file)
  # fix logo image
  idx <- grep('<img src="docs/logo.png" align="right" height="150" />', readme)
  readme[idx] <- gsub(
    '<img src="docs/logo.png" align="right" height="150" />',
    '<img src= "logo.png" style="float:right;height:150px;width:auto" />',
    readme[idx]
  )

  # fix docs links
  idx <- grepl(r"{\[.*\]\(docs/.*\)|\[.*\]\(articles/docs/.*\)}", readme)
  readme[idx] <- gsub(r"{\[Logo\]\(docs/logo.png\)}", "Logo", readme[idx])
  readme[idx] <- gsub(
    r"{!\[\]\(docs/code_completion.gif\)}", r"{!\[\]\(img/code_completion\.gif\)}",
    readme[idx]
  )
  readme[idx] <- gsub("docs/articles|docs", "articles", readme[idx])

  # fix examples links
  idx <- grepl(r"{\[.*\]\(examples/.*\)}", readme)
  readme[idx] <- gsub(r"{\.R\)}", r"{\.md\)}", readme[idx])

  writeLines(readme, file)
}

edit_r_examples <- function(dir = "build/mkdocs/docs/examples") {
  files <- fs::dir_ls(dir)
  r_files <- files[grepl("\\.R$", files)]

  for (r_file in r_files) {
    r_file_edit <- readLines(r_file)
    r_file_edit <- c("```r", r_file_edit, "```")
    writeLines(r_file_edit, gsub("R$", "md", r_file))
  }
  fs::file_delete(r_files)
}

get_paws_pkgs <- function(dir = "vendor/paws/cran") {
  paws_desc <- fs::path(dir, "paws/DESCRIPTION")
  lines <- readLines(paws_desc)
  pkgs <- lines[grepl("paws\\.[a-z\\.]", lines, perl = T)]
  trimws(gsub("\\([^)]*\\).*", "", pkgs))
}
get_pkg_desc <- function(dir = "vendor/paws/cran") {
  vapply(get_paws_pkgs(dir), \(pkg) {
    lines <- readLines(file.path(dir, pkg, "DESCRIPTION"))
    start <- grep("Description:", lines)
    end <- grep("License:", lines) - 1
    lines[start] <- gsub("Description:", "", lines[start])
    lines <- trimws(paste(lines[start:end], collapse = "\n"))
    gsub("[[:space:]]+<.*>", "", lines)
  }, FUN.VALUE = "", USE.NAMES = F)
}

get_articles <- function(dir = "build/mkdocs/docs/articles") {
  articles <- sort(basename(fs::dir_ls(dir, type = "file")), decreasing = T)
  articles <- sprintf("%s: articles/%s", gsub("\\..*$", "", articles), articles)
  return(as.list(articles))
}

get_examples <- function(dir = "build/mkdocs/docs/examples") {
  example <- basename(fs::dir_ls(dir, type = "file"))
  example <- sprintf("%s: examples/%s", gsub("\\..*$", "", example), example)
  return(as.list(example))
}

get_version <- function(dir = "vendor/paws/cran/paws/DESCRIPTION") {
  desc <- readLines(dir)
  version <- desc[grepl("Version:*.[0-9]+\\.[0-9]+\\.[0-9]+", desc)]
  pattern <- "[0-9]+\\.[0-9]+\\.[0-9]+"
  m <- regexpr(pattern, version)
  regmatches(version, m)
}

get_reference_md <- function(dir = "build/mkdocs/docs/docs") {
  df <- data.frame(
    Package = get_paws_pkgs(),
    Description = get_pkg_desc()
  )
  writeLines(c("# Reference", knitr::kable(df, "html")), file.path(dir, "reference.md"))
}

make_hierarchy <- function(dir = "vendor/paws/cran") {
  paws_desc <- fs::path(dir, "paws/DESCRIPTION")
  lines <- readLines(paws_desc)
  pkgs <- lines[grepl("paws\\.[a-z\\.]", lines, perl = T)]
  paws_pkg <- trimws(gsub("\\([^)]*\\).*", "", pkgs))
  
  hierarchy <- unlist(lapply(paws_pkg, \(pkg) {
    sprintf(
      "https://dyfanjones.github.io/dev.%s/%s/",
      pkg,
      gsub("\\.Rd$", "", basename(fs::dir_ls(file.path(dir, pkg, "man"))))
    )
  }))
  
  
  lvl <- gsub("_.*", "", basename(hierarchy))
  ref <- sub("[a-zA-Z0-9]+_", "", basename(hierarchy), perl = T)
  ref[lvl == ref] <- "Client"
  
  hierarchy <- sprintf("%s: %s", ref, hierarchy)
  hierarchy <- split(hierarchy, lvl)
  
  # order hierarchy
  for (j in seq_along(hierarchy)) {
    idx <- grep("Client", hierarchy[[j]])
    hierarchy[[j]] <- c(hierarchy[[j]][idx], sort(hierarchy[[j]][-idx]))
  }
  return(hierarchy)
}




paws_make_hierarchy <- function(paws_dir = "vendor/paws/cran") {
  paws_desc <- fs::path(paws_dir, "/paws/DESCRIPTION")
  lines <- readLines(paws_desc)
  pkgs <- lines[grepl("paws\\.[a-z\\.]", lines, perl = T)]
  paws_pkg <- trimws(gsub("\\([^)]*\\).*", "", pkgs))
  
  hierarchy <- sapply(paws_pkg, \(x) gsub("\\.Rd$", "\\.md", basename(fs::dir_ls(file.path(paws_dir, x, "man")))), simplify = F)
  
  for (i in seq_along(hierarchy)) {
    lvl <- gsub("_.*|\\.md$", "", hierarchy[[i]])
    ref <- sub("[a-zA-Z0-9]+_", "", hierarchy[[i]], perl = T)
    ref <- gsub("\\.md$", "", ref)
    
    ref[lvl == ref] <- "Client"
    hierarchy[[i]] <- sprintf("%s: docs/%s", ref, hierarchy[[i]])
    hierarchy[[i]] <- split(hierarchy[[i]], lvl)
    
    # order hierarchy
    for (j in seq_along(hierarchy[[i]])) {
      idx <- grep("Client", hierarchy[[i]][[j]])
      hierarchy[[i]][[j]] <- c(hierarchy[[i]][[j]][idx], sort(hierarchy[[i]][[j]][-idx]))
    }
  }
  return(hierarchy)
}

build_site_yaml <- function() {
  site_yaml <- org_yaml <- yaml::yaml.load_file(
    "build/mkdocs.orig.yml"
  )

  for (i in c("extra_css", "plugins")) {
    if (!is.null(org_yaml[[i]]) && !is.list(length(org_yaml[[i]]))) {
      site_yaml[[i]] <- as.list(site_yaml[[i]])
    }
  }

  site_yaml$site_name <- sprintf("paws: %s", get_version())
  
  # add references
  ref_idx <- which(vapply(site_yaml$nav, \(x) names(x) == "Reference", FUN.VALUE = logical(1)))
  site_yaml$nav[[ref_idx]]$Reference <- make_hierarchy() # paws_make_hierarchy()

  # add articles
  ref_idx <- which(vapply(site_yaml$nav, \(x) names(x) == "Articles", FUN.VALUE = logical(1)))
  site_yaml$nav[[ref_idx]]$Articles <- get_articles()

  # add examples
  ref_idx <- which(vapply(site_yaml$nav, \(x) names(x) == "Examples", FUN.VALUE = logical(1)))
  site_yaml$nav[[ref_idx]]$Examples <- get_examples()

  site_yaml <- yaml::as.yaml(site_yaml, indent.mapping.sequence = T)
  site_yaml <- gsub("- '", "- ", site_yaml)

  # tidy up file paths
  for (ext in c("md", "pdf", "/")) {
    site_yaml <- gsub(
      sprintf("\\.%s'", ext),
      sprintf("\\.%s", ext),
      site_yaml
    )
  }
  writeLines(site_yaml, "build/mkdocs/mkdocs.yml", "")
}

get_reference_md()
edit_readme()
edit_r_examples()
build_site_yaml()
