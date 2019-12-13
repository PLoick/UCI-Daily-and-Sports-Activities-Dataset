#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

# Test the arguments were passed correctly
if (length(args) != 2) {
  stop("Two arguments for the tag_version and tag_name must be specified.", call. = FALSE)
}

# Set the tag elements from the command line args
tag_version <- args[1] # "Tagname"
tag_name <- args[2] # "Tag message"

# Get the last commit
library(rprojroot)
root <- is_rstudio_project$make_fix_file()
data_dir <- function(...) root("data", ...)
repo <- git2r::repository(root("."))
last_commit <- git2r::commits(repo, n = 1)[[1]]

# Set the archive name using the date, message and sha
short_sha <- substring(last_commit@sha, 0, 8)
message <- substring(gsub("[ \\.]", "_", tolower(last_commit@summary)), 0, 12)
commit_time <- lubridate::with_tz(as(last_commit@committer@when, "POSIXct"), "GMT")
dir_time <- format(commit_time, "%y%m%d_%H%M%S")

# Create the cache directories
cache_dir <- root(".drake")
output_dir <- root("caches", paste(dir_time, message, short_sha, sep = "_"))
outall_dir <- paste(output_dir, "allresults", sep = "_")

if(!dir.exists(cache_dir)) stop("Cache does not exist")
# system(sprintf("rm -rf %s", shQuote(output_dir)))
dir.create(output_dir)
dir.create(outall_dir)

# Copy the cache to the current folder and a new archive
system(sprintf("cp -r %s %s", shQuote(cache_dir), shQuote(output_dir)))
system(sprintf("cp -r %s %s", shQuote(cache_dir), shQuote(outall_dir)))

# Rename the .drake folder to make the cache visible
system(sprintf("mv %s %s", shQuote(file.path(output_dir, ".drake")), shQuote(file.path(output_dir, "drake"))))
system(sprintf("mv %s %s", shQuote(file.path(outall_dir, ".drake")), shQuote(file.path(outall_dir, "drake"))))

# Remove .gitignore
# file.remove(file.path(output_dir, ".drake/.gitignore"))

# Clean all non-immediate predecessors to report.Rmd
#TBA clean(..., output_dir)

# Upload cache to box.com
system(sprintf("cd %s && tar -cjf %s.tar.bz2 %s", dirname(output_dir), basename(output_dir), basename(output_dir)))
library(boxr)
box_auth(client_id = "dv0o16na0mdp3nwzehtjlehirquidwwm", client_secret = "ybD8JkRpk2tu7O3cFZYZ4xGOfzdb5Nfm", interactive = FALSE, cache = ".boxr-oauth")
# Set the remote box_dir to caches
box_setwd("48514169932")
box_ul(file = paste0(output_dir, ".tar.bz2"))

# Update project version
desc <- readLines(root("DESCRIPTION"))
writeLines(sub("^Version.*", paste0("Version: ", tag_version), desc), root("DESCRIPTION"))

# Commit output_dir and DESCRIPTION
git2r::add(repo, root("DESCRIPTION"))
# git2r::add(repo, paste0(output_dir, "/*"))
git2r::commit(repo, "Publish results")

# Tag this commit
git2r::tag(repo, tag_version, tag_name, session = TRUE)
system("git push")
system("git push origin master --tags")
                              