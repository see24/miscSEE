

#' Get Rich Diffs for Rmds
#'
#' Modified from
#' https://gist.github.com/benmarwick/85e853b258a653f0b44d2df293fba222. See rich
#' diffs of two commits of a single R Markdown document on a GitHub repo. Will
#' work on whatever branch is currently checked out.
#' @param gh_file_path path to the file to compare. Eg a .rmd or .qmd
#'
#' @return Viewer opens with diff highlighting individual word differences.
#' @export
#'
#' @examples
rich_diff <- function(gh_file_path){


  commit_shas <- git2r::commits(n=2)
  commit_1_sha <- commit_shas[[1]]$sha
  commit_2_sha <- commit_shas[[2]]$sha


  # consrtruct the terminal commands using the SHA of the commits we want to
  # compare, and the file path
  git_show_str_1 <- paste0('git show ', commit_1_sha, ":", gh_file_path)
  git_show_str_2 <- paste0('git show ', commit_2_sha, ":", gh_file_path)

  # get the contents of the file at each commit and
  # store in a vector
  earlier_commit <- system(git_show_str_2, intern = TRUE)
  later_commit <- system(git_show_str_1, intern = TRUE)

  # compute differences and visualise
  diffobj::diffChr(earlier_commit,
                   later_commit,
                   mode = "sidebyside")

}


