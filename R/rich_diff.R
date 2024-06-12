

#' Get Rich Diffs for Rmds
#'
#' Modified from
#' https://gist.github.com/benmarwick/85e853b258a653f0b44d2df293fba222. See rich
#' diffs of two commits of a single R Markdown document on a GitHub repo. Will
#' work on whatever branch is currently checked out.
#' @param gh_file_path path to the file to compare. Eg a .rmd or .qmd
#' @param wch_commits A vector of two numbers where 1 is HEAD. The first number
#'   is the more recent commit and the second is the older commit to compare it
#'   to. Use `git2r::commits()` to see all the commits available.
#'
#' @return Viewer opens with diff highlighting individual word differences.
#' @export
#'
#' @examples
rich_diff <- function(gh_file_path, wch_commits = c(1,2)){


  commit_shas <- git2r::commits(n=max(wch_commits))
  commit_1_sha <- commit_shas[[wch_commits[1]]]$sha
  commit_2_sha <- commit_shas[[wch_commits[2]]]$sha


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


