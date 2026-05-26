# install.packages("usethis") # DO this once
library(usethis)

# create token once: create_github_token()
# link github account to token: gitcreds::gitcreds_set()

# 1) Initialise git
usethis::use_git()

# 2) Create Repo - DO this once each time you run
# usethis::create_project("~/Desktop/my_git_project", open = TRUE)


