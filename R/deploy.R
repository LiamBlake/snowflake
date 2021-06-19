install.packages("rsconnect")

rsconnect::setAccountInfo(name="liamblake", 
                          token = Sys.getenv("RSHINY_TOKEN"),
                          secret = Sys.getenv("RSHINY_SECRET"))

# Ensure that the correct working directory is set
setwd(getSrcDirectory()[1])

rsconnect::deployApp("app.R", appName="snowflake")