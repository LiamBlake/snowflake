install.packages("rsconnect")

rsconnect::setAccountInfo(name="liamblake", 
                          token = Sys.getenv("RSHINY_TOKEN"),
                          secret = Sys.getenv("RSHINY_SECRET"))

rsconnect::deployApp("R/app.R", appName="snowflake")