install.packages(c("shiny", "rsconnect"))

rsconnect::setAccountInfo(name="liamblake", 
                          token = Sys.getenv("RSHINY_TOKEN"),
                          secret = Sys.getenv("RSHINY_SECRET"))

rsconnect::deployApp("R", appName="snowflake")