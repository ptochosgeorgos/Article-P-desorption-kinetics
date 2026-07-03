res <- readRDS("data/RES.rds")
print("RES.rds structure:")
print(str(res))

if (file.exists("data/all_P.rds")) {
  all_p <- readRDS("data/all_P.rds")
  print("all_P.rds structure:")
  print(str(all_p))
}
