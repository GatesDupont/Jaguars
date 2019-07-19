library(dplyr)

#----Getting GFC tiles----
H = seq(130, 40, -10) %>%
  as.character() %>%
  paste0("W")

V = seq(40, 0, -10) %>%
  as.character() %>%
  paste0("N") %>%
  c(paste0(as.character(seq(0, 50, 10)),"S"))

for(i in 1:length(H)){
  for(j in 1:length(V)){
    print(paste("i =", i, "of", length(H)))
    print(paste("j =", j, "of", length(V)))
    download.file(paste0("https://storage.googleapis.com/earthenginepartners-hansen/",
                         "GFC-2018-v1.6/Hansen_GFC-2018-v1.6_treecover2000_", V[j], "_", H[i], ".tif"),
                  destfile = paste0("~/Documents/Jaguar/data/GIS/gfc/Hansen_GFC-2018-v1.6_treecover2000_",
                                    V[j], "_", H[i], ".tif"))
  }
}
