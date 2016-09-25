library(stringdist)

substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}
updateChange <- function(firmName, n) {
  m1 <- which(!is.na(match(vbDirectors$Director.Of,
                           similarityList[[n]]$name)))
  for (i in 1:length(m1)) { vbDirectors$Director.Of[m1[i]] <- firmName }
  m2 <- which(!is.na(match(affiliations,
                           similarityList[[n]]$name)))
  for (i in 1:length(m2)) { similarityList[[m2[i]]] <- NA }
}

vbDirectors <- read.csv(file = "~/Desktop/VBDirectors/data/VB Directors.csv",
                        header = TRUE,
                        sep = "\t")
vbDirectors <- vbDirectors[1:(ncol(vbDirectors) - 3)]
save(vbDirectors,
     file  = "~/Desktop/VBDirectors/data/vbDirectors.rda")
years <- unique(vbDirectors$Year)
for (i in 1:length(years)) {
  dir.create(path = paste0("~/Desktop/VBDirectors/data/", years[i]),
             showWarnings = FALSE)
  decade <- substrRight(x = years[i],
                        n = 2)
  network <- subset(vbDirectors,
                    vbDirectors$Year == years[i])
  assign(paste0("vbDirectors", decade), network)
  save(network,
       file = paste0("~/Desktop/VBDirectors/data/", years[i], "/vbDirectors", years[i], ".rda"))
}
affiliations <- unique(vbDirectors$Director.Of)
similarityList <- lapply(1:length(affiliations),
       function(x) {
         sd <- data.frame(name = affiliations,
                          dist = 1 - stringdist(a = affiliations[x],
                                                b = affiliations,
                                                method = "jw",
                                                maxDist = Inf),
                          stringsAsFactors = FALSE)
         closeTo <- subset(sd,
                           sd$dist > 0.85)
         closeTo$name <- as.character(closeTo$name)
         c(closeTo)
})
save(similarityList,
     file = "~/Desktop/VBDirectors/data/similarityList.rda")

# ... for (i in 1:)


sinkerino <- file("~/Desktop/VBDirectors/data/similarityList.txt",
                  open = "wt")
sink(file = sinkerino)
similarityList[[1:9999]]$name[1]
sink()

sinkerino <- file("~/Desktop/VBDirectors/data/firstNames.txt",
                  open = "wt")
sink(file = sinkerino)
for (i in 1:9999) { print(similarityList[[i]]$name[1]) }
sink()