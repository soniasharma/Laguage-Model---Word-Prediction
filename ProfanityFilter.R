
### Filtering profanity words using regex


# Step 1: Create new files to store the cleaned text
files.NoProf <- character(p)
files.NoProf[i] <- paste("../en_US_NoProf/", cons[i])
#files.NoProf <- c("../en_US_NoProf/en_US.blog.txt", "../en_US_NoProf/en_US.news.txt", "../en_US_NoProf/en_US.twitter.txt")

#### Start writing the files after cleaning
for (i in 1:p)
{chunkSize <- 200000
k = 0

conWrite <- file(files.NoProf[i], open = "wt+")
while (k < file.size[i])
{chunk <- n.readLines(cons[i], n = chunkSize, skip = k, header = FALSE, comment = "")

# Replace the profanity words with " "
chunk <- gsub("\\b[Dd]{1,25}[0Oo]{1,25}[UuVv]{1,25}[Cc]{1,25}[Hh]{1,25}[Ee3]{0,25}[Bb]{0,25}[Aa@]{0,25}[Gg]{0,25}[Ss]{0,25}\\b", " ", chunk) # Douche/Douches/Douchbags
chunk <- gsub("\\b[B8b]{1,25}[Ii!1]{1,25}[Tt]{1,25}[Cc]{1,25}[Hh]{0,25}[Ii]{0,25}[Nn]{0,25}[Gg]{0,25}\\b", " ", chunk) # Bitching
chunk <- gsub("\\b[B8b]{1,25}[Ii!1]{1,25}[Tt]{1,25}[Cc]{1,25}[Hh]{0,25}[Ee3]{0,25}[SsZz]{0,25}\\b", " ", chunk) # Bitches
chunk <- gsub("\\b[Cc]{1,25}[UuVv]{1,25}[Nn]{1,25}[Tt]{1,25}[Ss]{0,25}\\b", " ", chunk) # Cunt/Cunts
chunk <- gsub("\\b[Ww]{1,25}[Hh]{1,25}[Oo0]{1,25}[Rr]{1,25}[Ee3]{0,25}[Ss]{0,25}\\b", " ", chunk) # Whores
chunk <- gsub("\\b[Ff]{1,25}[Aa@]{1,25}[Gg9]{1,25}[Gg9]{0,25}[Oo0]{0,25}[Tt]{0,25}[Ss]{0,25}\\b", " ", chunk) #Fag/Fags/Faggots
chunk <- gsub("\\b[Pp]{1,25}[UuVv]{1,25}[Ss5$]{1,25}[Yy]{1,25}[Ss]{0,25}\\b", " ", chunk) # Pussy/Pussys
chunk <- gsub("\\b[Ff]{1,25}[UVuv]{1,25}[Cc¢]{1,25}[Kk]{1,25}[Ee]{0,25}[RrDd]{0,25}[Ss]{0,25}\\b", " ", chunk) # Fuck/fucked/fucker
chunk <- gsub("\\b[Ff]{1,25}[UVuv]{1,25}[Cc¢]{1,25}[Kk]{1,25}[Ii]{0,25}[Nn]{0,25}[Gg]{0,25}\\b", " ", chunk) # Fucking
chunk <- gsub("\\b[Ss5$]{1,25}[LlI!1]{1,25}[UuVv]{1,25}[Tt]{1,25}[Ss5$]{0,25}\\b", " ", chunk) #Slut/Sluts
chunk <- gsub("\\b[S$s]{1,25}[Hh]{1,25}[I!il]{1,25}[Tt]{1,25}[Ii]{0,25}[Nn]{0,25}[Gg]{0,25}\\b", " ", chunk) # Shit/Shiting/Shitting
k = k + chunkSize
writeLines(chunk, con = conWrite)
}
close(conWrite)
}