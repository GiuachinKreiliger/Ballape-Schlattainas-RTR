#chargiar e preparar las datas
rankings_umens <- read.csv("C:/Users/KREILGIU/Downloads/rankings_umens.csv", header=FALSE, sep=";")
rankings_dunnas <- read.csv("C:/Users/KREILGIU/Downloads/rankings_dunnas.csv", header=FALSE, sep=";")
colnames(rankings_umens) <- c("ranking", "pajais", "puncts")
colnames(rankings_dunnas) <- c("ranking", "pajais", "puncts")

#far ina nova rangaziun per ils umens, damai che bu tut ils pajais han teams da dunnas
rankings_umens <- rankings_umens[,rankings_umens$pajais %in% rankings_dunnas$pajais]
rankings_umens$ranking <- 1:188

rankings <- merge(rankings_dunnas, rankings_umens, by = "pajais")

#calcular la differenza tranter umens e dunnas
rankings$differenza <- rankings$ranking_dunnas - rankings$ranking_umens

#arranschar las datas suenter la rangaziun da las dunnas
rankings <- rankings[order(rankings$ranking_dunnas),]

# calcular la summa da differenzas (absolutas) -> l'index da colliaziun
# Sche las equipas dal medem pajais èn mintgamai plazzadas medem, lure è l'index exact 0

sum(abs(rankings$differenza))

#----------------------- Test statistic ----------------------------#

#simulaziun da 10'000 rangaziuns da dunnas ed umens casualas, e calcular l'index da colliaziun
absdiffsum <- c()
for (u in 1 in 100000) {
  tmp <- data.frame(rk1 = sample(1:188, replace = FALSE), rk2 = sample(1:188, replace = FALSE)); 
  absdiffsum <- c(absdiffsum, sum(abs(tmp$rk1 - tmp$rk2)))
}

#Per cumparegliar: è l'index da colliaziun da las datas pli grond u pli pitschen che
#quel da las datas ch'èn generadas uschia ch'i n'ha nagina colliaziun?

any(absdiffsum < sum(abs(rankings$differenza)))
#sche na, lura è il resultad dal test statistic, lura èsi fitg pauc probabel che las datas en 
#la rangaziun da la FIFA actuala èn casualas, ed i na dat betg vairamain ina colliaziun tranter la fermezza da las equipas da dunas e d'umens


#Per cumparegliar: Quant fitg è la fermezza da las squadras colliada, 
#pon ins s'orientar vi da la scala: 0, nagina colliaziun, e la media da l'index da colliaziun 
#per rangaziuns generadas per computer:
mean(absdiffsum)