# TILM3511 -harjoitustyö, R-koodi
# Lasse Rintakumpu, 63555
# 11.3.2013

# Asetetaan työhakemisto

wd <- "C:/Users/Lasse Rintakumpu/Documents/GitHub/tilm3511"
wd <- "D:/Dropbox/Edu/Statistics/Peruskurssi C/Harjoitustyö/"
setwd(wd)

# Funktio kirjastojen asentamiselle / lataamiselle

lataa_kirjasto <- function(kirjasto) {
if(kirjasto %in% rownames(installed.packages()) == FALSE)
  {install.packages(kirjasto)} 
library(kirjasto, character.only = TRUE)
}

# Ladataan/asennetaan käytetyt kirjastot

#lapply(c("moments","car", "alr3"), lataa_kirjasto)
lapply(c("lawstat", "car"), lataa_kirjasto)

# Ladataan havaintoaineisto

talous <- read.csv("https://raw.github.com/rintakumpu/tilm3511/master/talous.csv", sep=";", dec=",");

########################################
# 1. Koulutustason vaikutus säästöihin #
########################################

# Poistetaan havaintoaineistosta kansakoulun / peruskoulun suorittaneet

talous_koulutustaso <- subset(talous, koulutus != 1)

# Kuvaillaan dataa sironta- ja laatikko-janakuviolla

plot(talous_koulutustaso$koulutus, talous_koulutustaso$saasto94)
plot(talous_koulutustaso$saasto94~as.factor(talous_koulutustaso$koulutus))

# Oletetaan havainnot toisistaan riippumattomiksi. Testataan varianssien homogeenisuutta.
# Jakaumat eivät näytä normaaleilta. Käytetään testaukseen Brown-Forsythea.
# Tehdään testi tasolla 0.05.
# H0: Luokkien varianssi sama.
# Hv: Luokkien varianssi eri.

levene.test(talous_koulutustaso$saasto94, as.factor(talous_koulutustaso$koulutus))

# Test Statistic = 1.1104, p-value = 0.3464 
# => Hylätään Hv tasolla 0.05. Varianssit ovat samat.

# Jakaumat eivät näytä normaaleilta, tarkastellaan normaalisuutta.
qqnorm(talous_koulutustaso$saasto94)
shapiro.test(talous_koulutustaso$saasto94)

# W = 0.8943, p-value = 7.199e-10
# => Normaalisuusoletus ei päde. Haetaan dataan optimaalinen potenssimuunnos.

pt_saasto94 <- powerTransform(talous_koulutustaso$saasto94)
# -0.001575101
shapiro.test(talous_koulutustaso$saasto94^-0.001575101)
# W = 0.995, p-value = 0.8181
# => Näyttää huomattavasti paremmalta. Jatketaan muunnetuilla arvolla.

saasto94_muunnettu <- (talous_koulutustaso$saasto94^-0.001575101)

# Jatketaan säästöerojen tarkastelua. Testataan erojen tilastollista merkitsevyyttä tasolla 0.05.

# H0: Säästöissä ei eroa.
# Hv: Säästöissä on eroa.
# Tehdään ainoistolle yksisuuntainen varianssianalyysi.

reg <- aov(saasto94_muunnettu~as.factor(talous_koulutustaso$koulutus))
summary(reg)

#                                          Df    Sum Sq   Mean Sq F value Pr(>F)
# as.factor(talous_koulutustaso$koulutus)   3 3.680e-06 1.227e-06   2.011  0.114
# Residuals                               172 1.049e-04 6.101e-07   

# => F-testisuure antaa p-arvon 0.114. Hylätään vastahypoteesi tasolla 0.05.
# Koulutustasolla ei ole tilastollisesti merkitsevää vaikutusta säästöihin,
# kun vain perus/kansakoulun suorittaneet rajataan tarkastelun ulkopuolelle.


# TODO: Tulosta kaaviot.
boxplot(nettotulot_miehet, nettotulot_naiset, col = c("skyblue", "pink"), xaxl="n", ylab = "€/kk")
axis(1, at = c(1,2), labels = c("Miehet", "Naiset"))
pdf('boxplot_tulot.pdf')
dev.off()


##############################
# 2. Iän vaikutus säästöihin #
##############################

# Kuvaillaan iän vaikutusta säästöihin sirontakuviolla
plot(talous$ika, talous$saasto94)

# Sirontakuvion perusteella iällä ja säästöillä näyttäisi olevan korrelaatio
# Varmennetaan tämä: Saadaan merkitsevä positiivinen korrelaatio.
cor(talous$ika, talous$saasto94) # 0.5726042
cor.test(talous$ika, talous$saasto94) # p-value < 2.2e-16

# Sovitetaan aineistoon lineaarinen regressiomalli
reg <- lm(talous$saasto94~talous$ika)

# (Intercept)   talous$ika  
# -5286         1057 
abline(-5286, 1057, col="red")

summary(reg)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -5285.6     4363.6  -1.211    0.227    
# talous$ika    1057.1      113.4   9.318   <2e-16 ***

# Adjusted R-squared:  0.3241.
# F-statistic: 86.83 on 1 and 178 DF,  p-value: < 2.2e-16
# => Malli on tilastollisesti merkitsevä. Kulmakerroin ei kuitenkaan ole.
# => F-testisuure kertoo iän vaikuttavan merkitsevästi säästöihin. 

# Lineaarinen regressio on herkkä poikkeaville havainnoille.
plot(reg)
# Sirontakuviosta huomataan, että havainnot 11, 116, 178 ovat selkeästi poikkeavia

# Käytetään jälleen dataan haettavaa optimia potenssimuunnosta normaalisuuden parantimseksi
pt_saasto94 <- powerTransform(talous$saasto94) # 0.1838458
plot(talous$saasto94^0.1838458~talous$ika)
reg_muunnettu <- lm(talous$saasto94^0.1838458~talous$ika) # 5.20707 / 0.03949 
abline(5.20707,0.03949, col="blue")
summary(reg_muunnettu)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 5.207066   0.150147   34.68   <2e-16 ***
# talous$ika  0.039491   0.003903   10.12   <2e-16 ***
# Adjusted R-squared:  0.3615 
# F-statistic: 102.4 on 1 and 178 DF,  p-value: < 2.2e-16

# Nyt mallin vakiotermi ja kulmakerroin ovat molemmat merkitseviä,
# lisäksi jälkimmäisen mallin korjattu selitysaste (ja F-testisuure) on hieman korkeampi.

# Vaihtoehtoisesti voidaan myös yrittää poistaa 
# poikkeavia havaintoja ja sovittaa suoraa uudelleen.
# Kumpiakin malleja voidaan kuitenkin jo pitää riittävän hyvinä,
# joten jätetään alkuperäinen data rauhaan.