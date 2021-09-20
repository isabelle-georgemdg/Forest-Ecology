library(SPEI)

prism = read.csv("PRISMdata2007_2017.csv")

TTSF = subset(prism, Name == "TTSF")

TTSFtho <- thornthwaite(TTSF$tmean,37.487)
TTSFhar <- hargreaves(TTSF$tmin,TTSF$tmax,lat=37.487)

SNF = subset(prism, Name == "SNF")

SNFtho <- thornthwaite(SNF$tmean,37.494)
SNFhar <- hargreaves(SNF$tmin,SNF$tmax,lat=37.494)

BWSP = subset(prism, Name == "BWSP")

BWSPtho <- thornthwaite(BWSP$tmean,38.351)
BWSPhar <- hargreaves(BWSP$tmin,BWSP$tmax,lat=38.351)

SSSP = subset(prism, Name == "SSSP")

SSSPtho <- thornthwaite(SSSP$tmean,39.899)
SSSPhar <- hargreaves(SSSP$tmin,SSSP$tmax,lat=39.899)

APR = subset(prism, Name == "APR")

APRtho <- thornthwaite(APR$tmean,lat = APR$Latitude[1])
APRhar <- hargreaves(APR$tmin,APR$tmax,lat=APR$Latitude[1])

KSP = subset(prism, Name == "KSP")

KSPtho <- thornthwaite(KSP$tmean,lat = KSP$Latitude[1])
KSPhar <- hargreaves(KSP$tmin,KSP$tmax,lat=KSP$Latitude[1])

NOW = subset(prism, Name == "NOW")

NOWtho <- thornthwaite(NOW$tmean,lat = NOW$Latitude[1])
NOWhar <- hargreaves(NOW$tmin,NOW$tmax,lat=NOW$Latitude[1])

KWW = subset(prism, Name == "KWW")

KWWtho <- thornthwaite(KWW$tmean,lat = KWW$Latitude[1])
KWWhar <- hargreaves(KWW$tmin,KWW$tmax,lat=KWW$Latitude[1])

KMSF = subset(prism, Name == "KMSF")

KMSFtho <- thornthwaite(KMSF$tmean,lat = KMSF$Latitude[1])
KMSFhar <- hargreaves(KMSF$tmin,KMSF$tmax,lat=KMSF$Latitude[1])

GTSP = subset(prism, Name == "GTSP")

GTSPtho <- thornthwaite(GTSP$tmean,lat = GTSP$Latitude[1])
GTSPhar <- hargreaves(GTSP$tmin,GTSP$tmax,lat=GTSP$Latitude[1])

LLW = subset(prism, Name == "LLW")

LLWtho <- thornthwaite(LLW$tmean,lat = LLW$Latitude[1])
LLWhar <- hargreaves(LLW$tmin,LLW$tmax,lat=LLW$Latitude[1])

KNRS = subset(prism, Name == "KNRS")

KNRStho <- thornthwaite(KNRS$tmean,lat = KNRS$Latitude[1])
KNRShar <- hargreaves(KNRS$tmin,KNRS$tmax,lat=KNRS$Latitude[1])

library(reshape2)
TTSF2 = cbind(rep("TTSF", times=132), rep(seq(1:12), times=11), melt(TTSFtho, value = "tho"), melt(TTSFhar, value="har"))
SNF2 = cbind(rep("SNF", times=132), rep(seq(1:12), times=11), melt(SNFtho, value = "tho"), melt(SNFhar, value="har"))
BWSP2 = cbind(rep("BWSP", times=132), rep(seq(1:12), times=11), melt(BWSPtho, value = "tho"), melt(BWSPhar, value="har"))
SSSP2 = cbind(rep("SSSP", times=132), rep(seq(1:12), times=11), melt(SSSPtho, value = "tho"), melt(SSSPhar, value="har"))
APR2 = cbind(rep("APR", times=132), rep(seq(1:12), times=11), melt(APRtho, value = "tho"), melt(APRhar, value="har"))
KSP2 = cbind(rep("KSP", times=132), rep(seq(1:12), times=11), melt(KSPtho, value = "tho"), melt(KSPhar, value="har"))
NOW2 = cbind(rep("NOW", times=132), rep(seq(1:12), times=11), melt(NOWtho, value = "tho"), melt(NOWhar, value="har"))
KWW2 = cbind(rep("KWW", times=132), rep(seq(1:12), times=11), melt(KWWtho, value = "tho"), melt(KWWhar, value="har"))
KMSF2 = cbind(rep("KMSF", times=132), rep(seq(1:12), times=11), melt(KMSFtho, value = "tho"), melt(KMSFhar, value="har"))
GTSP2 = cbind(rep("GTSP", times=132), rep(seq(1:12), times=11), melt(GTSPtho, value = "tho"), melt(GTSPhar, value="har"))
LLW2 = cbind(rep("LLW", times=132), rep(seq(1:12), times=11), melt(LLWtho, value = "tho"), melt(LLWhar, value="har"))
KNRS2 = cbind(rep("KNRS", times=132), rep(seq(1:12), times=11), melt(KNRStho, value = "tho"), melt(KNRShar, value="har"))

PET = cbind(TTSF2, SNF2, BWSP2, SSSP2, APR2, KSP2, NOW2, KWW2, KMSF2, GTSP2, LLW2, KNRS2)










