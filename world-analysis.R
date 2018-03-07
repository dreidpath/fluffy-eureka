# Analyse global trends
library (ggplot2)

worldDF$date <- as.integer(worldDF$date)

worldDFw <- worldDF %>%
  select(date, indicator = indicatorID, value) %>%
  spread(key = indicator, value = value) %>%
  select(date, 
         co2 = EN.ATM.CO2E.PC,
         gni = NY.GNP.PCAP.PP.KD,
         gdp = NY.GDP.PCAP.PP.KD,
         le = SP.DYN.LE00.IN,
         imr = SP.DYN.IMRT.IN) %>%
  filter(date >=1990)

# Set up annual differences
worldDFw$imrdiff <- c(NA, diff(worldDFw$imr))
worldDFw$lediff <- c(NA, diff(worldDFw$le))
worldDFw$gnidiff <- c(NA, diff(worldDFw$gni))
worldDFw$co2diff <- c(NA, diff(worldDFw$co2))
worldDFw$gdpdiff <- c(NA, diff(worldDFw$gdp))



worldDFw %>%
  ggplot(aes(date, co2)) +
  geom_point()

worldDFw %>%
  ggplot(aes(date, le)) +
  geom_point()

worldDFw %>%
  ggplot(aes(co2, le)) +
  geom_point() +
  geom_smooth(method = "lm")

data.frame(co2 = diff(worldDFw$co2), le = diff(worldDFw$le)) %>%
  ggplot(aes(co2, le)) +
  geom_point() +
  geom_smooth(method = "lm")


data.frame(co2 = diff(worldDFw$co2), gni = diff(worldDFw$gni)) %>%
#  filter(gni<1000) %>%  # Use this line to check an outlier effect
  ggplot(aes(co2, gni)) +
  geom_point() +
  geom_smooth(method = "lm")

worldDFw %>%
  ggplot(aes(co2diff/co2*100, lediff/le*100)) +
  geom_point() +
  geom_smooth(method = "lm")

le_m1 <- lm(lediff ~ co2diff + date + gnidiff, data = worldDFw)
summary(le_m1)

le_m2 <- lm(lediff ~ co2diff*date + gnidiff, data = worldDFw)
summary(le_m2)

anova(le_m1, le_m2)

worldDFw %>%
  ggplot(aes(co2diff/co2*100, imrdiff/imr*100)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Annual %change in per capita CO2 production") +
  ylab("Annual %change in IMR")

imr_m1 <- lm(imrdiff ~ co2diff + date + gnidiff, data = worldDFw)
summary(imr_m1)

imr_m2 <- lm(imrdiff ~ co2diff*date + gnidiff, data = worldDFw)
summary(imr_m2)

anova(imr_m1, imr_m2)

