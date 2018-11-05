require(smatr)

mod<- sma(PhiPS2 ~ PhiCO2*genotype, data= fluro_iso[fluro_iso$treat=="WS",])
summary(mod)

plot(mod)
require(visreg)
require(phia)




p_mod<- lm(Rd~ harvest*genotype*treat, data= fluro_iso)
plot(p_mod)
anova(p_mod)



## visreg plotting
visreg(p_mod)
visreg(p_mod, "harvest", "treat", overlay= T)
visreg(p_mod,  "treat","harvest", overlay= T)
## test which subtype was varried with other at which growth condition
testInteractions(p_mod, pairwise="harvest", fixed= "treat")

testInteractions(p_mod, pairwise="genotype", fixed= "harvest") 
