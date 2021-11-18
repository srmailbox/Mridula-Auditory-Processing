#### Reading and Hearing Project
# A project looking at the relationship between auditory processing and reading
# in children.
#
include(tidyverse); include(lavaan)

DataVersion = "2021.11.15"
ScriptVersion = "1.0"

#### 1. Import the data ####
# Data is split across two sheets, with the attention tasks in Sheet1.
rawDat = readxl::read_xlsx(paste0("DataSet.", DataVersion, ".xlsx"), "Feuil1") %>% 
  select(1:13) %>% 
  merge(readxl::read_xlsx(paste0("DataSet.", DataVersion, ".xlsx"), "Sheet1")
        , by=c("Participant", "Group", "Age")) %>% 
  mutate(Group  = factor(Group, labels=c("Ctrl", "Exp"))) %>% 
  mutate(ToEASky = scale(`ToEATestA Sky Search`)
         , ToEATime = scale(`ToEATestATime Per Target`)
         , ToEACreature = scale(`ToEATestBCreature Counting`)
         , ToEACreatureT = scale(`ToEATestBCreature Counting Time`)
         , ToEAMap = scale(`ToEATestCMap Mission`)
         , ToEASameW = scale(`ToEATestDSame World`)
         , ToEAOppW = scale(`ToEATestFOpposite World`))


#### 2. Variable Reduction ####
# Some quick checks to see if it makes sense to merge some sets of variables into
# latent variables.

### 2A. Cognitive skill variables####
cor(rawDat %>% select(PPVT, WNV, RANDigits))
#- not terribly correlated really. Given sample size
# maybe stick with just WNV (Nonverbal IQ)

### 2B. Memory ####
cor(rawDat %>% select(BackDigitSpan, FwdDigitSpan))
# These are better, keep both

### 2C. Attention ####
cor(rawDat %>% select(starts_with("ToEA"), -starts_with("ToEATest")
                      , -ToEACreatureT, -ToEATime))
# - not super well correlated, except the two "world" tests. Maybe we should do 
# a factor analysis:

factanal(~ToEASky+ToEACreature+ToEAMap+ToEASameW+ToEAOppW, data = rawDat
         , factors=2)

# Factor analysis suggests two factors: one that uses the two "World" variables
# and one that is just an average of them all.

summary(princomp(rawDat %>% select(starts_with("ToEA"), -starts_with("ToEATest")
                           , -ToEACreatureT, -ToEATime)))
loadings(princomp(rawDat %>% select(starts_with("ToEA"), -starts_with("ToEATest")
                                   , -ToEACreatureT, -ToEATime)))
# PCA says one overall component, and then one that compares the two World tests
# and to Creature (and Sky to a lesser extent)

# I think we just keep them all.

#### 3. Set up the Models ####

### 3A. Reading ####

read.sem = '
#### Latent Variables:
Cogskill =~ WNV#+PPVT+RANDigits
Memory =~ BackDigitSpan + FwdDigitSpan
Attention =~ ToEASky + ToEACreature + ToEAMap + ToEASameW + ToEAOppW

#### Regressions
CC2Nonwords ~ CTOPPElision + FPT + Log10FD + Attention + Memory + Cogskill
CTOPPElision ~ FPT + Log10FD
FPT ~ Memory + Attention

#### Correlations
# BackDigitSpan ~~ FwdDigitSpan
# ToEASameW ~~ ToEAOppW #+ToEAMap+ToEACreature+ToEASky

#### To deal with poor fits
Cogskill ~~ Attention + Memory
Memory ~~ Attention

## Terms added to deal with poor fits of Log10FD
Log10FD ~~ FPT+Cogskill+Attention+Memory
'

#### 4. Fit CFAs. ####

### 4A. Reading Only ####

read.cfa = cfa(read.sem, data = rawDat, std.ov=T, std.lv=T, orthogonal=T)
# Negative variance on the CC2.
summary(read.cfa)
fitmeasures(read.cfa, fit.measures = c("rmsea", "srmr", "cfi", "agfi"))
# Fit statistics are terrible - none of them meet the target rules of thumb.
semPlot::semPaths(read.cfa)

# Compare observed vs implied correlations to identify potential missing 
# structure
implied.cors = lavInspect(read.cfa, "cov.ov")
class(implied.cors)="matrix"

obs.cors = cor(rawDat %>% select(all_of(rownames(implied.cors))))
implied.cors[upper.tri(implied.cors)]=obs.cors[upper.tri(obs.cors)]
round(implied.cors, 3)

### OK, so the problem is clearly that Log10FD correlates with everything, 
# but the current structure assumes it correlates with nothing except CTOPP and
# CC2.
