#### Reading and Hearing Project
# A project looking at the relationship between auditory processing and reading
# in children.
#
# Change Log:
# V 2.0
# 2021-11-18: - model specification changed in order to better capture the
#     the correlations with Log10FD (among a few other adjustments). This
#     version also combines log10FD and FPT into an Auditory Processing LV,
#     and merges attention and memory into a single Executive Functioning LV.
# V 2.1
# 2021-11-18: - factor analysis suggests we really only need BackDigitSpan,
#     ToEASameW and OppW in the ExecFunc. I'm a little concerned that this means
#     ExecFunc is really just memory, but I'll fit a version (branched) of it
#     anyway, and we can decide later if we want it to be the main version.
#     This version turns out to fit slightly better. The main question is
#     Executive Function really represents EF, or if it's now just memory.
#     - Consensus from MS and LC is to use the simplified version.


include(tidyverse); include(lavaan)

DataVersion = "2021.11.15"
ScriptVersion = "2.1"

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

### 2A. Language variables####
cor(rawDat %>% select(PPVT, WNV, RANDigits))
#- not terribly correlated really. Given sample size
# Will now use only PPVT and instead of Cognitive Skill, this is "Language"

### 2B. Executive Functioning ####
cor(rawDat %>% select(BackDigitSpan, FwdDigitSpan, starts_with("ToEA")
                      , -starts_with("ToEATest"), -ToEACreatureT, -ToEATime))
# Curious. It looks like the ToEASameW, *DigitSpan and ToEAOppW variables all
# hang together. Let's see if we are ok to use these all as one factor, or
# or better with 2.

ef.fa = factanal(rawDat %>% 
           select(BackDigitSpan, FwdDigitSpan, starts_with("ToEA")
                  , -starts_with("ToEATest"), -ToEACreatureT, -ToEATime)
         , 2)

# Looks like there are at least two factors, but importantly it seems like
# we may want to drop FwdDigitSpan, and all of the ToEA other than the SameW and
# OppW

## PCA approach

ef.pca = 
  princomp(rawDat %>% 
             select(BackDigitSpan, FwdDigitSpan, starts_with("ToEA")
                    , -starts_with("ToEATest"), -ToEACreatureT, -ToEATime) %>% 
             scale)

# This suggests one or maybe two components, but particularly the first:
# 1. Everything, but especially BackDigitSpan, ToEASameW and OppW (41.5% var)
# 2. The other three ToEA* vs *DigitSpan

# If we really want to combine these into a single latent variable, I think
# we're talking about memory using the BackDigitSpan and ToEASameW and OppW
# Need to confirm that ToEASameW and OppW are memory heavy?

### 2C. Auditory Processing ####
cor(rawDat %>% select(Log10FD, FPT))

#### 3. Set up the Models ####

### 3A. Reading SEM Model####

read.sem = '
#### Latent Variables:
# Language =~ PPVT
#Memory =~ BackDigitSpan + FwdDigitSpan
ExecFunc =~ BackDigitSpan + ToEASameW + ToEAOppW
AudProc =~ Log10FD + FPT

#### Regressions
CC2Nonwords ~ CTOPPElision + AudProc + ExecFunc + PPVT
CTOPPElision ~ ExecFunc + AudProc# + PPVT
AudProc ~ ExecFunc

#### Correlations
PPVT ~~ ExecFunc
'

# I added PPVT to CTOPPElision since it seems to me that the PPVT really
# ought to correlate with phoneme awareness, but I guess not.

#### 4. Fit CFAs. ####

### 4A. Reading Only Fit ####

read.cfa = cfa(read.sem, data = rawDat, std.ov=T, std.lv=T, orthogonal=T)
# Negative variance on the CC2.
summary(read.cfa)
fitmeasures(read.cfa, fit.measures = c("rmsea", "srmr", "cfi", "agfi"))
# If we restrict to just the three variables that FA provided (see 
#   SimpleEFForReadingOnly), we get:
# rmsea  srmr   cfi  agfi 
# 0.120 0.071 0.931 0.808
# bad   good  good  ok
# Seems to me to be six of one half-dozen of the other. we trade RMSEA for CFI.
# I think the simpler model may actually be more sensible.
semPlot::semPaths(read.cfa)

# Compare observed vs implied correlations to identify potential missing 
# structure
implied.cors = lavInspect(read.cfa, "cov.ov")
class(implied.cors)="matrix"

obs.cors = cor(rawDat %>% select(all_of(rownames(implied.cors))))
summary.cors = implied.cors
summary.cors[upper.tri(implied.cors)]=obs.cors[upper.tri(obs.cors)]
round(summary.cors, 3)

round(obs.cors - implied.cors, 3)
### OK, so the problem is clearly that Log10FD correlates with everything, 
# but the current structure assumes it correlates with nothing except CTOPP and
# CC2.

#### 5. Parameter summaries ####

### 5A. Reading Only Params ####

coefs = list(
  Reading = list(c("CC2Nonwords", "CTOPPElision"), c("CC2Nonwords", "AudProc")
                 , c("CC2Nonwords", "PPVT"), c("CC2Nonwords", "ExecFunc"))
  , PhonAwar = list(c("CTOPPElision", "AudProc"), c("CTOPPElision", "ExecFunc"))
  , Language = list(c("PPVT", "ExecFunc"))
  , AudProc = list(c("AudProc", "ExecFunc"))
  , AudProcLV = list(c("AudProc", "Log10FD"), c("AudProc", "FPT"))
)
extract.coef(read.cfa, coefs) %>% bind_rows


