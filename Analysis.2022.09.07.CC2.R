#### Reading and Hearing Project
# A project looking at the relationship between auditory processing and reading
# in children.
#
# Change Log:
# 2022-09-14: see branch CoreLangLatentVariable for an exploration of the CELF
#             subtests


include(tidyverse); include(lavaan)

DataVersion = "2022.08.30"
ScriptVersion = "1.0"

#### 1. Import the data ####

rawDat = readxl::read_xlsx(paste0("Data.", DataVersion, ".xlsx"), "Sheet1") %>%
  select(FPT, AuditoryAttention, VisualAttention, `No. Rept_backward`
         , `No. Rept_forward`, Q_PhoMani, CoreLang, Nonword_zscore) %>% 
  rename(BackDigSpan = `No. Rept_backward`
         , FwdDigSpan = `No. Rept_forward`)

#### 2. Variable Reduction ####
# Some quick checks to see if it makes sense to merge some sets of variables into
# latent variables.

### 2A. Exec Functioning variables####
cor(rawDat %>% select(BackDigSpan, FwdDigSpan, AuditoryAttention, VisualAttention))

# Hm. Attentions are correlated, .74 - but the DigSpans are poorly correlated
# and they do not correlate at all across "memory" vs "attention"

ef.fa = factanal(rawDat %>% 
                   select(BackDigSpan, FwdDigSpan, AuditoryAttention, VisualAttention)
                 , 1)

# Yeah, whether I use just BackDigSpan or both, the factor is clearly largely
# Attention, with the other component eliminated.

## PCA approach

ef.pca = 
  princomp(rawDat %>% 
             select(BackDigSpan, FwdDigSpan, AuditoryAttention, VisualAttention) %>% 
             scale)

# This suggests one or two components, but particularly the first:
# 1. Everything, but especially the attention measures (47% var)
# 2. Memory "vs" attention (but heavy on the memory) (30%)

# If we really want to combine these into a single latent variable, I think
# the best approach is to simply mix them as .5 BackDigSpan, .25 AudAtt, .25
# VisAtt

rawDat= rawDat %>% 
  mutate(EF = .5*scale(BackDigSpan)+
           .25*(scale(AuditoryAttention)+scale(VisualAttention))
         , Attention = scale(.5*(scale(AuditoryAttention)+scale(VisualAttention))))

# The best approach, though, is probably to ditch the BDS and make it an
# attention LV

#### 3. Set up the Model ####

cc2.sem = '
#### Latent Variables:
# ExecFunc =~ .5*BackDigSpan + .25*AuditoryAttention + .25*VisualAttention
# ExecFunc =~ BackDigSpan + AuditoryAttention + VisualAttention
Attention =~ AuditoryAttention + VisualAttention
#### Regressions
Nonword_zscore ~ CoreLang + Q_PhoMani + Attention + FPT
CoreLang ~ Attention
Q_PhoMani ~ Attention+FPT
FPT ~ Attention

'

#### 4. Fit CFA. ####

cc2.cfa = cfa(cc2.sem, data = rawDat %>% select(-Attention)
              , std.ov=T, std.lv=T, orthogonal=T)
# Negative variance on the CC2.
summary(cc2.cfa)
fitmeasures(cc2.cfa, fit.measures = c("rmsea", "srmr", "cfi", "agfi"))
# Abysmal, if we fix the "EF" stuff.
# rmsea  srmr   cfi  agfi 
# 0.235 0.169 0.635 0.630 

# If we allow the weights for EF to vary, we get a much better fit
# But still not a good fit:
# rmsea  srmr   cfi  agfi 
# 0.153 0.105 0.881 0.749 

# OH, of course. The whole point is that I don't want to "properly reconstruct"
# the correltations amongst Aud/VisAttn/BDS and the other stuff. If we just use
# EF = .5*BDS + .25*VisAttn + .25*AudAttn

# still abysmal :(
# rmsea  srmr   cfi  agfi 
# 0.275 0.113 0.863 0.430

# Dropping the BackDigSpan variable and going with just attention helps a bit
# but it's still not a good model.
# rmsea  srmr   cfi  agfi 
# 0.185 0.100 0.908 0.731 
# poor  okish good  poor

# Why does that work better than just averaging Vis and Aud attention, when
# the LV is basically just the average anyway?

semPlot::semPaths(cc2.cfa, whatLabels="est")

# Compare observed vs implied correlations to identify potential missing 
# structure
cc2.implied = lavInspect(cc2.cfa, "cov.ov")
class(cc2.implied)="matrix"

cc2.obs = cor(rawDat %>% select(all_of(rownames(cc2.implied))))
cc2.cors = cc2.implied
cc2.cors[upper.tri(cc2.implied)]=cc2.obs[upper.tri(cc2.obs)]
round(cc2.cors, 3)

round(cc2.obs - cc2.implied, 3) %>% rowMeans
# The problem here is CoreLang which has very poor reconstructions for 4 of
# of the 6 correlations, + Q_PhoMani~~BackDigSpan.

# So CoreLang is just not being reconstructed no matter what we do.

#### 5. Parameter summaries  ####
# TODO: still need to find a better fitting model.


### 5C. Merged Params.
write.csv(rbind(data.frame(Model="Reading", cc2.coefs)
                , data.frame(Model="Read+Pass", readpass.coefs)) %>% 
            reshape(direction="wide", idvar="Param", timevar="Model")
          , file=paste0("ModelParameters.", DataVersion, ".v", ScriptVersion, ".csv")
)

#### 6 Reported Coefficients ####
write.csv(readpass.coefs
          , file=paste0("ReadPassModel.Parameters.", DataVersion, ".v"
                        , ScriptVersion, ".csv"))
