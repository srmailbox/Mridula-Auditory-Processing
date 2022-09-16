#### Reading and Hearing Project
# A project looking at the relationship between auditory processing and reading
# in children.
#
# Change Log:


include(tidyverse); include(lavaan)

DataVersion = "2022.08.30"
ScriptVersion = "1.0"

# 1. Import the data ####

rawDat = readxl::read_xlsx(paste0("Data.", DataVersion, ".xlsx"), "Sheet1") %>%
  select(`AB 65`, DDT, AuditoryAttention, VisualAttention, `No. Rept_backward`
         , `No. Rept_forward`, Q_PhoMani, LangContent, `HINT 65`) %>% 
  rename(BackDigSpan = `No. Rept_backward`
         , FwdDigSpan = `No. Rept_forward`
         , HINT = `HINT 65`
         , AB = `AB 65`)

# 2. Variable Reduction ####
# Some quick checks to see if it makes sense to merge some sets of variables into
# latent variables.

## 2A. Exec Functioning variables####

### correlations ####
cor(rawDat %>% select(BackDigSpan, FwdDigSpan, AuditoryAttention, VisualAttention))

# Hm. Attentions are correlated, .74 - but the DigSpans are poorly correlated
# and they do not correlate at all across "memory" vs "attention"

### Factor analysis ####
ef.fa = factanal(rawDat %>% 
                   select(BackDigSpan, FwdDigSpan, AuditoryAttention, VisualAttention)
                 , 1)

# Yeah, whether I use just BackDigSpan or both, the factor is clearly largely
# Attention, with the other component eliminated.

### PCA approach ####

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
           .25*(scale(AuditoryAttention)+scale(VisualAttention)))


rawDat= rawDat %>% 
  mutate(EF = .5*scale(BackDigSpan)+
           .25*(scale(AuditoryAttention)+scale(VisualAttention))
         , Attention = .5*(scale(AuditoryAttention)+scale(VisualAttention)))


## 2B. Auditory Processing ####
cor(rawDat %>% select(DDT, AB))

# Hm. These are completely uncorrelated so this won't work at all.  

# Use just DDT which has the highest general correlations


# 3. Set up the Model ####

hint.sem = '
#### Latent Variables:
# ExecFunc =~ .5*BackDigSpan + .25*AuditoryAttention + .25*VisualAttention
# ExecFunc =~ BackDigSpan + AuditoryAttention + VisualAttention
Attention =~ AuditoryAttention + VisualAttention
#### Regressions
HINT ~ a*Q_PhoMani + Attention + d*DDT
LangContent ~ Attention
Q_PhoMani ~ Attention+b*DDT
DDT ~ Attention

HINT ~~ LangContent

## Parameters:
APtoHINTindirect := a*b
APtoHINTtotal :=a*b+d
'

# 4. Fit CFA ####

hint.cfa = cfa(hint.sem, data = rawDat %>% select(-Attention)
               , std.ov=T, std.lv=T, orthogonal=T)
# Negative variance on the hint.
summary(hint.cfa)
fitmeasures(hint.cfa, fit.measures = c("rmsea", "srmr", "cfi", "agfi"))
# poor, although 3 of the 4 are now close.
# rmsea  srmr   cfi  agfi 
# 0.130 0.090 0.893 0.811 

# With just an Attention LV instead of Executive Function:
# Becomes 2 good fit indices, 1 moderately good, and 1 poor
# rmsea  srmr   cfi  agfi 
# 0.110 0.062 0.958 0.859 
semPlot::semPaths(hint.cfa, whatLabels="est")

# Compare observed vs implied correlations to identify potential missing 
# structure
hint.implied = lavInspect(hint.cfa, "cov.ov")
class(hint.implied)="matrix"

hint.obs = cor(rawDat %>% select(all_of(rownames(hint.implied))))
hint.cors = hint.implied
hint.cors[upper.tri(hint.implied)]=hint.obs[upper.tri(hint.obs)]
round(hint.cors, 3)

round(hint.obs - hint.implied, 3) #%>% rowMeans

# 5. Parameter summaries  ####

parameterestimates(hint.cfa) %>% 
  # filter(op=="~" | op==":=" | op==":=") %>% 
  mutate(
    label = paste(lhs, op, rhs)
  ) %>% 
  mutate(psig = cut(pvalue, breaks=c(-Inf, .001, .01, .05, .1, 1)
                    , labels=c("***", "**", "*", "+", ""))
  ) %>% 
  select(label, est, psig, pvalue, se, z, ci.lower, ci.upper)

## 5C. Merged Params.
write.csv(rbind(data.frame(Model="Reading", hint.coefs)
                , data.frame(Model="Read+Pass", readpass.coefs)) %>% 
            reshape(direction="wide", idvar="Param", timevar="Model")
          , file=paste0("ModelParameters.", DataVersion, ".v", ScriptVersion, ".csv")
)

# 6 Reported Coefficients ####
write.csv(readpass.coefs
          , file=paste0("ReadPassModel.Parameters.", DataVersion, ".v"
                        , ScriptVersion, ".csv"))
