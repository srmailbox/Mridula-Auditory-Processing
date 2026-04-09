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


# 3. Attention SEM ####

attention.sem = '
#### Latent Variables:
# ExecFunc =~ .5*BackDigSpan + .25*AuditoryAttention + .25*VisualAttention
# ExecFunc =~ BackDigSpan + AuditoryAttention + VisualAttention
Attention =~ AuditoryAttention + VisualAttention
#### Regressions
HINT ~ a*Q_PhoMani + Attention + d*DDT
LangContent ~ Attention
Q_PhoMani ~ Attention+b*DDT
DDT ~ c*Attention

HINT ~~ LangContent

## Parameters:
AP.PA.HINTindirect := a*b
APtoHINTtotal :=a*b+d

Attn.AP.HINTpath := d*c
'
## 3A. Fit Model ####

attention.cfa = cfa(attention.sem, data = rawDat %>% select(-Attention)
                    , std.ov=T, std.lv=T, orthogonal=T)
# Negative variance on the attention.
summary(attention.cfa)
fitmeasures(attention.cfa, fit.measures = c("rmsea", "srmr", "cfi", "agfi"))
# poor, although 3 of the 4 are now close.
# rmsea  srmr   cfi  agfi 
# 0.130 0.090 0.893 0.811 

# With just an Attention LV instead of Executive Function:
# Becomes 2 good fit indices, 1 moderately good, and 1 poor
# rmsea  srmr   cfi  agfi 
# 0.110 0.062 0.958 0.859 
semPlot::semPaths(attention.cfa, whatLabels="est")

# Compare observed vs implied correlations to identify potential missing 
# structure
attention.implied = lavInspect(attention.cfa, "cov.ov")
class(attention.implied)="matrix"

attention.obs = cor(rawDat %>% select(all_of(rownames(attention.implied))))
attention.cors = attention.implied
attention.cors[upper.tri(attention.implied)]=attention.obs[upper.tri(attention.obs)]
round(attention.cors, 3)

round(attention.obs - attention.implied, 3) #%>% rowMeans

## 3B. Parameter summaries  ####

parameterestimates(attention.cfa) %>% 
  # filter(op=="~" | op==":=" | op==":=") %>% 
  mutate(
    label = paste(lhs, op, rhs)
  ) %>% 
  mutate(psig = cut(pvalue, breaks=c(-Inf, .001, .01, .05, .1, 1)
                    , labels=c("***", "**", "*", "+", ""))
  ) %>% 
  select(label, est, psig, pvalue, se, z, ci.lower, ci.upper)

# 4. Memory SEM ####

memory.sem = '
#### Latent Variables:
# ExecFunc =~ .5*BackDigSpan + .25*AuditoryAttention + .25*VisualAttention
# ExecFunc =~ BackDigSpan + AuditoryAttention + VisualAttention
# Attention =~ AuditoryAttention + VisualAttention
Memory =~ BackDigSpan
#### Regressions
HINT ~ a*Q_PhoMani + Memory + d*DDT
LangContent ~ Memory
Q_PhoMani ~ Memory+b*DDT
DDT ~ c*Memory

HINT ~~ LangContent

## Parameters:
AP.PA.HINTindirect := a*b
APtoHINTtotal :=a*b+d
Mem.AP.HINTpath := d*c
'

## 4A. Fit SEM ####

memory.cfa = cfa(memory.sem, data = rawDat #%>% select(-memory)
                    , std.ov=T, std.lv=T, orthogonal=T)

summary(memory.cfa)
fitmeasures(memory.cfa, fit.measures = c("rmsea", "srmr", "cfi", "agfi"))
# 1 good, 1 ok, 2 poor.
# rmsea  srmr   cfi  agfi 
# 0.175 0.085 0.912 0.711

semPlot::semPaths(memory.cfa, whatLabels="est")

# Compare observed vs implied correlations to identify potential missing 
# structure
memory.implied = lavInspect(memory.cfa, "cov.ov")
class(memory.implied)="matrix"

memory.obs = cor(rawDat %>% select(all_of(rownames(memory.implied))))
memory.cors = memory.implied
memory.cors[upper.tri(memory.implied)]=memory.obs[upper.tri(memory.obs)]
round(memory.cors, 3)

round(memory.obs - memory.implied, 3) #%>% rowMeans

## 4B. Parameter summaries  ####

parameterestimates(memory.cfa) %>% 
  filter(op=="~" | op==":=" | op=="~~") %>%
  mutate(
    label = paste(lhs, op, rhs)
  ) %>% 
  mutate(psig = cut(pvalue, breaks=c(-Inf, .001, .01, .05, .1, 1)
                    , labels=c("***", "**", "*", "+", ""))
  ) %>% 
  select(label, est, psig, pvalue, se, z, ci.lower, ci.upper)
