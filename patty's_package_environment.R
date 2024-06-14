# Function to check and install packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of packages to use
packages <- c(
  "tidyverse", "lme4", "lmerTest", "cowplot", "plyr", "dplyr", "readr", "tidyr", 
  "purrr", "effectsize", "DataExplorer", "GGally", "ggfortify", "ghibli", "shiny", 
  "ggpubr", "rstatix", "plotly", "umap", "tsne", "viridis", "RColorBrewer", 
  "see", "performance", "patchwork", "outliers", "readxl", "tibble", "flextable", 
  "kableExtra", "gt", "gtsummary", "lsr", "apaTables", "knitr", "forcats", 
  "hrbrthemes", "vegan", "iml", "tidyposterior", "skimr", "tidymodels", 
  "nnet", "kknn", "sjPlot", "sjmisc", "effects", "sjstats", "webshot", "Hmisc", "conflicted"
)

# Install and load packages
lapply(packages, install_if_missing)

# Packages that need to be loaded
library(tidyverse)
library(lme4)
library(lmerTest)
library(cowplot)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(effectsize)
library(DataExplorer)
library(GGally)
library(ggfortify)
library(ghibli)
library(shiny)
library(ggpubr)
library(rstatix)
library(plotly)
library(umap)
library(tsne)
library(viridis)

# Extras:
library(RColorBrewer)
library(see)
library(performance)
library(patchwork)
library(outliers)
library(readxl)
library(tibble)
library(flextable)
library(kableExtra)
library(gt)
library(gtsummary)
library(lsr)
library(apaTables)
library(knitr)
library(viridis)
library(forcats)
library(hrbrthemes)
library(vegan)
#library(ggvegan) #depreciated
library(iml)
library(tidyposterior)
library(skimr)
library(tidyverse)
library(tidymodels)
library(nnet)
library(kknn)
library(sjPlot)
library(sjmisc)
library(effects)
library(sjstats)
library(webshot)
library(Hmisc)
library(conflicted)

tidymodels_prefer()

conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(yardstick::accuracy)
conflicts_prefer(rstanarm::logit)
conflicts_prefer(readr::cols)
conflicts_prefer(base::`:`)
conflicts_prefer(broom::bootstrap)
conflicts_prefer(svglite::font_face)
conflicts_prefer(lmerTest::lmer)
conflicts_prefer(dplyr::recode)
conflicts_prefer(dplyr::summarise)
