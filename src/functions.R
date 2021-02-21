#{{{ load & read
require(devtools)
require(GenomicFeatures)
load_all('~/git/rmaize')
require(progress)
require(ape)
require(ggtree)
require(ggforce)
require(Rtsne)
require(ggpubr)
require(lubridate)
options(dplyr.summarise.inform = F)
dirg = '~/data/genome'
dirp = "~/projects/epi"
dird = file.path(dirp, 'data')
dirr = '~/projects/stress/nf/raw'
dirf = file.path(dird, '95_figures', 'plots')
gcfg = read_genome_conf(genome='Osativa')
cols100 = colorRampPalette(rev(brewer.pal(n = 6, name = "RdYlBu")))(100)
cols100v = viridis_pal(direction=-1,option='magma')(100)
colbright <- function(col) {x = col2rgb(col); as.integer(.2126*x[1] + .7152*x[2] + .0722*x[3])}
cols36 = c(pal_ucscgb()(18)[8], pal_igv()(18), pal_ucscgb()(18)[c(1:7,9:18)])
brights36 = tibble(col=cols36) %>% mutate(bright=map_int(col,colbright)) %>%
    mutate(b = ifelse(bright<128, 'white','black')) %>% pull(b)
#}}}




