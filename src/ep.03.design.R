source('functions.R')
dirw = glue('{dird}/01_design')

#{{{ F1 selfing design
pass_gt_1 <- function(gt, co.rate=.1) {
    #{{{
    nsite = nrow(gt)
    co1 = sample(1:100, nsite-1, replace=T) < 100 * co.rate
    co2 = sample(1:100, nsite-1, replace=T) < 100 * co.rate
    coc1 = c(0, cumsum(co1) %% 2)
    coc2 = c(0, cumsum(co2) %% 2)
    fj = sample(1:10, 2, replace=T) < 5
    gtn = gt %>% mutate(co1 = coc1, co2 = coc2) %>%
        mutate(fj1 = fj[1], fj2 = fj[2]) %>%
        mutate(an1 = ifelse(co1 == 1, b, a)) %>%
        mutate(bn1 = ifelse(co1 == 1, a, b)) %>%
        mutate(an2 = ifelse(co2 == 1, b, a)) %>%
        mutate(bn2 = ifelse(co2 == 1, a, b)) %>%
        mutate(a = ifelse(fj1, an1, bn1)) %>%
        mutate(b = ifelse(fj2, an2, bn2)) %>%
        select(i,a,b)
    gtn
    #}}}
}
pass_gt_n <- function(gt, gen=1, co.rate=.1) {
    #{{{
    to = gt %>% mutate(gen=0)
    for (j in 1:gen) {
        gtn = pass_gt_1(gt)
        to = to %>% bind_rows(gtn %>% mutate(gen=j))
        gt = gtn
    }
    to
    #}}}
}

nsite = 20
nsam = 10
gen = 5
co.rate = .1
gt = tibble(i=1:nsite, a=rep(0,nsite), b=rep(1,nsite))
ti = tibble(sam = 1:nsam, gt = list(gt)) %>%
    mutate(gtn = map(gt, pass_gt_n, gen=gen, co.rate=co.rate)) %>%
    select(sam, gtn) %>%
    unnest(gtn)
ti %>%
    mutate(ab=glue("{a}{b}")) %>%
    select(sam, i,gen,ab) %>% spread(gen,ab)
#{{{ plot
tp = ti %>% filter(gen>=1) %>% mutate(gen = glue("F{gen+1}")) %>%
    mutate(sam = ifelse(sam > 5, sam+.5, sam)) %>%
    mutate(a=as.factor(a), b=as.factor(b), gen=as.factor(gen))
sams = unique(tp$sam)
p = ggplot(tp) +
    geom_rect(aes(xmin=sam-.3,xmax=sam-.1,ymin=i-.5,ymax=i+.5,fill=a),size=0) +
    geom_rect(aes(xmin=sam+.1,xmax=sam+.3,ymin=i-.5,ymax=i+.5,fill=b),size=0) +
    geom_text(aes(x=5.75,y=5.5,label='...'),size=3) +
    facet_wrap(~gen, strip.position='left',ncol=1) +
    scale_x_continuous(name="Rice EpiRIL Population (500)", breaks=sams,expand=expansion(mult=c(.02,.02))) +
    scale_y_continuous(expand=expansion(mult=c(.05,.05))) +
    scale_fill_simpsons() +
    otheme(legend.pos='none',xtitle=T,panel.border=F) +
    theme(strip.text.y.left = element_text(angle=0))
#}}}
fo = glue("{dirw}/01.pdf")
ggexport(p, filename=fo, width=6, height=6)
#}}}

#{{{ BC2 design
pass_gt_1 <- function(gt, co.rate=.1) {
    #{{{
    nsite = nrow(gt)
    co1 = sample(1:100, nsite-1, replace=T) < 100 * co.rate
    co2 = sample(1:100, nsite-1, replace=T) < 100 * co.rate
    coc1 = c(0, cumsum(co1) %% 2)
    coc2 = c(0, cumsum(co2) %% 2)
    fj = sample(1:10, 2, replace=T) < 5
    gtn = gt %>% mutate(co1 = coc1, co2 = coc2) %>%
        mutate(fj1 = fj[1], fj2 = fj[2]) %>%
        mutate(an1 = ifelse(co1 == 1, b, a)) %>%
        mutate(bn1 = ifelse(co1 == 1, a, b)) %>%
        mutate(an2 = ifelse(co2 == 1, b, a)) %>%
        mutate(bn2 = ifelse(co2 == 1, a, b)) %>%
        mutate(a = ifelse(fj1, an1, bn1)) %>%
        mutate(b = ifelse(fj2, an2, bn2)) %>%
        select(i,a,b)
    gtn
    #}}}
}
pass_gt_n <- function(gt, gen=1, co.rate=.1) {
    #{{{
    to = gt %>% mutate(gen=0)
    for (j in 1:gen) {
        gtn = pass_gt_1(gt)
        to = to %>% bind_rows(gtn %>% mutate(gen=j))
        gt = gtn
    }
    to
    #}}}
}
pass_gt_1_bc <- function(gt, co.rate=.1) {
    #{{{
    nsite = nrow(gt)
    co1 = sample(1:100, nsite-1, replace=T) < 100 * co.rate
    coc1 = c(0, cumsum(co1) %% 2)
    fj = sample(10,1) > 5
    gtn = gt %>% mutate(co1 = coc1) %>%
        mutate(an = ifelse(co1 == 1, b, a)) %>%
        mutate(bn = ifelse(co1 == 1, a, b))
    if (fj) {
        gtn = gtn %>% mutate(a = an)
    } else {
        gtn = gtn %>% mutate(a = bn)
    }
    gtn %>%
        mutate(b = 1) %>%
        select(i,a,b)
    #}}}
}

nsite = 48
nsam = 10
gen = 5
co.rate = .1
gt1 = tibble(sam=4,i=1:nsite, a=rep(0,nsite), b=rep(0,nsite))
gt2 = tibble(sam=6,i=1:nsite, a=rep(1,nsite), b=rep(1,nsite))
tg0 = rbind(gt1,gt2) %>% mutate(gen=0)
gt1 = tibble(sam=4,i=1:nsite, a=rep(0,nsite), b=rep(1,nsite))
gt2 = tibble(sam=6,i=1:nsite, a=rep(1,nsite), b=rep(1,nsite))
tg1 = rbind(gt1,gt2) %>% mutate(gen=1)
gt1 = tibble(sam=2,i=1:nsite, a=c(rep(0,nsite/2),rep(1,nsite/2)), b=rep(1,nsite))
gt2 = tibble(sam=5,i=1:nsite, a=c(rep(0,nsite/4),rep(1,nsite/2),rep(0,nsite/4)), b=rep(1,nsite))
gt3 = tibble(sam=8,i=1:nsite, a=c(rep(1,nsite/2),rep(0,nsite/2)), b=rep(1,nsite))
tg2 = rbind(gt1,gt2,gt3) %>% mutate(gen=2)
tg3 = tg2 %>% select(-gen) %>%
    group_by(sam) %>% nest() %>% rename(gt=data) %>% ungroup() %>%
    crossing(rep = 1:3) %>%
    mutate(gtn = map(gt, pass_gt_1_bc)) %>%
    arrange(sam) %>% mutate(sam = 1:n(), gen=3) %>%
    select(gen, sam, gtn) %>% unnest(gtn)
#
tg4 = tg3 %>% select(-gen) %>%
    group_by(sam) %>% nest() %>% rename(gt=data) %>% ungroup() %>%
    mutate(gtn = map(gt, pass_gt_n, gen=5, co.rate=co.rate)) %>%
    select(sam, gtn) %>% unnest(gtn) %>%
    mutate(gen=3+gen)
tg3 %>%
    mutate(ab=glue("{a}{b}")) %>%
    select(sam, i,gen,ab) %>% spread(sam,ab)

#{{{ plot
tp = rbind(tg0,tg1,tg2,tg4) %>%# filter(gen>=1) %>%
    mutate(pan = ifelse(gen <= 1, glue("F{gen}"),
                        ifelse(gen <= 3, glue("BC{gen-1}F1"),
                        glue("BC2F{gen-2}")))) %>%
    mutate(pan = as_factor(pan)) %>%
    mutate(sam = ifelse(sam>3, ifelse(sam>6, sam+1, sam+.5), sam)) %>%
    mutate(a=as.factor(a), b=as.factor(b))
sams = unique(tp$sam)
tpa1 = tp %>% distinct(gen,pan) %>% filter(gen >= 3) %>%
    crossing(x = c(3.75,7.25))
tpa2 = tp %>% distinct(gen,pan) %>% filter(gen <= 1) %>%
    crossing(x = c(5.5))
p = ggplot(tp) +
    geom_rect(aes(xmin=sam-.3,xmax=sam-.1,ymin=i-.5,ymax=i+.5,fill=a),size=0) +
    geom_rect(aes(xmin=sam+.1,xmax=sam+.3,ymin=i-.5,ymax=i+.5,fill=b),size=0) +
    geom_text(data=tpa1, aes(x=x,y=nsite/2,label='...'),size=3) +
    geom_text(data=tpa2, aes(x=x,y=nsite/2,label='X'),size=3) +
    facet_wrap(~pan, strip.position='left',ncol=1) +
    scale_x_continuous(name="Rice EpiRIL Population (500)", breaks=sams,expand=expansion(mult=c(.02,.02))) +
    scale_y_continuous(expand=expansion(mult=c(.05,.05))) +
    scale_fill_simpsons() +
    otheme(legend.pos='none',xtitle=T,panel.border=F) +
    theme(strip.text.y.left = element_text(angle=0))
#}}}
fo = glue("{dirw}/01.pdf")
ggexport(p, filename=fo, width=6, height=6)
#}}}


