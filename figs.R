library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)

source('~/Dropbox/projetos/voronoys/jdssv/utils/theme_maps.R')

# read data
vor_mg <- readRDS('~/Dropbox/projetos/voronoys/jdssv/data/voronois_2018.RDS')

##--- aux data - naive method ----

mg_ibge <- readRDS('~/Dropbox/projetos/voronoys/jdssv/data/IBGE/MG_ibge_shp.RDS')
mg_tse <- readRDS('~/Dropbox/projetos/voronoys/jdssv/data/TSE/2018/sections/MG_2018_georref_votes.RDS')
cities <- brazilmaps::get_brmap(geo = 'City', geo.filter = list(State = 31),
                              class = 'sf') %>%
  mutate(City = substr(City, 1, 6))

mun_tse <- mg_tse %>%
  group_by(cd_ibge) %>%
  summarise_at(.vars = vars(fernando_haddad:jair_messias_bolsonaro), sum)

cities <- left_join(cities, mun_tse, c("City" = "cd_ibge"))

cities <- cities %>%
  mutate(color = ifelse(fernando_haddad > jair_messias_bolsonaro, "#C0122D", "#1D6434"))

# png("~/Dropbox/projetos/erum2020/erum20/img/map_mg_traditional.png", res = 300)
br  <- brazilmaps::get_brmap(geo = "Brazil", class = "sf")
uf  <- brazilmaps::get_brmap(geo = "State", class = "sf")
mun <- brazilmaps::get_brmap(geo = "City", class = "sf")
mun_pts <- st_centroid(mun)

png(filename = "~/Dropbox/projetos/erum2020/erum20/img/cover-map.png", width = 19.2, height = 10.8, units =  "in", res = 100)
par(mar = rep(0, 4), bg = '#1a1917')
# par(mar = rep(0, 4), bg = 'white')
plot(br, border = "gray60", lwd = .4)
plot(mun_pts$geometry, col = scales::alpha('#e74a2f', .3), pch = 19, cex = .2, add = T)
dev.off()

png(filename = "~/Dropbox/projetos/erum2020/erum20/img/cover-map-white.png", width = 19.2, height = 10.8, units =  "in", res = 100)
# par(mar = rep(0, 4), bg = '#1a1917')
par(mar = rep(0, 4), bg = 'white')
plot(br, border = "gray40", lwd = .4)
plot(mun_pts$geometry, col = scales::alpha('#cbd20a', .4), pch = 19, cex = .3, add = T)
dev.off()


png(filename = "~/Dropbox/projetos/erum2020/erum20/img/cover-mg-map.png", width = 19.2, height = 10.8, units =  "in", res = 100)
par(mar = rep(0, 4), bg = '#1a1917')
plot(subset(uf, State == 31)$geometry, border = "gray60", lwd = .4)
plot(subset(mun_pts, State == 31)$geometry, col = scales::alpha('#61acf0', .7), pch = 19, cex = .5, add = T)
dev.off()


bh_ibge <- readRDS("data/ibge/MG_ibge_shp.RDS") %>% 
  filter(nome_do_municipio == "BELO HORIZONTE")

# BELO HOZIZONTE city code
bh_code <- bh_ibge %>% 
  pull(cod_municipio) %>% 
  unique()

bh_code <- substr(bh_code, start = 1, stop = nchar(bh_code) - 1)
bh_tse <- readRDS("data/TSE/2018/sections/MG_2018_georref_votes.RDS") %>% 
  filter(cd_ibge == bh_code)

bh_vor <- readRDS("~/Dropbox/projetos/voronoys/jdssv/data/voronois_2018.RDS") 
bh_vor2 <- bh_vor
bh_vor <- bh_vor %>% 
  left_join(y = {
    bh_tse %>% 
      select(zon_sec_char, 
             cd_ibge)
  }, by = "zon_sec_char") %>% 
  filter(cd_ibge.x == bh_code) %>% 
  select(-cd_ibge.x)

# crs
crs_vor <- st_crs(bh_vor)

bh_tse <- sf::st_as_sf(bh_tse,
                       coords = c("longitude", "latitude"), 
                       crs = 4326) %>% 
  st_transform(crs = crs_vor)

ggplot() +
  geom_sf(data  = bh_ibge,
          color = "white",
          size  = .05,
          fill  = "red",
          alpha = .6) +
  geom_sf(data  = bh_tse,
          color = "black",
          size  = .6) +
  theme_void() +
  labs(title = "Census tracts and voting locations",
       subtitle = "Belo Horizonte - Brazil")

png(filename = "~/Dropbox/projetos/erum2020/erum20/img/secao-bh.png", width = 19.2, height = 10.8, units =  "in", res = 100)
par(mar = rep(0.01, 4), bg = 'transparent')
plot(bh_border, lwd = 2)
plot(bh_sec, add = T, pch = 19, col = "indianred", cex = 1)
# box()
dev.off()

##--- percent of pop ----
bh_ibge <- bh_ibge %>% 
  st_transform(crs = crs_vor)

bh_tse_aux <- bh_tse %>% 
  transmute(used_naive = "Yes")

bh_ibge_aux <- bh_ibge %>% 
  st_join(y = bh_tse_aux) %>% 
  mutate(used_naive = ifelse(is.na(used_naive), "No", used_naive)) %>% 
  select(cod_setor, pop, renda_media, dens_domiciliar, 
         used_naive) %>% 
  distinct(cod_setor, .keep_all = T)

prop_tracts_naive <- nrow(bh_ibge_aux %>% filter(used_naive == "Yes"))/nrow(bh_ibge_aux)
pop_prop_naive <- sum(bh_ibge_aux[bh_ibge_aux$used_naive == "Yes", ] %>% pull(pop))/sum(bh_ibge_aux$pop)

# computing number of voters
voters_bh <- sum(colSums(bh_tse[c("fernando_haddad", "jair_messias_bolsonaro", 
                                  "voto_branco", "voto_nulo")] %>% st_set_geometry(NULL)))
##--- variables estimate ----

renda_naive <- weighted.mean(x = bh_ibge_aux %>% filter(used_naive == "Yes") %>% pull(renda_media), 
                             w = bh_ibge_aux %>% filter(used_naive == "Yes") %>% pull(pop))
renda_true <- weighted.mean(x = bh_ibge$renda_media, 
                            w = bh_ibge$pop)
renda_vor <- weighted.mean(x = bh_vor$renda_media, 
                           w = bh_vor$pop)

df_renda <- data.frame(Variable = "Avg. Income", 
                       Census   = renda_true,
                       Naive    = renda_naive,
                       Voronoi  = renda_vor)

##--- plotting same scale ----

jet_col <- c("#00007F", "blue", "#007FFF", "cyan",
             "#7FFF7F", "yellow", "#FF7F00", "red",
             "#7F0000")

# Lab.palette <- colorRampPalette(c("red", "orange", "blue"),
#                                 space = "Lab")

bh_naive <- bh_tse %>% 
  select(geometry) %>% 
  st_join(
    y = {
      bh_ibge %>% 
        select(pop, renda_media)
    }, join = st_nearest_feature
  )

bh_naive <- bh_vor %>% 
  select(geometry) %>% 
  st_join(y = bh_naive)

bh_comparison <- bh_naive %>% 
  mutate(source = "naive") %>% 
  rbind(
    {
      bh_ibge %>% 
        select(pop, renda_media) %>% 
        mutate(source = "ibge") %>% 
        st_transform(st_crs(bh_vor))
    }
  ) %>% 
  rbind(
    {
      bh_vor %>% 
        select(pop, renda_media) %>% 
        mutate(source = "voronoi")
    }
  )

ggplot(data = bh_ibge_aux,
       aes(fill = used_naive)) +
  geom_sf(color = "black", 
          size = .05, alpha = .6) +
  scale_fill_manual(values = c("white", "red"), 
                    guide = guide_legend(title = "Used by naive?")) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.text = element_text(colour = "white"),
    legend.title = element_text(colour = "white"),
    legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
  )
ggsave("img/naive.png", bg = "transparent")

bh_comparison %>%
  filter(source == 'voronoi') %>%
ggplot() +
  geom_sf(color = "black", size  = .2) +
  geom_sf(data = bh_sec, pch = 19, cex = .2, col = "indianred") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.text = element_text(colour = "white"),
    legend.title = element_text(colour = "white"),
    legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
  )
ggsave("img/voronoi.png", bg = "transparent")


ll_sf %>% 
  filter(cd_ibge == cd_bh) %>% 
  ggplot(data = .) +
  geom_sf(data = bh_border, 
          color = 'black',
          fill  = 'white', 
          size  = .5*0.5714286) +
  geom_sf(aes(color = jair_messias_bolsonaro),
          pch  = 19,
          size = 0.5714286) +
  theme_void() +
  theme(legend.position = 'right',
        plot.title = element_text(hjust = .5, 
                                  face  = 'bold',
                                  size  = 11*0.5714286),
        legend.text = element_text(family = 'Helvetica', 
                                   size   = 8*0.5714286),
        text        = element_text(family = 'Helvetica', 
                                   size   = 8*0.5714286),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_color_viridis_c(label   = scales::percent,
                        breaks  = set_breaks,
                        guide   = guide_colorbar(title = '% Votes', 
                                                 ticks = F, raster = F, 
                                                 barheight = )) -> sects
sects
##-----------------
vor_mg %>% 
  filter(nm_municipio == 'BELO HORIZONTE') %>% 
  ggplot(data = .) +
  geom_sf(data = bh_border, 
          color = 'black',
          fill  = 'white', 
          size  = .5*0.5714286) +
  geom_sf(aes(fill = log(renda_media)),
          color = 'transparent',
          size  = .001) +
  theme_void() +
  scale_fill_viridis_c(
    label   = label_inc,
    breaks  = set_breaks,
    guide   = guide_colorbar(title = 'log(avg. income)', 
                             ticks = F, raster = F)
  ) +
  # ggtitle('(b)') +
  theme(legend.position = 'right',
        plot.title  = element_text(hjust = .5, 
                                   face  = 'bold',
                                   size  = 11*0.5714286),
        legend.text = element_text(family = 'Helvetica', 
                                   size   = 8*0.5714286),
        text        = element_text(family = 'Helvetica', 
                                   size   = 8*0.5714286),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) -> bh_inc_vor

fig_1 <- cowplot::plot_grid(bh_inc, bh_inc_vor, ncol = 2, align = 'h', 
                            rel_widths = c(1.05, 1)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))
fig_1
cowplot::save_plot(filename = 'img/Fig_1.png', plot = fig_1, 
                   # base_height = 16, base_asp = szs[1]/szs[2],
                   base_width = 3.149606, base_height = 1.716535, 
                   # type = 'cairo', # pointsize = 12*ggplot2:::.pt,
                   dpi = 300)

vor_mg %>% 
  filter(nm_municipio == 'BELO HORIZONTE') %>% 
  ggplot(data = .) +
  geom_sf(data = bh_border, 
          color = 'black',
          fill  = 'white', 
          size  = .5*0.5714286) +
  geom_sf(aes(fill = jair_messias_bolsonaro),
          color = 'transparent',
          size  = .001) +
  theme_void() +
  scale_fill_viridis_c(
    label   = scales::percent,
    # breaks  = set_breaks,
    guide   = guide_colorbar(title = '% votes', 
                             ticks = F, raster = F)
  ) +
  theme(legend.position = 'left',
        plot.title  = element_text(hjust = .5, 
                                   face  = 'bold',
                                   size  = 11*0.5714286),
        legend.text = element_text(family = 'Helvetica', 
                                   size   = 8*0.5714286),
        text        = element_text(family = 'Helvetica', 
                                   size   = 8*0.5714286),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) -> jair_vor

fig_2<- cowplot::plot_grid(jair_vor, bh_inc_vor, ncol = 2, align = 'h', 
                            rel_widths = c(1, 1.1)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))
fig_2

cowplot::save_plot(filename = 'img/Fig_2.png', plot = fig_2, 
                   # base_height = 16, base_asp = szs[1]/szs[2],
                   base_width = 3.149606, base_height = 1.716535, 
                   # type = 'cairo', # pointsize = 12*ggplot2:::.pt,
                   dpi = 300)

rm_bh <- c("Baldim", "Belo Horizonte", "Betim", "Brumadinho", "Caeté", "Capim Branco", "Confins", "Contagem", "Esmeraldas", "Florestal", "Ibirité", "Igarapé", "Itaguara", "Itatiaiuçu", "Jaboticatubas", "Nova União", "Juatuba", "Lagoa Santa", "Mário Campos", "Mateus Leme", "Matozinhos", "Nova Lima", "Pedro Leopoldo", "Raposos", "Ribeirão das Neves", "Rio Acima", "Rio Manso", "Sabará", "Santa Luzia", "São Joaquim de Bicas", "São José da Lapa", "Sarzedo", "Taquaraçu de Minas", "Vespasiano")

rm_bh <- mg_ibge %>%
  filter(nome_do_municipio %in% toupper(rm_bh))

scales::comma(sum(mg_ibge$pop, na.rm = T))
scales::comma(sum(rm_bh$pop, na.rm = T))

scales::comma(sum(mg_ibge$pop, na.rm = T))
scales::comma(sum(rm_bh$pop, na.rm = T))

sum(rm_bh$pop, na.rm = T)/sum(mg_ibge$pop, na.rm = T)

br <- brazilmaps::get_brmap(geo = "Brazil", class = "sf")
uf <- brazilmaps::get_brmap(geo = "State", class = "sf")
mun <- brazilmaps::get_brmap(geo = "City", class = "sf")


  