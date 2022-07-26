{
  tfn <- 'E:/a/CUG/data/Geopotential/R/hws22-5-21/'
  source('E:/a/CUG/R/librarys&funcs.R')  # source(paste0(tfn, '???ʰ?.R'))
  ChinaMap_.25 <- readRDS(paste0(tfn, 'ChinaMap2_.25.RDS'))
  # ?????й??????͸??㣺 C:/Users/xioli/Documents/Untitled5.R
  # ChinaMap_.25 is calculate by 'F:/xiol/20220413_7features/makeChinaMap.R'
  ## Recalculate the 'Sgrid' with these grid points of ChinaMap_.25??ChinaMap2_.25
  # ChinaMap_.25 <- readRDS(paste0(tfn, 'ChinaMap_.25.RDS'))
  # colnames(ChinaMap_.25)[1: 2] <- c('lon', 'lat')
  # ChinaMap2_.25 <- DOgridArea(ChinaMap_.25['lon', 'lat'])
  # ChinaMap2_.25 %<>% cbind(., ChinaMap_.25$area_p)
  # colnames(ChinaMap2_.25) <- c('lonxy', 'latxy', 'Sgrid', 'area_p')
  # 
  # ggplot(ChinaMap_.25) + 
  #   geom_point(aes(lonxy, latxy, color=area_p)) + 
  #   scale_color_gradient2(low = 'white', high='red') + 
  #   geom_polygon(data=mydat2, aes(long, lat, group=group), color = 'grey', # black
  #                show.legend = F, size=0.001, fill = NA)
  # 
  # ChinaMap2_.25$area <- ChinaMap2_.25$Sgrid*ChinaMap2_.25$area_p
  # # saveRDS(ChinaMap2_.25, paste0(tfn, 'ChinaMap2_.25.RDS'))
  
}
## ͳ?ƺ???  Statistical time features
{
  Scalar_Statistics <- function(Hwsm) {  # ID time lon lat lonxy latxy
    Output <- data.table()
    # duation  length  speed  u  v  ??λ?? ??ת??
    if (sum(c('ID', 'time', 'lon', 
              'lat') %in% colnames(Hwsm)) != 4) {  # , 'lonxy', 'latxy'  6
      message("Input frame need 'ID', 'time', 'lon', 'lat'")
    } else {
      # duation
      Output <- dplyr::count(Hwsm, ID, name = 'duation')
      # # st_length(line_st) # Բ  Mydistance  ????Բ 
      # geoDist(lat1, lon1, lat2, lon2, NAOK = TRUE, DUP = TRUE) ???? ??Mydistance һ??   geoXY?ľ?????geoDist????
      Output_L <- ddply(Hwsm, c('ID'), summarise, 
                        length = Mydistance(data.table(time = time, lon = lon, lat = lat)) / 1000 )
      Output <- merge(Output, Output_L)
      Output$length[which(is.na(Output$length))] = 0
      # speed
      Output$speed <- Output$length / Output$duation
      if (length(which(Output$duation < 3)) > 0) {
        # ??ת??
        Output_mrogle <- data.table(
          ID = unique(Output$ID[which(Output$duation < 3)]), mrogle = 0)
        # ?ۼ???ת rotation
        Output_rotation <- data.table(
          ID = unique(Output$ID[which(Output$duation < 3)]), rotation = 0)
        # v u ??λ??
        Output_uv <- data.table(
          ID = unique(Output$ID[which(Output$duation < 2)]), v = 0, u = 0, s_uv = 0,
          azimuth = 0)
      } else {
        Output_mrogle <- data.table(ID = c(), mrogle = c())
        Output_rotation <- data.table(ID = c(), rotation = c())
        Output_uv <- data.table(ID = c(), v = c(), u = c(), s_uv = c(), azimuth = c())
      }
      # ??Ϊ?漰??ת?ǵ?ƽ???????Զ?һ??ת?ɴ???????   geoXY ??lat0γȦ????lonX????
      XY <- geoXY(Hwsm$lat, Hwsm$lon, unit = 1000, lon0 = 108.9236, lat0 = 34.54083) # 
      Hwsm$lonX <- XY[, 1] # plot(Hwsm$lon, Hwsm$lat, asp = 1)
      Hwsm$latY <- XY[, 2] # plot(Hwsm$lonX, Hwsm$latY, asp = 1)
      for (id in unique(Output$ID[which(Output$duation >= 2)])) { # unique(Output$ID[which(Output$duation == 10)])
        points <- Hwsm[which(Hwsm$ID == id), ]
        temp_d <- Output$duation[which(Output$ID == id)]
        points2 <- points[which(points$time %in% range(points$time)), ]
        # # ??ת??  ?Ƕȡ?u v??geoXY????????????ϵ????  ??С???ÿ???ߵ???ת
        # # ! # ????ת??ֹ??  v ???ؾ??ߵ??ٶ?  u????ת???????Ͻ?Ϊԭ?㡢???㰴????չ??ƽ???Ľ?
        # XY <- geoXY(points2$lat, points2$lon, unit = 1) # ת???ѿ?????Ϊ????
        # points2$lonX <- XY[, 1]
        # points2$latY <- XY[, 2] # ??֤??Mydistance(points2[c('time', 'lon', 'lat')])
        p1 <- points2[which(points2$time == min(points2$time)), c('lonX', 'latY')]
        p2 <- points2[which(points2$time == max(points2$time)), c('lonX', 'latY')]
        temp <- data.table(
          ID = id, # v = Mydistance(points) / Output$duation[which(Output$ID == i)]
          u = (p2$lonX - p1$lonX) / temp_d, v = (p2$latY - p1$latY) / temp_d, 
          azimuth = angle3(c(p2$lonX - p1$lonX, p2$latY - p1$latY))
          # u = (p2$lonX[1] - p1$lonX[1]) / temp_d, v = (p2$latY[2] - p1$latY[2]) / temp_d, 
          # azimuth = angle3(c(p2$lonX[1] - p1$lonX[1], p2$latY[2] - p1$latY[2]))# ??λ??
        )
        # s_uv ????ƽ???ϵ??ٶ?
        temp$s_uv <- sqrt(temp$u^2 + temp$v^2) # / temp_d # geoDist(lat1, lon1, lat2, lon2) 
        Output_uv <- rbind(Output_uv, temp)
        # ????3???? ???????? ??ת??
        if (nrow(Hwsm[which(Hwsm$ID == id ), ]) >= 3) {
          temp <- Hwsm[which(Hwsm$ID == id ), ]
          n <- (nrow(temp) - 1)
          ans <- 0; ans2 <- 0
          for (i in c(2: n)){
            ans <- ans +  angle2(
              c(temp$lonX[i] - temp$lonX[i-1], temp$latY[i] - temp$latY[i-1]),
              c(temp$lonX[i+1] - temp$lonX[i], temp$latY[i+1] - temp$latY[i]))
            ans2 <- ans2 +  abs(angle2(
              c(temp$lonX[i] - temp$lonX[i-1], temp$latY[i] - temp$latY[i-1]),
              c(temp$lonX[i+1] - temp$lonX[i], temp$latY[i+1] - temp$latY[i])))
          }
          Output_mrogle <- rbind(Output_mrogle, data.table(
            ID = id, mrogle = ans/(n-1)
          ))
          Output_rotation <- rbind(Output_rotation, data.table(
            ID = id, rotation = ans2))
        }
      }
      Output <- merge(Output, Output_mrogle)
      Output <- merge(Output, Output_rotation)
      Output <- merge(Output, Output_uv)
      ## λ?ƾ??? displacement
      ttemp <- data.frame()
      for (i in unique(Hwsm$ID)) {
        temp <- Hwsm[which(Hwsm$ID == i), ]
        a <- Mydistance(temp[c(1, nrow(temp)), c('lon', 'lat', 'time')]) / 1000
        ttemp <- rbind(ttemp, data.frame(ID = i, displacement = a))
      }
      Output <- merge(Output, ttemp)
      as.data.table(Output)
    }
  }
}

#####
## ?㵽·??  
#####
mt = 'mov'; dt = 'max'; pn = .99
hws0 <- readRDS(paste0(tfn, 'hwHIID_', mt, dt, as.numeric(pn)*100, '%.RDS'))
hws0$lonxy <- hws0$lon # LLon[(hws0$lon - 60)%/%q + 1]
hws0$latxy <- hws0$lat  # LLat[(hws0$lat - 15)%/%q + 1]
hws0 <- merge(hws0, ChinaMap_.25[c('lonxy', 'latxy', 'area')],
              by = intersect(colnames(ChinaMap_.25), colnames(hws0)))
hws0 <- ddply(hws0, c('ID', 'time'), summarise, S = sum(area), # ????ÿ1??km2 HI
              Clon = sum(Percentile(HI*area)/sum(Percentile(HI*area))*lon), # ????ռ??(???????ߵĵ㼯??)
              Clat = sum(Percentile(HI*area)/sum(Percentile(HI*area))*lat))

hws <- hws0
ID_dua <- dplyr::count(hws, ID)
ID_dua <- ID_dua[which(ID_dua$n >= 3), ]
hws <- hws[which(hws$ID %in% ID_dua$ID), ]
colnames(hws)[4: 5] <- c('lon', 'lat')

# saveRDS(hws, paste(tfn, 'hws__', mt, dt, pn*100, '%.RDS', sep = ''))




#####
## ·?????ݵļ?????????ʱ?????У?Scalar_Statistics(hws)  # ID time lon lat lonxy latxy
#####

##  ?۲?????
{# ɸѡ??????
  #### ɸѡ??????
  hws <- readRDS(paste(tfn, 'hws__', mt, dt, pn*100, '%.RDS', sep = ''))
  hws$year <- year(hws$time)
  ## ??????   1. duation   2. area (S)   3. length
  ID_dua <- dplyr::count(hws, ID, name = 'duation')
  ID_dua$year <- as.numeric(str_sub(ID_dua$ID, 1, 4))
  year_max_D <- ddply(ID_dua, 'year', summarise,
                      duation = max(duation))
  IDs_maxD <- merge(year_max_D, ID_dua)
  if (nrow(IDs_maxD) > 56) {
    ID_S <- ddply(hws[which(hws$ID %in% IDs_maxD$ID), ], 'ID', summarise, S = sum(S))
    ID_S$year <- as.numeric(str_sub(ID_S$ID, 1, 4))
    year_max_S <- ddply(ID_S, 'year', summarise,
                        S = max(S))
    year_max_S <- merge(year_max_S, ID_S)
    # sum(year_max_S$ID %in% IDs_maxD$ID)
    IDs_maxD <- year_max_S
  }
  if (nrow(IDs_maxD) > 56) {
    ID_L
    # break
  }
  hws <- hws[which(hws$ID %in% IDs_maxD$ID), ]
  XY <- geoXY(hws$lat, hws$lon, unit = 1000, lon0 = 108.9236, lat0 = 34.54083) # km
  hws$X <- XY[, 1] # plot(hws1$lonxy, hws1$latxy, asp = 1)
  hws$Y <- XY[, 2]
}
# hws <- readRDS(paste(tfn, 'hws__', mt, dt, pn*100, '%.RDS', sep = ''))
tryCatch({
  ID_fea <- Scalar_Statistics(hws)  # ID time lon lat lonxy latxy
}, error=function(e){
  cat("ERROR :",conditionMessage(e),
      paste('HI', mt,pn, dt, sep = ' '), "\n")})
# saveRDS(ID_fea, paste0(tfn, 'ID_fea__', mt, dt, pn*100, '%.RDS'))

## ģʽ????
dirname <- paste0(tfn, 'models_hws_selected/')
# fname <- dir(dirname, ".RDS")
modelsfn90 <- dir(dirname, "*90%.RDS")
modelsfn95 <- dir(dirname, "*95%.RDS")
modelsfn99 <- dir(dirname, "*-99%.RDS")
fname <- list('90%'= modelsfn90, '95%'= modelsfn95, '99%'= modelsfn99)

# save_fn <- paste0(tfn, 'models_feas_hws_selected/')
modelsd <- data.frame()
for (modelsfn_chr in names(fname)) {  # fname[1: length(fname)]
  modelsfn <- fname[modelsfn_chr]
  for (fn in modelsfn[[1]]) {
    hws <- readRDS(paste(dirname, fn, sep = ''))
    tryCatch({
      ID_fea <- Scalar_Statistics(hws)  # ID time lon lat lonxy latxy
    }, error=function(e){
      cat("ERROR :",conditionMessage(e),
          paste('HI', mt,pn, dt, sep = ' '), "\n")})
    # saveRDS(ID_fea,
    #         paste(save_fn, 'feas__', strsplit(fn, '__')[[1]][2], sep = ''))
    a <- strsplit(fn, '__')[[1]][2]
    ID_fea$model <- strsplit(a, '%.')[[1]][1]
    ID_fea$year <- str_sub(ID_fea$ID, 1, 4)
    ID_fea$per <- modelsfn_chr
    modelsd <- rbind(modelsd, ID_fea)
  }
}
# saveRDS(modelsd, paste0(save_fn, 'modelsd.RDS'))


#####
## ??ͼ????
#####
pn = 99
modelsd <- readRDS(paste0(save_fn, 'modelsd.RDS'))
modelsd <- modelsd[which(modelsd$per == paste0(pn, '%')), ]
ID_fea <- readRDS(paste0(tfn, 'ID_fea__', mt, dt, pn, '%.RDS'))
low_avg_high <- data.frame()
feanames <- c('Duation', 'Length', 'Speed', 'U', 'V', 'Speed_UV', 
              'Mrogle', 'Azimuth', 'Displacement') # , 'Rotation'
feanames2 <- c('duation', 'length', 'speed', 'u', 'v', 's_uv', 
              'mrogle', 'azimuth', 'displacement') # , 'rotation'
for (i in c(1: length(feanames2))) {
  colnames(modelsd)[which(colnames(modelsd) == feanames2[i])] <- feanames[i]
}
for (i in c(1: length(feanames2))) {
  colnames(ID_fea)[which(colnames(ID_fea) == feanames2[i])] <- feanames[i]
}
ID_fea$year <- as.numeric(str_sub(ID_fea$ID, 1, 4))

Mdata <- as.data.frame(modelsd); ID_fea <- as.data.frame(ID_fea)
# Mdata$Rotation <- Mdata$Rotation / 10^3
# range(Mdata$Rotation)
for (i in feanames) {   # i = feanames[1]
  temp <- Mdata[, colnames(Mdata) %in% c('year', 'per', i)]
  colnames(temp)[which(colnames(temp) == i)] = 'value'
  temp <- ddply(temp, c('year', 'per'), summarise, 
                low = quantile(value, probs = 0.1, na.rm = T), 
                high = quantile(value, probs = 0.9, na.rm = T),
                avg = mean(value)
  )
  temp$data <- i
  temp <- rbind(temp, data.frame(
    year = rep(2015: 2016, 3), per = rep(paste0(pn, '%'), 3), 
    low = NA, high = NA, avg = NA, data = i
  ))
  temp2 <- ID_fea[, colnames(ID_fea) %in% c('year', i)]
  colnames(temp2)[which(colnames(temp2) == i)] = 'observe'
  low_avg_high <- rbind(low_avg_high, merge(temp, temp2))
  # low_avg_high <- rbind(low_avg_high, temp)
}
low_avg_high$year <- as.numeric(low_avg_high$year)

draw_Fig1 <- function(fea, data, labeln) {
  data <- data[which(data$data == fea), ]
  t_ylab <- ''
  if (fea == 'Duation') {t_ylab <- 'days'
  } else if (fea %in% c('Length', 'Displacement')) {
    # t_ylab <- "10<sup>3</sup>km"
    t_ylab <- bquote(10^3*'km')
    # data[, c('observe', 'low', 'high', 'avg')] <-
    #   data[, c('observe', 'low', 'high', 'avg')] / 10^3
  }else if (fea %in% c('Speed', 'U', 'V', 'Speed_UV'))
  {t_ylab <- 'km/day'}else if (fea %in% c('Mrogle', 'Azimuth'))
  {t_ylab <- '??'} else if (fea == 'Rotation') {
    # t_ylab <- "10<sup>3</sup> ??"
    t_ylab <- bquote(10^3*' ??')
    # data[, c('observe', 'low', 'high', 'avg')] <-
    #   data[, c('observe', 'low', 'high', 'avg')] / 10^3
  }
  message(fea)
  # t_ylab <- paste(fea, t_ylab, sep = ' ')
  mylabs <- ''
  if (fea == 'Duation') {
    # mylabs <- '(a) Duation  days'
    mylabs <- '(a) Duation'
  } else if (fea == 'Length') {
    # mylabs <- "(b) Length  10<sup>3</sup>km"
    # mylabs <- expression(bold(paste('(b) Length  ', bold(10)^bold(3), 'km', sep = '')))
    mylabs <- '(b) Length  '
    data[, c('observe', 'low', 'high', 'avg')] <-
      data[, c('observe', 'low', 'high', 'avg')] / 10^3
  }else if (fea == 'Speed') {
    # mylabs <- '(c) Speed  km/day'
    mylabs <- '(c) Speed'
  } else if (fea == 'U') {
    # mylabs <- '(d) U  km/day'
    mylabs <- '(d) U'
  } else if (fea == 'V') {
    # mylabs <- '(e) V  km/day'
    mylabs <- '(e) V'
  } else if (fea == 'Speed_UV') {
    # mylabs <- '(f) Speed_UV  km/day'
    mylabs <- '(f) Speed_UV'
  } else if (fea == 'Mrogle') {
    # mylabs <- '(g) Mrogle  ??'
    mylabs <- '(g) Mrogle'
  } else if (fea == 'Displacement') {
    # # mylabs <- '(h) Rotation  10<sup>3</sup> ??'
    # mylabs <- expression(bold(paste('(h) Rotation  ', bold(10)^bold(3), ' ??', sep = '')))
    mylabs <- '(h) Displacement'
    data[, c('observe', 'low', 'high', 'avg')] <-
      data[, c('observe', 'low', 'high', 'avg')] / 10^3
    # # mylabs <- bquote(bold('(h) Rotation  '*bold(10)^bold(3)*' ??'))
    # # mylabs <- paste('(h) Rotation  ', expression(10^3), '??', sep = '')
  } else if (fea == 'Azimuth') {
    # mylabs <- '(i) Azimuth  ??'
    mylabs <- '(i) Azimuth'
  }
  
  dMAX <- max(data$observe, data$high, na.rm = T)
  dMIN <- min(data$observe, data$low, na.rm = T)
  
  
  # Mlm <- lm(formula = avg ~ year, data = data)
  # Ma = summary(Mlm)
  # Mp_value <- paste('p_value = ', round(Ma$coefficients[2, 4], 3), sep = '')
  r <- mkTrend(data$avg)
  ML <- data.frame(
    x = c(1961: 2014), y = r['slp']*c(1: 54) + r['intercept']
  )
  Mp_value <- round(slope_mk(data$avg)[2], 3)
  if (Mp_value <= 0.001) {
    Mp_value <- 'p-value < 0.001'
  } else {
    Mp_value <- str_pad(as.character(Mp_value), 5, side = "right", "0")
    Mp_value <- paste('p-value = ', Mp_value, sep = '')
  }
  Mslope <- round(slope_mk(data$avg)[1], 3)
  Mslope <- str_pad(as.character(Mslope), 5, side = "right", "0")
  # MR <- round(Ma$r.squared, 3)
  # Olm <- lm(formula = observe ~ year, data = data)
  # Oa = summary(Olm)
  # Op_value <- paste('p_value = ', round(Oa$coefficients[2, 4], 3), sep = '')
  r <- mkTrend(data$observe)
  OL <- data.frame(
    x = c(1961: 2016), y = r['slp']*c(1: 56) + r['intercept']
  )
  Op_value <- round(slope_mk(data$observe)[2], 3)
  if (Op_value <= 0.001) {
    Op_value <- 'p-value < 0.001'
  } else {
    Op_value <- str_pad(as.character(Op_value), 5, side = "right", "0")
    Op_value <- paste('p-value = ', Op_value, sep = '')
  }
  Oslope <- round(slope_mk(data$observe)[1], 3)
  Oslope <- str_pad(as.character(Oslope), 5, side = "right", "0")
  # OR <- round(Oa$r.squared, 3)
  # y??��??????
  ylr <- 140/100 # 140/100
  p1 <- ggplot(data = data, aes(year, avg)) + # duation  length  Mspeed  Mrogle
    geom_ribbon(aes(ymin = low, ymax = high), fill = 'grey90') + # grey70
    xlab('') + ylab('') + # xlab('Year') + ylab(t_ylab) + 
    # labs(x = 'Year', y = t_ylab) + 
    scale_x_continuous(breaks = c(seq(1961, 2001, 10), 2016), # limits = c(1961, 2016), 
                       labels = c(seq(1961, 2001, 10), 2016)) + # expand = c(0, 0),     
    scale_y_continuous(expand = c(0.1, 0.1)) +
    geom_vline(aes(xintercept=1991), colour="grey", linetype="dashed", size = 1) +  # #BB0000
    # annotate('text', x = 1961, y = ylr*dMAX+(1-ylr)*dMIN,
    #          label = paste(labeln, ' ',  fea, sep = ''),
    #          family = 'serif', size = 8, color = 'black', hjust = 0) +
    # ggtitle(paste(labeln, ' ',  fea, '  ', t_ylab, sep = '')) + 
    labs(title = paste(labeln, fea), subtitle = t_ylab) +  # mylabs
    # library(gridExtra)
    # library(grid)
    # grid.rect(x = unit(0, 'npc'), y = unit(1, 'npc'), 
    #           width = unit(0.06,"npc"), height = unit(0.16,"npc"),)
    
    
    
    
    ########### Models
    geom_line(aes(x = year, y = avg), linetype = 'solid') +   # linetype = 'dashed'
    geom_line(data = ML, mapping = aes(x, y), color = 'black', size = 1.2) +
    annotate('text', x = 1961, y = ylr*dMAX+(1-ylr)*dMIN, # y = (ylr-0.12)*dMAX+(1-ylr)*dMIN,
             label = paste('Models: slope = ', Mslope, '  ', Mp_value, 
                           '  ', sep = ''),
             family = 'serif', size = 7, color = 'black', hjust = 0, vjust = 0.6 # vjust = 2.5
    ) +
    
    # annotate('text', x = 1998, y = (ylr-0.12)*dMAX+(1-ylr)*dMIN,
    #          label = expression(R^2),
    #          family = 'serif', size = 6, color = 'black', hjust = 0, vjust = 0.75
    # ) + 
    # annotate('text', x = 2000.5, y = (ylr-0.12)*dMAX+(1-ylr)*dMIN,
    #          label = paste(' = ', MR, sep = ''),
    #          family = 'serif', size = 6, color = 'black', hjust = 0, vjust = 1
    # ) +
    ########## Observe
    geom_line(aes(x = year, y = observe), linetype = 'solid', color = '#268054') + 
    geom_line(data = OL, mapping = aes(x, y), color = '#268054', size = 1.2) + 
    annotate('text', x = 1961, y = ylr*dMAX+(1-ylr)*dMIN, # y = (ylr-0.24)*dMAX+(1-ylr)*dMIN,
             label = paste('OBS: slope = ', Oslope, '  ', Op_value, 
                           '  ', sep = ''),
             family = 'serif', size = 7, color = '#268054', hjust = 0, vjust = 2.6 # vjust = 4.2
    ) +
    # annotate('text', x = 1998, y = (ylr-0.24)*dMAX+(1-ylr)*dMIN,
    #          label = expression(R^2),
    #          family = 'serif', size = 6, color = 'blue', hjust = 0, vjust = 0.75
    # ) + 
    # annotate('text', x = 2000.5, y = (ylr-0.24)*dMAX+(1-ylr)*dMIN,
    #          label = paste(' = ', OR, sep = ''),
    #          family = 'serif', size = 6, color = 'blue', hjust = 0, vjust = 1
    # ) +
    
    theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0, vjust = -5.5, family = 'serif', size = 26, face = 'bold'),
      plot.subtitle = element_text(hjust = 1, vjust = 0, family = 'serif', size = 26),
      text = element_text(family = 'serif', size = 24), # , face="bold"
      aspect.ratio = 0.58, #1??????
      panel.background = element_blank(),
      panel.grid = element_blank(),   #????
      legend.position='none',    
      strip.text = element_blank(),  
      panel.spacing.x = unit(0, "cm"), 
      panel.spacing.y = unit(0, "cm"), # 0.8
      plot.margin=unit(c(5,0,0,0),"mm"),  # t, r, b, l
      axis.text.y=element_text(angle=90, hjust=0.5),  # , hjust=1
      axis.title.y = element_markdown()
    )
  ## ????0??
  if (between(0, dMIN, dMAX)) {
    p1 <- p1 + 
      geom_hline(aes(yintercept=0), colour="grey", linetype="solid") # , size = 1.5  #dashed solid #BB0000
  }
  # ??λ?? ?? 270???Ķ??Ϸ???
  if (fea == 'Azimuth') {
    p1 <- p1 +   # colour="#8a8a8a"
      geom_hline(aes(yintercept=270), colour="grey", linetype="solid") +   # #BB0000
      scale_y_continuous(expand = c(0.1, 0.1), breaks = c(0, 90, 180, 270, 360))
  }
  if (fea == 'Rotation') {
    p1 <- p1 + 
      xlab('Year') 
  }
  p1
}

for (pn in c('99')) { # '90', '95', 
  # pn = '99'
  data <- low_avg_high[which(low_avg_high$per == paste(pn, '%', sep = '')), ]
  p1 <- draw_Fig1(feanames[1], data, '(a)')
  p2 <- draw_Fig1(feanames[2], data, '(b)')
  p3 <- draw_Fig1(feanames[3], data, '(c)')
  p4 <- draw_Fig1(feanames[4], data, '(d)')
  p5 <- draw_Fig1(feanames[5], data, '(r)')
  p6 <- draw_Fig1(feanames[6], data, '(f)')
  p7 <- draw_Fig1(feanames[7], data, '(g)')
  p8 <- draw_Fig1(feanames[8], data, '(h)')
  p9 <- draw_Fig1(feanames[9], data, '(i)')
  
  
  p0 <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                  ncol = 3, nrow = 3, align = "hv")
  # p0 <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
  #                 ncol = 4, nrow = 2, align = "hv")
  p0 <- p0 + 
    # labs(tag = paste(mt, ' ', dt, ' ', pn*100, '%', sep = '')) + 
    # labs(caption = 'caption') + 
    theme(
      plot.margin=unit(c(20,20,20,20),"mm"),  # t, r, b, l
      # plot.margin=unit(t = 20, l = 20, unit = "mm"), 
    ) # paste(mt, ' ', dt, ' ', pn*100, '%', sep = '')
  ggsave(p0, file =
           paste0(tfn, 'Fig1-22-714/Fig1__', pn, '.pdf', sep = ''),
         width=22, height=16)
}









