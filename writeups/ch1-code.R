## ----include = F--------------------------------------------------------------------
library(tidyverse)
library(RSQLite)
library(lme4)

con = dbConnect(SQLite(), '_data/department.db')
dbListTables(con)
results = dbReadTable(con, 'results')
users = dbReadTable(con, 'user')
userMatrix = dbReadTable(con, 'userMatrix')
dbDisconnect(con)

valid.users <- users %>% 
  mutate(subject = paste0(nickname, participantUnique)) %>% 
  filter(age != 'Under 19')


#Fill in correct values for incorrect 3d graph kits
# 1 result, need to fill in fileID, graphtype, and plot
results = results %>% 
  mutate(fileID = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', 1, fileID),
         graphtype = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', 'Type1', graphtype),
         plot = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', '3dPrint', plot))



load('_data/set85data.Rdata')
load('_data/kits.Rdata')


trueRatios = datasets %>% 
  mutate(ratio.df = map(data, function(x)(x[!is.na(x[,'IDchr']),4])),
         trueRatio = map(ratio.df, function(x)(x[1,'Height'] / x[2,'Height']))) %>% 
  unnest(trueRatio) %>% 
  filter(fileID != 15) %>% 
  #mutate(Height = Height * 100) %>% 
  select(fileID, Height)

res_all = results %>% 
  left_join(trueRatios, by = 'fileID') %>% 
  mutate(response = log2(abs(byHowMuch - Height*100) + 1/8),
         subject = paste0(nickname, participantUnique),
         ratioLabel = round(100*Height, 1)) %>% 
  filter(subject %in% valid.users$subject
         , as.Date.POSIXct(appStartTime) <= '2023-05-22'
         ) %>% 
  arrange(appStartTime)

valid.users <- valid.users %>% 
  filter(subject %in% res_all$subject)

res <- res_all %>% 
  filter(whichIsSmaller == 'Triangle (▲)'
         , as.Date.POSIXct(appStartTime) <= '2023-05-22'
         ) %>% 
  #Only verified results from before the start of SDSS 
  filter(!is.na(ratio))



## ----fig-bar-types------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-cap: "@clevelandGraphical1984 used two different types of grouped bar charts: comparisons between adjacent bars, and comparisons between separated bars. It is widely acknowledged that comparisons between separated bars (Type 3 comparisons, in Cleveland \\& McGill's terminology) are more difficult and error-prone."
#| fig-scap: "Adjacent versus separated grouped bar comparisons"
#| layout-ncol: 2
#| out-width: 90%
library(ggplot2)
library(ragg)
datasets$data[[1]] %>%
  ggplot(aes(x = Group, y = Height, group = GroupOrder)) + 
  geom_col(position = "dodge", fill = "white", color = "black") + 
  geom_point(aes(x = Group, y = 5, group = GroupOrder, shape = IDchr), size = 3, position = position_dodge(0.9)) + 
  guides(shape = "none") + 
  theme_bw() + 
  theme(axis.title = element_blank(), axis.text.x = element_blank()) + 
  ggtitle("Adjacent (Type 1)")


datasets$data[[1]] %>%
  ggplot(aes(x = factor(Order %%2), y = Height, group = GroupOrder)) + 
  geom_col(position = "dodge", fill = "white", color = "black") + 
  geom_point(aes(x = factor(Order %%2), y = 5, group = GroupOrder, shape = IDchr), size = 3, position = position_dodge(0.9)) +
  guides(shape = "none") + 
  theme_bw() + 
  theme(axis.title = element_blank(), axis.text.x = element_blank()) + 
  ggtitle("Separated (Type 3)")



## -----------------------------------------------------------------------------------
#| label: fig-sine-illusion
#| message: false
#| error: false
#| warning: false
#| cache: false
#| fig-width: 4
#| fig-height: 4
#| out-width: "33%"
#| fig-cap: "An illustration of the sine illusion [@vanderplasSignsSineIllusion2015], also known as the line-width illusion. All vertical lines are the same length, but the lines in the middle of the curve appear to be much shorter. The illusion results when implicit perceptual corrections useful for perceiving the size of objects with depth are applied to 2D stimuli with no actual depth. As 3D heuristics occasionally cause inaccurate communication when applied to 2D objects, it is reasonable to think that there might be some situations where charts making use of a realistic third dimension might be perceived more accurately than their 2D equivalents."
#| fig-scap: "Sine illusion and 3D depth heuristic effects"
#| fig-subcap:
#|    - "Sine Illusion"
#|    - "3D surface with similar features"
#|    - "Perceived vs. actual length"
#' Function to data frame
#'  
#' @param n number of values 
#' @param ell extent of vertical range
#' @param x range of horizontal values
#' @param f function
#' @param fp first derivative of function f
#' @param f2p second derivative of function f
#' @return data frame with n function values and derivatives along the x axis for a range given by x
#' @example
#' f <- function(x) 2*sin(x)
#' fp <- function(x) 2*cos(x)
#' f2p <- function(x) -2*sin(x)
#' dframe <- function.frame(n=50, ell=1, x=c(0,2*pi), f, fp, f2p)
#' require(ggplot2)
#' qplot(x, y=ystart, yend=yend, xend=x, geom="segment", data=dframe) +
#'    geom_point(aes(x,y))
function.frame <- function(n=200, ell=1, x, f, fprime, f2prime) {
  x <- seq(x[1], x[2], length=n)
  if (length(ell) != length(x)) ell <- rep(ell, length(x))
  y <- f(x)
  ystart <- y - ell/2
  yend <- y + ell/2
  # now correct for line illusion in vertical direction
  dy <- diff(range(y))
  dyl <- diff(range(c(ystart, yend)))
  # fprime and f2prime are sensitive to the aspect ratio of a plot
  # we represent it in framework of dy and dy+len
  # needs to be fixed by factor a:  
  a <- dy/dyl 
  
  fp <- a*fprime(x)
  f2p <- a*f2prime(x)
  data.frame(x,y,ystart, yend, fp, f2p, a)
}

#' This function has to be split in parts and renamed
#' 
#' Right now this function is a convoluted mess.  We need to have separate
#' functions that 
#' (1) create a data frame for a given function,
#' its first and second derivatives and range in x
#' (this is what the function function.frame is supposed to do), 
#' (2) a trig transform function
#' (3) a quadratic approach function - better even, make (2) and (3) one function and use a parameter to decide on the method to use.
#' @example
#' f <- function(x) 2*sin(x)
#' fp <- function(x) 2*cos(x)
#' f2p <- function(x) -2*sin(x)
#' dframe <- createSine(n=50, ell=1, x=c(0,2*pi), f, fp, f2p)
#' require(ggplot2)
#' qplot(x=x, xend=x, y=y+ellx4.u, yend=y-ellx4.l, geom="segment", data=dframe, linetype=I(1)) + 
#'    theme_bw() + coord_fixed(ratio=1) + 
#'    xlab("x") + ylab("y") + 
#'    scale_x_continuous(breaks=seq(0, 2*pi, by=pi/2), minor_breaks=minor.axis.correction,
#'      labels=c("0", expression(paste(pi,"/2")), expression(pi), expression(paste("3",pi, "/2")), 
#'      expression(paste("2",pi))))

createSine <- function(n=200, len=1, f=f, fprime=fprime, f2prime=f2prime, a=0, b=2*pi) {
  #  if(getquadapprox & !is.function(f2prime)) f2prime <- function(x) -1*f(x) # for backwards compatibility
  x <- seq(a, b, length=n+2)[(2:(n+1))]
  ell <- rep(len, length=length(x))
  fx <- f(x)
  ystart <- fx - .5*ell
  yend <- fx + .5*ell
  
  # now correct for line illusion in vertical direction
  dy <- diff(range(fx))
  dx <- diff(range(x))
  # fprime works in framework of dx and dy, but we represent it in framework of dx and dy+len
  # needs to be fixed by factor a:  
#  a <- dy/(dy + len) 
  dyl <- diff(range(c(ystart, yend)))
  a <- dy/(dyl) 
  # ellx is based on the "trig" correction
  ellx <- ell / cos(atan(abs(a*fprime(x))))
  # ellx2 is based on linear approximation of f  
  ellx2 <- ell * sqrt(1 + a^2*fprime(x)^2)
  
  # make this a data frame - ggplot2 doesn't do well with floating vectors
  dframe <- data.frame(x=x, xstart=x, xend=x, y=fx, ystart=ystart, yend=yend, ell=ell, ellx = ellx, ellx2=ellx2)
  
  # third adjustment is based on quadratic approximation of f.
  # this needs two parts: correction above and below f(x)  
  #   if(getquadapprox & is.function(f2prime)){
  #     secseg <- do.call("rbind", lapply(dframe$x, function(i) getSecantSegment(i, dframe, f, fprime, f2prime)))
  #     dframe$ellx3.u <- secseg$sec.ell1
  #     dframe$ellx3.l <- secseg$sec.ell2
  #   }
  
  fp <- a*fprime(x)
  f2p <- a*f2prime(x)
  v <- 1 + fp^2
  lambdapinv <- 0.5*(sqrt(v^2-f2p*fp^2*ell) + v)    
#  lambdaminv <- -0.5*(sqrt(v^2+f2p*fp^2*ell) + v)
  lambdaminv <- 0.5*(sqrt(v^2+f2p*fp^2*ell) + v)
  
  dframe$ellx4.l <- 0.5*abs(lambdapinv)/sqrt(v)
  dframe$ellx4.u <- 0.5*abs(lambdaminv)/sqrt(v)
#  dframe$lambdam <- 1/lambdaminv
#  dframe$lambdap <- 1/lambdapinv
  fp <- fprime(x)
  f2p <- f2prime(x)
  v <- 1 + fp^2
#  lambda1 <- (-v + sqrt(v^2 - fp^2*f2p*ell))/(fp^2*f2p) # these two work
#  lambda2 <- (-v + sqrt(v^2 + fp^2*f2p*ell))/(fp^2*f2p) # these two work

  lambdaa <- -2*(sqrt(v^2 - f2p*fp^2*ell) + v)^-1 # identical to top
  lambdab <- 2*(sqrt(v^2 + f2p*fp^2*ell) + v)^-1

  dframe$lambdam <- lambdaa
  dframe$lambdap <- lambdab
   
  # qplot(a*(sqrt(1 + a^2*fprime(x)^2)*abs(lambdab))^-1, ellx4.u, data=dframe)
  # qplot(a*(sqrt(1 + a^2*fprime(x)^2)*abs(lambdaa))^-1, ellx4.l, data=dframe)
  # ggplot(aes(x, y+ell*(a*sqrt(v)*abs(lambdaa))^-1, xend=x, yend=y-ell*(a*sqrt(v)*abs(lambdab))^-1), data=dframe) + geom_segment()  
     
  dframe
}

#' what is the function doing? - in one sentence
#' 
#' function description in a paragraph
#' @param x0 is a vector of locations for which the secant segments are supposed to be calculated
#' @param df df is a data frame
#' @param f function
#' @param fp first derivative of function f
#' @param f2p second derivative of function f
#' @return data frame consisting of .... what is the output of the function?
#' @example
#' # need an example here of how to use the function
getSecantSegment <- function(x0, df, f, fprime, f2prime){
# find the closest values of the grid df corresponding to x0
  # doesn't work right now
  ell.x0 <- sapply(x0, function(x) df$ell[which.min(x-df$x)])

  dy <- diff(range(with(df, c(y+ell.x0, y-ell.x0))))
  dx <- diff(range(df$x))
  a <- dx/dy
  
  #---- Actual Roots
  #   secSlope   <- -a/fprime(x0)
  #   temp       <- seq(min(df$x)-pi, max(df$x)+pi, .0001)
  #   leftend    <- temp[which.min(abs(f(temp) + ell.x1 - secSlope*(temp-x1)))]
  #   rightend   <- temp[which.min(abs(f(temp) - ell.x1 - secSlope*(temp-x1)))]
  #----
  
  #---- Approximation
  lambda1 <- (-(fprime(x0)^2 + 1) + sqrt((fprime(x0)^2 + 1)^2 - 2*fprime(x0)^2*f2prime(x0)*ell.x0))/(fprime(x0)^2*f2prime(x0))
  lambda2 <- (-(fprime(x0)^2 + 1) + sqrt((fprime(x0)^2 + 1)^2 + 2*fprime(x0)^2*f2prime(x0)*ell.x0))/(fprime(x0)^2*f2prime(x0))
  
  x1 <- lambda1*fprime(x0)+x0
  x2 <- lambda2*fprime(x0)+x0
  y1 <- f(x0)-lambda1
  y2 <- f(x0)-lambda2
  #----
  
  #---- Approximation V2 ----
  #   dx <- fprime(x0)
  #   dx1 <- 1+dx^2   
  #   ddx <- f2prime(x0)
  #   
  #   corr1 <- 1/a*(((dx1)-sqrt(dx1^2-2*ddx*(dx1-1)^2*ell.x0))/(ddx*dx))
  #   corr2 <- 1/a*(((dx1)-sqrt(dx1^2+2*ddx*(dx1-1)^2*ell.x0))/(ddx*dx))
  #   if(abs(dx)<.01) {
  #     dx1 <- 2
  #     corr1 <- corr2 <- 0
  #   }
  #   if(abs(dx)<.01){
  #     ddx <- 1
  #   }
  #   
  #   x1 = corr1 + x0
  #   x2 = corr2 + x0
  #   y1 = f(x1)+ell.x0
  #   y2 = f(x2)-ell.x0
  #---- 
  
  df2 <- data.frame(x=x0, y=f(x0), deriv=fprime(x0),
                    sec.xstart=x1, sec.xend = x2, 
                    sec.ystart=y1, sec.yend = y2,
                    ell.orig = 2*ell.x0)
  
  df2$sec.ell1 <- a*.5*ell.x0/with(df2, sqrt((sec.yend-y)^2+(sec.xend-x)^2))
  df2$sec.ell2 <- a*.5*ell.x0/with(df2, sqrt((y-sec.ystart)^2+(x-sec.xstart)^2))
  #   df2$sec.ell1 <- with(df2, sqrt((sec.yend-y)^2+(sec.xend-x)^2))
  #   df2$sec.ell2 <- with(df2, sqrt((y-sec.ystart)^2+(x-sec.xstart)^2))
  df2$type <- "Perceived Width"
  df2$a <- a
  return(df2)
}

correctx <- function(z, fprime, a=0, b=2*pi, w=1) {
  # w = 1/(shrink+1)
  const <- integrate(function(x) abs(fprime(x)), a, b)$value
  trans <- sapply(z, function(i) integrate(function(x) abs(fprime(x)), a, i)$value*(b-a)/const + a)
  # alternatively to the rowMeans, you could report back  
  # trans*(1-w) + z*w
  trans*w + z*(1-w)
}

f <- function(x) 2*sin(x)
fprime <- function(x) 2*cos(x)
f2prime <- function(x) -2*sin(x)

x <- seq(0, 2*pi, length=42)[2:41]
data <- do.call("rbind", lapply(seq(-.5, .5, 1), function(i) data.frame(x=x, y=3*sin(x), z=i)))

data.persp <- reshape2::acast(data, x~z, value.var="y")
x <- sort(unique(data$x))
y <- sort(unique(data$y))
z <- sort(unique(data$z))
dframe <- createSine(n = 50, len = 1, f=f, fprime=fprime, f2prime=f2prime)
dframe %>%
  ggplot(aes(x = xstart, xend = xend, y = ystart, yend = yend)) + geom_segment() + coord_fixed() + 
  theme_void()

par(mar = c(0, 0, 0, 0))
persp(x, z, data.persp,  xlab="", ylab="", zlab="", theta=0, phi=45, border="black", shade=.35, col="white", xlim=c(-pi/12, 2*pi+pi/12), ylim=c(-1, 1), scale=FALSE, box=FALSE, expand=2.5/pi, d=50) # , ltheta=0, lphi=-15

# persp(x, z, data.persp,  xlab="", ylab="", zlab="", theta=0, phi=45, border="black", shade=.35, col="white", xlim=c(-pi/12, 2*pi+pi/12), ylim=c(-1.75, 1.75), scale=FALSE, box=FALSE, expand=3/pi, d=3) # , ltheta=0, lphi=-15

dframe <- createSine(n = 150, len = 1, f=f, fprime=fprime, f2prime=f2prime)
dframe$ystartcts <- dframe$ystart
dframe$yendcts <- dframe$yend
dframe[1:150,c(2, 3, 5, 6)] <- NA
dframe[(1:15)*10-5, c(2, 3)] <- dframe[(1:15)*10-5, 1] 
dframe[(1:15)*10-5, 5] <- dframe[(1:15)*10-5, 4] - .5
dframe[(1:15)*10-5, 6] <- dframe[(1:15)*10-5, 4] + .5
dframe$type <- "Vertical Width"

idx <- which(!is.na(dframe$xstart))
dframe$ell <- dframe$ell/2
dframe.1 <- getSecantSegment(dframe$xstart[idx], dframe, f, fprime, f2prime)
dframe.1$x <- dframe$x[idx]
dframe.1$y <- dframe$y[idx]
dframe.1$ystartcts <- dframe$ystartcts[idx]
dframe.1$yendcts <- dframe$yendcts[idx]
names(dframe.1) <- c("x", "y", "deriv", "xstart", "xend", "ystart", "yend", "ell", "ell.quad1", "ell.quad2", "type", "a", "ystartcts", "yendcts")
dframe.1$vangle <- with(dframe.1, atan(deriv))
dframe <- bind_rows(dframe, dframe.1)
dframe$type <- factor(dframe$type)

ggplot(aes(x=x, y=y), data=dframe, colour=I("grey50")) + 
  geom_line(color = "grey") + theme_void() + 
  geom_line(aes(y=ystartcts), colour="grey50", alpha = .25) +
  geom_line(aes(y=yendcts), colour="grey50", alpha = .25) +
  geom_segment(data=subset(dframe, !is.na(type)), 
               aes(x=xstart, xend = xend, y=ystart, yend=yend, colour=type, linetype=type), linewidth=0.8)  + 
  xlab("") + ylab("")  +
  coord_equal(ratio=1) + scale_colour_manual("", values=c("blue", "grey30")) + 
  # geom_text(aes(label=paste("theta", "%~~%", round(abs(vangle)/pi*180), "^o", sep=""), 
                # x=pmax(xstart, xend)/2+x/2+.35 , y=y-sign(vangle)*.6+.02), colour="blue",
            # data=dframe.1, parse=TRUE, hjust=.9, vjust=.5, size=3) + 
  scale_x_continuous(breaks=seq(0, 2*pi, by=pi/2), 
                     labels=c("0", expression(paste(pi,"/2")), expression(pi), 
                              expression(paste("3",pi, "/2")), expression(paste("2",pi)))) +
  scale_linetype_manual("", values=c("11", "solid")) + 
  theme(legend.key.width = unit(3, "line"), plot.margin = unit(c(0,0,0,0), "cm"), legend.position=c(.25, .2)) 




## ----experiment-values--------------------------------------------------------------
library(papaja)
extract_values <- function(data){
  data %>% 
    dplyr::filter(!is.na(IDchr)) %>% 
    dplyr::select(Height)
}

datasets %>% 
  mutate(setID = str_extract(file, 'id-[0-9]{2}')) %>% 
  filter(fileID < 15) %>% 
  mutate(vals = map(data, extract_values)) %>% 
  unnest(vals) %>% 
  mutate(Height = round(Height,1)) %>% 
  group_by(fileID) %>% 
  mutate(Bar = ifelse(Height == min(Height), 'Smaller', 'Larger')) %>% 
  ungroup() %>% 
  select(fileID, Height, Bar, setID) %>% 
  filter(fileID %% 2 == 1) %>% 
  mutate(setID = paste0('ID: ', str_remove(setID, 'id-'))) %>% 
  select(-fileID) %>% 
  pivot_wider(names_from = Bar, values_from = Height) %>% 
  mutate(`Ratio (%)` = round(Smaller/Larger,3)*100) %>% arrange(`Ratio (%)`) %>% 
  pivot_longer(Smaller:`Ratio (%)`, values_to = 'Height', names_to = 'Bar') %>% 
  pivot_wider(names_from = setID, values_from = Height) %>% 
  knitr::kable(caption = 'Values used in the experiment to make ratio comparisons, sorted by ratio. The ID label corresponds to the file reference number.') 



## -----------------------------------------------------------------------------------

#| label: fig-plotTypes
#| fig-cap: "Two dimensional, 3D digital rendering, and 3D-printed charts used in this study."
#| out-width: [30%, 30%, 35%]
#| fig-show: "hold"

#| fig-scap: "2D, 3D digital, and 3D-printed chart types"
knitr::include_graphics("_images/Type1-Rep01.png", dpi = 300)
knitr::include_graphics("_images/RenderedChart.png", dpi = 300)
knitr::include_graphics("_images/Kit_of_charts.png", dpi = 300)


## -----------------------------------------------------------------------------------
#| label: fig-studyDesign
#| fig-cap: "A graphical representation of the study design. Only five of the seven ratios were used in each kit; at least one of the smallest or largest ratios were included along with 4 other charts; each selected ratio was displayed in all 3 mediums. For each medium $\\times$ ratio combination, the comparison type (separated or adjacent) was randomly determined. A total of 21 kits of 3D printed charts were created to include all combinations of the five ratios."
#| fig-scap: "Study design for bar chart perception experiment"
knitr::include_graphics("_images/design.pdf")


## -----------------------------------------------------------------------------------
#| label: fig-demographics
#| fig-cap: "Demographic characteristics of participants in the study."
#| message: false
#| warning: false
#| out-width: 100%
#| fig-width: 6.5
#| fig-height: 3.5
#| fig-scap: "Participant demographics"
library(patchwork)
valid.users$userAppStartTime <- as.POSIXct(valid.users$userAppStartTime,
           origin = '1970-1-1')

age.levels = sort(unique(valid.users$age))
p1 = valid.users[valid.users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(age) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = factor(age, levels = rev(age.levels)), y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(x = factor(age, levels = age.levels), y = count+.5, label = age), hjust = 0) + 
  coord_flip() + 
  labs(title = 'Age', y = 'Count') +
  theme_bw() + 
  scale_y_continuous(expand = expand_scale(mult = 0, add = c(.25, 4))) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())



p2 = valid.users[valid.users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(gender) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = gender, y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(x = gender, y = count + .5, label = gender), hjust = 0) + 
  coord_flip() + 
  labs(title = 'Gender', y = 'Count') +
  theme_bw() + 
  scale_y_continuous(expand = expand_scale(mult = 0, add = c(.25, 4))) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())



educ.levels = sort(unique(valid.users$education))[c(2,4,3,1)]
p3 = valid.users[valid.users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(education) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = factor(education, levels = educ.levels), y = count)) + 
  geom_bar(stat = 'identity') +
  coord_flip() + 
  geom_text(aes(x = education, y = count + .5, label = str_replace(education, "Graduate Degree", "Graduate\nDegree")), hjust = 0) + 
  scale_y_continuous(expand = expand_scale(mult = 0, add = c(.25, 15))) + 
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(title = 'Education', y = 'Count') 

p = p1 + p2 + p3
p


## -----------------------------------------------------------------------------------
#| label: fig-practice
#| fig-cap: "Screenshot of Shiny application practice screen. Three 2D bar charts with different ratios were provided, along with sliders indicating the correct proportion. Participants could practice with the sliders and preview the questions that would be asked as part of the task."
#| out.width: 80%
#| fig-scap: "Shiny application practice screen"
knitr::include_graphics("_images/03-Practice-2.png")


## -----------------------------------------------------------------------------------
#| label: fig-experiment3dRender
#| fig-cap: "Screenshot of the applet collecting data for a 3D rendered chart task. Participants were asked to select which bar (circle or triangle) was smaller, and then to estimate the ratio of the smaller bar to the larger bar."
#| out-width: 80%
#| fig-scap: "3D rendered chart task interface"
knitr::include_graphics("_images/05-Experiment-05-filled-in.png")


## ----include = F--------------------------------------------------------------------
res %>%
  group_by(subject, ratio) %>%
  summarize(mean = mean(byHowMuch), sd = sd(byHowMuch))


## -----------------------------------------------------------------------------------
#| include: false
res_sub_count <- res_all %>% 
  group_by(nickname, participantUnique, appStartTime, subject) %>% 
  count()


## -----------------------------------------------------------------------------------
#| label: fig-midmeans-log-errors
#| fig-cap: "Midmeans and observed values of log absolute errors for the true ratio of bars. Summary lines are computed from raw data using a loess smooth."
#| message: false
#| warning: false
#| out-width: \textwidth
#| fig-height: 3
#| fig-scap: "Midmeans of log absolute error by chart type"
plot.types = c('2D', 'Rendered 3D', '3D Printed')
names(plot.types) = c('2dDigital', '3dDigital', '3dPrint')



tmp <- res %>% 
  group_by(Height, graphtype, plot) %>% 
  summarize(midmean = mean(response, trim=0.25, na.rm = T)) %>%
  mutate(type = "Midmean") %>%
  rename(response = midmean)


res %>% 
  mutate(type = "Raw Data") %>%
  bind_rows(tmp) %>%
  # group_by(Height, graphtype, plot) %>% 
  ggplot(mapping = aes(x = Height*100, y = response,
                       color = graphtype, fill = graphtype)) +
  geom_smooth(se = T, alpha=.125,  expand = F, method = "loess") +
  geom_point(aes(shape = type, alpha = type, size = type)) +
  scale_shape_manual("Data", values = c(19, 1)) + 
  scale_size_manual("Data", values = c(2, 1)) + 
  scale_alpha_manual("Data", values = c(1, .25)) + 
  scale_fill_discrete(labels = c('Adjacent', 'Separated')) +
  scale_color_discrete(labels = c('Adjacent', 'Separated')) +
  scale_x_continuous(limits = c(0, 100)) + 
  facet_wrap(~plot, labeller = labeller(plot = plot.types)) +
  labs(title = '',
       x = 'True Proportional Difference (%)',
       y = 'Log Error',fill = 'Comparison Type',
       color = 'Comparison Type') +
  theme_bw() +
  theme(legend.position = 'bottom')


## -----------------------------------------------------------------------------------
#| label: gam
library(mgcv)
library(gratia)
res2 <- res %>%
  mutate(participant = factor(as.numeric(factor(subject)))) %>%
  mutate(ratio_prop = ratioLabel/100) %>%
  mutate(type = factor(type, levels = 1:2, labels = c("Separated", "Adjacent"))) %>%
  mutate(plot = factor(plot)) %>%
  mutate(plottype = interaction(plot, type))
# res2

gam_mod <- gam(response ~ 
                 s(ratio_prop, by = plot, k = 6) +
                 s(ratio_prop, by = type, k = 6) + 
                 plot + type + 
                 s(participant, bs = "re"),
               data = res2, method = 'REML')
# gam_mod

gam_sum <- summary(gam_mod)


## ----results = 'asis'---------------------------------------------------------------
gam_sum$p.table %>% knitr::kable(digits = 4, caption = "Parametric coefficients in gam model.", label = "gam-param", booktabs = T)

gam_sum$s.table %>% as.data.frame() %>% 
  mutate(Smooth = rownames(.),
         Smooth = str_replace(Smooth, "ratio_prop", "Ratio")) %>%
  select(Smooth, everything()) %>%
  knitr::kable(digits = 4, row.names = F, caption = "Approximate significance of smooth terms in gam model.", label = "gam-smooth", booktabs = T)


## -----------------------------------------------------------------------------------
#| label: fig-gam-pred-chart
#| fig-cap: "Predictions with standard errors for fixed effects in a generalized additive model with splines for ratio by comparison and ratio by display method. It is clear that the separated comparisons are easier to estimate when small, right around 50\\%, or large, and harder to estimate between these points. What is interesting is that no such trend is present for adjacent comparisons."
#| fig-width: 6.5
#| fig-height: 3
#| out-width: 100%
#| fig-scap: "GAM predictions by ratio and comparison type"
# 
# coef(gam_mod)
# draw(gam_mod, ncol = 4)


newdata <- expand.grid(ratio_prop = seq(0.15, .85, .001), type = c("Separated", "Adjacent"), plot = unique(res2$plot), participant = 1) %>%
  mutate(plottype = interaction(plot, type))
pred <- predict(gam_mod, newdata = newdata, type = "terms", se.fit = T) %>% as.data.frame() 

pred_se <- select(pred, matches("se.fit")) %>%
  select(-matches("participant"))
pred_fit <- select(pred, matches("^fit.")) %>%
  select(-matches("participant"))

newdata %>%
  mutate(pred = rowSums(pred_fit), 
         se = rowSums(pred_se)) %>%
  ggplot(aes(x = ratio_prop, y = pred + gam_mod$coefficients[1], color = type)) + 
  geom_ribbon(aes(ymin = pred + gam_mod$coefficients[1] - 1.96*se, ymax = pred+ gam_mod$coefficients[1] + 1.96*se, fill = type), alpha = .25) +  
  geom_line() + facet_grid(.~plot) + 
  scale_color_discrete("Comparison", na.translate = F) + scale_fill_discrete("Comparison", na.translate = F) + 
  
  geom_point(data = res, aes(x = ratioLabel/100, y = response, color = factor(type, levels = 1:2, labels = c("Separated", "Adjacent")))) +
  theme_bw() + ylab("Prediction") + xlab("True Proportion Difference") + 
  xlim(c(0, 1)) + 
  theme(legend.position = 'bottom')



## -----------------------------------------------------------------------------------
#| fig-width: 3
#| label: fig-residualplots-gam
#| fig-height: 3
#| include: false
#| fig-cap: "Residual diagnostic plots for the linear mixed model."
#| fig-scap: "GAM residual diagnostics"
#| fig-subcap:
#| - Fitted vs. predicted residuals
#| - Q-Q plot
#| out-width: ".40\\linewidth"
#| fig-show: "hold"
#| layout-ncol: 2

res2 <- res2 %>%
  mutate(predict = predict(gam_mod, newdata = res2),
         resid = response - predict)

ggplot(res2, aes(x = predict, y = resid)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') + 
  theme_bw() + 
  facet_wrap(~plottype,) + 
  labs(x = 'Fitted value',
       y = 'Residual')

ggplot(res2, aes(sample = resid)) + 
  geom_qq() + 
  geom_qq_line(color = 'red', linetype = 'dashed') + 
  facet_grid(type ~ plot) + 
  theme_bw() + 
  labs(x = 'Theoretical Quantiles',
       y = 'Sample Quantiles')



## -----------------------------------------------------------------------------------
#| label: fig-clicks
#| fig-cap: ""
#| fig-width: 4
#| fig-height: 4
#| out-width: 90%
#| fig-show: hold
#| fig-scap: "WebGL interaction counts versus estimation error"
res %>%
  filter(plot == "3dDigital") %>%
  ggplot(aes(x = plot3dClicks)) + geom_bar() + 
  xlab("# Clicks on WebGL Plot") + ylab("Count") + ggtitle("Distribution of WebGL Interaction") + 
  theme_bw()

res %>%
  filter(plot == "3dDigital") %>%
  ggplot(aes(x = plot3dClicks, y = response)) + geom_point() + 
  xlab("# Clicks on WebGL Plot") + ylab("Log Error") + ggtitle("Accuracy and Participant Interactivity") + 
  geom_smooth(method = "lm", se = T) + 
  theme_bw()


## ----include=F, eval = F------------------------------------------------------------
# library(papaja)
# library(lme4)
# 
# mod <- lmer(response ~ (1|subject) + ratioLabel + plot + type,
#             data = res)
# 
# modsum <- summary(mod)
# modaov <- anova(mod)
# modaovsum <- summary(modaov)
# 
# f_df <- as.numeric(length(modsum$residuals)-modsum$ngrps - 1)
# 
# apa_lm <- apa_print(mod)
# knitr::kable(apa_lm$table, caption = "Analysis of Fixed Effects")


## ----fig-residualplots, include = F, eval = F---------------------------------------
#| fig-width: 3
#| fig-height: 3
#| fig-cap: "Residual diagnostic plots for the linear mixed model."
#| fig-scap: "LMM residual diagnostics"
#| fig-subcap:
#| - Fitted vs. predicted residuals
#| - Q-Q plot
#| out-width: ".40\\linewidth"
#| fig-show: "hold"
#| layout-ncol: 2

# r = residuals(mod)
# p = predict(mod)
# 
# ggplot(mapping = aes(x = p, y = r)) +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
#   theme_bw() +
#   labs(x = 'Fitted value',
#        y = 'Residual')
# 
# ggplot(mapping = aes(sample = r)) +
#   geom_qq() +
#   geom_qq_line(color = 'red', linetype = 'dashed') +
#   theme_bw() +
#   labs(x = 'Theoretical Quantiles',
#        y = 'Sample Quantiles')
# 


## ----fig-percent-error-model, eval = F----------------------------------------------
# # Model using % error instead of the log2 error...
# 
# res3 <- res2 %>%
#   mutate(estimation_error = (byHowMuch - ratioLabel)/(ratioLabel))
# res3 %>%
#   mutate(type = "Raw Data") %>%
#   ggplot(mapping = aes(x = Height*100, y = estimation_error,
#                        color = graphtype, fill = graphtype)) +
#   geom_smooth(se = T, alpha=.125,  expand = F, method = "loess") +
#   geom_point(aes(shape = type, alpha = type, size = type)) +
#   scale_shape_manual("Data", values = c(19, 1)) +
#   scale_size_manual("Data", values = c(2, 1)) +
#   scale_alpha_manual("Data", values = c(1, .25)) +
#   scale_fill_discrete(labels = c('Adjacent', 'Separated')) +
#   scale_color_discrete(labels = c('Adjacent', 'Separated')) +
#   scale_x_continuous(limits = c(0, 100)) +
#   facet_wrap(~plot, labeller = labeller(plot = plot.types)) +
#   labs(title = '',
#        x = 'True Proportional Difference (%)',
#        y = 'Log Error',fill = 'Comparison Type',
#        color = 'Comparison Type') +
#   theme_bw() +
#   theme(legend.position = 'bottom')
# gam_mod2 <- gam(estimation_error ~
#                  s(ratio_prop, by = plot, k = 6) +
#                  s(ratio_prop, by = type, k = 6) +
#                  plot + type +
#                  s(participant, bs = "re"),
#                data = res3, method = 'REML')
# # gam_mod
# 
# gam_sum2 <- summary(gam_mod2)
# 
# 
# gam_sum2$p.table %>% knitr::kable(digits = 4, caption = "Parametric coefficients in gam model.", label = "gam-param", booktabs = T)
# 
# gam_sum2$s.table %>% as.data.frame() %>%
#   mutate(Smooth = rownames(.),
#          Smooth = str_replace(Smooth, "ratio_prop", "Ratio")) %>%
#   select(Smooth, everything()) %>%
#   knitr::kable(digits = 4, row.names = F, caption = "Approximate significance of smooth terms in gam model.", label = "gam-smooth", booktabs = T)
# 
# 
# 
# newdata <- expand.grid(
#   ratio_prop = seq(0.15, .85, .001),
#   type = c("Separated", "Adjacent"),
#   plot = unique(res3$plot),
#   participant = 1)
# pred2 <- predict(gam_mod2, newdata = newdata, type = "terms", se.fit = T) %>% as.data.frame()
# 
# pred_se2 <- select(pred2, matches("se.fit")) %>%
#   select(-matches("participant"))
# pred_fit2 <- select(pred2, matches("^fit.")) %>%
#   select(-matches("participant"))
# 
# newdata %>%
#   mutate(pred = rowSums(pred_fit2) + gam_mod2$coefficients[1],
#          se = rowSums(pred_se2) + gam_sum2$p.table[1,2]) %>%
#   ggplot(aes(x = ratio_prop, y = pred , color = type)) +
#   geom_ribbon(aes(ymin = pred - 1.96*se, ymax = pred + 1.96*se, fill = type), alpha = .25) +
#   geom_line() + facet_grid(.~plot) +
#   scale_color_discrete("Comparison", na.translate = F) + scale_fill_discrete("Comparison", na.translate = F) +
# 
#   geom_point(data = res3, aes(x = ratioLabel/100, y = estimation_error, color = type)) +
#   theme_bw() + ylab("Estimation Error") + xlab("True Proportion Difference") +
#   xlim(c(0, 1))
# 
# 
# r2 = residuals(gam_mod2)
# p2 = predict(gam_mod2)
# 
# ggplot(mapping = aes(x = p2, y = r2)) +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
#   theme_bw() +
#   labs(x = 'Fitted value',
#        y = 'Residual')
# 
# ggplot(mapping = aes(sample = r2)) +
#   geom_qq() +
#   geom_qq_line(color = 'red', linetype = 'dashed') +
#   theme_bw() +
#   labs(x = 'Theoretical Quantiles',
#        y = 'Sample Quantiles')


## -----------------------------------------------------------------------------------
#| include: false
knitr::purl(input = 'index.qmd', output = 'ch1-code.R')

