library(geometry)
library(units)
library(jsonlite)
library(ggplot2)
library(ggsoccer)
library(dplyr)
library(tidyr)
library(logr)
library(futile.logger)
library(mongolite)

conm <- mongo(
  db="sbfem25",
  collection = "matches",
  url = "mongodb://www.talmedos.com:27017"
  #url = "mongodb://localhost:27017"
)
conl <- mongo(
  db="sbfem25",
  collection = "lineups",
  url = "mongodb://www.talmedos.com:27017"
  #url = "mongodb://localhost:27017"
)
conev <- mongo(
  db="sbfem25",
  collection = "events",
  url = "mongodb://www.talmedos.com:27017"
  #url = "mongodb://localhost:27017"
)
conmev <- mongo(
  db="sbfem25",
  collection = "maleevents",
  url = "mongodb://www.talmedos.com:27017"
  #url = "mongodb://localhost:27017"
)

#q='{"matchId":{"$in":[3906390,3893806,3893822,3906389]}}'
#res=conev$find(query = q,fields = '{}')
#resall=jsonlite::flatten(res)

getAllMatches <- function() {
  am=conm$find(query = "{}")
  amflat=jsonlite::flatten(am)
  return(amflat)
}

getLineupsSingleMatch<- function(matchid) {
  query = paste0('{"matchid":',matchid,'}')
  allEv=conl$find(query = query)
  retval=jsonlite::flatten(allEv)
  return(retval)
}

getLineupsMultipleMatches<- function(teamids) {
  query = jsonlite::toJSON(list(`team_id` = list(`$in` = teamids)), auto_unbox = TRUE)
  allEv=conl$find(query = query)
  retval=jsonlite::flatten(allEv)
  return(retval)
}

getAllEventsMultipleMatches <- function(matchidList, gender = "f") {
  if (gender == "f") {
    query <- jsonlite::toJSON(
      list(`match_id` = list(`$in` = matchidList)),
      auto_unbox = TRUE)
    allEv <- conev$find(query = query)
  } else {
    query <- jsonlite::toJSON(
      list(`matchId` = list(`$in` = matchidList)),
      auto_unbox = TRUE)
    allEv <- conmev$find(query = query)
  }
  retval <- jsonlite::flatten(allEv)
  return(retval)
}

# getAllEventsMultipleMatches<- function(matchidList, gender="f") {
  #query=paste0('{"match_id":',matchid,'}')
  #matchidList=vmids[1:3]
#  query = jsonlite::toJSON(list(`match_id` = list(`$in` = matchidList)), auto_unbox = TRUE)
#  query
  #query = jsonlite::toJSON(list(matchId = list(`$in` = matchidList)), auto_unbox = TRUE)
#  if (gender == "f" ) {
#    allEv=conev$find(query = query)
#  } else {
#    allEv=conmev$find(query = query)
#  }
#  retval=jsonlite::flatten(allEv)
#  return(retval)
# }

getAllEvents <- function(matchid, gender="f") {
  matchid=as.integer(3753983)
  query
  if (gender == "f" ) {
    query=paste0('{"match_id":',matchid,'}')
    allEv=conev$find(query = query)
  } else {
    query=paste0('{"matchId":',matchid,'}')
    allEv=conmev$find(query = query)
  }
  retval=jsonlite::flatten(allEv)
  return(retval)
}

getAllEventsTypeForMatches <- function(matchidlist, gender="f",type="Shot") {
  query=paste0('{"match_id":',matchid,',"type.name":"',type,'"}')
  if (gender == "f" ) {
    allEv=conev$find(query = query, fields = '{}')
  } else {
    allEv=conmev$find(query = query)
  }
  retval=jsonlite::flatten(allEv)
  return(retval)
}

getAllEventsType <- function(matchid, gender="f",type="Shot") {
  query=paste0('{"match_id":',matchid,',"type.name":"',type,'"}')
  if (gender == "f" ) {
    allEv=conev$find(query = query, fields = '{}')
  } else {
    allEv=conmev$find(query = query)
  }
  retval=jsonlite::flatten(allEv)
  return(retval)
}


init_logging <- function(location) {
  flog.appender(appender.file(location))
}

findAllFShots <- function() {
  return(readRDS("allFshots.rds"))
}

findAllMShots <- function() {
  return(readRDS("allMshots.rds"))
}

findAllMMatchInfo <- function() {
  return(readRDS("allMKampe.rds"))
}

findAllFMatchInfo <- function() {
  return(readRDS("allFKampe.rds"))
}
calculate_angle_degrees <- function(ax, ay, bx, by, ox, oy) {
  # Calculate vectors AO and AB
  theta_degrees = 360
  tryCatch({
    
  AO <- c(ox - ax, oy - ay)
  AB <- c(bx - ax, by - ay)
  #print(AO)
  #print(AB)
  #print("SS")
  
  # Dot product of AO and AB
  dot_product <- sum(AO * AB)
  
  #print(dot_product)
  # Magnitudes of AO and AB
  magnitude_AO <- sqrt(sum(AO^2))
  magnitude_AB <- sqrt(sum(AB^2))
  #print(magnitude_AB)
  
  # Cosine of  angle
  cos_theta <- dot_product / (magnitude_AO * magnitude_AB)
  
  # Angle in radians, then converting to degrees
  theta_radians <- acos(cos_theta)
  theta_degrees <- theta_radians * (180 / pi)
  
  # Return the angle in degrees
  }, error = function(e) { print(e) } )
  return(theta_degrees)
}



calculate_triangle_area_goal <- function(x1, y1) {
  #stolpe 1 (120,44)
  x2=120
  y2=44
  #stolpe 2 (120,36)
  x3=120
  y3=36
  
  area <- 0.5 * abs(x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
  return(area)
}

  calculate_triangle_area <- function(x1, y1, x2, y2, x3, y3) {
  area <- 0.5 * abs(x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
  return(area)
}

is_opponent_inside_triangleII <- function(o,shooter) {
  # Coordinates of the three vertices of the triangle
  x1 <- 120
  y1 <- 44
  x2 <- 120
  y2 <- 36
  x3 <- shooter[1]
  y3 <- shooter[2]
  o_x <- o[1]
  o_y <- o[2]
  
  # Calculate the area of the entire triangle
  total_area <- calculate_triangle_area(x1, y1, x2, y2, x3, y3)
  
  # Calculate the area of three sub-triangles formed by the goalkeeper and two vertices of the triangle
  area1 <- calculate_triangle_area(o_x, o_y, x2, y2, x3, y3)
  area2 <- calculate_triangle_area(x1, y1, o_x, o_y, x3, y3)
  area3 <- calculate_triangle_area(x1, y1, x2, y2, o_x, o_y)
  
  # If the sum of the areas of the sub-triangles is equal to the total area, the opponent is inside the triangle
  trisum = (area1 + area2 + area3)
  diff=trisum - total_area < 0.1
  return(diff)
}

is_opponent_inside_triangle <- function(o_x, o_y,shooter_x,shooter_y) {
  # Coordinates of the three vertices of the triangle
  #goalkeeper_x=117
  #goalkeeper_y=40
  x1 <- 120
  y1 <- 44
  x2 <- 120
  y2 <- 36
  x3 <- shooter_x
  y3 <- shooter_y
  
  # Calculate the area of the entire triangle
  total_area <- calculate_triangle_area(x1, y1, x2, y2, x3, y3)
  
  # Calculate the area of three sub-triangles formed by the goalkeeper and two vertices of the triangle
  area1 <- calculate_triangle_area(o_x, o_y, x2, y2, x3, y3)
  area2 <- calculate_triangle_area(x1, y1, o_x, o_y, x3, y3)
  area3 <- calculate_triangle_area(x1, y1, x2, y2, o_x, o_y)
  
  # If the sum of the areas of the sub-triangles is equal to the total area, the goalkeeper is inside the triangle
  trisum = (area1 + area2 + area3)
  diff=trisum - total_area < 0.1
  return(diff)
}



oppInTri <- function (x1,y1,x,y) {
  x2=120
  y2=36
  x3=120
  y3=44
  # Calculate area of triangle ABC
  A = area(x1, y1, x2, y2, x3, y3)
  # Calculate area of triangle PBC
  A1 = area (x, y, x2, y2, x3, y3)
  # Calculate area of triangle PAC
  A2 = area (x1, y1, x, y, x3, y3)
  # Calculate area of triangle PAB
  A3 = area (x1, y1, x2, y2, x, y)
  retval=ifelse(A == A1 + A2 + A3,T,F)
  return(retval)
}

area <- function(x1,y1,x2,y2,x3,y3) {
  val=abs((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2.0)
  return(val)
}

plotSingleShotTri <-  function(shot) {
  g1=c(120, 36)
  g2=c(120,44)
  x=c(shot[1],g1[1],g2[1],shot[1])
  y=c(shot[2],g1[2],g2[2],shot[2])
  resshot=data.frame(x,y)
  colnames(resshot)=c("sx","sy")
  return(resshot)
}

plotSingleShotWithFF <-  function(shot_ff) {
  g1=c(120,36)
  g2=c(120,44)
  t=unlist(shot_ff$location)
  x=c(t[1])
  y=c(t[2])
  name=shot_ff$player.name
  thisff=shot_ff$shot.freeze_frame %>% as.data.frame()
  thisff=thisff %>% select(-c(position.id,position.name))
  
  fframe=as.data.frame(matrix(ncol = 5,nrow=dim(thisff)+1))
  colnames(fframe)=c("x","y","name","team","shooter")
  fframe[1,1]=x
  fframe[1,2]=y
  fframe[1,3]=name
  fframe[1,4]=team
  fframe[1,5]=5
  for(i in 1:dim(thisff)[1]) {
    #t=unlist(thisff[i-1,1])
    fframe[i+1,1]=thisff[i,1][[1]][1]
    fframe[i+1,2]=thisff[i,1][[1]][2]
    fframe[i+1,3]=thisff[i,4]
    fframe[i+1,4]=thisff[i,2]
    fframe[i+1,5]=ifelse(thisff[i,2]==T,2,1)
  }
  return(fframe)
}

ggplotshootertri <- function(df) {
  g <- ggplot(df) +
    annotate_pitch(
      dimensions = pitch_statsbomb,
      colour = "black",
      fill = "white")+
    geom_point(data=df,aes(x = x, y = y, color=as.factor(shooter)), size = 3) +
    theme_pitch() +
    direction_label() +
    ggtitle("Simple passmap Taylor", 
            "ggsoccer example")+
    coord_flip(xlim = c(75, 121)) +
    scale_y_reverse() +
    geom_text(data=df,aes(x=x,y=y,label=name), angle=30,size=2.5,vjust=-1)
  
  return(g)
}

mxangle <- function(a){
  b=c(120,36)
  c=c(120,44)
  ab = as.matrix(c((b[1]-a[1]),(b[2]-a[2])))
  ac = as.matrix(c((c[1]-a[1]),(c[2]-a[2])))
  # dot prod
  dv = dot(ab,ac)
  # norm
  nab=norm(ab,type="2")
  nac=norm(ac,type="2")
  # angel in rad
  ang=acos(dv/(nab*nac))
  retval = rad2deg(ang)
  return(retval)
}

mangle <- function(S_x, S_y) {
  # Define stolper
  G1 <- c(120, 36)
  G2 <- c(120, 44)
  
  # Calculate the midpoint M of G1 and G2
  M <- c(mean(c(G1[1], G2[1])), mean(c(G1[2], G2[2])))
  
  # Vector from S to M
  vector_SM <- c(M[1] - S_x, M[2] - S_y)
  
  reference_vector <- c(0, -1)
  # Calculate the angle using the dot product
  angle_cosine <- sum(vector_SM * reference_vector) / (sqrt(sum(vector_SM^2)) * sqrt(sum(reference_vector^2)))
  
  # Convert cosine value to angle in degrees
  angle_degrees <- acos(angle_cosine) * (180 / pi)
  return(angle_degrees)
}

disttogoal <- function(a){
  m=c(120,40)
  am = as.matrix(c((m[1]-a[1]),(m[2]-a[2])))
  # dot prod
  # norm
  nam=norm(am,type="2")
  return(nam)
}

rad2deg <- function(rad) {(rad * 180) / (pi)}
