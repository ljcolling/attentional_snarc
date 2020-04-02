library(reticulate)

use_python('/usr/local/bin/python3')


source_python(here::here("helper_functions/readResponseData.py"),convert = T)

ReadFicherData.SecondaryOnly<-function(file){
  secondaryData.temp = read_secondary_data(file)
  the.names = secondaryData.temp[[length(secondaryData.temp)]]
  secondaryData = tibble(
    handedness = secondaryData.temp[[which(the.names == "handedness")]],
    mathTest = secondaryData.temp[[which(the.names == "mathTest")]],
    language = secondaryData.temp[[which(the.names == "language")]],
    age = secondaryData.temp[[which(the.names == "age")]],
    amas = secondaryData.temp[[which(the.names == "amas")]])
  
  return(list(secondaryData = secondaryData))
}


ReadFischerData<-function(file,hasSecondary){
  subjectData = read_response_data(file,hasSecondary)
  
  
  subjectData[[length(subjectData)]] -> the.names
  
  
  hasEye  = subjectData[[which(the.names == "hasEye")]]
  
  # make response data tibble
  responseData = tibble(trial = 1:800, 
                             RT =  subjectData[[which(the.names == "RT")]] %>% unlist(),
                             thekey = subjectData[[which(the.names == "thekey")]] %>% unlist(),
                             correct  = subjectData[[which(the.names == "correct")]] %>% unlist(),
                             targetLocation  = subjectData[[which(the.names == "targetLocation")]] %>% unlist(),
                             cue  = subjectData[[which(the.names == "cue")]] %>% unlist(),
                             delayDur = subjectData[[which(the.names == "delayDur")]] %>% unlist())
  
  # Double check that correct trials have been marked as correct
  responseData %>% # fix weird error where hancock lab has "f" instead of "space"
    mutate(thekey = case_when(thekey == "nr" ~ "nr", thekey == "space" ~ "space", TRUE ~ "space")) -> responseData
  
  
  # Fix error where on some trials the RT is negative
  responseData %>% mutate(RT = ifelse(RT < 0,0,RT)) -> responseData

  
  responseData %>% 
    mutate(correct = 
             case_when(targetLocation == "c" & thekey == "nr" ~ 1, 
                       targetLocation == "c" & thekey == "space" ~ 0, 
                       targetLocation != "c" & thekey == "space" ~ 1, 
                       targetLocation != "c" & thekey == "nr" ~ 0,
                       targetLocation != "c" & RT == 0 ~ 0)) -> responseData
  # make the secondary data tibble
  if(hasSecondary == 1){
    # make the tibble from the data struct
    secondaryData = tibble(
      handedness =  subjectData[[which(the.names == "handedness")]],
      mathTest =  subjectData[[which(the.names == "mathTest")]],
      language =  subjectData[[which(the.names == "language")]],
      age =  subjectData[[which(the.names == "age")]],
      amas = subjectData[[which(the.names == "amas")]]
    )
  } else {
    # make the tibble from the secondary data file
  }
  
  eyeTracker = tibble(trial = 1:800, 
                      Reject = rep(0,800),
                      maxHorzDeviation = rep(0,800))
  
  # make the eye tracker tibb;e
  if(hasEye == 1){
    # make the tibble from the data struct
    eyeTracker.intermediate = tibble(
      trial = subjectData[[which(the.names == "ntrial")]],
      position = subjectData[[which(the.names == "pos")]],
      frame = subjectData[[which(the.names == "frame")]] %>% unlist(),
      xloc = subjectData[[which(the.names == "xloc")]] %>% unlist(),
      yloc = subjectData[[which(the.names == "yloc")]] %>% unlist())
    
    eyeParams = tibble(
      xcenter = subjectData[[which(the.names == "xcenter")]] %>% unlist(),
      ycenter = subjectData[[which(the.names == "ycenter")]] %>% unlist(),
      pixPerDegWidth = subjectData[[which(the.names == "pixPerDegWidth")]] %>% unlist(),
      pixPerDegHeight = subjectData[[which(the.names == "pixPerDegHeight")]] %>% unlist())
  
    
    
    timePoints = c("initDisplay", "digitDisplay", "variableDelay")
    timePointReject = timePoints[2:3]
    maxXDeviation = 2
    
    eyeTracker.intermediate %>% 
      filter(xloc > 0)  %>%
      filter(position %in% timePointReject) %>%
      mutate(Xoffset = abs(eyeParams$xcenter - xloc),
                Yoffset = abs(eyeParams$ycenter - yloc)) %>% group_by(trial) %>% 
      summarise(Xoffset = max(Xoffset), Yoffset = max(Yoffset)) %>%
      mutate(Xdeviation = Xoffset/eyeParams$pixPerDegWidth,
             Ydeviation = Yoffset/eyeParams$pixPerDegHeight) -> eyeTracker.intermediate
      
    eyeTracker %>% full_join(eyeTracker.intermediate, by = "trial") %>% 
      mutate(maxHorzDeviation = ifelse(is.na(Xdeviation), maxHorzDeviation,Xdeviation)) %>% 
      mutate(Reject = ifelse(maxHorzDeviation > maxXDeviation, 1, 0)) %>% 
      select(trial,Reject,maxHorzDeviation) -> eyeTrackerData
  } else{
    eyeTracker -> eyeTrackerData
  }
  
  if(hasSecondary == 1){
  return(list(responseData = responseData, eyeTrackerData = eyeTrackerData, secondaryData = secondaryData, hasEye = hasEye))}
  else {
    return(list(responseData = responseData, eyeTrackerData = eyeTrackerData, hasEye = hasEye))}
}
  
  
  








