import scipy.io as sio
import numpy as np

def read_response_data(file):
  mat_contents = sio.loadmat(file)
  responseStruct = mat_contents['responseStruct']

  RT = responseStruct['RT'].tolist()
  thekey = responseStruct['thekey'].tolist()
  correct = responseStruct['correct'].tolist()
  targetLocation = responseStruct['targetLocation'].tolist()
  cue = responseStruct['cue'].tolist()
  delayDur = responseStruct['delayDur'].tolist()

  RTs =  list()
  thekeys = list()
  corrects = list()
  targetLocations = list()
  cues = list()
  delayDurs = list()
  for i in range(800):
    RTs.append(1.0 * RT[0][i][0][0])
  pressedkey = thekey[0][i]
  if pressedkey.size == 0:
    pressedkey = 'nr'
  else:
    pressedkey = pressedkey[0]
  
  thekeys.append(pressedkey)
  corrects.append(correct[0][i][0][0])
  targetLocations.append(targetLocation[0][i][0][0])
  cues.append(cue[0][i][0][0])
  delayDurs.append(delayDur[0][i][0][0])
  
  responseStruct_toWrite = np.stack((RTs,thekeys,corrects,targetLocations,cues,delayDurs),axis = 2)  
  return responseStruct_toWrite

