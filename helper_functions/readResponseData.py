import scipy.io as sio
import numpy as np

# Fix issue if age is empty

def read_response_data(file,hasSecondary):
    hasSecondary = int(hasSecondary)
    mat_contents = sio.loadmat(file)
    responseStruct = mat_contents['responseStruct']

    RT = responseStruct['RT'].tolist()
    thekey = responseStruct['thekey'].tolist()
    correct = responseStruct['correct'].tolist()
    targetLocation = responseStruct['targetLocation'].tolist()
    cue = responseStruct['cue'].tolist()
    delayDur = responseStruct['delayDur'].tolist()
    
    if hasSecondary == 1:
      amas = sum(mat_contents['aMAS'])[0]
      handedness = (sum(mat_contents['handed'])[0] ) + (sum(mat_contents['handed'])[1] ) + (sum(mat_contents['handed'])[2] )
      mathTest = mat_contents['mathTestScore'][0][0]
      if mat_contents['age'].size == 0:
        age = 0
      else:
        age = mat_contents['age'][0][0]
        
      language = mat_contents['lang'][0][0]
      
    
      
    
      
    


        

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
        
    if 'eyeTrackingStruct' in mat_contents.keys():
        eyetracking = mat_contents['eyeTrackingStruct']
        pos = list()
        frame = list()
        xloc = list()
        yloc = list()
        ntrial = list()
        hasEye = 1
        xcenter = mat_contents['params']['xCenter'][0][0][0][0]
        ycenter = mat_contents['params']['yCenter'][0][0][0][0]
        pixPerDegWidth = mat_contents['params']['pixPerDegWidth'][0][0][0][0]
        pixPerDegHeight = mat_contents['params']['pixPerDegHeight'][0][0][0][0]    
        for i in range(800):
            if eyetracking[0][i][0].size != 0:
                trial = i + 1
                nbreaks = int(eyetracking[0][i][0].size / 4)
                for t in range(nbreaks):
                    pos.append(eyetracking[0][i][0][t][0][0])
                    frame.append(eyetracking[0][i][0][t][1][0])
                    xloc.append(eyetracking[0][i][0][t][2][0])
                    yloc.append(eyetracking[0][i][0][t][3][0])
                    ntrial.append(trial)
    else:
        hasEye = 0
    


    #print("has secondary = ", hasSecondary)
    
    if hasSecondary == 1 and hasEye == 0:
        names = ['hasSecondary','hasEye','RT', 'thekey', 'correct', 'targetLocation','cue', 'delayDur', 'amas', 'handedness', 'mathTest', 'age', 'language', 'names']
        return [hasSecondary,hasEye,RTs, thekeys, corrects, targetLocations,cues, delayDurs, amas, handedness, mathTest, age, language, names]
    
    if hasSecondary == 0 and hasEye == 0:
        names = ['hasSecondary','hasEye','RT', 'thekey', 'correct', 'targetLocation','cue', 'delayDur', 'names']
        return [hasSecondary,hasEye,RTs, thekeys, corrects, targetLocations,cues, delayDurs, names ]
        
    if hasSecondary == 1 and hasEye == 1:
        names = ['hasSecondary','hasEye','RT', 'thekey', 'correct', 'targetLocation','cue', 'delayDur', 'amas', 'handedness', 'mathTest', 'age', 'language', 'ntrial', 'pos', 'frame', 'xloc', 'yloc','xcenter','ycenter','pixPerDegWidth','pixPerDegHeight', 'names']
        return [hasSecondary,hasEye,RTs, thekeys, corrects, targetLocations,cues, delayDurs, amas, handedness, mathTest, age, language, ntrial, pos, frame, xloc, yloc,xcenter,ycenter,pixPerDegWidth,pixPerDegHeight, names]
    
    if hasSecondary == 0 and hasEye == 1:
        names = ['hasSecondary','hasEye','RT', 'thekey', 'correct', 'targetLocation','cue', 'delayDur', 'ntrial', 'pos', 'frame', 'xloc', 'yloc','xcenter','ycenter','pixPerDegWidth','pixPerDegHeight', 'names']
        return [hasSecondary,hasEye,RTs, thekeys, corrects, targetLocations,cues, delayDurs,ntrial, pos, frame, xloc, yloc,xcenter,ycenter,pixPerDegWidth,pixPerDegHeight, names]

def read_secondary_data(file):
    mat_contents = sio.loadmat(file)
    amas = sum(mat_contents['aMAS'])[0]
    handedness = (sum(mat_contents['handed'])[0]) + (sum(mat_contents['handed'])[1]) + (sum(mat_contents['handed'])[2])
    mathTest = mat_contents['mathTestScore'][0][0]
    if mat_contents['age'].size == 0:
        age = 0
    else:
        age = mat_contents['age'][0][0]
        
    language = mat_contents['lang'][0][0]
    names = ['amas','handedness','mathTest','age','language','names']
    return [amas,handedness,mathTest,age,language,names]
