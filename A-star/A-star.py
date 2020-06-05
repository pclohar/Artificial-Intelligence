#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 18 20:38:25 2020

@author: pallavilohar
"""
import sys
import math as m
import numpy as np
import collections as c
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Function to read and store locations.txt and connections.txt files
def createDataStructure(locationFile, connectionFile, visited):
    locationsFile = open(locationFile,"r+")
    locDict = {}
    locLine = locationsFile.readline()
    
    # Read the locations file
    while locLine != "END":
        key, x1, y1 = locLine.split(' ', 2)
        coordinateList = [int(x1), int(y1)]
        ax.scatter(int(x1), int(y1), color = "black", marker = 'o')
        ax.annotate(key, (int(x1), int(y1)))
        locDict[key] = coordinateList
        visited[key] = []
        locLine = locationsFile.readline()    
    locationsFile.close()
    
    connectionsFile = open(connectionFile,"r+")
    conDict = {}
    connectionsLine = connectionsFile.readline()
    
    # Read the connections file
    while connectionsLine != "END":
        key, conLen, conn = connectionsLine.split(' ', 2)
        conLen = int(conLen)
        conn = conn.replace('\n','')
        connections = conn.split(' ', conLen)
        conDict[key] = {}
        
        for i in range(conLen):
            x1 = locDict[key][0]
            y1 = locDict[key][1]
            x2 = locDict[connections[i]][0]
            y2 = locDict[connections[i]][1]
            
            dist = round(m.sqrt(pow((abs(x2 - x1)),2) + pow((abs(y2 - y1)),2)),3)
            conDict[key].update({connections[i] : dist})
        
            ax.plot([x1,x2],[y1, y2],color="black")
            
        conDict[key] = dict(c.OrderedDict(sorted(conDict[key].items(), key = lambda t: t[0], reverse = False)))
        
        connectionsLine = connectionsFile.readline()
    
    connectionsFile.close()
    
    

    return locDict, conDict, visited    


def calculateEstimatedDistance():
    x_end = locDict[dest][0]
    y_end = locDict[dest][1]
    

    estDist = {}
    
    for x in locDict.keys():
        x1=locDict[x][0]
        y1=locDict[x][1]
        estDist[x]=round(m.sqrt(pow((x_end - x1),2) + pow((y_end - y1),2)),3)
    return estDist


def markVisited(node1,node2):
    tempList1 = visited[node1]
    tempList1.append(node2)
    visited[node1] = tempList1
    
    if node2!=None:
        tempList2 = visited[node2]
        tempList2.append(node1)
        visited[node2] = tempList2


def derivePath(source, dest, nodeExp, finalP):
    if(dest == source):
        finalP.append(dest)
        return finalP[::-1]
    else:
        finalP.append(dest)
        finalP = derivePath(source, nodeExp[dest], nodeExp, finalP)
        return finalP
  
def printStepByStep(openList,possibleCities, firstElementKey):
    print()
    tempOpenList = {}
    for i in openList.keys():
        tempOpenList[i] = openList[i]['total_dist']
    
    print('City selected : ', firstElementKey)
    if possibleCities: 
        print('Possible cities from where to travel:', str(possibleCities)[1:-1].replace(',','').replace("'",''))
        print('Cities at the end of possible paths:', (str(tempOpenList)[1:-1].replace(',',') ').replace("'",'').replace(': ',' ('))+')')
      
def printPath(path):
    pathLen = len(path)
    total = 0
    for i in range(0, pathLen-1):
        p[0].append(locDict[path[i]][0])
        p[1].append(locDict[path[i]][1])
        if heuristicChosen == '1':
            print(path[i],'to' ,path[i+1], 'length',str(conDict[path[i]][path[i+1]])+',')
            total += conDict[path[i]][path[i+1]]
            
        if heuristicChosen == '2':
            print(path[i],'to' ,path[i+1], 'length',str(1)+',')
            total += 1
    p[0].append(locDict[path[i]][0])
    p[1].append(locDict[path[i]][1])
    p[0].append(locDict[dest][0])
    p[1].append(locDict[dest][1])    
    print("Total path length:", round(total,3) )
    print()


def straightLineHeuristic(firstElementKey, costCurrentNode, i):
    return round(estDist[i]+conDict[firstElementKey][i]+costCurrentNode,3)

def fewerCitiesHeuristic(costCurrentNode):
    return 1+costCurrentNode


def animate(num, data, line):
    line.set_data(data[..., :num])
    return line,


def findPath(dest, visited, openList, nodeExp):
    
    # Sort open w.r.t total_dist
    sortedOpenList = sorted(openList.items(), key=lambda x: x[1]['total_dist'], reverse=False)
    openList = c.OrderedDict(sortedOpenList)
    
    if openList == {}:
        print('Unfortunately, there is no path excluding the given cities!')
        return 

    firstElementKey = list(openList.keys())[0]
    firstElementValue = openList[firstElementKey]
    costCurrentNode = firstElementValue['cost']
    
    # Add the expanded node to nodeExp
    possibleCities = {}
    nodeExp[firstElementKey] = firstElementValue['parentnode']


    if firstElementKey == dest:
        return
    
    markVisited(firstElementKey, firstElementValue['parentnode'])
    
    del openList[firstElementKey]
    possibleCities = []    
    for i in conDict[firstElementKey]:
        if i not in visited[firstElementKey] and i not in nodeExp.keys() and i not in excludeCities:
            totalDist = 0
            cost = 0
            possibleCities.append(i)
            if heuristicChosen == '1':
                
                totalDist = straightLineHeuristic(firstElementKey, costCurrentNode, i)
                cost = costCurrentNode+conDict[firstElementKey][i]
                
            if heuristicChosen == '2':
                
                totalDist = fewerCitiesHeuristic(costCurrentNode)
                cost = costCurrentNode + 1
                
            if i in openList.keys():
                
                if openList[i]['total_dist'] > totalDist:
                    openList[i] = {'parentnode': firstElementKey, 'total_dist': totalDist,'est_dist': estDist[i], 'cost':cost}
                    
            else:
                openList[i] = {'parentnode': firstElementKey, 'total_dist': totalDist,'est_dist': estDist[i], 'cost':cost}
    
     
    if stepByStep =='Y':
        printStepByStep(openList,possibleCities,firstElementKey)
    
    findPath(dest, visited, openList, nodeExp)


p = [[],[]]
path = []
visited = {}
nodeExp = {}
finalPath = []
excludeCities = []
fig, ax = plt.subplots()

print("Input the source and destination locations : ")
source, dest = input().split()
print()

print("Do you want to exclude some cities? [Y]es or [N]o")
exDecision = input()
print()

if exDecision == 'Y':
    print("Input the cities to be excluded seperated by a space(' '): ")
    excludeCities = input().split(' ')

print("Do you want a step by step Output? [Y]es or [N]o")
stepByStep = input()
print()

print("Choose the Heuristic: \n 1.Straight Line Distance(Input 1) \n 2.Fewest Cities(Input 2)")
heuristicChosen=input()
print()

locDict, conDict, visited = createDataStructure(sys.argv[1], sys.argv[2], visited)
estDist=calculateEstimatedDistance()

if source in locDict.keys() and dest in locDict.keys():
    count = 0

    openList = {source:{'parentnode':None, 'total_dist':estDist[source], 'cost':0}}
 
    findPath(dest, visited, openList, nodeExp)
    
    if dest in nodeExp.keys():
        print()
        pathF = derivePath(source, dest, nodeExp, finalPath)
        printPath(pathF)
        startNode = 0

        d = np.array([p[0], p[1]], dtype=np.int64) 

        lenP = len(d[0])+1
        l, = plt.plot([], [], 'r-')
        myAnimation = animation.FuncAnimation(fig, animate,lenP, fargs=(d, l),interval=500, blit=True)
        plt.show()
    

else:
    print("The Source or Destination locations does not exist !")