
# coding: utf-8

# Distribuição de produtos de 10 armazens para 25 lojas 
# ===========================================================================================
# Otimização de Planejamento
# Manoel de Souza e Silva Neto
# Data: 24/Mai/2017
# Versão: 0.1

# Definições:
# Linhas = Lojas
# Colunas = Armazens

# Baseado na descrição em: https://lethain.com/genetic-algorithms-cool-name-damn-simple/


# Dados do problema apresentado
# ===========================================================================================
# Custo do transporte por rota armazem (coluna) x loja (linha)
custoTransporte = [ 
[315.00, 299.00, 131.00, 314.00, 151.00, 317.00, 314.00, 300.00, 126.00, 284.00], 
[312.00, 267.00, 125.00, 288.00, 142.00, 277.00, 285.00, 307.00, 129.00, 270.00], 
[309.00, 221.00, 113.00, 266.00, 295.00, 142.00, 258.00, 240.00, 139.00, 271.00], 
[100.00, 281.00, 280.00, 116.00, 245.00, 273.00, 169.00, 130.00, 317.00, 295.00], 
[144.00, 102.00, 134.00, 169.00, 155.00, 114.00, 306.00, 310.00, 295.00, 167.00], 
[308.00, 315.00, 124.00, 305.00, 152.00, 306.00, 286.00, 307.00, 128.00, 301.00], 
[281.00, 298.00, 105.00, 295.00, 298.00, 278.00, 114.00, 314.00, 249.00, 320.00], 
[110.00, 258.00, 117.00, 313.00, 143.00, 104.00, 128.00, 126.00, 314.00, 148.00], 
[109.00, 240.00, 116.00, 260.00, 140.00, 141.00, 108.00, 249.00, 112.00, 113.00], 
[128.00, 134.00, 255.00, 106.00, 309.00, 307.00, 259.00, 274.00, 111.00, 320.00], 
[310.00, 290.00, 125.00, 305.00, 134.00, 289.00, 292.00, 304.00, 152.00, 297.00], 
[306.00, 196.00, 132.00, 313.00, 149.00, 312.00, 315.00, 296.00, 127.00, 313.00], 
[245.00, 169.00, 151.00, 152.00, 312.00, 137.00, 128.00, 123.00, 120.00, 285.00], 
[273.00, 125.00, 116.00, 115.00, 116.00, 294.00, 296.00, 151.00, 145.00, 145.00], 
[114.00, 112.00, 246.00, 122.00, 120.00, 119.00, 286.00, 261.00, 157.00, 154.00], 
[286.00, 295.00, 114.00, 291.00, 142.00, 286.00, 311.00, 284.00, 129.00, 281.00], 
[107.00, 108.00, 282.00, 258.00, 301.00, 286.00, 266.00, 140.00, 261.00, 252.00], 
[160.00, 285.00, 305.00, 292.00, 152.00, 140.00, 281.00, 159.00, 295.00, 117.00], 
[248.00, 250.00, 133.00, 265.00, 126.00, 272.00, 290.00, 300.00, 133.00, 269.00], 
[126.00, 297.00, 273.00, 264.00, 137.00, 314.00, 142.00, 245.00, 118.00, 279.00], 
[261.00, 253.00, 146.00, 109.00, 247.00, 278.00, 155.00, 103.00, 130.00, 145.00], 
[281.00, 314.00, 293.00, 170.00, 298.00, 111.00, 300.00, 145.00, 275.00, 250.00], 
[293.00, 101.00, 117.00, 130.00, 294.00, 301.00, 102.00, 126.00, 165.00, 134.00], 
[126.00, 129.00, 252.00, 299.00, 110.00, 163.00, 315.00, 282.00, 260.00, 252.00], 
[242.00, 121.00, 103.00, 167.00, 135.00, 127.00, 117.00, 106.00, 101.00, 283.00], 
]

# quantidade de produto transportado em cada caminhão
capacidadeCaminhao = 6    

# demanda de cada loja (límite mínimo de entregas)
demandaLojas = [ 201, 120, 180, 120, 110, 220, 150, 160, 120, 160, 190, 210, 140, 141, 110, 230, 161, 130, 170, 131, 180, 120, 160, 140, 170 ]

# capacidade de cada armazem (limite máximo de entrega)
capacidadeArmazens = [ 450, 430, 370, 320, 250, 490, 470, 450, 300, 470 ]


# Carrega bibliotecas
# ===========================================================================================
import numpy as np
import random as rnd


# Parâmetros de execução do algorítmo evolucionário
# ===========================================================================================
populationSize = 10
generationsNumber = 50


# Manipulação dos conjuntos
# ===========================================================================================
# Identifica o índice da n-ésima ocorrência de um item em listContents
def nth_item(n, item, listContents):
    i = ii = 0
    while i < len(listContents):
        if (listContents[i] == item):
            ii += 1
            if (ii == n):
                return i
                break
        i += 1

# Conta as ocorrências de um item em listContents
def count_item(item, listContents):
    i = ii = 0
    while i < len(listContents):
        if (listContents[i] == item):
            ii += 1
        i += 1
    return ii

# Cálcula o fitness de cada indivíduo
def fitness(chromosome):
    return np.sum(np.multiply(np.ceil(np.array(chromosome) / capacidadeCaminhao), custoTransporte))

# Gera aleatóriamente cada indivíduo e cálcula o fitness
def GenerateIndividual():
    newChromosome = np.zeros((len(demandaLojas), len(capacidadeArmazens)))
    isIndividualOk = False
    isDemandOk = [False] * len(demandaLojas)
    isCapacityOk = [True] * len(capacidadeArmazens)

    while (isIndividualOk != True):
        randomRow = nth_item(rnd.randint(1, count_item(False, isDemandOk)), False, isDemandOk)
        randomCol = nth_item(rnd.randint(1, count_item(True, isCapacityOk)), True, isCapacityOk)

        restDem = int(demandaLojas[randomRow] - np.sum(newChromosome[randomRow,:]))
        restCap = int(capacidadeArmazens[randomCol] - np.sum(newChromosome[:,randomCol]))

        if ((restDem == 0)):
            isDemandOk[randomRow] = True
            write = "No"
        if (restCap == 0):
            isCapacityOk[randomCol] = False
            write = "No"
        if ((restDem != 0) and (restCap != 0)):
            write = "Yes"
            randomValue = rnd.randint(0, min(restCap, restDem))
            newChromosome[randomRow,randomCol] += randomValue
        if (all(isDemandOk) == True):  
            isIndividualOk == True
            break
  
    return [newChromosome, fitness(newChromosome)]  
# Descomente para testar
# testIndividual = GenerateIndividual()

# Gera uma coleção (lista) de indivíduos. Caso não seja apresentado argumento count, assume 2 indivíduos   
def GeneratePopulation(count = 2):
    return [ GenerateIndividual() for x in range(count) ]
# Descomente para testar
# testPopulation = GeneratePopulation()    


# Cálculos para a população
# ===========================================================================================
# Cálcula a média de fitness de uma dada população (pop)
def populationAvg(pop):
    return np.mean([pop[i][1] for i in range(len(pop))])

# Cálcula o maior valor absoluto (menor fitness) de uma dada população (pop)   
def populationMax(pop):
    return np.amax([pop[i][1] for i in range(len(pop))])

# Cálcula o menor valor absoluto (maior fitness) de uma dada população (pop)   
def populationMin(pop):
    return np.amin([pop[i][1] for i in range(len(pop))])

# Cálcula o indivíduo de maior valor absoluto (menor fitness) de uma dada população (pop)
def populationMaxIndiv(pop):
    return np.argmax([pop[i][1] for i in range(len(pop))])

# Cálcula o indivíduo de menor valor absoluto (maior fitness) de uma dada população (pop)
def populationMinIndiv(pop):
    return np.argmin([pop[i][1] for i in range(len(pop))])


# Aplica processo evolucionário
# ===========================================================================================
# NOTA: Pendente Cruzamento e mutação
def evolve(initPop, genNum = 5):
    generationsList = []
    for gen in range(genNum):
        
        if (gen > 0):
            LastGeneration = generationsList[gen - 1]
            bestIndividualLastGeneration = LastGeneration[0][LastGeneration[1]]
                        
            currentPop = GeneratePopulation(populationSize - 1)
            currentPop.append(bestIndividualLastGeneration)

            currentGeneration = [currentPop, populationMinIndiv(currentPop), populationMin(currentPop)]
        else:
            currentGeneration = [initPop, populationMinIndiv(initPop), populationMin(initPop)]
            
        generationsList.append(currentGeneration)  
        print("Generation " + str(gen) + ": " + "best " + str(currentGeneration[1]) + " = " + str(currentGeneration[2]))
        
    return currentGeneration[0][currentGeneration[1]]


def main():   
    initialPopulation = GeneratePopulation(populationSize)  
    BestDNA = evolve(initialPopulation, generationsNumber)
   
    return np.array(BestDNA[0])

main()

