#!/usr/bin/python3
# -*- coding: utf-8 -*-

print('''
      Script: collect_speaches_camara.py
      Author: Manoel de Souza
      E-Mail: msouza.rj@gmail.com      
     Version: 1.1.1
        Date: 26-Fev-2017
       
   Objective: Coletar todos os discursos parlamentares da Câmara dos deputados 
              em determinado período e exporta em formato CSV (delimitador: "|")
       Usage: Ajustar parâmetros: Arquivo de saída, Data Inicio e Data fim
Observations: Desenvolvido utilizando a distribuição Anaconda Python 3-4.3.0
''')
print('=' * 80)

'''
Required Libraries:
===================
bs4     pip install bs4 (or conda install bs4)
lxml    pip install lxml (or conda install lxml)

Version History:
================
 0.1.1 : 21/Fev/2017 : First prototype
 1.0.2 : 23/Fev/2017 : Minimum functional program
 1.1.1 : 26/Fev/2017 : Use of command line arguments
'''

import urllib.request
import urllib.parse
import bs4 as bs
import re
import csv
import socket
import argparse

# Entry parameters (adjust as needed):
# ====================================
#Output_File    = 'Discursos_Camara_dos-Deputados-2015.csv'  # make sure to include .CSV extension
#Start_Date     = '01/01/2017'     # DD/MM/YYYY format
#End_Date       = '30/04/2017'     # DD/MM/YYYY format
Max_Retries    = 20               # number of attempts
Time_out       = 10               # seconds to time-out
Load_to_Memory = True
Save_to_File   = True

parser = argparse.ArgumentParser()
parser.add_argument('--output', default="2016.csv", help='Output filename with .cvs extension')
parser.add_argument('--append', default="True", help='Prevent file from being overwriten')
parser.add_argument('--uf', default="", help='Select representants of specific state')
parser.add_argument('--party', default="", help='Select representants of specific party')
parser.add_argument('--start', default="01/01/2017" , help='Start date in DD/MM/YYYY format')
parser.add_argument('--end', default="31/12/2017" , help='End date in DD/MM/YYYY format')

args = parser.parse_args()    
Output_File = args.output
Append_File = args.append
Uf = args.uf
Party = args.party
Start_Date = args.start
End_Date = args.end


def main():
    global Total_Pages
    global Headers
    global Url
    global Master_List
    global f

    Rows_Page = 200
    Current_Page = 1
    Total_Pages = 10000

    Master_List = []
    Number_of_Speaches = 0
    List_of_Speaches = []
    Url = 'http://www.camara.leg.br/internet/sitaqweb/'
    Main_Url = Url + 'resultadoPesquisaDiscursos.asp'    
    Headers = {}
    Headers['User-Agent'] = "Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.27 Safari/537.17"
    socket.setdefaulttimeout(Time_out)

    if Append_File == "True": f = open(Output_File, 'a')
    if Append_File == "False": f = open(Output_File, 'w')

    while Current_Page <= Total_Pages:
        Main_Page_Return = Collect_Main_Page(Current_Page, Main_Url, Rows_Page)
        Current_Page += 1
    f.close()


def Collect_Main_Page(Local_Current_Page, Local_Url, Local_Rows):
    global Master_List
    global Total_Pages
    global Number_of_Speaches
    if Local_Current_Page > 1:
        Values = {
            'txIndexacao':'',
            'CurrentPage':Local_Current_Page,
            'BasePesq':'plenario',
            'txOrador':'',
            'txPartido':Party,
            'dtInicio':Start_Date,
            'dtFim':End_Date,
            'txUF':Uf,
            'txSessao':'',
            'listaTipoSessao':'',
            'listaTipoInterv':'',
            'inFalaPres':'',
            'listaTipoFala':'',
            'listaFaseSessao':'',
            'txAparteante':'',
            'listaEtapa':'',
            'CampoOrdenacao':'dtSessao',
            'TipoOrdenacao':'ASC',
            'PageSize':Local_Rows,
            'txTexto':'',
            'txSumario':'' }
    else:
        Values = {
            'txOrador':'',
            'txPartido':Party,
            'txUF':Uf,
            'dtInicio':Start_Date,
            'dtFim':End_Date,
            'txTexto':'',
            'txSumario':'',
            'basePesq':'plenario',
            'CampoOrdenacao':'dtSessao',
            'PageSize':Local_Rows,
            'TipoOrdenacao':'ASC',
            'btnPesq':'Pesquisar' }

    Attempt = 0
    while Attempt <= Max_Retries:
        try:
            respData = urllib.request.urlopen(urllib.request.Request(Local_Url, urllib.parse.urlencode(Values).encode('utf-8'), headers = Headers)).read()
            Speach_Links = re.findall(r'TextoHTML\.asp\?etapa=5.*?txEtapa=', str(respData), re.MULTILINE)

            if Local_Current_Page == 1:
                Total_Pages = int(re.findall('\d+', re.findall('"TotalPages"\svalue="\d+"', str(respData), re.MULTILINE)[0])[0])
                Number_of_Speaches = int(re.findall('\d+', re.findall('"TotalRecords"\svalue="\d+"', str(respData), re.MULTILINE)[0])[0])
            
            print('-' * 80)
            print('Scrapping Page:', Local_Current_Page, 'of', Total_Pages)
            print('Total Speaches:', Number_of_Speaches) 
            print('Speaches in this page:', len(Speach_Links))
            print('-' * 80)

            Page_Soup = bs.BeautifulSoup(respData, 'lxml')
            Target_Section = Page_Soup.find("tbody", class_="coresAlternadas")
            Table_Rows = Target_Section.find_all('tr')

            i = 0
            for tr in Table_Rows:
                td = tr.find_all('td')
                if len(td) == 8:
                    Date = str(td[0].text)
                    Session = str(re.sub(r'  ', '', td[1].text))
                    Phase = str(re.findall(r'.*',re.sub(r'  ', '', td[2].text))[0])
                    Speaker = str(re.findall(r'.*',re.sub(r'  ', '', td[5].text))[2])
                    Hour = str(td[6].text)
                    Diary = str(re.findall(r'\S+\s+\S+',re.sub(r'  ', '', td[7].text), re.MULTILINE)[0])

                    Clean_Speach_Link = re.sub(r'\\r\\n', '', Speach_Links[i])
                    Clean_Speach_Link = re.sub(r' ', '', Clean_Speach_Link)
                    Clean_Speach_Link = Url + re.sub(r'amp;', '', Clean_Speach_Link)

                    print('Collecting speach: ' + str(i + 1) + ' ' + Speaker)
                    Speach_Text = Collect_Speach(Clean_Speach_Link)

                    if Load_to_Memory == True:
                        Master_List.append([Date, Hour, Session, Phase, Speaker, Speach_Text])

                    if Save_to_File == True:
                        export_csv = csv.writer(f, delimiter = '|')       
                        export_csv.writerow([Date, Hour, Session, Phase, Speaker, Speach_Text])
                    
                    i += 1
            break

        except Exception as e:
            print(str(e))
            print('Retrying to scrap main page...')
   

def Collect_Speach(Local_Speach_Link):
    Attempt = 0
    while Attempt <= Max_Retries:
        try: 
            respData = urllib.request.urlopen(Local_Speach_Link).read()            
            Speach_Soup = bs.BeautifulSoup(respData, 'lxml')
            Local_Speach_Text = Speach_Soup.find_all('p')[2].text
        
            return Local_Speach_Text
            
        except:
            print('Retrying to scrap speach...')
            Attempt += 1


main()
