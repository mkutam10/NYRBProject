# LIBRARIES.
import requests
import re
import pandas as pd
from timeit import default_timer as timer
import time
import random
from bs4 import BeautifulSoup

# START OF TIMER.
START = timer()

# Foundations. USE '' for CODE, STRINGS, TERMINAL OUTPUT; "" for saying stuff like "FOR" loops, PARAMETERS, and GENERAL QUOTATION USE.
FBREF = 'https://fbref.com'
PAGE_url = FBREF + '/en/comps/22/10090/2020-Major-League-Soccer-Stats'  # ENSURE LINK IS FOR RIGHT YEAR!
FULL_response = requests.get(PAGE_url, timeout = 20)
COUNTER = 0
# print(FULL_response)  # 'Response [200]>' means GOOD!
# print(type(FULL_response)) # Returns '<class 'requests.models.Response'>'
# print(FULL_response.text)  # A STRING, with UNICODE as the default character encoding in PYTHON (SEE: https://docs.python.org/3/howto/unicode.html).
# IGNORES, for what I see, the "iframe", "::after" comments, and general formatting content. # DOING a type(response.text) returns a '<class 'str'>'.

# COMPILING and PARSING.
compiledchars = re.compile('<!--|-->')  # Use for EFFICIENCY as detailed on https://docs.python.org/3/library/re.html and, more importantly, for using PATTERN.SUB PROPERLY.
FULL_beautifulsoup = BeautifulSoup(compiledchars.sub('', FULL_response.text), 'lxml')  # Specify 'lxml' as the PARSER we want to use. # ALSO, DELETES the commented <!-- and --> parts (for some reason)?
# print(type(beautifulsoup))  # Returns '<class 'bs4.BeautifulSoup'>'.

# FINDING TEXTS to NAVIGATE TO EACH TEAM's HOME STATISTICS PAGE.
hyperlinkTEXTS = FULL_beautifulsoup.find_all('tbody')[10].find_all(href = re.compile('squads'))  # THEN 11th TABLE after 2 * OVERALL, 2 * HOME/AWAY, and then GROUPS A through F. # '(href = re.compile('squads'))' narrows JUST LIKE '.find('a')'.
# print(len(hyperlinkTEXTS))  # 26, as it SHOULD BE for 2020 SEASON.

# EMPTY DATAFRAME TO HOLD !ALL! THE DATA.
DATAFRAME = pd.DataFrame()

# ------------------------------------------------------------------------------------------ #.

# DA "FOR" LOOP!
for hyperlinkTEXT in hyperlinkTEXTS:

    # FIRST RANDOM SLEEP.
    SLEEPER = random.random()
    if SLEEPER >= 0.7:
        SLEEPING = random.uniform(random.randint(18, 22), random.randint(25, 30))
        print('FIRST STOP:', SLEEPER, '(%g seconds)' % SLEEPING)
        time.sleep(SLEEPING)  # DOUBLE RANDOM NUMBER.
    elif SLEEPER >= 0.22:
        SLEEPING = random.uniform(random.randint(4, 7), random.randint(11, 15))
        print('FIRST STOP:', SLEEPER, '(%g seconds)' % SLEEPING)
        time.sleep(SLEEPING)  # DOUBLE RANDOM NUMBER.
    else:
        SLEEPING = random.uniform(random.random(), random.randint(1, 3))
        print('FIRST STOP:', SLEEPER, '(%g seconds)' % SLEEPING)
        time.sleep(SLEEPING)   # DOUBLE RANDOM NUMBER.

    # HYPERLINK FOR TEAM.
    TEAM_hyperlink = hyperlinkTEXT.get('href')

    # ---------------------------------------- #.
    # SCHEDULE DATA.

    # REQUESTING..FOR SCHEDULE.
    SCHEDULE_url = FBREF + TEAM_hyperlink  # NOTE: NOT an ISSUE for two string concatenation, ONLY a worry with LONGER LISTS.
    SCHEDULE_response = requests.get(SCHEDULE_url, timeout = 20)

    # COMPILING and PARSING (see first instance for details).
    SCHEDULE_beautifulsoup = BeautifulSoup(compiledchars.sub('', SCHEDULE_response.text), 'lxml')

    # SCRAPING for SCHEDULE column names.
    SCHEDULE_columnnamesTEXT = SCHEDULE_beautifulsoup.find_all('thead')[1]
    SCHEDULE_columnnames = []

    # RETRIEVING column names using a "FOR" loop...
    for tableheader in SCHEDULE_columnnamesTEXT.find_all('th'):  # SECOND <tr> to avoid the first HEADER.
        columnnameencoded = tableheader.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        columnname = columnnameencoded.decode('utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        SCHEDULE_columnnames.append(columnname)

    # print(SCHEDULE_columnnames)

    # ----- #.

    # SCRAPING for SCHEDULE data.
    SCHEDULE_datarows = SCHEDULE_beautifulsoup.find_all('tbody')[1].find_all('tr')  # EACH TABLE ROW in TABLE'S BODY.
    SCHEDULE_rows = []  # the SHORTER, the MORE IMPORTANT.

    for row in SCHEDULE_datarows:
        rowdata = []  # A .clear() METHOD, but WITHOUT the backwards movement that deleted contents in the list.
        datepart = row.find('th')
        dateencoded = datepart.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        date = dateencoded.decode(
            'utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        rowdata.append(date)
        for element in row.find_all('td'):
            dataencoded = element.text.strip().encode()
            data = dataencoded.decode('utf-8')
            rowdata.append(data)
        SCHEDULE_rows.append(rowdata)  # NOTE: columnname or rowdata[] have limited scope, meaning that it's perfectly okay to only use those within a "FOR" Loop.

    # DATA-FRAMING.
    SCHEDULE_dataframe = pd.DataFrame(SCHEDULE_rows, columns = SCHEDULE_columnnames)  # No need for REFINING yet...
    # print(SCHEDULE_dataframe)

    # --------------------------------------------- #.

    # RETRIEVING LINKS for NON-SCHEDULE data.
    SCHEDULE_FILTER = SCHEDULE_beautifulsoup.find('div', {'class': 'filter'})  # SEE https://www.crummy.com/software/BeautifulSoup/bs4/doc/ for further documentation on (name, attrs = {}).
    SCHEDULE_AWAY = (SCHEDULE_FILTER.find_all('div', {'class': ''})[2]).find('a').get('href')  # 3rd NON-CURRENT class. # AGAIN, need to HAVE a '(href = re.compile({something}))' OR a '.find('a')' to access href with '.get('href')'.
    # print(SCHEDULE_AWAY) # SCHEDULE_AWAY will be a RECURRING VARIABLE CALL.

    # --------------------------------------------- #.
    # PASSING DATA.

    # SECOND RANDOM SLEEP.
    SLEEPER = random.random()
    if SLEEPER >= 0.09:
        SLEEPING = random.uniform(random.randint(8, 11), random.randint(13, 16))
        print('SECOND STOP:', SLEEPER, '(%g seconds)' % SLEEPING)
        time.sleep(SLEEPING)  # DOUBLE RANDOM NUMBER.
    else:
        print('SECOND STOP:', SLEEPER, '(0 seconds)')

    # REQUESTING...for PASSING.
    PASSING_url = FBREF + SCHEDULE_AWAY
    PASSING_response = requests.get(PASSING_url, timeout = 20)
    # print(PASSING_response)  # CHECK for '<Response [200]>'.

    # COMPILING and PARSING (see first instance for details).
    PASSING_beautifulsoup = BeautifulSoup(compiledchars.sub('', PASSING_response.text), 'lxml')

    # SCRAPING for PASSING column names.
    PASSING_datacolumnsTEXT = PASSING_beautifulsoup.find('thead')  # RETURNS as a '<class 'bs4.element.Tag'>', useful during text method to return a STRING (though without clean separators).
    PASSING_datacolumnsTEXTNARROWED = PASSING_datacolumnsTEXT.find_all('tr')  # RETURNS as a '<class 'bs4.element.Tag'>', useful during .text method to return a STRING (though without clean separators).

    PASSING_columnnames = []
    # RETRIEVING column names using a "FOR" loop...
    for tableheader in PASSING_datacolumnsTEXTNARROWED[1].find_all('th'):  # SECOND <tr> to avoid the first HEADER.
        columnnameencoded = tableheader.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        columnname = columnnameencoded.decode('utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        PASSING_columnnames.append(columnname)

    # print(PASSING_columnnames)

    # REFINING column names.
    PASSING_suffixes = ('', '-Short', '-Medium', '-Long')  # CAN DEFINE as a TUPLE for efficiency.
    PASSING_columnnamesREFINED = []
    cmpsuffixer = 0
    attsuffixer = 0
    cmppercentsuffixer = 0
    for name in PASSING_columnnames:  # ORIGINAL ELEMENTS WON'T CHANGE from the "IF" branch.
        if name == 'Cmp':
            PASSING_columnnamesREFINED.append(name + PASSING_suffixes[cmpsuffixer])
            cmpsuffixer += 1
        elif name == 'Att':  # ELIF is KEY. FAIL TO RUN the OTHER "IF" statements if an "IF" or an "ELIF" already holds TRUE and avoids the preceding double print.
            PASSING_columnnamesREFINED.append(name + PASSING_suffixes[attsuffixer])
            attsuffixer += 1
        elif name == 'Cmp%':  # ELIF is KEY. FAIL TO RUN the OTHER "IF" statements if an "IF" or an "ELIF" already holds TRUE and avoids the preceding double print.
            PASSING_columnnamesREFINED.append(name + PASSING_suffixes[cmppercentsuffixer])
            cmppercentsuffixer += 1
        else:  # A loop's else RUNS when NO BREAK by IF or ELIF occurs; see https://docs.python.org/3/tutorial/controlflow.html. # BEFORE, as long as name != 'Cmp%', the ELSE statement would run.
            PASSING_columnnamesREFINED.append(name)

    # print(PASSING_columnnamesREFINED)

    # ----- #.

    # SCRAPING for PASSING data.
    PASSING_datarows = PASSING_beautifulsoup.find('tbody').find_all('tr')  # print(len(PASSING_beautifulsoup.find_all('tbody')))  # DO...brings back only the length of ONE!  # EACH TABLE ROW in TABLE'S BODY.
    PASSING_rows = []  # the SHORTER, the MORE IMPORTANT.

    for row in PASSING_datarows:
        rowdata = []  # A .clear() METHOD, but WITHOUT the backwards movement that deleted contents in the list.
        datepart = row.find('th')
        dateencoded = datepart.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        date = dateencoded.decode('utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        rowdata.append(date)
        for element in row.find_all('td'):
            dataencoded = element.text.strip().encode()
            data = dataencoded.decode('utf-8')
            rowdata.append(data)
        PASSING_rows.append(
            rowdata)  # NOTE: columnname or rowdata[] have limited scope, meaning that it's perfectly okay to only use those within a "FOR" loop.

    # DATA-FRAMING.
    PASSING_dataframe = pd.DataFrame(PASSING_rows, columns = PASSING_columnnamesREFINED)
    # print(PASSING_dataframe)

    # --------------------------------------------- #.
    # PASSTYPES DATA.

    # THIRD RANDOM SLEEP.
    SLEEPER = random.random()
    if SLEEPER >= 0.26:
        SLEEPING = random.uniform(random.randint(9, 10), random.randint(11, 40))
        print('THIRD STOP:', SLEEPER, '(%g seconds)' % SLEEPING)
        time.sleep(SLEEPING)  # DOUBLE RANDOM NUMBER.
    else:
        print('THIRD STOP:', SLEEPER, '(0 seconds)')

    # SPLITTING SCHEDULE_AWAY and REQUESTING...FOR PASSING TYPES.
    linkparts = SCHEDULE_AWAY.split('/')  # ORIGINAL extension for PASSING.
    linkparts = [part.replace('passing', 'passing_types') for part in linkparts]
    PASSTYPES_url = FBREF + '/'.join(linkparts)
    PASSTYPES_response = requests.get(PASSTYPES_url, timeout = 20)
    # print(PASSTYPES_response)  # CHECK for '<Response [200]>'.

    # COMPILING and PARSING (see first instance for details).
    PASSTYPES_beautifulsoup = BeautifulSoup(compiledchars.sub('', PASSTYPES_response.text), 'lxml')

    # SCRAPING for PASSTYPES column names.
    PASSTYPES_datacolumnsTEXT = PASSTYPES_beautifulsoup.find('thead')  # RETURNS as a '<class 'bs4.element.Tag'>', useful during text method to return a STRING (though without clean separators).
    PASSTYPES_datacolumnsTEXTNARROWED = PASSTYPES_datacolumnsTEXT.find_all('tr')  # RETURNS as a '<class 'bs4.element.Tag'>', useful during .text method to return a STRING (though without clean separators).

    PASSTYPES_columnnames = []
    # RETRIEVING column names using a "FOR" loop...
    for tableheader in PASSTYPES_datacolumnsTEXTNARROWED[1].find_all('th'):  # SECOND <tr> to avoid the first HEADER .
        columnnameencoded = tableheader.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        columnname = columnnameencoded.decode('utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        PASSTYPES_columnnames.append(columnname)

    # print(PASSTYPES_columnnames)

    # REFINING column names.
    PASSTYPES_columnnamesREFINED = []
    outsuffixer = 0
    for name in PASSTYPES_columnnames:  # ORIGINAL ELEMENTS WON'T CHANGE from the "IF" branch. # MODIFIED FOR CLARIFYING PURPOSES.
        if name == 'Att' or name == 'Live' or name == 'Dead' or name == 'Cmp':
            PASSTYPES_columnnamesREFINED.append(name + '-Passes')
        elif name == 'In' or name == 'Str':
            PASSTYPES_columnnamesREFINED.append(name + ' CKs')
        elif name == 'Out':
            if outsuffixer == 0:
                PASSTYPES_columnnamesREFINED.append(name + ' CKs')
                outsuffixer += 1
            else:
                PASSTYPES_columnnamesREFINED.append(name + ' of')
        elif name == 'Off':
            PASSTYPES_columnnamesREFINED.append(name + 'sides')
        elif name == 'TB':
            PASSTYPES_columnnamesREFINED.append(name + ' (Through Balls)')
        elif name == 'TI':
            PASSTYPES_columnnamesREFINED.append(name + ' (Throw Ins)')
        else:  # A loop's else RUNS when NO BREAK by IF or ELIF occurs; see https://docs.python.org/3/tutorial/controlflow.html.
            PASSTYPES_columnnamesREFINED.append(name)

    # print(PASSTYPES_columnnamesREFINED)

    # ----- #.

    # SCRAPING for PASSTYPES data.
    PASSTYPES_datarows = PASSTYPES_beautifulsoup.find('tbody').find_all('tr')  # print(len(PASSTYPES_beautifulsoup.find_all('tbody')))  # DO...brings back only the length of ONE!  # EACH TABLE ROW in TABLE'S BODY.
    PASSTYPES_rows = []  # the SHORTER, the MORE IMPORTANT.

    for row in PASSTYPES_datarows:
        rowdata = []  # A .clear() METHOD, but WITHOUT the backwards movement that deleted contents in the list.
        datepart = row.find('th')
        dateencoded = datepart.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        date = dateencoded.decode('utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        rowdata.append(date)
        for element in row.find_all('td'):
            dataencoded = element.text.strip().encode()
            data = dataencoded.decode('utf-8')
            rowdata.append(data)
        PASSTYPES_rows.append(rowdata)  # NOTE: columnname or rowdata[] have limited scope, meaning that it's perfectly okay to only use those within a "FOR" loop.

    # DATA-FRAMING.
    PASSTYPES_dataframe = pd.DataFrame(PASSTYPES_rows, columns = PASSTYPES_columnnamesREFINED)
    # print(PASSTYPES_dataframe)

    # --------------------------------------------- #.
    # POSSESSION DATA.

    # FOURTH RANDOM SLEEP.
    SLEEPER = random.random()
    if SLEEPER >= 0.42:
        SLEEPING = random.uniform(random.randint(6, 9), random.randint(24, 27))
        print('FOURTH STOP:', SLEEPER, '(%g seconds)' % SLEEPING)
        time.sleep(SLEEPING)  # DOUBLE RANDOM NUMBER.
    else:
        print('FOURTH STOP:', SLEEPER, '(0 seconds)')

    # SPLITTING SCHEDULE_AWAY and REQUESTING...FOR POSSESSION.
    linkparts = SCHEDULE_AWAY.split('/')  # ORIGINAL extension for PASSING.
    linkparts = [part.replace('passing', 'possession') for part in linkparts]
    POSSESSION_url = FBREF + '/'.join(linkparts)
    POSSESSION_response = requests.get(POSSESSION_url, timeout = 20)
    # print(PASSTYPES_response)  # CHECK for '<Response [200]>'.

    # COMPILING and PARSING (see first instance for details).
    POSSESSION_beautifulsoup = BeautifulSoup(compiledchars.sub('', POSSESSION_response.text), 'lxml')

    # SCRAPING for POSSESSION column names.
    POSSESSION_datacolumnsTEXT = POSSESSION_beautifulsoup.find('thead')  # RETURNS as a '<class 'bs4.element.Tag'>', useful during text method to return a STRING (though without clean separators).
    POSSESSION_datacolumnsTEXTNARROWED = POSSESSION_datacolumnsTEXT.find_all('tr')  # RETURNS as a '<class 'bs4.element.Tag'>', useful during .text method to return a STRING (though without clean separators).

    POSSESSION_columnnames = []
    # RETRIEVING column names using a "FOR" loop...
    for tableheader in POSSESSION_datacolumnsTEXTNARROWED[1].find_all('th'):  # SECOND <tr> to avoid the first HEADER .
        columnnameencoded = tableheader.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        columnname = columnnameencoded.decode('utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        POSSESSION_columnnames.append(columnname)

    # print(POSSESSION_columnnames)

    # REFINING column names.
    POSSESSION_columnnamesREFINED = []
    progsuffixer = 0
    for name in POSSESSION_columnnames:
        if name == 'Live':
            POSSESSION_columnnamesREFINED.append(name + '-Touches')
        elif name == 'Succ' or name == 'Att' or name == 'Succ%':
            POSSESSION_columnnamesREFINED.append(name + '-Dribbles')
        elif name == '#Pl':
            POSSESSION_columnnamesREFINED.append(name + ' (Players Past)')
        elif name == 'TotDist' or name == 'PrgDist' or name == '1/3':
            POSSESSION_columnnamesREFINED.append(name + '-Car.')
        elif name == 'Prog':  # TWO TYPES of 'Prog' that conflict with 'Prog' from PASSING_dataframe.
            if progsuffixer == 0:
                POSSESSION_columnnamesREFINED.append(name + '-Car.')
                progsuffixer += 1
            else:  # See NOTEBOOK for faulty difference, or rather similarity, between the statistics that's due to a data inputting mistake.
                POSSESSION_columnnamesREFINED.append(name + '-Rec.')
        elif name == 'Mis':
            POSSESSION_columnnamesREFINED.append(name + 'takes')
        elif name == 'Dis':
            POSSESSION_columnnamesREFINED.append(name + 'poss')
        elif name == 'Targ' or name == 'Rec' or name == 'Rec%':
            POSSESSION_columnnamesREFINED.append(name + '-Rec.')
        else:
            POSSESSION_columnnamesREFINED.append(name)

    # print(POSSESSION_columnnamesREFINED)

    # ----- #.

    # SCRAPING for POSSESSION data.
    POSSESSION_datarows = POSSESSION_beautifulsoup.find('tbody').find_all('tr')  # print(len(POSSESSION_beautifulsoup.find_all('tbody')))  # DO...brings back only the length of ONE!  # EACH TABLE ROW in TABLE'S BODY.
    POSSESSION_rows = []  # the SHORTER, the MORE IMPORTANT.

    for row in POSSESSION_datarows:
        rowdata = []  # A .clear() METHOD, but WITHOUT the backwards movement that deleted contents in the list.
        datepart = row.find('th')
        dateencoded = datepart.text.strip().encode()  # .TEXT takes TAG object into a STRING that .encode() transforms into bytes represented in UTF-8, or Unicode Transformation Format — 8-BIT. See that "8-BIT"/"1 BYTE"!
        date = dateencoded.decode('utf-8')  # NOW, back into string form. Maybe, just maybe being safe on WEIRD characters with .encode and .decode here. "Since Python 3.0, the language’s str type contains Unicode characters..."
        rowdata.append(date)
        for element in row.find_all('td'):
            dataencoded = element.text.strip().encode()
            data = dataencoded.decode('utf-8')
            rowdata.append(data)
        POSSESSION_rows.append(rowdata)  # NOTE: columnname or rowdata[] have limited scope, meaning that it's perfectly okay to only use those within a "FOR" loop.

    # DATA-FRAMING.
    POSSESSION_dataframe = pd.DataFrame(POSSESSION_rows, columns = POSSESSION_columnnamesREFINED)
    # print(POSSESSION_dataframe)

    # ------------------------------------------------------------------------------------------ #.

    # CONCATENATING TABLES.

    # Modifying SCHEDULE TABLE.
    SCHEDULE_dataframe = SCHEDULE_dataframe.iloc[:, :(len(SCHEDULE_dataframe.columns) - 2)]
    # print(SCHEDULE_dataframe)

    # Modifying PASSING TABLE.
    PASSING_dataframe = PASSING_dataframe.iloc[:, 10:(len(PASSING_dataframe.columns) - 1)]
    # print(PASSING_dataframe)

    # Modifying PASSTYPES TABLE.
    PASSTYPES_dataframe = PASSTYPES_dataframe.iloc[:, 10:(len(PASSTYPES_dataframe.columns) - 1)]
    PASSTYPES_dataframe = PASSTYPES_dataframe.drop(columns = ['Att-Passes', 'Cmp-Passes'])  # DUPLICATED in PASSING_dataframe.
    # print(PASSTYPES_dataframe)

    # Modifying POSSESSION TABLE.
    POSSESSION_dataframe = POSSESSION_dataframe.iloc[:, 11:(len(POSSESSION_dataframe.columns) - 1)]
    # print(POSSESSION_dataframe)

    # CONCATENATION (within a TEAM).
    FULL_dataframe = pd.concat((SCHEDULE_dataframe, PASSING_dataframe, PASSTYPES_dataframe, POSSESSION_dataframe), axis = 1)
    FULL_columnnames = tuple(FULL_dataframe.columns)  # NOTE: USE TUPLES when possible (not for appending elements), but for indexed lookups, unpacking, and more efficient storage space (SEE: https://stackoverflow.com/questions/68630/are-tuples-more-efficient-than-lists-in-python).
    FULL_columnnames = tuple(FULL_columnnames[0:13] + FULL_columnnames[17:38] + FULL_columnnames[84:85] + FULL_columnnames[38:81] + FULL_columnnames[82:83] + FULL_columnnames[81:82] + FULL_columnnames[83:84] + FULL_columnnames[13:17])  # With TUPLES, no real need to OVER-ALLOCATE in case of .append as tuples have !FIXED! size.
    FULL_dataframe = FULL_dataframe.reindex(columns = FULL_columnnames)  # CAN do both "columns" for columns, or "index" for indices. 85 COLUMNS!

    TEAM_name = TEAM_hyperlink.split('/')[5].split('-')  # THOUGH..let's ADD a COLUMN for TEAM NAME.
    FULL_dataframe.insert(0, 'Team', ' '.join(TEAM_name[:len(TEAM_name) - 1]))  # ADDING the TEAM column.

    # CONCATENATION (outside a TEAM).
    DATAFRAME = pd.concat((DATAFRAME, FULL_dataframe), axis = 0, ignore_index = True)  # EQUIVALENT (or SLIGHTLY BETTER) than .append? # 86 ROWS for TEAM and COMPETITION. # ignore_index MAY IMPROVE PERFORMANCE as method won't worry about PAST INDICES.

    # WATCHING...
    COUNTER += 1  # FOR NEXT ITERATION.
    print('(%d)' % COUNTER)
    print()

# FINAL POLISHES.
# DATAFRAME = DATAFRAME.reset_index(drop = True)  # NOTE: RETURNS a NEW OBJECT.
print(DATAFRAME)
print()
DATAFRAME.to_csv('MLS_2020.csv', index = False)

# END OF TIMER.
END = timer()
print('FINAL TIMES:')
print(END - START, 'seconds')
print((END - START)/60, 'minutes')
