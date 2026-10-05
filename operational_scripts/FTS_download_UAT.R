#Get latest FTS data and merge with climate database - Production version
#commented "#" out lines do not run

library(httr)
library(jsonlite)
library(lubridate)
library(stringr)
library(purrr)

#get FTS data function
downloadFTS = function (start_date = "2025-06-29T00:00:00Z",
                        end_date = "2025-07-16T00:00:00Z",
                        output_date = "July16_2025"){
  
  
  # Step 1: Authenticate
  login_response <- POST(
    url = "https://fts360api.com/auth/users/authenticate",
    body = list(
      email = Sys.getenv("FTS_EMAIL"), # enter email
      password = Sys.getenv("FTS_PWD") # enter password
    ),
    encode = "json",
    accept_json()
  )
  
  login_content <- content(login_response, as = "parsed")
  token <- login_content$access_token
  if (is.null(token)) stop("Login failed. Check credentials.")
  
  # Step 2: Define download parameters for each group
  station_groups <- list(
    
    dehchostations = list(
      stationIds = c(
        "6160c6b164699463087279d1",   # Fort Liard
        "6160c6b164699463087279ec",   # Fort Simpson
        "6160c6b164699463087279d8",   # Jean Marie River
        "6160c6b164699463087279d6",   # Nahanni Butte
        "6160c6b96469946308728193",   # Ndulee Crossing
        "6160c6b164699463087279e8",   # Redknife Hills
        "6160c6b164699463087279ed",   # Trout Lake
        "6160c6b164699463087279ef",   # West Horn
        "6160c6b164699463087279c9"    # Wrigley
      ),
      fields = c(
        # Fort Liard
        "649dbf5050647525a192e209",
        "649dbf5050647525a192e216",
        "649dbf5050647525a192e215",
        "649dbf5050647525a192e228",
        "649dbf5050647525a192e21d",
        "649dbf5050647525a192e220",
        "649dbf5050647525a192e206",
        # Fort Simpson
        "649dbf41b10bd1051fa00d90",
        "649dbf41b10bd1051fa00d85",
        "649dbf41b10bd1051fa00d86",
        "649dbf41b10bd1051fa00d8d",
        "649dbf41b10bd1051fa00d98",
        "649dbf41b10bd1051fa00d79",
        "649dbf41b10bd1051fa00d76",
        # Jean Marie River
        "649dbf89b10bd1051fa00ed7",
        "649dbf89b10bd1051fa00ee3",
        "649dbf89b10bd1051fa00eee",
        "649dbf89b10bd1051fa00ee4",
        "649dbf89b10bd1051fa00eeb",
        "649dbf89b10bd1051fa00ef6",
        "649dbf89b10bd1051fa00ed4",
        # Nahanni Butte
        "649dbf85b10bd1051fa00e9e",
        "649dbf85b10bd1051fa00ea8",
        "649dbf85b10bd1051fa00e91",
        "649dbf85b10bd1051fa00e9d",
        "649dbf85b10bd1051fa00ea5",
        "649dbf85b10bd1051fa00eb0",
        "649dbf85b10bd1051fa00e8e",
        # Ndulee Crossing
        "649dc01950647525a192e6ea",
        "649dc01950647525a192e6e3",
        "649dc01950647525a192e6ed",
        "649dc01950647525a192e6f5",
        "649dc01950647525a192e6d6",
        "649dc01950647525a192e6e2",
        "649dc01950647525a192e6d3",
        # Redknife Hills
        "649dc017b10bd1051fa01167",
        "649dc017b10bd1051fa01168",
        "649dc017b10bd1051fa01172",
        "649dc017b10bd1051fa0116f",
        "649dc017b10bd1051fa0117a",
        "649dc017b10bd1051fa0115b",
        "649dc017b10bd1051fa01158",
        # Trout Lake
        "649dbfb3b10bd1051fa01027",
        "649dbfb3b10bd1051fa01028",
        "649dbfb3b10bd1051fa01032",
        "649dbfb3b10bd1051fa0102f",
        "649dbfb3b10bd1051fa0101b",
        "649dbfb3b10bd1051fa0103a",
        "649dbfb3b10bd1051fa01018",
        # West Horn
        "649dbfacb10bd1051fa00f9b",
        "649dbfacb10bd1051fa00f9c",
        "649dbfacb10bd1051fa00fae",
        "649dbfacb10bd1051fa00f8f",
        "649dbfacb10bd1051fa00fa3",
        "649dbfacb10bd1051fa00fa6",
        "649dbfacb10bd1051fa00f8c",
        # Wrigley
        "649dca92b10bd1051fa02caa",
        "649dca92b10bd1051fa02cb6",
        "649dca92b10bd1051fa02cb7",
        "649dca92b10bd1051fa02cbe",
        "649dca92b10bd1051fa02cc1",
        "649dca92b10bd1051fa02cc9",
        "649dca92b10bd1051fa02ca7"
      )
    ),
    
    sahtustations = list(
      stationIds = c(
        "6160c6b164699463087279e4",   # Colville Lake
        "6160c6b164699463087279e7",   # Deline
        "6160c6b164699463087279e9",   # Fort Good Hope
        "6160c6b96469946308728118",   # Mile 222
        "6160c6b164699463087279d9",   # Norman Wells
        "6160c6b164699463087279e5",   # St Charles Creek
        "6160c6b7646994630872801e",   # Tulita
        "6160c6b164699463087279e6"    # Yaltea Lake
      ),
      fields = c(
        # Colville Lake
        "649dbf5750647525a192e287",
        "649dbf5750647525a192e291",
        "649dbf5750647525a192e27a",
        "649dbf5750647525a192e28e",
        "649dbf5750647525a192e299",
        "649dbf5750647525a192e286",
        "649dbf5750647525a192e277",
        # Deline
        "649dbf8c50647525a192e3a8",
        "649dbf8c50647525a192e3bc",
        "649dbf8c50647525a192e3bf",
        "649dbf8c50647525a192e3c7",
        "649dbf8c50647525a192e3b4",
        "649dbf8c50647525a192e3b5",
        "649dbf8c50647525a192e3a5",
        # Fort Good Hope
        "649dbfaab10bd1051fa00f6c",
        "649dbfaab10bd1051fa00f78",
        "649dbfaab10bd1051fa00f8b",
        "649dbfaab10bd1051fa00f79",
        "649dbfaab10bd1051fa00f80",
        "649dbfaab10bd1051fa00f83",
        "649dbfaab10bd1051fa00f69",
        # Mile 222
        "649dbfaeb10bd1051fa00fbe",
        "649dbfaeb10bd1051fa00fb2",
        "649dbfaeb10bd1051fa00fd1",
        "649dbfaeb10bd1051fa00fbf",
        "649dbfaeb10bd1051fa00fc6",
        "649dbfaeb10bd1051fa00fc9",
        "649dbfaeb10bd1051fa00faf",
        # Norman Wells
        "649dbf97b10bd1051fa00f0f",
        "649dbf97b10bd1051fa00f17",
        "649dbf97b10bd1051fa00f22",
        "649dbf97b10bd1051fa00f10",
        "649dbf97b10bd1051fa00f03",
        "649dbf97b10bd1051fa00f1a",
        "649dbf97b10bd1051fa00f00",
        # St Charles Creek
        "649dbfa3b10bd1051fa00f45",
        "649dbfa3b10bd1051fa00f26",
        "649dbfa3b10bd1051fa00f32",
        "649dbfa3b10bd1051fa00f33",
        "649dbfa3b10bd1051fa00f3a",
        "649dbfa3b10bd1051fa00f3d",
        "649dbfa3b10bd1051fa00f23",
        # Tulita
        "649dc7e5b10bd1051fa026a8",
        "649dc7e5b10bd1051fa026b0",
        "649dc7e5b10bd1051fa026bb",
        "649dc7e5b10bd1051fa0269c",
        "649dc7e5b10bd1051fa026a9",
        "649dc7e5b10bd1051fa026b3",
        "649dc7e5b10bd1051fa02699",
        # Yaltea Lake
        "649dc9a650647525a1931d47",
        "649dc9a650647525a1931d5b",
        "649dc9a650647525a1931d53",
        "649dc9a650647525a1931d54",
        "649dc9a650647525a1931d5e",
        "649dc9a650647525a1931d66",
        "649dc9a650647525a1931d44"
      )
    ),
    
    southslavestations = list(
      stationIds = c(
        "6160c6b16469946308727a04",   # Buffalo Junction
        "6160c6b164699463087279e1",   # Cameron Hills
        "6160c6b164699463087279df",   # Crown Fire
        "6160c6b164699463087279d7",   # Enterprise
        "6160c6ba646994630872823d",   # Forestry Lake
        "6160c6b164699463087279ce",   # Fort Providence
        "6160c6b164699463087279cf",   # Fort Resolution
        "6160c6b164699463087279e0",   # Fort Smith
        "6160c6b164699463087279db",   # Gray Lake
        "6160c6b164699463087279ee",   # Grumbler Creek
        "6160c6b164699463087279dc",   # Hay River
        "6160c6b164699463087279fc",   # Horn Plateau
        "6160c6b864699463087280d9",   # Kasba Lake
        "6160c6b164699463087279d5",   # Kimble Tower
        "6160c6b164699463087279f2",   # Long Island
        "6160c6b164699463087279cc",   # Powder Lake
        "6160c6b164699463087279da"    # Talston River Dam
      ),
      fields = c(
        # Buffalo Junction
        "649dbf4cb10bd1051fa00e1c",
        "649dbf4cb10bd1051fa00e12",
        "649dbf4cb10bd1051fa00e11",
        "649dbf4cb10bd1051fa00e19",
        "649dbf4cb10bd1051fa00e05",
        "649dbf4cb10bd1051fa00e24",
        "649dbf4cb10bd1051fa00e02",
        # Cameron Hills
        "649dbf58b10bd1051fa00e35",
        "649dbf58b10bd1051fa00e3f",
        "649dbf58b10bd1051fa00e28",
        "649dbf58b10bd1051fa00e3c",
        "649dbf58b10bd1051fa00e34",
        "649dbf58b10bd1051fa00e47",
        "649dbf58b10bd1051fa00e25",
        # Crown Fire
        "649dbf4e50647525a192e1e4",
        "649dbf4e50647525a192e1f1",
        "649dbf4e50647525a192e1f8",
        "649dbf4e50647525a192e1fb",
        "649dbf4e50647525a192e203",
        "649dbf4e50647525a192e1f0",
        "649dbf4e50647525a192e1e1",
        # Enterprise
        "649dbf5bb10bd1051fa00e7b",
        "649dbf5bb10bd1051fa00e82",
        "649dbf5bb10bd1051fa00e8d",
        "649dbf5bb10bd1051fa00e6e",
        "649dbf5bb10bd1051fa00e7a",
        "649dbf5bb10bd1051fa00e85",
        "649dbf5bb10bd1051fa00e6b",
        # Forestry Lake
        "649dbf8250647525a192e375",
        "649dbf8250647525a192e388",
        "649dbf8250647525a192e369",
        "649dbf8250647525a192e37d",
        "649dbf8250647525a192e376",
        "649dbf8250647525a192e380",
        "649dbf8250647525a192e366",
        # Fort Providence
        "649dbf87b10bd1051fa00ec0",
        "649dbf87b10bd1051fa00ed3",
        "649dbf87b10bd1051fa00ecb",
        "649dbf87b10bd1051fa00ec1",
        "649dbf87b10bd1051fa00eb4",
        "649dbf87b10bd1051fa00ec8",
        "649dbf87b10bd1051fa00eb1",
        # Fort Resolution
        "649dbf4550647525a192e199",
        "649dbf4550647525a192e19a",
        "649dbf4550647525a192e1a1",
        "649dbf4550647525a192e1ac",
        "649dbf4550647525a192e18d",
        "649dbf4550647525a192e1a4",
        "649dbf4550647525a192e18a",
        # Fort Smith
        "649dbf6a50647525a192e2e4",
        "649dbf6a50647525a192e2dd",
        "649dbf6a50647525a192e2e7",
        "649dbf6a50647525a192e2d0",
        "649dbf6a50647525a192e2dc",
        "649dbf6a50647525a192e2ef",
        "649dbf6a50647525a192e2cd",
        # Gray Lake
        "649dbf9150647525a192e3e0",
        "649dbf9150647525a192e3df",
        "649dbf9150647525a192e3d3",
        "649dbf9150647525a192e3e7",
        "649dbf9150647525a192e3f2",
        "649dbf9150647525a192e3ea",
        "649dbf9150647525a192e3d0",
        # Grumbler Creek
        "649dbf7150647525a192e31d",
        "649dbf7150647525a192e325",
        "649dbf7150647525a192e30f",
        "649dbf7150647525a192e31a",
        "649dbf7150647525a192e313",
        "649dbf7150647525a192e302",
        "649dbf7150647525a192e312",
        "649dbf7150647525a192e2ff",
        # Hay River
        "649dbf5e50647525a192e2ac",
        "649dbf5e50647525a192e2b4",
        "649dbf5e50647525a192e2bf",
        "649dbf5e50647525a192e2a0",
        "649dbf5e50647525a192e2ad",
        "649dbf5e50647525a192e2b7",
        "649dbf5e50647525a192e29d",
        # Horn Plateau
        "649dbfa5b10bd1051fa00f56",
        "649dbfa5b10bd1051fa00f49",
        "649dbfa5b10bd1051fa00f60",
        "649dbfa5b10bd1051fa00f68",
        "649dbfa5b10bd1051fa00f55",
        "649dbfa5b10bd1051fa00f5d",
        "649dbfa5b10bd1051fa00f46",
        # Kasba Lake
        "649dbfb2b10bd1051fa0100c",
        "649dbfb2b10bd1051fa0100f",
        "649dbfb2b10bd1051fa01005",
        "649dbfb2b10bd1051fa00ff8",
        "649dbfb2b10bd1051fa01004",
        "649dbfb2b10bd1051fa01017",
        "649dbfb2b10bd1051fa00ff5",
        # Kimble Tower
        "649dc01c50647525a192e72b",
        "649dc01c50647525a192e737",
        "649dc01c50647525a192e738",
        "649dc01c50647525a192e742",
        "649dc01c50647525a192e73f",
        "649dc01c50647525a192e74a",
        "649dc01c50647525a192e728",
        # Long Island
        "649dc94050647525a1931b15",
        "649dc94050647525a1931b27",
        "649dc94050647525a1931b08",
        "649dc94050647525a1931b14",
        "649dc94050647525a1931b1c",
        "649dc94050647525a1931b1f",
        "649dc94050647525a1931b05",
        # Powder Lake
        "649dcbc250647525a193280b",
        "649dcbc250647525a1932817",
        "649dcbc250647525a1932818",
        "649dcbc250647525a193281f",
        "649dcbc250647525a1932822",
        "649dcbc250647525a193282a",
        "649dcbc250647525a1932808",
        # Talston River Dam
        "649dcd01b10bd1051fa03289",
        "649dcd01b10bd1051fa03295",
        "649dcd01b10bd1051fa03296",
        "649dcd01b10bd1051fa0329d",
        "649dcd01b10bd1051fa032a0",
        "649dcd01b10bd1051fa032a8",
        "649dcd01b10bd1051fa03286"
      )
    ),
    
    northslavestations = list(
      stationIds = c(
        "6160c6b86469946308728061",   # Awry Lake
        "666c9015375e9543b1f93d46",   # Behchoko
        "6160c6b164699463087279c6",   # Bliss Lake
        "6160c6b164699463087279d3",   # Faber Lake
        "6160c6b164699463087279cb",   # Francois Lake
        "6160c6b164699463087279e2",   # Indin Lake
        "6160c6b164699463087279c7",   # Lutsel'Ke
        "6160c6b164699463087279f7",   # McCrea Lake
        "6160c6b164699463087279f4",   # Mosquito Tower
        "6160c6ba64699463087282a8",   # Wekweeti
        "6160c6b164699463087279d4",   # Wha Ti
        "6160c6b164699463087279d2"    # Yellowknife
      ),
      fields = c(
        # Awry Lake
        "649dbf43b10bd1051fa00da9",
        "649dbf43b10bd1051fa00db0",
        "649dbf43b10bd1051fa00db3",
        "649dbf43b10bd1051fa00dbb",
        "649dbf43b10bd1051fa00da8",
        "649dbf43b10bd1051fa00d9c",
        "649dbf43b10bd1051fa00d99",
        # Behchoko
        "649dbf4650647525a192e1cf",
        "649dbf4650647525a192e1c4",
        "649dbf4650647525a192e1c7",
        "649dbf4650647525a192e1b0",
        "649dbf4650647525a192e1bc",
        "649dbf4650647525a192e1bd",
        "649dbf4650647525a192e1ad",
        # Bliss Lake
        "649dbf5150647525a192e238",
        "649dbf5150647525a192e239",
        "649dbf5150647525a192e24b",
        "649dbf5150647525a192e22c",
        "649dbf5150647525a192e240",
        "649dbf5150647525a192e243",
        "649dbf5150647525a192e229",
        # Faber Lake
        "649dbf7b50647525a192e34e",
        "649dbf7b50647525a192e355",
        "649dbf7b50647525a192e358",
        "649dbf7b50647525a192e341",
        "649dbf7b50647525a192e34d",
        "649dbf7b50647525a192e360",
        "649dbf7b50647525a192e33e",
        # Francois Lake
        "649dbf5ab10bd1051fa00e58",
        "649dbf5ab10bd1051fa00e5f",
        "649dbf5ab10bd1051fa00e6a",
        "649dbf5ab10bd1051fa00e4b",
        "649dbf5ab10bd1051fa00e57",
        "649dbf5ab10bd1051fa00e62",
        "649dbf5ab10bd1051fa00e48",
        # Indin Lake
        "649dbf5350647525a192e263",
        "649dbf5350647525a192e25b",
        "649dbf5350647525a192e266",
        "649dbf5350647525a192e24f",
        "649dbf5350647525a192e25c",
        "649dbf5350647525a192e26e",
        "649dbf5350647525a192e24c",
        # Lutsel'Ke
        "649dbfa650647525a192e45f",
        "649dbfa650647525a192e469",
        "649dbfa650647525a192e466",
        "649dbfa650647525a192e471",
        "649dbfa650647525a192e452",
        "649dbfa650647525a192e45e",
        "649dbfa650647525a192e44f",
        # McCrea Lake
        "649dbfa850647525a192e4a6",
        "649dbfa850647525a192e49e",
        "649dbfa850647525a192e4a9",
        "649dbfa850647525a192e4b1",
        "649dbfa850647525a192e492",
        "649dbfa850647525a192e49f",
        "649dbfa850647525a192e48f",
        # Mosquito Tower
        "649dc01e50647525a192e758",
        "649dc01e50647525a192e764",
        "649dc01e50647525a192e765",
        "649dc01e50647525a192e76c",
        "649dc01e50647525a192e76f",
        "649dc01e50647525a192e777",
        "649dc01e50647525a192e755",
        # Wekweeti
        "649dc84750647525a19314cc",
        "649dc84750647525a19314d8",
        "649dc84750647525a19314e3",
        "649dc84750647525a19314eb",
        "649dc84750647525a19314e0",
        "649dc84750647525a19314d9",
        "649dc84750647525a19314c9",
        # Wha Ti
        "649dcc28b10bd1051fa02fac",
        "649dcc28b10bd1051fa02fb8",
        "649dcc28b10bd1051fa02fb9",
        "649dcc28b10bd1051fa02fc0",
        "649dcc28b10bd1051fa02fc3",
        "649dcc28b10bd1051fa02fcb",
        "649dcc28b10bd1051fa02fa9",
        # Yellowknife
        "666c96498ac1c20708381ab5",
        "666c96498ac1c20708381ab7",
        "666c96498ac1c20708381aba",
        "666c96498ac1c20708381ac0",
        "666c96498ac1c20708381ac1",
        "666c96498ac1c20708381ac3",
        "666c96498ac1c20708381ab0"
      )
    ),
    
    watersstations = list(
      stationIds = c(
        "6160c6b164699463087279f1",   # Daring Lake
        "6160c6b16469946308727a01",   # Harry Channel
        "6160c6b164699463087279f5",   # Hoarfrost
        "6160c6b16469946308727a00",   # ITH
        "6160c6b16469946308727a09",   # Peel Plateau
        "6160c6b164699463087279f6"    # Winter Lake
      ),
      fields = c(
        # Daring Lake
        "649dc02050647525a192e79f",
        "649dc02050647525a192e7ab",
        "649dc02050647525a192e787",
        "649dc02050647525a192e79b",
        "649dc02050647525a192e7a4",
        "649dc02050647525a192e79c",
        "649dc02050647525a192e7a6",
        "649dc02050647525a192e793",
        "649dc02050647525a192e794",
        "649dc02050647525a192e7a5",
        "649dc02050647525a192e784",
        # Harry Channel
        "649dc02150647525a192e7ba",
        "649dc02150647525a192e7b9",
        "649dc02150647525a192e7c4",
        "649dc02150647525a192e7cb",
        "649dc02150647525a192e7d0",
        "649dc02150647525a192e7af",
        "649dc02150647525a192e7c2",
        "649dc02150647525a192e7c9",
        "649dc02150647525a192e7c1",
        "649dc02150647525a192e7ca",
        "649dc02150647525a192e7ac",
        # Hoarfrost
        "649dc02550647525a192e7ff",
        "649dc02550647525a192e80f",
        "649dc02550647525a192e7f8",
        "649dc02550647525a192e800",
        "649dc02550647525a192e80a",
        "649dc02550647525a192e810",
        "649dc02550647525a192e807",
        "649dc02550647525a192e808",
        "649dc02550647525a192e811",
        "649dc02550647525a192e816",
        "649dc02550647525a192e7f6",
        # ITH
        "649dc02350647525a192e7e9",
        "649dc02350647525a192e7ef",
        "649dc02350647525a192e7de",
        "649dc02350647525a192e7df",
        "649dc02350647525a192e7ee",
        "649dc02350647525a192e7f0",
        "649dc02350647525a192e7f5",
        "649dc02350647525a192e7e7",
        "649dc02350647525a192e7d4",
        "649dc02350647525a192e7e6",
        "649dc02350647525a192e7d1",
        # Peel Plateau
        "649dc02750647525a192e81a",
        "649dc02750647525a192e82f",
        "649dc02750647525a192e834",
        "649dc02750647525a192e825",
        "649dc02750647525a192e83b",
        "649dc02750647525a192e835",
        "649dc02750647525a192e836",
        "649dc02750647525a192e824",
        "649dc02750647525a192e82c",
        "649dc02750647525a192e82d",
        "649dc02750647525a192e817",
        # Winter Lake
        "649dc02950647525a192e863",
        "649dc02950647525a192e851",
        "649dc02950647525a192e859",
        "649dc02950647525a192e85a",
        "649dc02950647525a192e852",
        "649dc02950647525a192e85c",
        "649dc02950647525a192e861",
        "649dc02950647525a192e868",
        "649dc02950647525a192e847",
        "649dc02950647525a192e862",
        "649dc02950647525a192e844"
      )
    ),
    
    mobilestations = list(
      stationIds = c(
        "66abaf6a00e01a5a0ce79172",   # Mobile 2
        "6685893700e01a5a0ce79130"    # Mobile 5 -Tsu Lake
      ),
      fields = c(
        # Mobile 2
        "66abaf6b6857502d02964180",
        "66abaf6b6857502d0296418d",
        "66abaf6b6857502d02964194",
        "66abaf6b6857502d0296419f",
        "66abaf6b6857502d0296418c",
        "66abaf6b6857502d02964197",
        "66abaf6b6857502d0296417d",
        # Mobile 5 -Tsu Lake
        "668589387e5cf74a1a631f85",
        "668589387e5cf74a1a631f9c",
        "668589387e5cf74a1a631f92",
        "668589387e5cf74a1a631f99",
        "668589387e5cf74a1a631fa4",
        "668589387e5cf74a1a631f91",
        "668589387e5cf74a1a631f82"
      )
    ),
    
    inuvikstations = list(
      stationIds = c(
        "6477ac69684ae637e57ae14a",   # Aklavik
        "689f58c9bd48a62845887713",   # Dempster H8K193
        "6160c6b164699463087279ca",   # Fort MacPherson
        "6160c6b164699463087279cd",   # Ninelin Lake
        "6160c6b164699463087279eb",   # Shell Lake
        "6160c6b164699463087279bf"    # Tsiigehtchic
      ),
      fields = c(
        # Aklavik
        "649dbf3fb10bd1051fa00d56",
        "649dbf3fb10bd1051fa00d6d",
        "649dbf3fb10bd1051fa00d6a",
        "649dbf3fb10bd1051fa00d75",
        "649dbf3fb10bd1051fa00d62",
        "649dbf3fb10bd1051fa00d63",
        "649dbf3fb10bd1051fa00d53",
        # Dempster H8K193
        "689f58ca0467d84e45ae5f66",
        "689f58ca0467d84e45ae5f69",
        "689f58ca0467d84e45ae5f75",
        "689f58ca0467d84e45ae5f76",
        "689f58ca0467d84e45ae5f7d",
        "689f58ca0467d84e45ae5f80",
        "689f58ca0467d84e45ae5f88",
        # Fort MacPherson
        "649dbf48b10bd1051fa00dcb",
        "649dbf48b10bd1051fa00dcc",
        "649dbf48b10bd1051fa00dd3",
        "649dbf48b10bd1051fa00dbf",
        "649dbf48b10bd1051fa00dd6",
        "649dbf48b10bd1051fa00dde",
        "649dbf48b10bd1051fa00dbc",
        # Ninelin Lake
        "649dbf4ab10bd1051fa00df9",
        "649dbf4ab10bd1051fa00def",
        "649dbf4ab10bd1051fa00de2",
        "649dbf4ab10bd1051fa00dee",
        "649dbf4ab10bd1051fa00df6",
        "649dbf4ab10bd1051fa00e01",
        "649dbf4ab10bd1051fa00ddf",
        # Shell Lake
        "649dbfb0b10bd1051fa00fd5",
        "649dbfb0b10bd1051fa00fe1",
        "649dbfb0b10bd1051fa00fe2",
        "649dbfb0b10bd1051fa00fe9",
        "649dbfb0b10bd1051fa00fec",
        "649dbfb0b10bd1051fa00ff4",
        "649dbfb0b10bd1051fa00fd2",
        # Tsiigehtchic
        "649dcf0b50647525a193384c",
        "649dcf0b50647525a1933853",
        "649dcf0b50647525a1933854",
        "649dcf0b50647525a193385b",
        "649dcf0b50647525a193385e",
        "649dcf0b50647525a1933866",
        "649dcf0b50647525a193384b"
      )
    ),
    
    highwaysstations = list(
      stationIds = c(
        "6160c6b8646994630872810f",   # H1K254
        "64d2c067684ae637e57ae863",   # H1K629
        "6160c6b8646994630872810a",   # H3K285
        "6160c6b96469946308728135",   # H5K208
        "64ee57ed684ae637e57ae875"    # H7K202
      ),
      fields = c(
        # H1K254
        "649dc99e50647525a1931d24",
        "649dc99e50647525a1931d1d",
        "649dc99e50647525a1931d25",
        "649dc99e50647525a1931d2c",
        "649dc99e50647525a1931d37",
        "649dc99e50647525a1931d2f",
        "649dc99e50647525a1931d1b",
        # H1K629
        "649dc99c50647525a1931d05",
        "649dc99c50647525a1931d0c",
        "649dc99c50647525a1931cfd",
        "649dc99c50647525a1931d04",
        "649dc99c50647525a1931d17",
        "649dc99c50647525a1931d0f",
        "649dc99c50647525a1931cfb",
        # H3K285
        "649dc99fb10bd1051fa02a69",
        "649dc99fb10bd1051fa02a74",
        "649dc99fb10bd1051fa02a62",
        "649dc99fb10bd1051fa02a6a",
        "649dc99fb10bd1051fa02a71",
        "649dc99fb10bd1051fa02a7c",
        "649dc99fb10bd1051fa02a60",
        # H5K208
        "64ef5d0a5e13d30e1e228959",
        "64ef5d0a5e13d30e1e228955",
        "64ef5d0a5e13d30e1e228953",
        "64ef5d0a5e13d30e1e228954",
        "64ef5d0a5e13d30e1e228958",
        "64ef5d0a5e13d30e1e228964",
        "64ef5d0a5e13d30e1e228965",
        # H7K202
        "64ee5b36552e756fb87f0821",
        "64ee5b36552e756fb87f0820",
        "64ee5b36552e756fb87f0822",
        "64ee5b36552e756fb87f0825",
        "64ee5b36552e756fb87f0831",
        "64ee5b36552e756fb87f0826",
        "64ee5b36552e756fb87f0832"
      )
    )
    
  )
  
  
  # Step 3: Set date range and output date string
  # start_date <- "2025-06-29T00:00:00Z"
  # end_date <- "2025-06-30T00:00:00Z"
  # output_date <- format(ymd("2025-06-30"), "%B%d_%Y")  # "June30_2025"
  
  # Step 4: Loop through each group and download
  for (group_name in names(station_groups)) {
    group <- station_groups[[group_name]]
    
    download_body <- list(
      startDate = start_date,
      endDate = end_date,
      addUnits = FALSE,
      fields = group$fields,
      stationIds = group$stationIds
    )
    
    response <- POST(
      url = "https://fts360api.com/data/v1/agencies/420/records/csv",
      body = toJSON(download_body, auto_unbox = TRUE),
      encode = "raw",
      add_headers(
        Authorization = paste("Bearer", token),
        `Content-Type` = "application/json",
        Accept = "text/csv"
      )
    )
    
    if (status_code(response) == 200) {
      file_name <- paste0("ftsdownload_", group_name, "_", output_date, ".csv")
      writeBin(content(response, "raw"), file_name)
      cat("✅ Saved:", file_name, "\n")
    } else {
      cat("❌ Download failed for", group_name, ". Status code:", status_code(response), "\n")
      print(content(response, as = "text"))
    }
  }
  
  # merge data together
  # list all the matching CSV files
  csv_files <- list.files(pattern = paste0("^ftsdownload_.*_", output_date, "\\.csv$"))
  
  # Read them in and tag them with 'type' from the filename
  #replace /// values with NA
  merged_data <- map_dfr(csv_files, function(file) {
    df <- read.csv(file)
    
    # Skip empty files (0 rows or all logical columns)
    if (nrow(df) == 0 || all(sapply(df, is.logical))) {
      cat("⚠️  Skipping empty file:", file, "\n")
      return(NULL)  # This will be filtered out by map_dfr
    }
    
    # Replace placeholders with NA
    df[df == "/////"] <- NA
    df[df == "///"] <- NA
    df[df == -9999999.0] <- NA
    
    # Define target columns for numeric coercion
    all_numeric_cols <- c("Temp", "Rh", "Rn_1", "PYR", "Wspd", "Dir", "SD", "TS1", "TS2", "TS3", "Baro")
    
    # Only use columns that exist in the current dataframe
    cols_present <- intersect(all_numeric_cols, names(df))
    
    # Coerce only those to numeric
    df[cols_present] <- lapply(df[cols_present], function(col) {
      suppressWarnings(as.numeric(col))
    })
    
    df
  })
  
  #Adjust timestamp for timezone UTC -> UTC-6
  # Format time column if needed
  merged_data <- merged_data %>%
    dplyr::mutate(Date = as.POSIXct(Date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC") - lubridate::hours(6))  # Adjust column name
  
  #add other date/time columns
  merged_data["cd_year"] <- lubridate::year(merged_data$Date)
  merged_data["JD"] <- lubridate::yday(merged_data$Date)
  merged_data["cd_month"] <- lubridate::month(merged_data$Date)
  merged_data["cd_day"] <- lubridate::day(merged_data$Date)
  merged_data["cd_hour"] <- lubridate::hour(merged_data$Date)
  merged_data["cd_time"] <- base::sprintf("%02d:00",merged_data$cd_hour)
  
  #change station names to match database - add "FTS" at the end
  sitename <- unique(merged_data$Station.name)
  for (i in sitename){
    merged_data$Station.name[merged_data$Station.name == i]<- paste0(i, " FTS")
  }
  
  #change other station names
  merged_data$Station.name[merged_data$Station.name == "Daring Lake FTS"] <- "Daring FTS"
  merged_data$Station.name[merged_data$Station.name == "Fort MacPherson FTS"] <- "Fort McPherson FTS"
  merged_data$Station.name[merged_data$Station.name == "Harry Channel FTS"] <- "Harry FTS"
  merged_data$Station.name[merged_data$Station.name == "Lutsel'Ke FTS"] <- "Lutsel Ke FTS"
  merged_data$Station.name[merged_data$Station.name == "Mobile 5 -Tsu Lake FTS"] <- "Tsu Lake FTS"
  merged_data$Station.name[merged_data$Station.name == "Peel Plateau FTS"] <- "Peel FTS"
  merged_data$Station.name[merged_data$Station.name == "Wha Ti FTS"] <- "Whati FTS"
  merged_data$Station.name[merged_data$Station.name == "Buffalo  Junction FTS"] <- "Buffalo Junction FTS"
  merged_data$Station.name[merged_data$Station.name == "Talston River Dam FTS"] <- "Taltson River Dam FTS"
  
  #Change column names
  merged_data <- merged_data %>%
    dplyr::rename("STATION_NAME" = "Station.name",
                  "FTS360_ID" = "Station.ID",
                  "T_AIR_HIGH_C"="Temp",
                  "RH_HIGH_PER"="Rh",
                  "RAIN_MM"="Rn_1",
                  "SW_IN_WM2"="PYR",
                  "WIND_SP_MS"="Wspd",
                  "WIND_DIR_DEG"="Dir",
                  "SR50_CM"="SD",
                  "T_SOIL_1_C"="TS1",
                  "T_SOIL_2_C"="TS2",
                  "T_SOIL_3_C"="TS3",
                  "BARO_PRESSURE_MBAR"="Baro")
  
  #flag data and add additional flag columns as needed
  #flag function - adjusted slightly for merged_data dataframe
  
  # Data flagging function
  flag_data = function (df){
    
    df$rain_flag <- dplyr::if_else((df$T_AIR_HIGH_C<(-0.1)&df$RAIN_MM>0|df$RAIN_MM<0), "QC", NA)
    df$wind_sp_flag <- dplyr::if_else((df$WIND_SP_MS < 0|(zoo::rollapply(df$WIND_SP_MS, width = 5, FUN = sd, fill = NA)<0.001)), "QC", NA)
    df$wind_dir_flag <- dplyr::if_else((df$WIND_DIR_DEG < 0|df$WIND_DIR_DEG > 360|df$WIND_SP_MS <=0 | (zoo::rollapply(df$WIND_DIR_DEG, width = 5, FUN = sd, fill = NA)<0.1)), "QC", NA)
    df$RH_flag <- dplyr::if_else((df$RH_HIGH_PER<0|df$RH_HIGH_PER>100), "QC", NA)
    df$t_air_high_flag <- dplyr::if_else((df$T_AIR_HIGH_C<(-60)|df$T_AIR_HIGH_C>50), "QC", NA)
    df$sw_in_flag <- dplyr::if_else((df$SW_IN_WM2<0), "QC", NA)
    
    df <- dplyr::arrange(df, STATION_NAME, cd_year, JD)
    df  <- dplyr::group_by(df, STATION_NAME, cd_year)
    df  <- dplyr::mutate(df, year.start.JD = min(JD, na.rm=TRUE))
    df  <- dplyr::mutate(df, first.above.zero = head(c(JD[c(T_AIR_HIGH_C[-length(T_AIR_HIGH_C)] > 0, TRUE)]), 1))
    df$first.above.zero[df$first.above.zero == df$year.start.JD] <- NA
    
    df  <- dplyr::arrange(.data = df , STATION_NAME, cd_year, JD)
    df  <- dplyr::group_by(.data = df , STATION_NAME, cd_year)
    df  <- dplyr::mutate(.data = df , rollmeanAT = zoo::rollmean(T_AIR_HIGH_C, k=720, fill=NA))
    df$rain_flag <- dplyr::if_else((df$RAIN_MM>0& df$first.above.zero==df$JD)| (df$RAIN_MM>0& df$first.above.zero+1==df$JD)| (df$T_AIR_HIGH_C>0&df$rollmeanAT<0&df$RAIN_MM>0), "QC_PSM", df$rain_flag)
    
    drops <- c("rollmeanAT","first.above.zero", "year.start.JD")
    df <- df[ , !(names(df) %in% drops)]
    
    return(df)
    
  }
  
  merged_data <- flag_data(merged_data)
  
  #adjust wind speed units from km hr-1 to m s-1
  merged_data$WIND_SP_MS = dplyr::if_else(!is.na(merged_data$WIND_SP_MS),  merged_data$WIND_SP_MS/3.6, merged_data$WIND_SP_MS, missing = merged_data$WIND_SP_MS)
  
  #add additional flag columns
  new_flag_cols <- c("sr50_flag",
                     "t_soil_1_flag",
                     "t_soil_2_flag",
                     "t_soil_3_flag",
                     "t_soil_4_flag",
                     "t_soil_5_flag",
                     "t_soil_6_flag",
                     "t_air_2_flag",
                     "RH_2_flag",
                     "t_water_flag",
                     "t_water_2_flag",
                     "water_depth_flag",
                     "water_depth_corr_flag",
                     "baro_pressure_flag")
  
  merged_data[,new_flag_cols] <- NA
  
  merged_data<<-merged_data
  
}


#get tables ready for merge statements
#Climate UAT edit
connection_UAT <- odbc::dbConnect(drv = odbc::odbc(), dsn = "ECC_CLIMATE_UAT", uid = "CLIMATE_EDIT", pwd = Sys.getenv("ORACLE_CLIM_EDIT_PWD"))

# stations table
cd_stations<-odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_STATIONS")

# data flags table
cd_data_flags<-odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_DATA_FLAGS_LUT")

# data types table
cd_data_types<-odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_DATA_TYPES_LUT")

# #data tables
# Table0<-odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_0_V")
# Table3<-odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_3_V")
# Table1<-odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_1_V")
# Table2<-odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_2_V")

#select most recent data from FTS stations
#get_latest_date <- odbc::dbGetQuery(connection_UAT, "SELECT * FROM ECC_CLIMATE.CD_CLIMATE_DATA_1_V ORDER BY CD_YEAR DESC, CD_MONTH DESC, CD_DAY DESC, CD_TIME DESC FETCH FIRST 1 ROWS ONLY")
#latest_date <- as.Date(paste(get_latest_date$CD_YEAR, get_latest_date$CD_MONTH, get_latest_date$CD_DAY, sep = "-"), format = "%Y-%m-%d")

#get data from most recent timestamp until today's date at midnight
downloadFTS(start_date = paste0((Sys.Date()-1), "T05:00:00Z"), 
            end_date = paste0((Sys.Date()), "T07:00:00Z"),
            output_date = Sys.Date())

#map variables to tables
variable_map <- tibble::tribble(
  ~var_name,       ~data_type_id, ~target_table,         ~flag_col,
  "T_AIR_HIGH_C",   11,           "CD_CLIMATE_DATA_1", "t_air_high_flag",
  "RH_HIGH_PER",     1,           "CD_CLIMATE_DATA_1", "RH_flag",
  "RAIN_MM",         5,           "CD_CLIMATE_DATA_1", "rain_flag",
  "WIND_SP_MS",      3,           "CD_CLIMATE_DATA_1", "wind_sp_flag",
  "WIND_DIR_DEG",    4,           "CD_CLIMATE_DATA_0", "wind_dir_flag",
  "SW_IN_WM2",       7,           "CD_CLIMATE_DATA_1", "sw_in_flag",
  "SR50_CM",         6,           "CD_CLIMATE_DATA_1", "sr50_flag",
  "T_SOIL_1_C",     17,           "CD_CLIMATE_DATA_1", "t_soil_1_flag",
  "T_SOIL_2_C",     18,           "CD_CLIMATE_DATA_1", "t_soil_2_flag",
  "T_SOIL_3_C",     19,           "CD_CLIMATE_DATA_1", "t_soil_3_flag",
  "BARO_PRESSURE_MBAR", 23,       "CD_CLIMATE_DATA_2", "baro_pressure_flag"
)

#left join station_id using station name
merged_data <- merged_data %>%
  dplyr::left_join(cd_stations %>% dplyr::select(STATION_NAME, STATION_ID), by = c("STATION_NAME"))

#convert to long format
long_data <- merged_data %>%
  tidyr::pivot_longer(
    cols = all_of(variable_map$var_name),
    names_to = "var_name",
    values_to = "DATA_VALUE"
  )

#add metadata and data flag
long_data <- long_data %>%
  dplyr::left_join(variable_map, by = "var_name") %>%
  dplyr::rowwise() %>%
  dplyr:: mutate(
    # Get the flag string from the named flag column in merged_data (e.g., t_air_high_flag)
    flag_value = if (!is.null(flag_col) && !is.na(flag_col)) get(flag_col) else NA_character_,
    
    # Convert to DATA_FLAG_ID using lookup table; default to 7 if NA
    DATA_FLAG_ID = if (!is.na(flag_value)) {
      cd_data_flags$DATA_FLAG_ID[match(flag_value, cd_data_flags$DATA_FLAG)]
    } else {
      7  # Default: no issue
    }
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-flag_value, -PP)  # Clean up

#split long data into tables for merge statements
#filter for NULL values
table_0 <- long_data %>%
  dplyr::filter(target_table == "CD_CLIMATE_DATA_0") %>%
  dplyr::select(STATION_ID, CD_YEAR = cd_year, JD, CD_MONTH = cd_month,
                CD_DAY = cd_day, CD_TIME = cd_time,
                DATA_FLAG_ID, DATA_TYPE_ID = data_type_id, DATA_VALUE) %>%
  dplyr::filter(is.na(DATA_VALUE)==FALSE)

table_1 <- long_data %>%
  dplyr::filter(target_table == "CD_CLIMATE_DATA_1") %>%
  dplyr::select(STATION_ID, CD_YEAR = cd_year, JD, CD_MONTH = cd_month,
                CD_DAY = cd_day, CD_TIME = cd_time,
                DATA_FLAG_ID, DATA_TYPE_ID = data_type_id, DATA_VALUE) %>%
  dplyr::filter(is.na(DATA_VALUE)==FALSE)

table_2 <- long_data %>%
  dplyr::filter(target_table == "CD_CLIMATE_DATA_2") %>%
  dplyr::select(STATION_ID, CD_YEAR = cd_year, JD, CD_MONTH = cd_month,
                CD_DAY = cd_day, CD_TIME = cd_time,
                DATA_FLAG_ID, DATA_TYPE_ID = data_type_id, DATA_VALUE) %>%
  dplyr::filter(is.na(DATA_VALUE)==FALSE)

table_3 <- long_data %>%
  dplyr::filter(target_table == "CD_CLIMATE_DATA_3") %>%
  dplyr::select(STATION_ID, CD_YEAR = cd_year, JD, CD_MONTH = cd_month,
                CD_DAY = cd_day, CD_TIME = cd_time,
                DATA_FLAG_ID, DATA_TYPE_ID = data_type_id, DATA_VALUE) %>%
  dplyr::filter(is.na(DATA_VALUE)==FALSE)

#make table 1 a bit smaller for test
# table_1 <- table_1[1:100, ]

#test looping through rows
for (i in seq_len(nrow(table_1))) {
  row <- table_1[i, ]
  
  sql <- "
    MERGE INTO ECC_CLIMATE.CD_CLIMATE_DATA_1 db
    USING (
      SELECT
        ? AS STATION_ID,
        ? AS CD_YEAR,
        ? AS CD_MONTH,
        ? AS CD_DAY,
        ? AS CD_TIME,
        ? AS DATA_TYPE_ID,
        ? AS DATA_VALUE,
        ? AS DATA_FLAG_ID
      FROM DUAL
    ) df
    ON (
      db.STATION_ID = df.STATION_ID AND
      db.CD_YEAR = df.CD_YEAR AND
      db.CD_MONTH = df.CD_MONTH AND
      db.CD_DAY = df.CD_DAY AND
      db.CD_TIME = df.CD_TIME AND
      db.DATA_TYPE_ID = df.DATA_TYPE_ID
    )
    WHEN MATCHED THEN UPDATE SET
      db.DATA_VALUE = df.DATA_VALUE,
      db.DATA_FLAG_ID = df.DATA_FLAG_ID
    WHEN NOT MATCHED THEN INSERT (
      STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME, DATA_TYPE_ID, DATA_VALUE, DATA_FLAG_ID
    ) VALUES (
      df.STATION_ID, df.CD_YEAR, df.CD_MONTH, df.CD_DAY, df.CD_TIME, df.DATA_TYPE_ID, df.DATA_VALUE, df.DATA_FLAG_ID
    )
  "
  
  # Safely bind the parameters
  DBI::dbExecute(connection_UAT, sql, params = list(
    row$STATION_ID,
    row$CD_YEAR,
    row$CD_MONTH,
    row$CD_DAY,
    row$CD_TIME,
    row$DATA_TYPE_ID,
    row$DATA_VALUE,
    row$DATA_FLAG_ID
  ))
}

for (i in seq_len(nrow(table_0))) {
  row <- table_0[i, ]
  
  sql <- "
    MERGE INTO ECC_CLIMATE.CD_CLIMATE_DATA_0 db
    USING (
      SELECT
        ? AS STATION_ID,
        ? AS CD_YEAR,
        ? AS CD_MONTH,
        ? AS CD_DAY,
        ? AS CD_TIME,
        ? AS DATA_TYPE_ID,
        ? AS DATA_VALUE,
        ? AS DATA_FLAG_ID
      FROM DUAL
    ) df
    ON (
      db.STATION_ID = df.STATION_ID AND
      db.CD_YEAR = df.CD_YEAR AND
      db.CD_MONTH = df.CD_MONTH AND
      db.CD_DAY = df.CD_DAY AND
      db.CD_TIME = df.CD_TIME AND
      db.DATA_TYPE_ID = df.DATA_TYPE_ID
    )
    WHEN MATCHED THEN UPDATE SET
      db.DATA_VALUE = df.DATA_VALUE,
      db.DATA_FLAG_ID = df.DATA_FLAG_ID
    WHEN NOT MATCHED THEN INSERT (
      STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME, DATA_TYPE_ID, DATA_VALUE, DATA_FLAG_ID
    ) VALUES (
      df.STATION_ID, df.CD_YEAR, df.CD_MONTH, df.CD_DAY, df.CD_TIME, df.DATA_TYPE_ID, df.DATA_VALUE, df.DATA_FLAG_ID
    )
  "
  
  # Safely bind the parameters
  DBI::dbExecute(connection_UAT, sql, params = list(
    row$STATION_ID,
    row$CD_YEAR,
    row$CD_MONTH,
    row$CD_DAY,
    row$CD_TIME,
    row$DATA_TYPE_ID,
    row$DATA_VALUE,
    row$DATA_FLAG_ID
  ))
}

for (i in seq_len(nrow(table_2))) {
  row <- table_2[i, ]
  
  sql <- "
    MERGE INTO ECC_CLIMATE.CD_CLIMATE_DATA_2 db
    USING (
      SELECT
        ? AS STATION_ID,
        ? AS CD_YEAR,
        ? AS CD_MONTH,
        ? AS CD_DAY,
        ? AS CD_TIME,
        ? AS DATA_TYPE_ID,
        ? AS DATA_VALUE,
        ? AS DATA_FLAG_ID
      FROM DUAL
    ) df
    ON (
      db.STATION_ID = df.STATION_ID AND
      db.CD_YEAR = df.CD_YEAR AND
      db.CD_MONTH = df.CD_MONTH AND
      db.CD_DAY = df.CD_DAY AND
      db.CD_TIME = df.CD_TIME AND
      db.DATA_TYPE_ID = df.DATA_TYPE_ID
    )
    WHEN MATCHED THEN UPDATE SET
      db.DATA_VALUE = df.DATA_VALUE,
      db.DATA_FLAG_ID = df.DATA_FLAG_ID
    WHEN NOT MATCHED THEN INSERT (
      STATION_ID, CD_YEAR, CD_MONTH, CD_DAY, CD_TIME, DATA_TYPE_ID, DATA_VALUE, DATA_FLAG_ID
    ) VALUES (
      df.STATION_ID, df.CD_YEAR, df.CD_MONTH, df.CD_DAY, df.CD_TIME, df.DATA_TYPE_ID, df.DATA_VALUE, df.DATA_FLAG_ID
    )
  "
  
  # Safely bind the parameters
  DBI::dbExecute(connection_UAT, sql, params = list(
    row$STATION_ID,
    row$CD_YEAR,
    row$CD_MONTH,
    row$CD_DAY,
    row$CD_TIME,
    row$DATA_TYPE_ID,
    row$DATA_VALUE,
    row$DATA_FLAG_ID
  ))
}

