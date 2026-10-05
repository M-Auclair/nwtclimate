#QAQC false positives for rain - from Casey's manual QAQC 

#read in sensor metadata sheets:
path <- "C:/Users/Emma_Riley/Documents/NT_Hydrology/Abiotic Monitoring/Data/cleaned/excel_files"
Blueberry <- readxl::read_excel(paste0(path, "/Blueberry_Station_ID_10BB01.xlsx"),
                                sheet = "README", 
                                col_types = c(rep("numeric", 5), "text"),
                                skip = 15)
Blueberry["station"]<-"Blueberry"
BDL <- readxl::read_excel(paste0(path, "/BDL_station_ID_16BD01.xlsx" ),
                          sheet = "READ ME", 
                          col_types = c(rep("numeric", 5), "text"),
                          skip = 14)
BDL["station"]<-"BDL"
Daring_Lake <- readxl::read_excel(paste0(path, "/Daring_Lake_Station_ID_96DL01.xlsx"), 
                                  sheet = "README", 
                                  col_types = c(rep("numeric", 5), "text"),
                                  skip = 15) 
Daring_Lake["station"]<-"Daring Lake"
Daring_Lake_2 <- readxl::read_excel(paste0(path, "/Daring_Lake_Station_ID_96DL02.xlsx"),
                                    sheet = "README", 
                                    col_types = c(rep("numeric", 5), "text"),
                                    skip = 15)
Daring_Lake_2["station"]<-"Daring FTS"
Dempster85 <- readxl::read_excel(paste0(path, "/Dempster_Hwy_Station_ID_14DH85.xlsx"),
                                 sheet = "README", 
                                 col_types = c(rep("numeric", 5), "text"),
                                 skip = 15)
Dempster85["station"]<-"Dempster85"
Dempster515 <- readxl::read_excel(paste0(path, "/Dempster_Hwy_Station_ID_14DH515.xlsx"),
                                  sheet = "README", 
                                  col_types = c(rep("numeric", 5), "text"),
                                  skip = 17)
Dempster515["station"]<-"Dempster515"
Harry1 <- readxl::read_excel(paste0(path, "/Harry_Channel_Station_ID_08HC01.xlsx"), 
                             sheet = "README", 
                             col_types = c(rep("numeric", 5), "text"),
                             skip = 15)
Harry1["station"]<- "Harry"
Harry2 <- readxl::read_excel(paste0(path, "/Harry_Channel_Station_ID_08HC02.xlsx"), 
                             sheet = "README", 
                             col_types = c(rep("numeric", 5), "text"),
                             skip = 15)
Harry2["station"]<-"Harry FTS"
Hoarfrost <- readxl::read_excel(paste0(path, "/Hoarfrost_Station_ID_16HF01.xlsx"),
                                sheet = "README", 
                                col_types = c(rep("numeric", 5), "text"),
                                skip = 16)
Hoarfrost["station"]<-"Hoarfrost FTS"
ITH <- readxl::read_excel(paste0(path, "/ITH_Station_ID_17ITH01.xlsx"),
                          sheet = "README", 
                          col_types = c(rep("numeric", 5), "text"),
                          skip = 15)
ITH["station"]<-"ITH FTS"
Nanisivik <- readxl::read_excel(paste0(path, "/Nanisivik_Station_ID_95NN01.xlsx"),
                                sheet = "README", 
                                col_types = c(rep("numeric", 5), "text"),
                                skip = 15)
Nanisivik["station"]<-"Nanisivik"
Peel1 <- readxl::read_excel(paste0(path, "/Peel_Plateau_Station_ID_10PP01.xlsx"),
                            sheet = "README", 
                            col_types = c(rep("numeric", 5), "text"),
                            skip = 15)
Peel1["station"]<-"Peel"
Peel2 <- readxl::read_excel(paste0(path, "/Peel_Plateau_Station_ID_10PP02.xlsx"),
                            sheet = "README", 
                            col_types = c(rep("numeric", 5), "text"),
                            skip = 15)
Peel2["station"]<-"Peel FTS"
Pocket <- readxl::read_excel(paste0(path, "/Pocket_Lake_Station_ID_93PL01.xlsx"),
                             sheet = "README", 
                             col_types = c(rep("numeric", 5), "text"),
                             skip = 15)
Pocket["station"]<- "Pocket"
Salmita <- readxl::read_excel(paste0(path, "/Salmita_Station_ID_92SM01.xlsx"),
                              sheet = "README", 
                              col_types = c(rep("numeric", 5), "text"),
                              skip = 15)
Salmita["station"]<-"Salmita"
Snare <- readxl::read_excel(paste0(path, "/Snare_Rapids_Station_ID_17SR01.xlsx"),
                            sheet = "README", 
                            col_types = c(rep("numeric", 5), "text"),
                            skip = 15)
Snare["station"]<- "Snare rapids"
Taglu <- readxl::read_excel(paste0(path, "/Taglu_Tower_Station_ID_08TT01.xlsx"),
                            sheet = "README", 
                            col_types = c(rep("numeric", 5), "text"),
                            skip = 15)
Taglu["station"]<-"Taglu"
TibbittM <- readxl::read_excel(paste0(path, "/Tibbitt_Muskeg_Station_ID_99TM01.xlsx"),
                               sheet = "README", 
                               col_types = c(rep("numeric", 5), "text"),
                               skip = 15)
TibbittM["station"]<- "Tibbitt"
Tuktoyaktuk <- readxl::read_excel(paste0(path, "/Tuktoyaktuk_Station_ID_09TK01.xlsx"),
                                  sheet = "README", 
                                  col_types = c(rep("numeric", 5), "text"),
                                  skip = 15)
Tuktoyaktuk["station"]<-"Tuk"
WinterL <- readxl::read_excel(paste0(path, "/Winter_Lake_Station_ID_18WL01.xlsx"),
                              sheet = "README", 
                              col_types = c(rep("numeric", 5), "text"),
                              skip = 15)
WinterL["station"]<- "Winter Lake FTS"

QAQCdf<-rbind(Blueberry, BDL, Daring_Lake, 
              Daring_Lake_2,
              Dempster85,
              Dempster515,
              Harry1,
              Harry2,
              Hoarfrost,
              ITH,
              Nanisivik,
              Peel1,
              Peel2,
              Pocket,
              Salmita,
              Snare,
              Taglu,
              TibbittM,
              Tuktoyaktuk,
              WinterL)

QAQCraindf <- QAQCdf %>%
  dplyr::filter(stringr::str_detect(Note, "melt"))

path <- "C:/Users/emma_riley/Documents/NT_Hydrology/Abiotic Monitoring/data/cleaned/"

saveRDS(QAQCraindf, paste0(path, "/QAQCraindf.rds"))
