       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. F06CIP1.                                             00020000
       DATA DIVISION.                                                   00030000
       WORKING-STORAGE SECTION.                                         00040000
           COPY F06CIM1.                                                00050000
       01 WRK-SAIDA   PIC X(30) VALUE SPACES.                           00051000
                                                                        00051101
                                                                        00054001
       PROCEDURE DIVISION.                                              00060000
           EXEC CICS SEND                                               00070000
             MAPSET('F06CIM1')                                          00080000
             MAP('MAPTEST')                                             00090000
             ERASE                                                      00100000
             MAPONLY                                                    00110000
           END-EXEC.                                                    00120000
                                                                        00120100
           EXEC CICS RECEIVE                                            00120200
             MAPSET('F06CIM1')                                          00120300
             MAP('MAPTEST')                                             00120400
             INTO(MAPTESTI)                                             00120600
           END-EXEC.                                                    00120700
                                                                        00120800
           IF EIBAID = '3'                                              00120901
           EXEC CICS                                                    00121001
             RETURN                                                     00121101
           END-EXEC                                                     00121201
           END-IF.                                                      00121301
                                                                        00121401
           EVALUATE TRUE                                                00121501
             WHEN IDI EQUAL '00001'                                     00121601
               MOVE 'CARLA PEREZ      '   TO NOMEO                      00121701
               MOVE 'TCHN'                TO SETORO                     00121801
               MOVE '5000'                TO SALO                       00121901
               MOVE '16'                  TO DATADIAO                   00122002
               MOVE '11'                  TO DATAMESO                   00122102
               MOVE '2022'                TO DATAANOO                   00122202
               MOVE 'LOIRA@TCHAN.COM.BR'  TO EMAILO                     00122401
               MOVE 'ID - ENCONTRADO  '   TO MSGO                       00122501
                                                                        00122601
             WHEN IDI EQUAL '00002'                                     00122701
               MOVE 'RINGO STAR       '   TO NOMEO                      00122801
               MOVE 'BEAT'                TO SETORO                     00122901
               MOVE '5000'                TO SALO                       00123001
               MOVE '16'                  TO DATADIAO                   00123102
               MOVE '11'                  TO DATAMESO                   00123202
               MOVE '2022'                TO DATAANOO                   00123302
               MOVE 'STAR@BETLES.COM'     TO EMAILO                     00123401
               MOVE 'ID - ENCONTRADO  '   TO MSGO                       00123501
                                                                        00123601
             WHEN IDI EQUAL '00003'                                     00123701
               MOVE 'TIRIRICA         '   TO NOMEO                      00123801
               MOVE 'ABST'                TO SETORO                     00123901
               MOVE '5000'                TO SALO                       00124001
               MOVE '16'                  TO DATADIAO                   00124102
               MOVE '11'                  TO DATAMESO                   00124202
               MOVE '2022'                TO DATAANOO                   00124302
               MOVE 'ABESTADO@CAMARA.COM' TO EMAILO                     00124401
               MOVE 'ID - ENCONTRADO  '   TO MSGO                       00124501
                                                                        00124601
             WHEN OTHER                                                 00124701
               MOVE '-----------------'   TO NOMEO                      00124801
               MOVE '----'                TO SETORO                     00124901
               MOVE 'ID-NAO ENCONTRADA'   TO MSGO                       00125001
                                                                        00125101
             END-EVALUATE.                                              00125201
                                                                        00125301
           EXEC CICS SEND                                               00125401
             MAPSET('F06CIM1')                                          00126001
             MAP('MAPTEST')                                             00130000
             DATAONLY                                                   00130200
           END-EXEC.                                                    00130300
                                                                        00130400
            MOVE 'FINAL DO PRIMEIRO PROGRAMA CICS' TO WRK-SAIDA         00131000
                                                                        00132000
           EXEC CICS                                                    00140000
             RETURN TRANSID('T061')                                     00150001
           END-EXEC.                                                    00160001
                                                                        00170000
                                                                        00180000
                                                                        00190000
                                                                        00200000
                                                                        00210000
