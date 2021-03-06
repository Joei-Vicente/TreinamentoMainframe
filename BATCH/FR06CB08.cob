      *================================================================*00001004
       IDENTIFICATION                  DIVISION.                        00010000
      *================================================================*00010104
                                                                        00011004
       PROGRAM-ID. FR06CB08.                                            00020000
                                                                        00021004
      *================================================================*00030000
      *     AUTOR....: JOEI LORENTI                                     00040004
      *     EMPRESA..: FOURSYS                                          00050004
      *     DATA.....: 20/04/2022                                       00060004
      *     OBJETIVO.: RECEBER WRK-NUM DA SYSIN E IMPRIMIR NA SYSOUT O  00070004
      *                RESULTADO EM WRK-R.                              00071004
      *================================================================*00072000
       ENVIRONMENT                      DIVISION.                       00073004
      *================================================================*00074000
                                                                        00074104
      *----------------------------------------------------------------*00074204
       CONFIGURATION                    SECTION.                        00074304
      *----------------------------------------------------------------*00074404
                                                                        00074504
       SPECIAL-NAMES.                                                   00074604
           DECIMAL-POINT                IS COMMA.                       00074704
                                                                        00074804
      *================================================================*00075000
       DATA                             DIVISION.                       00076000
      *================================================================*00077000
                                                                        00077104
      *----------------------------------------------------------------*00078004
       WORKING-STORAGE                  SECTION.                        00079000
      *----------------------------------------------------------------*00080004
                                                                        00080104
       01 WRK-NUM.                                                      00080200
          05 WRK-N1          PIC 9(02)        VALUE ZEROS.              00081001
          05 WRK-N2          PIC 9(02)        VALUE ZEROS.              00082001
       77 WRK-RES            PIC 9(03)        VALUE ZEROS.              00082304
                                                                        00082800
      *================================================================*00082900
       PROCEDURE                        DIVISION.                       00083000
      *================================================================*00083100
                                                                        00083206
      *----------------------------------------------------------------*00083306
       0000-PRINCIPAL                   SECTION.                        00083406
      *----------------------------------------------------------------*00083506
                                                                        00083606
           ACCEPT WRK-NUM          FROM SYSIN.                          00083700
                                                                        00083804
      *----------------------ADD COM GIVING----------------------------*00083904
                                                                        00084004
           ADD WRK-N1 WRK-N2 GIVING WRK-RES.                            00084100
                                                                        00084204
           DISPLAY '-------------------------'                          00084304
           DISPLAY 'A SOMA DE    : ' WRK-N1.                            00084400
           DISPLAY 'COM          : ' WRK-N2.                            00084500
           DISPLAY 'RESULTOU     : ' WRK-RES.                           00084600
           DISPLAY '-------------------------'                          00084704
                                                                        00084804
           STOP RUN.                                                    00085000
      *----------------------------------------------------------------*00085106
       0000-99-FIM.                    SECTION.                         00085207
      *----------------------------------------------------------------*00085306
                                                                        00085404
      *----------------------------------------------------------------*00086004
       9000-TRATAR-ERRO                SECTION.                         00087005
      *----------------------------------------------------------------*00087104
                                                                        00087205
      *----------------------------------------------------------------*00087304
       9000-99-FIM.                    EXIT.                            00088004
      *----------------------------------------------------------------*00089104
