      *================================================================*00001011
       IDENTIFICATION                  DIVISION.                        00010000
      *================================================================*00011011
                                                                        00012011
       PROGRAM-ID. FR06CB20.                                            00020011
                                                                        00021011
      *================================================================*00030000
      *                                                                 00041011
      *     AUTOR......: JOEI LORENTI                                   00042015
      *     EMPRESA....: FOURSYS                                        00050015
      *     DATA.......: 22/04/2022                                     00060015
      *                                                                 00060115
      *----------------------------------------------------------------*00061015
      *                                                                 00062015
      *     OBJETIVO: RECEBER VIA SYSIN NAS NAS VARIAVEIS WRK-VALOR E   00070000
      *               WRK-QTPARCELAS, DIVIDIR VALOR POR PARCELAS        00071000
      *               GERAR NA SYSOUT O VALOR DA PARCELA.               00071100
      *                                                                 00071211
      *================================================================*00071300
                                                                        00071413
      *================================================================*00071513
       ENVIRONMENT                      DIVISION.                       00071614
      *================================================================*00071700
                                                                        00071811
      *================================================================*00071900
       DATA                             DIVISION.                       00072000
      *================================================================*00072100
                                                                        00072211
      *----------------------------------------------------------------*00072311
       WORKING-STORAGE                  SECTION.                        00072400
      *----------------------------------------------------------------*00072511
                                                                        00073111
       01 WRK-REGISTRO.                                                 00073211
          05 WRK-VALOR       PIC 9(05)        VALUE ZEROS.              00074006
          05 WRK-QTPARCELAS  PIC 9(03)        VALUE ZEROS.              00075006
          05 WRK-VALPARCELA  PIC 9(05)        VALUE ZEROS.              00077006
       77 WRK-ERRO           PIC X(30) VALUE SPACES.                    00077113
                                                                        00077214
      *================================================================*00079000
       PROCEDURE                        DIVISION.                       00080000
      *================================================================*00080100
                                                                        00080211
      *----------------------------------------------------------------*00080311
       0000-PRINCIPAL                   SECTION.                        00080511
      *----------------------------------------------------------------*00080611
                                                                        00080711
           ACCEPT WRK-REGISTRO     FROM SYSIN.                          00080811
                                                                        00080911
      *--------------- DIVIDE - ON SIZE ERROR -------------------------*00081011
                                                                        00081111
           DIVIDE WRK-VALOR    BY WRK-QTPARCELAS                        00081211
                               GIVING WRK-VALPARCELA                    00081311
               ON SIZE ERROR                                            00081411
                MOVE 'IMPOSSIVEL DIVIDIR POR ZERO' TO WRK-ERRO          00081513
                 PERFORM 9000-TRATAR-ERRO                               00081611
               NOT ON SIZE ERROR                                        00081711
                 DISPLAY '--------------------------------------------' 00081811
                 DISPLAY 'VALOR TOTAL......: ' WRK-VALOR                00081911
                 DISPLAY 'QTD PARCELAS.....: ' WRK-QTPARCELAS           00082011
                 DISPLAY 'VALOR PARCELA....: ' WRK-VALPARCELA           00082111
                 DISPLAY '--------------------------------------------' 00082211
           END-DIVIDE.                                                  00082312
           STOP RUN.                                                    00083011
                                                                        00083115
      *----------------------------------------------------------------*00083215
       0000-99-FIM.                    EXIT.                            00083315
      *----------------------------------------------------------------*00083415
                                                                        00083915
      *----------------------------------------------------------------*00084011
       9000-TRATAR-ERRO                SECTION.                         00084111
      *----------------------------------------------------------------*00085011
                                                                        00085211
                 DISPLAY WRK-ERRO.                                      00085313
                                                                        00085511
      *----------------------------------------------------------------*00086011
       9000-99-FIM.                    EXIT.                            00087011
      *----------------------------------------------------------------*00088011
