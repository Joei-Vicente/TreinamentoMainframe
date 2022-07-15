       IDENTIFICATION                  DIVISION.                        00010000
       PROGRAM-ID. FR06CB03.                                            00020000
      *============================================================     00030000
      *     AUTOR   : JOEI LORENTI                                      00040000
      *     EMPRESA : FOURSYS                                           00050000
      *     DATA    : 19/04/2022                                        00060000
      *     OBJETIVO: RECEBER DADOS DA SYSIN                            00070000
      *============================================================     00080000
      *ENVIRONMENT                      DIVISION.                       00081000
       DATA                             DIVISION.                       00082000
       WORKING-STORAGE                  SECTION.                        00083000
       77 WRK-NOME      PIC X(15)       VALUE SPACES.                   00084001
       PROCEDURE                        DIVISION.                       00090000
           ACCEPT WRK-NOME FROM SYSIN.                                  00091000
           DISPLAY 'NOME INFORMADO .........' WRK-NOME.                 00100000
           STOP RUN.                                                    00110000
