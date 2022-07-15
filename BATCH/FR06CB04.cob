       IDENTIFICATION                  DIVISION.                        00010001
       PROGRAM-ID. FR06CB03.                                            00020001
      *============================================================     00030001
      *     AUTOR   : JOEI LORENTI                                      00040001
      *     EMPRESA : FOURSYS                                           00050001
      *     DATA    : 19/04/2022                                        00060001
      *     OBJETIVO: RECEBER REGISTRO DA SYSIN E GERAR RELATORIO       00070002
      *============================================================     00080001
      *ENVIRONMENT                      DIVISION.                       00081001
       DATA                             DIVISION.                       00082001
       WORKING-STORAGE                  SECTION.                        00083001
       01 WRK-REGISTRO.                                                 00084001
      *   05 FILLER       PIC X(80)     VALUE ALL "-".                  00084101
          05 WRK-ID       PIC 9(05)     VALUE ZEROS.                    00084201
          05 WRK-CLIENTE  PIC X(30)     VALUE SPACES.                   00084301
          05 WRK-INTERNO.                                               00084401
            10 WRK-TELEFONE PIC 9(10)     VALUE ZEROS.                  00084501
            10 WRK-GERENTE  PIC X(15)     VALUE SPACES.                 00084603
      *     10 FILLER       PIC X(80)     VALUE ALL "-".                00084701
       PROCEDURE                        DIVISION.                       00085001
           ACCEPT WRK-REGISTRO FROM SYSIN.                              00086001
           DISPLAY '-----------------------------'.                     00087001
           DISPLAY 'ID...    :'  WRK-ID.                                00087104
           DISPLAY 'CLIENTE  :'  WRK-CLIENTE.                           00087202
           DISPLAY 'TELEFONE :'  WRK-TELEFONE.                          00087302
           DISPLAY 'GERENTE  :'  WRK-GERENTE.                           00087402
           DISPLAY '-----------------------------'.                     00087501
           STOP RUN.                                                    00088001
