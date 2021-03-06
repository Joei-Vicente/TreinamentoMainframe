      *================================================================J00001000
       IDENTIFICATION                  DIVISION.                        00002000
      *================================================================*00003000
                                                                        00004000
       PROGRAM-ID. FR06CB28.                                            00005003
                                                                        00006000
      *================================================================*00007000
      *                                                                 00008000
      *     AUTOR.....: JOEI LORENTI                                    00009000
      *     ANALISTA..: IVAN SANCHES                                    00009100
      *     EMPRESA...: FOURSYS                                         00009200
      *     DATA......: 29/04/2022                                      00009300
      *                                                                 00009400
      *----------------------------------------------------------------*00009500
      *                                                                 00009600
      *     OBJETIVO: ABERTURA DO AQUIVO FUNC                           00009700
      *                                                                 00022000
      *================================================================*00022100
                                                                        00023000
      *================================================================*00024000
       ENVIRONMENT                      DIVISION.                       00025000
      *================================================================*00026000
                                                                        00027000
      *----------------------------------------------------------------*00028000
       CONFIGURATION                    SECTION.                        00029000
      *----------------------------------------------------------------*00030000
                                                                        00040000
        SPECIAL-NAMES.                                                  00050000
              DECIMAL-POINT            IS COMMA.                        00060000
                                                                        00070000
      *----------------------------------------------------------------*00071000
       INPUT-OUTPUT                    SECTION.                         00071201
      *----------------------------------------------------------------*00071301
                                                                        00071401
       FILE-CONTROL.                                                    00071501
             SELECT FUNC               ASSIGN TO JCLFUNC                00071601
                FILE STATUS            IS WRK-FS-FUNC.                  00071701
      *----------------------------------------------------------------*00071801
                                                                        00071901
      *================================================================J00073000
       DATA                            DIVISION.                        00074000
      *================================================================*00074100
                                                                        00074200
      *----------------------------------------------------------------*00074301
       FILE                            SECTION.                         00074401
      *----------------------------------------------------------------*00074501
                                                                        00074601
       FD FUNC                                                          00074701
           RECORDING MODE IS F                                          00074801
           BLOCK CONTAINS 0 RECORDS.                                    00074901
                                                                        00075001
       01 FD-REGISTRO.                                                  00075101
          05 FD-LINHA     PIC X(40).                                    00075201
                                                                        00075301
      *----------------------------------------------------------------*00075400
       WORKING-STORAGE                 SECTION.                         00075500
      *----------------------------------------------------------------D00075601
                                                                        00076201
       77 WRK-FS-MSG       PIC X(30) VALUE SPACES.                      00076301
                                                                        00076500
      *----------------------------------------------------------------*00076600
      *                        AREA DE ACUMULADORES                    *00076700
      *----------------------------------------------------------------*00076800
                                                                        00076900
      *01 WRK-ACU-LIDOS                PIC 9(02)        VALUE ZEROS.    00077001
                                                                        00077100
      *----------------------------------------------------------------*00077201
      *                        TESTE DE FILE-STATUS                    *00077301
      *----------------------------------------------------------------*00077401
                                                                        00077501
                                                                        00077601
       77 WRK-FS-FUNC PIC X(02) VALUE SPACES.                           00077701
                                                                        00077801
                                                                        00077900
      *================================================================*00078000
       PROCEDURE                       DIVISION.                        00078100
      *================================================================*00078200
                                                                        00078300
      *----------------------------------------------------------------*00078400
       0000-PRINCIPAL                   SECTION.                        00078500
      *----------------------------------------------------------------*00078600
                                                                        00078700
           PERFORM 0100-INICIAR.                                        00078800
                                                                        00078901
           PERFORM 0200-PROCESSAR                                       00079001
                                                                        00079300
           PERFORM 0300-FINALIZAR.                                      00079400
                                                                        00079500
           STOP RUN.                                                    00079600
                                                                        00079700
      *----------------------------------------------------------------*00079800
       0000-99-FIM.                    EXIT.                            00079900
      *----------------------------------------------------------------*00080000
                                                                        00080100
      *----------------------------------------------------------------*00080200
       0100-INICIAR                    SECTION.                         00080300
      *----------------------------------------------------------------*00080400
                                                                        00080500
           OPEN INPUT FUNC.                                             00080601
                                                                        00080700
           MOVE ' STATUS NO OPEN ' TO WRK-FS-MSG.                       00080801
                                                                        00080901
           PERFORM 9000-TRATAR-ERRO.                                    00081001
                                                                        00081101
      *----------------------------------------------------------------*00081200
       0100-99-FIM                     SECTION.                         00081300
      *----------------------------------------------------------------*00081400
      *----------------------------------------------------------------*00081500
       0200-PROCESSAR                  SECTION.                         00081600
      *----------------------------------------------------------------*00081700
                                                                        00081802
           CONTINUE.                                                    00081901
                                                                        00082900
      *----------------------------------------------------------------*00083000
       0200-99-FIM.                    EXIT.                            00083100
      *----------------------------------------------------------------*00083200
                                                                        00083300
      *----------------------------------------------------------------*00083400
       0300-FINALIZAR                  SECTION.                         00083500
      *----------------------------------------------------------------*00083600
                                                                        00083700
             CLOSE FUNC.                                                00083801
             MOVE  ' STATUS NO CLOSE ' TO WRK-FS-MSG.                   00083901
                                                                        00084001
             PERFORM 9000-TRATAR-ERRO.                                  00084101
                                                                        00084201
                                                                        00084300
      *----------------------------------------------------------------*00084400
       0300-99-FIM.                    EXIT.                            00084500
      *----------------------------------------------------------------*00084600
                                                                        00084700
      *----------------------------------------------------------------*00084800
       9000-TRATAR-ERRO                SECTION.                         00084900
      *----------------------------------------------------------------*00085000
                                                                        00085101
             DISPLAY '----------------------------'.                    00085202
             DISPLAY WRK-FS-MSG.                                        00085302
             DISPLAY WRK-FS-FUNC.                                       00085401
             DISPLAY '----------------------------'                     00085502
                                                                        00085601
                                                                        00085701
      *----------------------------------------------------------------*00085800
       9000-99-FIM.                    EXIT.                            00085900
      *----------------------------------------------------------------*00086000
