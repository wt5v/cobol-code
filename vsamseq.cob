//HERC01V  JOB (COBOL),'VSAMSEQ',CLASS=A,MSGCLASS=H,                    00000100
//             REGION=8M,TIME=1440,                                     00000200
//             MSGLEVEL=(1,1)                                           00000300
//VSAMSEQ  EXEC COBUCG,                                                 00000400
//             PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'        00000500
//COB.SYSPUNCH DD DUMMY                                                 00000600
//COB.SYSIN DD *                                                        00000700
      ********************************************************          00000800
      * A COBOL PROGRAM TEMPLATE                                        00000900
      ********************************************************          00001000
       IDENTIFICATION DIVISION.                                         00001100
       PROGRAM-ID. VSAMSEQ.                                             00001201
       AUTHOR. KRIS W KEENER.                                           00001300
       INSTALLATION.   THE LAB.                                         00001400
       DATE-WRITTEN.   DECEMBER 04 2021.                                00001500
       DATE-COMPILED.  DECEMBER 04 2021.                                00001600
       SECURITY. HOME USE ONLY.                                         00001700
       REMARKS. AN EXAMPLE PROGRAM DEMONSTRATING SQUENTIAL              00001800
           VSAM FILE ACCESS.                                            00001900
      *>                                                                00002000
      *>                                                                00002100
       ENVIRONMENT DIVISION.                                            00002200
      **                                                                00002300
       CONFIGURATION SECTION.                                           00002400
       SOURCE-COMPUTER.    IBM-370.                                     00002500
       OBJECT-COMPUTER.    IBM-370.                                     00002600
      **                                                                00002700
       INPUT-OUTPUT SECTION.                                            00002801
      *                                                                 00002901
       FILE-CONTROL.                                                    00003000
           SELECT VALTRAN  ASSIGN TO UT-S-VALTRAN.                      00003102
           SELECT INVMAST  ASSIGN TO UT-I-INVMAST                       00003202
                           ORGANIZATION IS INDEXED                      00003302
                           ACCESS IS SEQUENTIAL                         00003402
                           RECORD KEY IS MR-ITEM-NO                     00003501
                           FILE STATUS IS INVMAST-ERROR-CODE.           00003601
           SELECT ERRTRAN  ASSIGN TO UT-S-ERRTRAN.                      00003702
      *>                                                                00003801
       DATA DIVISION.                                                   00003901
      *                                                                 00004001
       FILE SECTION.                                                    00004101
      *                                                                 00004201
       FD  VALTRAN                                                      00004301
           LABEL RECORDS ARE STANDARD                                   00004401
           RECORD CONTAINS 21 CHARACTERS.                               00004501
      *                                                                 00004601
       01  VALID-TRANSACTION-AREA           PIC X(21).                  00004701
      *                                                                 00004801
       FD  INVMAST                                                      00004901
           LABEL RECORDS ARE STANDARD                                   00005001
           RECORD CONTAINS 50 CHARACTERS.                               00005101
      *                                                                 00005201
       01  MASTER-RECORD-AREA                                           00005301
           05  MR-ITEM-NO                  PIC X(5)                     00005401
           05  FILLER                      PIC X(45).                   00005501
      *                                                                 00005601
       FD  ERRTRAN                                                      00005701
           LABEL RECORDS ARE STANDARD                                   00005801
           RECORD CONTAINS 21 CHARACTERS   PIC X(21).                   00005901
      *                                                                 00006001
       01  ERROR-TRANSACTION               PIC X(21).                   00006101
      *                                                                 00006202
       WORKING-STORAGE SECTION.                                         00006301
      *                                                                 00006401
       01  SWITCHES.                                                    00006501
           05  ALL-RECORDS-PROCESSED-SWITCH    PIC X   VALUE   'N'.     00006601
               88  ALL-RECORDS-PROCESSED               VALUE   'Y'.     00006701
           05  MASTER-UPDATED-SWITCH           PIC X   VALUE   'N'.     00006801
               88  MASTER-UPDATED                      VALUE   'Y'.     00006901
      *                                                                 00007001
       01  FILE-STATUS-FIELD.                                           00007101
           05 INVMAST-ERROR-CODE           PIC XX.                      00007201
      *                                                                 00007301
       01  INVENTORY-TRANSACTION-RECORD.                                00007401
           05  IT-ITEM-NO                  PIC X(5).                    00007501
           05  IT-VENDOR-NO                PIC X(5).                    00007601
           05  IT-RECEIPT-DATE             PIC X(6).                    00007701
           05  IT-RECEIPT-QUANTITY         PIC S9(5).                   00007801
      *                                                                 00007901
       01  INVENTORY-MASTER-RECORD.                                     00008001
           05  IM-DESCRIPTIVE-DATA.                                     00008101
               10  IM-ITEM-NO              PIC X(5).                    00008201
               10  IM-ITEM-DESC            PIC X(20).                   00008301
               10  IM-UNIT-COST            PIC S999V99.                 00008401
               10  IM-UNIT-PRICE           PIC S999V99.                 00008501
           05  IM-INVENTORY-DATA.                                       00008601
               10  IM-REORDER-POINT        PIC S9(5).                   00008701
               10  IM-ON-HAND              PIC S9(5).                   00008801
               10  IM-ON-ORDER             PIC S9(5).                   00008901
      *>                                                                00009000
       PROCEDURE DIVISION.                                              00009100
      *                                                                 00009202
       000-UPDATE-INVENTORY-FILE.                                       00009302
           OPEN    INPUT   VALTRAN                                      00009402
                   I-O INVMAST.                                         00009502
                   OUTPUT ERRTRAN.                                      00009602
           MOVE LOW-VALUE TO IM-ITEM-NO.                                00009702
           PERFORM 300-PROCESS-INVENTORY-TRAN                           00009802
               UNTIL ALL-RECORDS-PROCESSED.                             00009902
           CLOSE   VALTRAN, INVMAST, ERRTRAN.                           00010002
           DISPLAY 'VSAMSEQ    1   1   NORMAL EOJ'.                     00010102
           STOP RUN.                                                    00010200
          *                                                             00010302
           300-PROCESS-INVENTORY-TRAN.                                  00010402
               PERFORM 310-READ-INVENTORY-TRAN.                         00010502
               PERFORM 320-GET-INVENTORY-MASTER                         00010602
                   UNTIL   IM-ITEM-NO NO < IT-ITEM-NO.                  00010702
               IF          IM-ITEM-NO = HIGH-VALUE                      00010802
                    AND    IT-TIEM-NO = HIGH-VALUE                      00010902
                   MOVE    'Y' TO ALL-RECORDS-PROCESSED-SWITCH          00011002
               ELSE                                                     00011102
                   IF IM-ITEM-NO = IT-ITEM-NO                           00011202
                       PERFORM 350-UPDATE-INVENTORY-MASTER              00011302
                   ELSE                                                 00011402
                       PERFORM 360-WRITE-ERROR-TRAN.                    00011502
          *                                                             00011602
           310-READ-INVENTORY-TRAN.                                     00011702
               READ VALTRAN INTO INVENTORY-TRANSACTION-RECORD           00011802
                   AT  END                                              00011902
                           MOVE HIGH-VALUE TO IT-ITEM-NO.               00012002
          *                                                             00012102
           320-GET-INVENTORY-MASTER.                                    00012202
               IF  MASTER-UPDATED                                       00012302
                       PERFORM 340-REWRITE-INVENTORY-MASTER             00012402
                       PERFORM 330-READ-INVENTORY-MASTER                00012502
               ELSE                                                     00012602
                       PERFORM 330-READ-INVENTORY-MASTER.               00012702
          *                                                             00012802
           330-READ-INVENTORY-MASTER.                                   00012902
               READ    INVMAST INTO    INVENTORY-MASTER-RECORD.         00013002
               IF  INVMAST-ERROR-CODE  NOT =   '00'                     00013102
                   IF INVMAST-ERROR-CODE   =   '10'                     00013202
                       MOVE HIGH-VALUE TO IM-ITEM-NO                    00013302
                   ELSE                                                 00013402
                       MOVE 'Y' TO ALL-RECORDS-PROCESSED-SWITCH.        00013502
          *                                                             00013602
           340-REWRITE-INVENTORY-MASTER.                                00013702
               REWRITE MASTER-RECORD-AREA FROM                          00013802
                   INVENTORY-MASTER-RECORD.                             00013902
               IF INVMAST-ERROR-CODE   NOT =   '00'                     00014002
                  DISPLAY 'VSAMSEQ   A 2   REWRITE ERROR FOR INVMAST'   00014102
                  DISPLAY 'VSAMSEQ   A 2   ITEM NUMBER = ' IM-ITEM-NO.  00014202
                  DISPLAY 'VSAMSEQ   A 2   FILE STATUS = '              00014302
                       INVMAST-ERROR-CODE                               00014402
                  MOVE 'Y' TO ALL-RECOREDS-PROCESSED-SWITCH.            00014502
               MOVE 'N' TO MASTER-UPDATED-SWITCH.                       00014602
          *                                                             00014702
           350-UPDATE-INVENTORY-MASTER.                                 00014802
               ADD IT-RECEIPT-QUANTITY TO  IM-ON-HAND.                  00014902
               MOVE 'Y' TO MASTER-UPDATED-SWITCH.                       00015002
          *                                                             00015102
           360-WRITE-ERROR-TRAN.                                        00015202
               WRITE ERROR-TRANSACTION FROM                             00015302
                   INVENTORY-TRANSACTION-RECORD.                        00015402
      *>                                                                00015502
/*                                                                      00015600
//COB.SYSLIB DD DSNAME=SYS1.COBLIB,DISP=SHR                             00015700
//COB.VALTRAN DD DSNAME=HERC01.SAMPLE.VALTRAN,DSP=SWR                   00015803
//COB.INVMAST DD DSNAME=HERC01.SAMPLE.INVMAST,DSP=SWR                   00015903
//COB.ERRTRAN DD DSNAME=HERC01.SAMPLE.ERRTRAN,DSP=SWR                   00016003
//GO.SYSOUT DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=16100)         00016100
//GO.SYSIN DD *                                                         00016200
/*                                                                      00016300
/&                                                                      00016404
