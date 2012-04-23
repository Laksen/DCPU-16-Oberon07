MODULE internals;

   CONST
      InitSize = 3;

      NrRESET*      = InitSize+0;
      NrCOPY*       = InitSize+1;
      NrASSERT*     = InitSize+2;
      NrNEW*        = InitSize+3;
      NrPACK*       = InitSize+4;
      NrUNPACK*     = InitSize+5;
      NrHeapStart* = InitSize+6;
      NrHeapEnd*   = InitSize+7;  

END internals.
