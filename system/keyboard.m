MODULE keyboard;

   IMPORT SYSTEM;
   
   CONST
      KeyboardBase = 09000H;
      KeyboardIndex = 09010H;

   PROCEDURE ReadKey*(): CHAR;
   VAR res: CHAR;
       idx: INTEGER;
   BEGIN
      SYSTEM.GET(KeyboardIndex, idx);
      SYSTEM.GET(KeyboardBase+Idx, res);
      SYSTEM.PUT(KeyboardBase+Idx, 0X);
      
      idx := (idx+1) MOD 16;
      
      SYSTEM.PUT(KeyboardIndex, idx);
      
      RETURN res
   END ReadKey;

BEGIN
   SYSTEM.PUT(KeyboardIndex, 0);
END keyboard.