---------------------------------------------------------------------------
-- FILE    : hermes-oid.adb
-- SUBJECT : Body of Object Identifier package.
-- AUTHOR  : (C) Copyright 2022 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Hermes.OID is

   ---------------------
   -- Public Subprograms
   ---------------------

   procedure To_Object_Identifier
     (Separates : in  Component_Array;
      Result    : out Object_Identifier;
      Status    : out Status_Type) is

      function Bad_First_Level(Root : Component_Type) return Boolean is
        (not (Root = 0 or Root = 1 or Root = 2));

      function Bad_Second_Level
        (Root : Component_Type; Second : Component_Type) return Boolean is
        (not (case Root is
                 when 0 => Second < 40,
                 when 1 => Second < 40,
                 when 2 => Second <= Second_Level_Component_Type'Last,
                 when others => False));

   begin
      Result := Object_Identifier'(0, 0, Other_Component_Array'(others => 0), 0);
      Status := Success;

      if Separates'Length < 1 or else Bad_First_Level(Separates(Separates'First)) then
         Status := Invalid_Root;
      else
         Result.Root_Component := Separates(Separates'First);
         if Separates'Length < 2 or else
           Bad_Second_Level(Separates(Separates'First), Separates(Separates'First + 1)) then
            Status := Invalid_Second_Level;
         else
            Result.Second_Level_Component := Separates(Separates'First + 1);
            for I in Component_Index_Type range Separates'First + 2 .. Separates'Last loop
               Result.Other_Components(1 + ((I - Separates'First) - 2)) := Separates(I);
            end loop;
            Result.Other_Component_Count := Separates'Length - 2;
         end if;
      end if;
   end To_Object_Identifier;


   function Component_Count(Identifier : Object_Identifier) return Component_Count_Type is
   begin
      return Identifier.Other_Component_Count + 2;
   end Component_Count;


   procedure To_Separates
     (Identifier           : Object_Identifier;
      Result               : out Component_Array;
      Number_Of_Components : out Component_Count_Type) is
   begin
      Result := (others => 0);
      Number_Of_Components := 0;

      if Identifier.Other_Component_Count + 2 <= Result'Length then
         Result(Result'First) := Identifier.Root_Component;
         Result(Result'First + 1) := Identifier.Second_Level_Component;
         for I in Other_Count_Type range 1 .. Identifier.Other_Component_Count loop
            pragma Loop_Invariant(Check =>
               I <= Identifier.Other_Component_Count                 and
               Identifier.Other_Component_Count + 2 <= Result'Length and
               Identifier = Identifier'Loop_Entry);

            Result((Result'First + 2) + (I - 1)) := Identifier.Other_Components(I);
         end loop;
         Number_Of_Components := Identifier.Other_Component_Count + 2;
      end if;
   end To_Separates;

end Hermes.OID;
