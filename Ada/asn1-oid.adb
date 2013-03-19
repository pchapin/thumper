---------------------------------------------------------------------------
-- FILE    : asn1-oid.adb
-- SUBJECT : Body of Object Identifier package.
-- AUTHOR  : (C) Copyright 2013 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body ASN1.OID is

   ---------------------
   -- Public Subprograms
   ---------------------

   procedure To_Object_Identifier(Separates : in Components_Type; Result : out Object_Identifier; Status : out Status_Type) is

      function Bad_First_Level(Root : Component) return Boolean
      --# return X => (not X -> (Root = 0 or Root = 1 or Root = 2));
      is
         Is_Bad : Boolean := False;
      begin
         if Root /= 0 and Root /= 1 and Root /= 2 then
            Is_Bad := True;
         end if;
         return Is_Bad;
      end Bad_First_Level;

      function Bad_Second_Level(Root : Component; Second : Component) return Boolean
      --# return X => (not X -> ( (Root = 0 -> Second < 40) and
      --#                         (Root = 1 -> Second < 40) and
      --#                         (Root = 2 -> Second <= Second_Level_Component_Type'Last) ) );
      is
         Is_Bad : Boolean := False;
      begin
         case Root is
            when 0 | 1 =>
               if Second > 39 then
                  Is_Bad := True;
               end if;

            when others =>
               if Second > Second_Level_Component_Type'Last then
                  Is_Bad := True;
               end if;
         end case;
         return Is_Bad;
      end Bad_Second_Level;

   begin
      Result := Object_Identifier'(0, 0, Other_Components_Type'(others => 0), 0);
      Status := Success;

      if Separates'Length < 1 or else Bad_First_Level(Separates(Separates'First)) then
         Status := Invalid_Root;
      else
         Result.Root_Component := Separates(Separates'First);
         if Separates'Length < 2 or else Bad_Second_Level(Separates(Separates'First), Separates(Separates'First + 1)) then
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
     (Identifier : Object_Identifier; Result : out Components_Type; Number_Of_Components : out Component_Count_Type) is
   begin
      Result := (others => 0);
      Number_Of_Components := 0;

      if Identifier.Other_Component_Count + 2 <= Result'Length then
         Result(Result'First) := Identifier.Root_Component;
         Result(Result'First + 1) := Identifier.Second_Level_Component;
         for I in Other_Count_Type range 1 .. Identifier.Other_Component_Count loop
            --# assert I <= Identifier.Other_Component_Count and
            --#        Identifier.Other_Component_Count + 2 <= Result'Length and
            --#        Identifier = Identifier%;
            Result((Result'First + 2) + (I - 1)) := Identifier.Other_Components(I);
         end loop;
         Number_Of_Components := Identifier.Other_Component_Count + 2;
      end if;
   end To_Separates;


   -- See X.690-2002-07.pdf (bottom of page 13) in the references folder for the specifics about the encoding.
   -- See also http://msdn.microsoft.com/en-us/library/bb540809(v=vs.85).aspx for a graphic.
   procedure To_Octet_Array(Identifier : in Object_Identifier; Result : out Network.Octet_Array; Octet_Count : out Natural) is
      Identifier_Index : Other_Index_Type;
   begin
      Result := (others => 0);
      Octet_Count := 0;
      Identifier_Index := Identifier.Other_Components'First;

      for Result_Index in Natural range Result'Range loop
         null;
      end loop;
   end To_Octet_Array;

end ASN1.OID;
