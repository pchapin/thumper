---------------------------------------------------------------------------
-- FILE    : hermes-der-encode.adb
-- SUBJECT : Body of a package for encoding DER encoded data.
-- AUTHOR  : (C) Copyright 2022 by Peter Chapin
--
-- Please send comments or bug reports to
--
--      Peter Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Hermes.DER.Encode is

   function Make_Leading_Identifier
     (Tag_Class       : Tag_Class_Type;
      Structured_Flag : Structured_Flag_Type;
      Tag             : Leading_Number_Type) return Octet is

      Tag_Class_Lookup_Table : constant array(Tag_Class_Type) of Octet :=
        (Class_Universal        => 2#0000_0000#,
         Class_Application      => 2#0100_0000#,
         Class_Context_Specific => 2#1000_0000#,
         Class_Private          => 2#1100_0000#);

      Structured_Flag_Lookup_Table : constant array(Structured_Flag_Type) of Octet :=
        (Primitive              => 2#0000_0000#,
         Constructed            => 2#0010_0000#);

      Leading_Number_Lookup_Table : constant array(Leading_Number_Type) of Octet :=
        (Tag_Reserved           =>  0,
         Tag_Boolean            =>  1,
         Tag_Integer            =>  2,
         Tag_Bit_String         =>  3,
         Tag_Octet_String       =>  4,
         Tag_Null               =>  5,
         Tag_Object_Identifier  =>  6,
         Tag_Object_Descriptor  =>  7,
         Tag_Instance_Of        =>  8,
         Tag_External           =>  8,  -- Same as Instance_Of
         Tag_Real               =>  9,
         Tag_Enumerated         => 10,
         Tag_Embedded_PDV       => 11,
         Tag_UTF8_String        => 12,
         Tag_Relative_OID       => 13,
         -- Values 14 .. 15 omitted (not defined?)
         Tag_Sequence           => 16,
         Tag_Sequence_Of        => 16,  -- Same as Sequence
         Tag_Set                => 17,
         Tag_Set_Of             => 17,  -- Same as Set
         Tag_Numeric_String     => 18,
         Tag_Printable_String   => 19,
         Tag_Teletex_String     => 20,
         Tag_T61_String         => 20,  -- Same as Teletex_String
         Tag_Videotex_String    => 21,
         Tag_IA5_String         => 22,
         Tag_UTC_Time           => 23,
         Tag_Generalized_Time   => 24,
         Tag_Graphic_String     => 25,
         Tag_Visible_String     => 26,
         Tag_ISO646_String      => 26,  -- Same as Visible_String
         Tag_General_String     => 27,
         Tag_Universal_String   => 28,
         Tag_Character_String   => 29,
         Tag_BMP_String         => 30,
         Tag_EXTENDED_TAG       => 31);

   begin
      return
        Tag_Class_Lookup_Table(Tag_Class)             or
        Structured_Flag_Lookup_Table(Structured_Flag) or
        Leading_Number_Lookup_Table(Tag);
   end Make_Leading_Identifier;


   function Put_Length_Value(Length : Natural) return Hermes.Octet_Array is
      Result : Hermes.Octet_Array(1..5):= (others => 0);
      Value : Natural;
   begin
      case Length is
         when 0 .. 127 =>
            Value := Length;
            Result(Result'First) := Hermes.Octet(Value);
            return Result(Result'First .. Result'First);

         when 128 .. 2**8 - 1 =>
            Value := Length;
            Result(Result'First) := 2#1000_0001#;
            Result(Result'First + 1) := Hermes.Octet(Value);
            return Result(Result'First .. Result'First + 1);

         when 2**8 .. 2**16 - 1 =>
            Value := Length;
            Result(Result'First) := 2#1000_0010#;
            Result(Result'First + 2) := Hermes.Octet(Value rem 2**8); Value := Value / 2**8;
            Result(Result'FIrst + 1) := Hermes.Octet(Value);
            return Result(Result'First .. Result'First + 2);

         when 2**16 .. 2**24 - 1 =>
            Value := Length;
            Result(Result'First) := 2#1000_0011#;
            Result(Result'First + 3) := Hermes.Octet(Value rem 2**8); Value := Value / 2**8;
            Result(Result'First + 2) := Hermes.Octet(Value rem 2**8); Value := Value / 2**8;
            Result(Result'First + 1) := Hermes.Octet(Value);
            return Result(Result'First .. Result'First + 3);

         when others =>
            Value := Length;
            Result(Result'First) := 2#1000_0100#;
            Result(Result'First + 4) := Hermes.Octet(Value rem 2**8); Value := Value / 2**8;
            Result(Result'First + 3) := Hermes.Octet(Value rem 2**8); Value := Value / 2**8;
            Result(Result'First + 2) := Hermes.Octet(Value rem 2**8); Value := Value / 2**8;
            Result(Result'First + 1) := Hermes.Octet(Value);
            return Result(Result'First .. Result'First + 4);
      end case;
   end Put_Length_Value;


   function Put_Boolean_Value(Value : Boolean) return Hermes.Octet_Array is
      Boolean_Value_Octet : constant Hermes.Octet := (if Value then 2#1111_1111# else 2#0000_0000#);
      Boolean_Octet_Array : constant Hermes.Octet_Array :=
        (Make_Leading_Identifier
           (Tag_Class       => Class_Universal,
            Structured_Flag => Primitive,
            Tag             => Tag_Boolean) & 2#0000_0001# & Boolean_Value_Octet);
   begin
      return Boolean_Octet_Array;
   end Put_Boolean_Value;


   function Put_Integer_Value(Value : Integer) return Hermes.Octet_Array is
      Integer_Octet_Array : Hermes.Octet_Array(1 .. 6) := (others => 0);
      Temp_Value : Integer;
   begin
      Integer_Octet_Array(1):= Make_Leading_Identifier(Class_Universal, Primitive, Tag_Integer);
      if Value <= 16#7F# then
         Integer_Octet_Array(2) := Put_Length_Value(1)(1);
         Integer_Octet_Array(3) := Octet(Value rem 256);
         return Integer_Octet_Array(1 .. 3);
      elsif Value <= 16#7FFF# then
         Integer_Octet_Array(2) := Put_Length_Value(2)(1);
         Integer_Octet_Array(4) := Octet(Value rem 256);
         Integer_Octet_Array(3) := Octet(Value / 256);
         return Integer_Octet_Array(1 .. 4);
      elsif Value <= 16#7FFFFF# then
         Integer_Octet_Array(2) := Put_Length_Value(3)(1);
         Integer_Octet_Array(5) := Octet(Value rem 256);
         Temp_Value := Value / 256;
         Integer_Octet_Array(4) := Octet(Temp_Value rem 256);
         Integer_Octet_Array(3) := Octet(Temp_Value / 256);
         return Integer_Octet_Array(1 .. 5);
      else
         Integer_Octet_Array(2) := Put_Length_Value(4)(1);
         Integer_Octet_Array(6) := Octet(Value rem 256);
         Temp_Value := Value / 256;
         Integer_Octet_Array(5) := Octet(Temp_Value rem 256);
         Temp_Value := Temp_Value / 256;
         Integer_Octet_Array(4) := Octet(Temp_Value rem 256);
         Integer_Octet_Array(3) := Octet(Temp_Value / 256);
         return Integer_Octet_Array(1 .. 6);
      end if;

   end Put_Integer_Value;


   function Put_Octet_String_Value(Value : Hermes.Octet_Array) return Hermes.Octet_Array is
     (Make_Leading_Identifier
        (Tag_Class       => Class_Universal,
         Structured_Flag => Primitive,
         Tag             => Tag_Octet_String) & Put_Length_Value(Value'Length) & Value);


   function Put_Null_Value return Hermes.Octet_Array is
     (Make_Leading_Identifier
        (Tag_Class       => Class_Universal,
         Structured_Flag => Primitive,
         Tag             => Tag_Null) & 2#0000_0000#);


   function Put_OID_Value(Value : Hermes.OID.Object_Identifier) return Hermes.Octet_Array is
      use Hermes.OID;

      -- Converts an object identifier into an array of raw bytes. Returns in the Octet_Count
      -- parameter the number of bytes used. If there is a problem with the conversion (for
      -- example, due to lack of space) a count of zero is returned. Unused space in the Result
      -- array is filled with zero values; although if a failure occurs the Result array has an
      -- indeterminate value.
      --
      -- See T-REC-X.690-2021-02.pdf (section 8.19) in the references folder for the specifics
      -- about the encoding. See also http://msdn.microsoft.com/en-us/library/bb540809(v=vs.85).aspx
      -- for a (slightly misleading) graphic.
      --
      -- TODO: Would this be better as a function returning Octet_Array?
      procedure To_Octet_Array
        (Identifier  : in  Object_Identifier;
         Result      : out Octet_Array;
         Octet_Count : out Natural)
      is
         Result_Index : Natural;
         Start_Index  : Natural;
         Left_Index   : Natural;
         Right_Index  : Natural;
         Separates            : Component_Array(1 .. Maximum_Component_Count);
         Number_Of_Components : Component_Count_Type;
         Current_Component    : Component_Type;
         Out_Of_Space : Boolean;
         Temp         : Octet;
      begin
         Result := (others => 0);
         Octet_Count := 0;
         To_Separates(Identifier, Separates, Number_Of_Components);

         if Result'Length > 0 then
            Result_Index := Result'First;
            Octet_Count  := 1;
            Result(Result_Index) := Octet((Separates(1) * 40) + Separates(2));
            for Other_Index in Component_Index_Type range 3 .. Number_Of_Components loop
               pragma Loop_Invariant(Result_Index in Result'Range);

               Current_Component := Separates(Other_Index);

               -- Break the Current_Component value into 7 bit units; store into Result in little
               -- endian order for now.
               Start_Index  := Result_Index;
               Out_Of_Space := False;
               loop
                  pragma Loop_Invariant(Result_Index in Result'Range);
                  if Result_Index = Result'Last then
                     Out_Of_Space := True;
                     exit;
                  else
                     Result_Index := Result_Index + 1;
                     Result(Result_Index) := Octet(Current_Component rem 128);
                     Current_Component := Current_Component / 128;
                     exit when Current_Component = 0;
                  end if;
               end loop;

               -- If the loop above broke due to a lack of space, deal with the error condition.
               if not Out_Of_Space then
                  -- The proofs fail here because SPARK doesn't know that Result isn't from
                  -- Natural'First to Natural'Last. In fact, due to limitations on the number and
                  -- size of the OID components, the amount of space used in Result will be far
                  -- less. Probably a more suitable type should be defined or at least more
                  -- information should be provided in the loop invariants above.
                  --
                  -- TODO: Fix this failing proof.
                  --
                  Octet_Count := Result_Index - Start_Index + 1;
               else
                  Octet_Count := 0;
                  exit;
               end if;

               -- Reverse the order so the 7 bit units are in big endian order instead.
               Left_Index  := Result'First + 1;
               Right_Index := Result_Index;
               while Left_Index < Right_Index loop
                  pragma Loop_Invariant(Left_Index in Result'Range and Right_Index in Result'Range);

                  Temp                := Result(Left_Index);
                  Result(Left_Index)  := Result(Right_Index);
                  Result(Right_Index) := Temp;

                  Left_Index  := Left_Index + 1;
                  Right_Index := Right_Index - 1;
               end loop;

               -- Set MSB of each unit to 1 except for the last one.
               for Index in Natural range Start_Index .. Result_Index - 1 loop
                  Result(Index) := Result(Index) + 128;
               end loop;
            end loop;
         end if;
      end To_Octet_Array;

      Leading_Identifier : constant Hermes.Octet :=     
        Make_Leading_Identifier(Class_Universal, Primitive, Tag_Object_Identifier);
      Encoded : Octet_Array(1 .. 128); -- TODO: Choose a more appropriate size.
      Count   : Natural;      
      
   begin -- Put_OID_Value
      To_Octet_Array    
        (Identifier  => Value,
         Result      => Encoded,
         Octet_Count => Count);

      return Leading_Identifier & Put_Length_Value(Length => Count) & Encoded(1 .. Count);
   end Put_OID_Value;

end Hermes.DER.Encode;
