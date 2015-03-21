pragma SPARK_Mode(On);

with Hermes.DER;
with Hermes.OID;
with Serial_Generator;

package Time_Stamps is

   -- Currently only version 1 is supported.
   subtype Version_Type is Positive range 1 .. 1;

   -- Currently only 160 bit hash values are supported. To lift this restriction Time_Stamp
   -- could be made discriminated on the hash size. However, that changes it to an indefinite
   -- type. See package Data_Storage for issues that will (currently) cause.
   --
   Hash_Size : constant := 20;

   type Time_Stamp is
      record
         Version : Version_Type;
         Policy  : Hermes.OID.Object_Identifier;

         -- The following two components specify the Message_Imprint sequence.
         Hash_Algorithm : Hermes.OID.Object_Identifier;
         Hashed_Message : Hermes.Octet_Array(1 .. Hash_Size);

         Serial_Number : Serial_Generator.Serial_Number_Type;
         Generalized_Time : Hermes.Octet_Array(1 .. 14);
      end record;


   -- Decodes a Time_Stamp from a DER encoded octet sequence.
   -- The behavior is the same as for the other Hermes.DER decoding procedures.
   procedure Get_Time_Stamp_Value
     (Message : in  Hermes.Octet_Array;
      Start   : in  Natural;
      Stop    : out Natural;
      Stamp   : out Time_Stamp;
      Status  : out Hermes.DER.Status_Type)
     with
       Global => null,
       Depends => ( (Stop, Stamp, Status) => (Message, Start) ),
       Pre => Start in Message'Range;


   -- Encodes a Time_Stamp to a DER encoded octet sequence.
   -- The behavior is the same as for the other Hermes.DER encoding procedures.
   function Put_Time_Stamp_Value(Stamp : Time_Stamp) return Hermes.Octet_Array;

end Time_Stamps;
