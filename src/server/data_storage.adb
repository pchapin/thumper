
package body Data_Storage is

   procedure Initialize is
   begin
      return;
   end Initialize;


   procedure Shutdown is
   begin
      return;
   end Shutdown;


   function Timestamp_Count return Count_Type is
   begin
      return 0;
   end Timestamp_Count;


   procedure Timestamp_Store(Stamp : in Timestamp) is
   begin
      raise Program_Error with "Data_Storage.Timestamp_Store not implemented";
   end Timestamp_Store;


   function Timestamp_Retrieve(Serial_Number : Serial_Number_Type) return Timestamp_Array is
      Dummy : Timestamp_Array(1 .. 0);
   begin
      return Dummy;
   end Timestamp_Retrieve;


   function Timestamp_Retrieve(Start : Time; Stop : Time) return Timestamp_Array is
      Dummy : Timestamp_Array(1 .. 0);
   begin
      return Dummy;
   end Timestamp_Retrieve;

end Data_Storage;
