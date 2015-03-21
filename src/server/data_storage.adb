
package body Data_Storage is

   procedure Initialize is
   begin
      return;
   end Initialize;


   procedure Shutdown is
   begin
      return;
   end Shutdown;


   function Time_Stamp_Count return Count_Type is
   begin
      return 0;
   end Time_Stamp_Count;


   procedure Time_Stamp_Store(Stamp : in Time_Stamp) is
   begin
      raise Program_Error with "Data_Storage.Time_Stamp_Store not implemented";
   end Time_Stamp_Store;


   function Time_Stamp_Retrieve(Serial_Number : Serial_Number_Type) return Time_Stamp_Array is
      Dummy : Time_Stamp_Array(1 .. 0);
   begin
      return Dummy;
   end Time_Stamp_Retrieve;


   function Time_Stamp_Retrieve(Start : Time; Stop : Time) return Time_Stamp_Array is
      Dummy : Time_Stamp_Array(1 .. 0);
   begin
      return Dummy;
   end Time_Stamp_Retrieve;

end Data_Storage;
