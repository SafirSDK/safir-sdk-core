with Safir.Dob.Typesystem.Object;
with Safir.Dob.Typesystem.Object_Factory;

pragma Elaborate_All (Safir.Dob.Typesystem.Object);
pragma Elaborate_All (Safir.Dob.Typesystem.Object_Factory);

package body Register is

   -- The factory registers the create function for Object ...
   Object_Create_Callback_Function : constant Safir.Dob.Typesystem.Object_Factory.Create_Object_Callback :=
      Safir.Dob.Typesystem.Object.Create_Object'Access;
   -- ... and for the smart pointer
   Smart_Ptr_Create_Callback_Function : constant Safir.Dob.Typesystem.Object_Factory.Create_Smart_Ptr_Callback :=
      Safir.Dob.Typesystem.Object.Create_Smart_Ptr'Access;

begin
   Safir.Dob.Typesystem.Object_Factory.Register_Type (Safir.Dob.Typesystem.Object.Class_Type_Id,
                  Object_Create_Callback_Function,
                  Smart_Ptr_Create_Callback_Function);
end Register;
