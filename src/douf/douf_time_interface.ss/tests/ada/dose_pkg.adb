-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
--
--  Created by: Erik Adolfsson / sterad
--
-------------------------------------------------------------------------------
--
--  This file is part of Safir SDK Core.
--
--  Safir SDK Core is free software: you can redistribute it and/or modify
--  it under the terms of version 3 of the GNU General Public License as
--  published by the Free Software Foundation.
--
--  Safir SDK Core is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
--
-------------------------------------------------------------------------------
with Safir.Dob.Connection;
with Safir.Dob.Consumer;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Operations;
with Safir.Time.TimeProvider;
--  with Safir.Dob.Typesystem.Si64;
with Ada.Text_IO;                         use Ada.Text_IO;
with Ada.Strings.Wide_Unbounded;          use Ada.Strings.Wide_Unbounded;
with Ada.Calendar;
--with Ada.Characters.Handling;             use Ada.Characters.Handling;
--with Safir.Dob.Response;
with Safir.Test.TimeConversion;
--with Win32;
--with Safir.Dob.Defs;
with Safir.Dob.ResponseSender;
with Safir.Dob.RegistrationStatus;
with Ada.Characters.Conversions;

package body Dose_Pkg is

   Connection : Safir.Dob.Connection.Class;

   task Main_Task is
      entry Start;
      entry Dispatch;
   end Main_Task;

   procedure Do_Dispatch is
   begin
      Main_Task.Dispatch;
   end Do_Dispatch;

   --======================================
   -- Dispatcher
   --======================================
   type Dispatcher_T is new Safir.Dob.Consumer.Dispatcher with null record;
   procedure OnDoDispatch
     (Self : in out Dispatcher_T);
   procedure OnDoDispatch
     (Self : in out Dispatcher_T) is
     pragma Unreferenced (Self);
   begin
        Do_Dispatch ;
   end OnDoDIspatch;
   Dispatcher                   : aliased Dispatcher_T;

   --======================================
   -- Connection Owner
   --======================================
   type Connection_Owner_T is new Safir.Dob.Consumer.StopHandler with null record;
   procedure OnStopOrder
     (Self : in out Connection_Owner_T);
   procedure OnStopOrder
     (Self : in out Connection_Owner_T) is
     pragma Unreferenced (Self);
   begin
      null;
   end OnStopOrder;
   Connection_Owner             : aliased Connection_Owner_T;

   procedure Open
     (Connection  : in out Safir.Dob.Connection.Class)
   is
   begin
      Safir.Dob.Connection.Open
        (Connection,
         To_Unbounded_Wide_String ("Douf"),
         To_Unbounded_Wide_String ("0"),
         0,
         Connection_Owner'access,
         Dispatcher'Access);
   end Open;

   task body Main_Task is
   begin
      accept Start;
      while True loop
         accept Dispatch do
            Safir.Dob.Connection.Dispatch(Connection);
         end Dispatch;
      end loop;
   end Main_Task;

   ------------------
   -- Entity_Owner --
   ------------------

   type Entity_Owner_T is new Safir.Dob.Consumer.EntityOwner with null record;
   procedure OnRegistrationStatus
     (Self               : in out Entity_Owner_T;
      ObjectId           : in Safir.Dob.Typesystem.ObjectId;
      RegistrationStatus : in Safir.Dob.RegistrationStatus.Enumeration);
   procedure OnRegisterAnyInstanceStatus
     (Self                  : in out Entity_Owner_T;
      ObjectId              : in Safir.Dob.Typesystem.ObjectId;
      RegistrationCompleted : in Boolean);
   procedure OnPersistentData
     (Self   : in out Entity_Owner_T;
      Entity : in Safir.Dob.Entity.Class);
   procedure OnCreateRequest
     (Self   : in out Entity_Owner_T;
      Entity : in Safir.Dob.Entity.Class;
      ResponseSender : in Safir.Dob.ResponseSender.Class'Class);
   procedure OnUpdateRequest
     (Self   : in out Entity_Owner_T;
      Entity : in Safir.Dob.Entity.Class;
      ResponseSender : in Safir.Dob.ResponseSender.Class'Class);
   procedure OnDeleteRequest
     (Self     : in out Entity_Owner_T;
      ObjectId : in Safir.Dob.Typesystem.ObjectId;
      ResponseSender : in Safir.Dob.ResponseSender.Class'Class);


   type Entity_Subscriber_T is new Safir.Dob.Consumer.EntitySubscriber with null record;
   procedure OnNewEntity
     (Self                      : in out Entity_Subscriber_T;
      Entity                    : in Safir.Dob.Entity.Class;
      Created                   : in Boolean);
   procedure OnUpdatedEntity
     (Self                      : in out Entity_Subscriber_T;
      Entity                    : in Safir.Dob.Entity.Class);
   procedure OnRemovedEntity
     (Self      : in out Entity_Subscriber_T;
      ObjectId  : in Safir.Dob.Typesystem.ObjectId;
      Deleted   : in Boolean);

--     type Requestor_T is new Safir.Dob.Consumer.Requestor with null record;
--     procedure OnResponse
--       (Self       : in out Requestor_T;
--        Response   : in Safir.Dob.Response.Class;
--        RequestId  : in Safir.Dob.Defs.RequestId);
--     procedure OnNotRequestOverflow
--       (Self       : in out Requestor_T);

--     type Service_Provider_T is new Safir.Dob.Consumer.ServiceProvider with null record;
--     procedure OnRegistrationStatus
--       (Self               : in out Service_Provider_T;
--        ObjectId           : in Safir.Dob.Typesystem.ObjectId;
--        RegistrationStatus : in Safir.Dob.RegistrationStatus.Enumeration);
--     procedure OnRegisterAnyInstanceStatus
--       (Self                  : in out Service_Provider_T;
--        ObjectId              : in Safir.Dob.Typesystem.ObjectId;
--        RegistrationCompleted : in Boolean);
--     procedure OnServiceRequest
--       (Self                  : in out Service_Provider_T;
--        Service               : in Safir.Dob.Service.Class;
--        ResponseSender        : in Safir.Dob.ResponseSender.Class'Class);

   Entity_Owner   : aliased Entity_Owner_T;
--   Requestor   : aliased Requestor_T;
   Entity_Subscriber  : aliased Entity_Subscriber_T;

--     procedure OnResponse
--       (Self       : in out Requestor_T;
--        Response   : in Safir.Dob.Response.Class;
--        RequestId  : in Safir.Dob.Defs.RequestId) is
--        pragma Unreferenced (Self);
--     begin
--        if not Safir.Replies.Reply.IsOfType(Reply, Safir.Replies.SuccessReply.ClassId) then
--           put_line("Success reply  recieved for " & Dose.RequestId'image(RequestId));
--        end if;
--        if not Safir.Replies.Reply.Is_Of_Type(Reply, Safir.Replies.ErrorReply.Class_Id) then
--           put_line("Error reply  recieved for " & Dose.Request_Id'image(Request_Id));
--        end if;
--     end On_Reply;
--
--     procedure OnNotRequestOverflow (Self : in Requestor_T) is
--        pragma Unreferenced (Self);
--     begin
--        null;
--     end OnNotRequestOverflow;
--
   procedure OnRegistrationStatus
     (Self               : in out Entity_Owner_T;
      ObjectId           : in Safir.Dob.Typesystem.ObjectId;
      RegistrationStatus : in Safir.Dob.RegistrationStatus.Enumeration) is
      pragma Unreferenced (Self);
      pragma Unreferenced (ObjectId);
      pragma Unreferenced (RegistrationStatus);
   begin
      null;
   end OnRegistrationStatus;

   procedure OnRegisterAnyInstanceStatus
     (Self                  : in out Entity_Owner_T;
      ObjectId              : in Safir.Dob.Typesystem.ObjectId;
      RegistrationCompleted : in Boolean) is
      pragma Unreferenced (Self);
      pragma Unreferenced (ObjectId);
      pragma Unreferenced (RegistrationCompleted);
   begin
      null;
   end OnRegisterAnyInstanceStatus;

   procedure OnPersistentData
     (Self   : in out Entity_Owner_T;
      Entity : in     Safir.Dob.Entity.Class) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Entity);
   begin
      null;
   end;

   procedure OnCreateRequest
     (Self   : in out Entity_Owner_T;
      Entity : in Safir.Dob.Entity.Class;
      ResponseSender : in Safir.Dob.ResponseSender.Class'Class) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Entity);
      pragma Unreferenced (ResponseSender);
   begin
      null;
   end;


   procedure OnUpdateRequest
     (Self   : in out Entity_Owner_T;
      Entity : in Safir.Dob.Entity.Class;
      ResponseSender : in Safir.Dob.ResponseSender.Class'Class) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Entity);
      pragma Unreferenced (ResponseSender);
   begin
      null;
   end;


   procedure OnDeleteRequest
     (Self     : in out Entity_Owner_T;
      ObjectId : in Safir.Dob.Typesystem.ObjectId;
      ResponseSender : in Safir.Dob.ResponseSender.Class'Class) is
      pragma Unreferenced (Self);
      pragma Unreferenced (ObjectId);
      pragma Unreferenced (ResponseSender);
    begin
      null;
    end;

   procedure OnNewEntity
     (Self                      : in out Entity_Subscriber_T;
      Entity                    : in Safir.Dob.Entity.Class;
      Created                   : in Boolean) is
      pragma Unreferenced (Created);

   begin
      Put_Line
        ("Entity" &
         Safir.Dob.Typesystem.Int32'Image (Safir.Dob.Entity.GetInstanceNumber (Entity)) &
         " " &
         Ada.Characters.Conversions.To_String
            (To_Wide_String
                (Safir.Dob.Typesystem.Operations.GetName (Safir.Dob.Entity.GetTypeId (Entity)))) &
         " is new!");

      OnUpdatedEntity(Self,Entity);
   end OnNewEntity;

   procedure OnUpdatedEntity
     (Self                      : in out Entity_Subscriber_T;
      Entity                    : in Safir.Dob.Entity.Class) is
      pragma Unreferenced (Self);
      Timestamp : Ada.Calendar.Time;
      Time_Conv : Safir.Test.TimeConversion.Class;
      package Integer_Text is new Ada.Text_IO.Integer_IO (Integer);
      package Duration_Text is new Ada.Text_IO.Fixed_IO (Duration);
      Hour : Integer;
      Min : Integer;
      Sec : Duration;
      use type Safir.Dob.Typesystem.TypeId;

      procedure Put_Int ( Val : in Integer) is
      begin
         if Val < 10 then
            Put("0");
            Integer_Text.Put ( Val, 1);
         else
            Integer_Text.Put ( Val, 2);
         end if;
      end;
      procedure Put_Duration ( Val : in Duration) is
      begin
         if Val < 10.0 then
            Put("0");
            Duration_Text.Put ( Val, 1, 6);
         else
            Duration_Text.Put ( Val, 2, 6);
         end if;
      end;
   begin
      if Safir.Dob.Entity.GetTypeId (Entity) = Safir.Test.TimeConversion.ClassId then
         Time_Conv := Safir.Test.TimeConversion.Clone (Entity);
         if Safir.Test.TimeConversion.IsNullTimeStamp (Time_Conv ) then
            Put_Line("Safir.Test.TimeConversion.TimeStamp is NULL");
         else
            Timestamp := Safir.Time.TimeProvider.CalendarTimeOf (Safir.Test.TimeConversion.
                                              GetTimeStamp (Time_Conv ));

            Hour := Integer( Ada.Calendar.Seconds ( Timestamp )) /3600;
            Min  := Integer( Ada.Calendar.Seconds ( Timestamp )) / 60 - Hour * 60;
            Sec  := Ada.Calendar.Seconds ( Timestamp ) - (Hour * 3600.0 + Min * 60.0);

            Integer_Text.Put( Ada.Calendar.Year( Timestamp ),4);
            Put ("-");
            Put_Int( Ada.Calendar.Month ( Timestamp ));
            Put("-");
            Put_Int( Ada.Calendar.Day ( Timestamp ));
            Put(" ");
            Put_Int( Hour );
            Put(":");
            Put_Int( Min );
            Put(":");
            Put_Duration ( Sec );
            Put_Line(" ");
         end if;
      end if;
   end OnUpdatedEntity;

   procedure OnRemovedEntity
     (Self      : in out Entity_Subscriber_T;
      ObjectId  : in Safir.Dob.Typesystem.ObjectId;
      Deleted   : in Boolean) is
      pragma Unreferenced (Self);
      pragma Unreferenced (Deleted);
   begin
      Put_Line
        ("Entity" &
         Safir.Dob.Typesystem.Int32'Image (ObjectId.Instance) &
         " " &
         Ada.Characters.Conversions.To_String
            (To_Wide_String (Safir.Dob.Typesystem.Operations.GetName (ObjectId.TypeId))) &
         " is removed!");
   end OnRemovedEntity;

   procedure Start is
   begin
      Open (Connection);
      Main_Task.Start;
   end Start;
   procedure Stop is
   begin
      Safir.Dob.Connection.Close (Connection);
   end Stop;

   ----------
   ----------

   procedure Register_Entity (ObjectId    : in Safir.Dob.Typesystem.ObjectId) is
   begin
      Safir.Dob.Connection.RegisterEntity(Connection, ObjectId, True ,Entity_Owner'Access);
   end;

   procedure Set (Entity     : in Safir.Dob.Entity.Class'Class) is
   begin
      Safir.Dob.Connection.Set
        (Connection,
         Entity);

   end;

--     procedure Send_Update_Request (Request : in Safir.Dob.Entity.Class'Class; ok : out boolean ) is
--        Request_Id : Dose.Request_Id;
--     begin
--        Safir.Dob.Connection.UpdateRequest
--          (Connection,
--           Request,
--           Requestor'Access,
--           Request_Id,
--           ok);
--     end Send_Update_Request;
--
   procedure Subscribe_Entity
     (EntityId : in Safir.Dob.Typesystem.TypeId;
      Instance  : in Safir.Dob.Typesystem.Int32)
   is
   begin
      Safir.Dob.Connection.SubscribeEntity
        (Connection,
         (EntityId, Instance),
         True,
         True,
         Entity_Subscriber'Access);
   end Subscribe_Entity;
--
--     procedure Scan (Entity_Id : in Safir.Dob.Typesystem.TypeId) is
--        Entity  : Safir.Dob.Entity.Class;
--        Existed : Boolean;
--     begin
--        for I in  0 .. Safir.Dob.Typesystem.Operations.GetNumberOfInstances (Entity_Id) -1 loop
--           Safir.Dob.Connection.Read
--             (Connection,
--              (Entity_Id, Safir.Dob.Typesystem.Int32 (I)),
--              Entity,
--              Existed);
--           if Existed then
--              Put_Line ("Instance " & Integer'Image (I));
--              put_line(to_string(to_wide_string(Safir.Dob.Typesystem.Operations.ToXml(Entity))));
--           end if;
--        end loop;
--     end Scan;

end Dose_Pkg;
