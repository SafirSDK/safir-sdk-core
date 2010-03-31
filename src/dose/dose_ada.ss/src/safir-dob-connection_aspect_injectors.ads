-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009 (http://www.safirsdk.com)
--
--  Created by: Anders Widén / stawi
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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Defs;
with Safir.Dob.Connection_Bases;
with Safir.Dob.Entity;
with Safir.Dob.Consumers;

package Safir.Dob.Connection_Aspect_Injectors is

   -- Type that provides methods for special applications that injects entities into
   -- the system apart from the normal handler.
   --
   type Connection_Aspect_Injector is tagged private;

   -- Constructor
   --
   -- Parameters: Connection_Base - The connection to operate through.
   --
   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class)
      return Connection_Aspect_Injector;

   -- Merge the changed members based on the timestamps.
   --
   -- All members of the given entity that are marked as changed will be merged into the
   -- current entity object in the pool given that the top member has a timestamp that is "newer" than
   -- the corresponding timestamp in the Dob.
   --
   -- Parameters: Entity - Entity to create or update.
   --             Instance_Id - Instance id.
   --             Timestamp - Timestamp valid for the top members that are marked as changed.
   --             Handler_Id - Handler id.
   --
   procedure Inject_Changes
     (Self        : in Connection_Aspect_Injector;
      Entity      : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Timestamp   : in Safir.Dob.Typesystem.Int_64;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- Delete the given instance based on the timestamp.
   --
   -- The given instance is deleted if the timestamp is "newer" than all top member timestamps
   -- for the current instance.
   --
   -- Parameters: Entity_Id - Entity id of the instance to delete.
   --             Timestamp - Timestamp Time of deletion.
   --             Handler_Id - Handler id.
   --
   procedure Inject_Delete
     (Self        : in Connection_Aspect_Injector;
      Entity_Id   : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Timestamp   : in Safir.Dob.Typesystem.Int_64;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- Allows an application to inject an initial entity state.
   --
   -- Parameters: Entity - Entity to delete.
   --             Instance_Id - Instance id.
   --             Handler_Id - The handler id to which the state belongs.
   --
   procedure Initial_Set
     (Self        : in Connection_Aspect_Injector;
      Entity      : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- Special entity subscription
   --
   -- Special subscription that also give the subscriber "ghost" entities, that is,
   -- entities with no current owner.
   --
   -- Parameters: Type_Id - Type id of the entity to subscribe for.
   --             Include_Updates - True => Subscription includes update, as well as create and delete.
   --                               False => Subscription includes no updates, only create and deletion.
   --             Include_Subclasses - True => Subscription for this entity type and all its subclasses.
   --                                  False => No subclasses will be included.
   --             Restart_Subscription - True => On_New_Entity callbacks are generated even if the subscription already exists.
   --                                    False => On_New_Entity callbacks are generated only for instances that are not previously subscribed.
   --             Wants_Ghost_Delete - True => Wants notification that an "inject new" or Initial_Set was immediately deleted by owner.
   --                                  False => Normal subscription, only get deletes when an On_New has been called.
   --             Wants_Last_State - True => Guarantee On_Updated/On_New even if application dies immediately after set.
   --                                False => Normal subscription, call On_Deleted only in this case.
   --             Doesnt_Want_Source_Is_PermanentStore - True => Doesnt want On_New if only accept of an InitialSet.
   --                                                    False => Normal Subscription. Gets On_New for all states.
   --                                                    (This flag is only applicable for AsynchronousPermanent types)
   --             Wants_All_State_Changes - True => Wants On_Do_Dispatch called for *all* state changes, even if they do not
   --                                               result in a callback. ONLY MEANT FOR DOSE_MAIN!
   --                                       False => Normal Subscription.
   --             Timestamp_Change_Info - True => ChangeInfo is based on timestamps instead of contents
   --                                     False => ChangeInfo is based on object contents, as with normal subscribe.
   --             EntitySubscriber - Entity subscriber that will receive the entities.
   --
   procedure Subscribe_Entity
     (Self                                 : in Connection_Aspect_Injector;
      Type_Id                              : in Safir.Dob.Typesystem.Type_Id;
      Include_Updates                      : in Boolean;
      Include_Subclasses                   : in Boolean;
      Restart_Subscription                 : in Boolean;
      Wants_Ghost_Delete                   : in Boolean;
      Wants_Last_State                     : in Boolean;
      Doesnt_Want_Source_Is_PermanentStore : in Boolean;
      Wants_All_State_Changes              : in Boolean;
      Timestamp_Change_Info                : in Boolean;
      Entity_Subscriber                    : access Safir.Dob.Consumers.Entity_Subscriber'Class);

private

   use type Safir.Dob.Defs.Controller_Id;

   type Connection_Aspect_Injector is tagged record
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

end Safir.Dob.Connection_Aspect_Injectors;
