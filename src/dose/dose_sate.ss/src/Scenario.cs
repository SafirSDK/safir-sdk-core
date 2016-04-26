/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
* 
* Created by: Joel Ottosson / stjoot
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Xml.Serialization;
using Safir.Dob.Typesystem;

namespace Sate
{
    public enum DobAction
    {
        //Pure Actions
        Connect,
        Disconnect,
        Register,
        Unregister,
        Subscribe,
        Unsubscribe,
        SubscribeRegistration,
        UnsubscribeRegistration,
        SetChangesEntity,
        SetAllEntity,
        InjectionSetChanges,
        InjectionInitialSet,
        InjectionDelete,
        DeleteEntity,
        CreateRequest,
        UpdateRequest,
        DeleteRequest,
        ServiceRequest,
        SendMessage,
        //Collection of multiple actions
        Sequence
    }

    //****************************************************************
    // Class that defines a scenario and can be serialized to XML
    //****************************************************************

    [XmlRoot("Action", Namespace = "", IsNullable = false)]
    public class Action
    {
        public DobAction action;
        public int delay;
        public bool instStepUp;

        //Deserialize object here before replay, better performance
        //[XmlIgnore]
        public ObjectInfo objInfo;
        public int repetitions = 1;
        public ScenarioInfo scenarioInfo;
        public Action[] sequence;
        public string xmlObject;

        public Action()
        {
        } //needed by xml-serializer


        public Action(DobAction action, int delay, ObjectInfo objInfo)
        {
            this.action = action;
            this.delay = delay;
            if (objInfo != null)
            {
                this.objInfo = objInfo;
                xmlObject = Serialization.ToXml(objInfo.Obj);
            }
        }


        public Action(DobAction action, int delay, ScenarioInfo scenarioInfo)
        {
            this.action = action;
            this.delay = delay;
            this.scenarioInfo = scenarioInfo;
        }

        public Action(ArrayList a, int reps)
        {
            action = DobAction.Sequence;
            repetitions = reps;
            sequence = new Action[a.Count];
            for (var i = 0; i < a.Count; i++)
                sequence[i] = (Action) a[i];
        }

        public bool HasInstanceNumber()
        {
            return action == DobAction.Register ||
                   action == DobAction.Unregister ||
                   action == DobAction.Subscribe ||
                   action == DobAction.Unsubscribe ||
                   action == DobAction.SetChangesEntity ||
                   action == DobAction.SetAllEntity ||
                   action == DobAction.InjectionDelete ||
                   action == DobAction.InjectionInitialSet ||
                   action == DobAction.InjectionSetChanges ||
                   action == DobAction.DeleteEntity ||
                   action == DobAction.CreateRequest ||
                   action == DobAction.UpdateRequest ||
                   action == DobAction.DeleteRequest;
        }

        public void StepUpInstance()
        {
#if STSYLI
            if (action == DobAction.Register ||
                action == DobAction.Unregister ||
                action == DobAction.Subscribe ||
                action == DobAction.Unsubscribe ||
                action == DobAction.DeleteEntity ||
                action == DobAction.DeleteRequest)
            {
              
                objectId.Instance++;
                if (objectId.Instance >= numberOfInstances)
                    objectId.Instance = 0;
            }
            else if ( action==DobAction.SetEntity ||
                    action==DobAction.CreateRequest ||
                    action==DobAction.UpdateRequest ||
                    action==DobAction.ServiceRequest ||
                    action==DobAction.SendMessage)
            {
                int i = obj.InstanceNumber;
                i++;
                if (i >= numberOfInstances)
                    i = 0;
                obj.InstanceNumber=i;
            }
#endif
        }

        public override string ToString()
        {
            if (instStepUp)
                return ScenarioTools.DobActionToString(action) + "++\tdelay " + delay;
            return ScenarioTools.DobActionToString(action) + "\tdelay " + delay;
        }

        //public Safir.Dob.Typesystem.Object obj = null;

        //public int numberOfInstances = 0;
    }

    //****************************************************************
    // Not serialized to XML-files.
    //****************************************************************
    public class RepeatStartMarker
    {
        public int Repetitions;

        public RepeatStartMarker(int reps)
        {
            Repetitions = reps;
        }

        public override string ToString()
        {
            if (Repetitions < 0)
            {
                return "-------- Repeat forever --------";
            }
            return "-------- Repeat " + Repetitions + " times --------";
        }
    }

    public class RepeatStopMarker
    {
        public override string ToString()
        {
            return "-------- End repeat --------";
        }
    }

    public class ScenarioTools
    {
        public static string DobActionToString(DobAction action)
        {
            switch (action)
            {
                case DobAction.Connect:
                    return "Connect";
                case DobAction.Disconnect:
                    return "Disconnect";
                case DobAction.Register:
                    return "Register";
                case DobAction.Unregister:
                    return "Unregister";
                case DobAction.Subscribe:
                    return "Subscribe";
                case DobAction.Unsubscribe:
                    return "Unsubscribe";
                case DobAction.SubscribeRegistration:
                    return "SubscribeRegistration";
                case DobAction.UnsubscribeRegistration:
                    return "UnsubscribeRegistration";
                case DobAction.SetChangesEntity:
                    return "SetChangesEntity";
                case DobAction.SetAllEntity:
                    return "SetAllEntity";
                case DobAction.InjectionSetChanges:
                    return "InjectionSetChanges";
                case DobAction.InjectionInitialSet:
                    return "InjectionInitialSet";
                case DobAction.InjectionDelete:
                    return "InjectionDelete";
                case DobAction.DeleteEntity:
                    return "DeleteEntity";
                case DobAction.CreateRequest:
                    return "CreateRequest";
                case DobAction.UpdateRequest:
                    return "UpdateRequest";
                case DobAction.DeleteRequest:
                    return "DeleteRequest";
                case DobAction.ServiceRequest:
                    return "ServiceRequest";
                case DobAction.SendMessage:
                    return "SendMessage";
                case DobAction.Sequence:
                    return "Sequence";
            }
            return "UnknownType";
        }

        public static Action ReadFile(string file)
        {
            try
            {
                Action action;
                var serializer = new XmlSerializer(typeof(Action));
                using (var fs = new FileStream(file, FileMode.Open))
                {
                    action = (Action) serializer.Deserialize(fs);
                    fs.Close();
                }
                return action;
            }
            catch
            {
                return null;
            }
        }

        public static void WriteFile(string file, Action scn)
        {
            var serializer = new XmlSerializer(typeof(Action));
            using (TextWriter writer = new StreamWriter(file))
            {
                serializer.Serialize(writer, scn);
                writer.Close();
            }
        }

        public class ScenarioIterator
        {
            private readonly Stack<ItHelp> _it = new Stack<ItHelp>();

            public ScenarioIterator(Action scenario)
            {
                _it.Push(new ItHelp(scenario));
            }

            public bool HasNext()
            {
                return _it.Count > 0;
            }

            public Action Next()
            {
                var ih = _it.Peek();
                var next = ih.Action.sequence[ih.SeqIx];
                ih.SeqIx++;
                if (ih.SeqIx >= ih.Action.sequence.Length && ih.Action.repetitions > 0)
                    ih.Rep++;

                if (next.action == DobAction.Sequence)
                {
                    _it.Push(new ItHelp(next));
                    return Next();
                }

                CleanUp();
                return next;
            }

            private void CleanUp()
            {
                if (HasNext())
                {
                    var lastRepeat = true;
                    var ih = _it.Peek();
                    if (ih.Rep != ih.Action.repetitions) //more iterations in loop
                    {
                        lastRepeat = false;
                    }

                    if (ih.SeqIx >= ih.Action.sequence.Length)
                    {
                        if (lastRepeat)
                        {
                            _it.Pop();
                            CleanUp();
                        }
                        else
                        {
                            ih.SeqIx = 0;
                        }
                    }
                }
            }

            private class ItHelp
            {
                public readonly Action Action;
                public int Rep;
                public int SeqIx;

                public ItHelp(Action a)
                {
                    Action = a;
                }
            }
        }
    }
}