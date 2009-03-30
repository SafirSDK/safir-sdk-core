/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

using System;
using System.Xml;
using System.Xml.Serialization;
using System.IO;

namespace Sate.Scenarios
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

    [XmlRootAttribute("Action", Namespace="", IsNullable = false)]
    public class Action
    {
        public DobAction action;
        public int delay = 0;
        public bool instStepUp = false;
        public string xmlObject = null;
        public ScenarioInfo scenarioInfo;
        public int repetitions = 1;
        public Action[] sequence = null;

        public Action()
        {
        } //needed by xml-serializer

    
        public Action(DobAction action, int delay, ObjectInfo objInfo)
        {
            this.action=action;
            this.delay=delay;
            if (objInfo != null)
            {
                this.objInfo = objInfo;
                xmlObject = Safir.Dob.Typesystem.Serialization.ToXml(objInfo.Obj);
            }
        }
     

        public Action(DobAction action, int delay, ScenarioInfo scenarioInfo)
        {
            this.action = action;
            this.delay = delay;
            this.scenarioInfo = scenarioInfo;
        }

        public Action(System.Collections.ArrayList a, int reps)
        {
            this.action=DobAction.Sequence;
            repetitions=reps;
            sequence=new Action[a.Count];
            for (int i=0; i<a.Count; i++)
                sequence[i]=(Action)a[i];
        }

        public bool HasInstanceNumber()
        {
            return  action==DobAction.Register ||
                    action==DobAction.Unregister ||
                    action==DobAction.Subscribe ||
                    action==DobAction.Unsubscribe ||
                    action==DobAction.SetChangesEntity ||
                    action==DobAction.SetAllEntity ||
                    action==DobAction.InjectionDelete ||
                    action==DobAction.InjectionInitialSet ||
                    action==DobAction.InjectionSetChanges ||
                    action==DobAction.DeleteEntity ||
                    action==DobAction.CreateRequest ||
                    action==DobAction.UpdateRequest ||
                    action==DobAction.DeleteRequest;
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
                return ScenarioTools.DobActionToString(action)+"++\tdelay "+delay;
            else
                return ScenarioTools.DobActionToString(action)+"\tdelay "+delay;
        }

        //Deserialize object here before replay, better performance
        //[XmlIgnore]
        public ObjectInfo objInfo = null;
        //public Safir.Dob.Typesystem.Object obj = null;
        //[XmlIgnore]
        //public int numberOfInstances = 0;
    }

    //****************************************************************
    // Help classes used during record/replay of scenarios.
    // Not serialized to XML-files.
    //****************************************************************
    public class RepeatStartMarker
    {
        public int repetitions;

        public RepeatStartMarker(int reps)
        {
            repetitions=reps;
        }

        public override string ToString()
        {
            if (repetitions<0)
            {
                return "-------- Repeat forever --------";
            }
            else
            {
                return "-------- Repeat "+repetitions+" times --------";
            }
        }
    }

    public class RepeatStopMarker
    {
        public RepeatStopMarker() {}

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

        public static Scenarios.Action ReadFile(string file)
        {
            try
            {
                Scenarios.Action action;
                XmlSerializer serializer = new XmlSerializer(typeof(Scenarios.Action));
                using (FileStream fs = new FileStream(file, FileMode.Open))
                {
                    action = (Scenarios.Action) serializer.Deserialize(fs);
                    fs.Close();
                }
                return action;
            }
            catch
            {
                return null;
            }

        }

        public static void WriteFile(string file, Scenarios.Action scn)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(Scenarios.Action));
            using (TextWriter writer = new StreamWriter(file))
            {
                serializer.Serialize(writer, scn);
                writer.Close();
            }
        }

        public class ScenarioIterator
        {
            private class ItHelp
            {
                public Action action = null;
                public int seqIx = 0;
                public int rep = 0;
                public ItHelp(Action a)
                {
                    action = a;
                }
            }

            private System.Collections.Generic.Stack<ItHelp> it = new System.Collections.Generic.Stack<ItHelp>();

            public ScenarioIterator(Action scenario)
            {
                it.Push(new ItHelp(scenario));
            }

            public bool HasNext()
            {
                return it.Count > 0;
            }

            public Action Next()
            {
                ItHelp ih=it.Peek();
                Action next = ih.action.sequence[ih.seqIx];
                ih.seqIx++;
                if (ih.seqIx>=ih.action.sequence.Length && ih.action.repetitions>0)
                    ih.rep++;

                if (next.action == DobAction.Sequence)
                {
                    it.Push(new ItHelp(next));
                    return Next();
                }

                CleanUp();
                return next;
            }

            private void CleanUp()
            {
                if (HasNext())
                {
                    bool lastRepeat = true;
                    ItHelp ih=it.Peek();
                    if (ih.rep!=ih.action.repetitions) //more iterations in loop
                    {
                        lastRepeat = false;
                    }

                    if (ih.seqIx >= ih.action.sequence.Length)
                    {
                        if (lastRepeat)
                        {
                            it.Pop();
                            CleanUp();
                        }
                        else
                        {
                            ih.seqIx = 0;
                        }
                    }
                }
            }
        }
    }
}

