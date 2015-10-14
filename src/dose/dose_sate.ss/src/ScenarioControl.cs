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

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Collections;

namespace Sate
{
    public partial class ScenarioControl : UserControl
    {
        public enum Status { Stopped, Paused, Playing, Recording };
        private Status status = Status.Stopped;
        
        private bool unsavedScenario = false;
        private Sate.Scenarios.Action scenario = null;
        private DelayForm delayForm = new DelayForm();

        private Timer playTimer = new Timer();
        private Scenarios.ScenarioTools.ScenarioIterator playIterator = null;

        public ScenarioControl()
        {
            InitializeComponent();
            playTimer.Tick += new EventHandler(playTimer_Tick);
        }

        void playTimer_Tick(object sender, EventArgs e)
        {
            PlaybackAction();
        }       

        public Status ScenarioStatus
        {
            get { return status; }
        }

        public bool ExistUnsavedScenario
        {
            get { return unsavedScenario; }
        }
/*
        public void Record(Scenarios.DobAction action, Safir.Dob.Typesystem.Object obj)
        {
            if (status != Status.Recording)
                return;

            Scenarios.Action a = new Sate.Scenarios.Action(action, delayForm.Delay, obj);
            this.reclistBox.Items.Add(a);
        }
        */
        public void Record(Scenarios.DobAction action, ObjectInfo objInfo)
        {
            if (status != Status.Recording)
                return;

            Scenarios.Action a = new Sate.Scenarios.Action(action, delayForm.Delay, objInfo);
            this.reclistBox.Items.Add(a);
        }
 
        public void Record(Scenarios.DobAction action, ScenarioInfo scenarioInfo)
        {
            if (status != Status.Recording)
                return;

            Scenarios.Action a = new Sate.Scenarios.Action(action, delayForm.Delay, scenarioInfo);
            this.reclistBox.Items.Add(a);
        }

        public void Record(Scenarios.Action action)
        {
            if (status != Status.Recording)
                return;

            action.delay = delayForm.Delay;
            this.reclistBox.Items.Add(action);
        }
        
        private void delaylinkLabel_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            if (delayForm.ShowDialog() == DialogResult.OK)
            {
                this.delaylinkLabel.Text = delayForm.Delay.ToString();
            }
        }

        private void playbutton_Click(object sender, EventArgs e)
        {
            if (scenario == null)
            {
                return;
            } 
                     
            OutputPanel.Instance.LogEvent("- Playback of scenario started", true);
            this.playbutton.Enabled = false;
            this.recbutton.Enabled = false;
            this.statuslabel.Text = "Playing...";
            this.status = Status.Playing;

            if (scenario.action == Scenarios.DobAction.Sequence)
            {
                playTimer.Interval = 10000;
                playTimer.Enabled = true;
                playIterator = new Sate.Scenarios.ScenarioTools.ScenarioIterator(scenario);
                PlaybackAction();
            }            
        }

        private void recbutton_Click(object sender, EventArgs e)
        {
            OutputPanel.Instance.LogEvent("- Recording of scenario started", true);
            this.playbutton.Enabled = false;
            this.recbutton.Enabled = false;
            this.statuslabel.Text = "Recording...";
            status = Status.Recording;
            this.reclistBox.Items.Clear();
            unsavedScenario = true;
        }

        private void BuildScenario()
        {
            ArrayList scn = new ArrayList();
            int i;
            for (i = 0; i < this.reclistBox.Items.Count; i++)
            {
                if (reclistBox.Items[i] is Scenarios.RepeatStartMarker)
                {
                    Scenarios.RepeatStartMarker startMark =
                        (Scenarios.RepeatStartMarker)reclistBox.Items[i];
                    scn.Add(CreateRepeat(startMark.repetitions, ref i));
                }
                else
                {
                    scn.Add(reclistBox.Items[i]);
                }
            }

            scenario = new Sate.Scenarios.Action(scn, 1);
        }

        private Scenarios.Action CreateRepeat(int reps, ref int ix)
        {
            ix++;
            ArrayList r = new ArrayList();
            while (!(reclistBox.Items[ix] is Scenarios.RepeatStopMarker))
            {
                if (reclistBox.Items[ix] is Scenarios.RepeatStartMarker)
                {
                    Scenarios.RepeatStartMarker startMark =
                        (Scenarios.RepeatStartMarker)reclistBox.Items[ix];
                    r.Add(CreateRepeat(startMark.repetitions, ref ix));
                    ix++;
                }
                else
                {
                    r.Add(reclistBox.Items[ix++]);
                }
            }            
            return new Scenarios.Action(r, reps);
        }

        public void LoadScenario(string path)
        {            
            this.Cursor = Cursors.WaitCursor;

            try
            {

                Scenarios.Action tmp = Scenarios.ScenarioTools.ReadFile(path);
                if (tmp == null)
                {
                    this.Cursor = Cursors.Default;
                    MessageBox.Show("Failed to load scenario from file " + path,
                        "Load failed",
                        MessageBoxButtons.OK,
                        MessageBoxIcon.Error);
                }
                else
                {
                    reclistBox.Items.Clear();                    
                    this.scenario = tmp;
                    this.Text = "Scenario (" + path + ")";
                    this.playbutton.Enabled = true;
                    HandleLoadedScenario(scenario);
                }
            }
            catch
            {
                MessageBox.Show("Failed to load scenario from file " + path,
                        "Load failed",
                        MessageBoxButtons.OK,
                        MessageBoxIcon.Error);
            }
            this.Cursor = Cursors.Default;
        }

        public void SaveScenario(string path)
        {
            this.Cursor = Cursors.WaitCursor;            
            Scenarios.ScenarioTools.WriteFile(path, scenario);
            this.Text = "Scenario (" + path + ")";            
            this.Cursor = Cursors.Default;
        }

        private void HandleLoadedScenario(Scenarios.Action a)
        {
            bool isLoop = a.repetitions > 1 || a.repetitions < 0;

            if (isLoop) //Start of loop
            {
                Scenarios.RepeatStartMarker startMark = new Sate.Scenarios.RepeatStartMarker(a.repetitions);
                this.reclistBox.Items.Add(startMark);
            }

            if (a.action == Scenarios.DobAction.Sequence)
            {
                for (int i = 0; i < a.sequence.Length; i++)
                {
                    HandleLoadedScenario(a.sequence[i]);
                }
            }
            else
            {
                this.reclistBox.Items.Add(a);
            }

            if (isLoop) //End of loop
            {
                Scenarios.RepeatStopMarker stopMark = new Sate.Scenarios.RepeatStopMarker();
                this.reclistBox.Items.Add(stopMark);
            }


            if (a.xmlObject != null)
            {
                try
                {
                    a.objInfo.Obj = Safir.Dob.Typesystem.Serialization.ToObject(a.xmlObject);
                    //long typeId = a.objInfo.Obj.GetTypeId();
                    //string className = Safir.Dob.Typesystem.Operations.GetName(typeId);
                    //System.Type dobType = MainForm.Instance.DotsGenerated.GetType(className);
                    //System.Reflection.ConstructorInfo constr =
                    //    dobType.GetConstructor(new System.Type[] { typeof(Safir.Dob.Typesystem.Object) });
                    //System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                    //a.obj = (Safir.Dob.Typesystem.Object)constr.Invoke(new object[] { a.obj });
              //      a.numberOfInstances = Safir.Dob.Typesystem.Operations.GetMaxNumberOfInstances(typeId);
                }
                catch (Safir.Dob.Typesystem.IllegalValueException e)
                {
                    MessageBox.Show("Failed to load serialized object:\n\n" + a.xmlObject, "Serialization Error: "+e.Message);
                }                
            }
        }
        
        //-------------------------------------------------------
        // Playback, running in a seperate thread
        //-------------------------------------------------------         
        private void PlaybackAction()
        {
            if (playIterator == null)
                return;
            if (!playIterator.HasNext())
            {                
                playIterator = null;
                stopbutton_Click(null, null);
                return;
            }

            Scenarios.Action a = playIterator.Next();            
           
            switch (a.action)
            {
                case Scenarios.DobAction.Connect:
                    PlaybackConnect(a);
                    break;
                case Scenarios.DobAction.Disconnect:
                    PlaybackDisconnect(a);
                    break;
                case Scenarios.DobAction.Register:
                    PlaybackRegister(a);
                    break;
                case Scenarios.DobAction.Unregister:
                    PlaybackUnregister(a);
                    break;
                case Scenarios.DobAction.Subscribe:
                    PlaybackSubscribe(a);
                    break;
                case Scenarios.DobAction.Unsubscribe:
                    PlaybackUnsubscribe(a);
                    break;
                case Sate.Scenarios.DobAction.SubscribeRegistration:
                    PlaybackSubscribeRegistration(a);
                    break;
                case Sate.Scenarios.DobAction.UnsubscribeRegistration:
                    PlaybackUnsubscribeRegistration(a);
                    break;
                case Scenarios.DobAction.SetChangesEntity:
                    PlaybackSetChangesEntity(a);
                    break;
                case Scenarios.DobAction.SetAllEntity:
                    PlaybackSetAllEntity(a);
                    break;
                case Scenarios.DobAction.DeleteEntity:
                    PlaybackDeleteEntity(a);
                    break;
                case Scenarios.DobAction.CreateRequest:
                    PlaybackCreateRequest(a);
                    break;
                case Scenarios.DobAction.UpdateRequest:
                    PlaybackUpdateRequest(a);
                    break;
                case Scenarios.DobAction.DeleteRequest:
                    PlaybackDeleteRequest(a);
                    break;
                case Scenarios.DobAction.ServiceRequest:
                    PlaybackServiceRequest(a);
                    break;
                case Scenarios.DobAction.SendMessage:
                    PlaybackSendMessage(a);
                    break;                    
            }                
                
            if (a.instStepUp)
            {
                a.StepUpInstance();
            }

            playTimer.Interval = a.delay;
                
        }               

        private void PlaybackConnect(Scenarios.Action a)
        {            
            MainForm.Instance.Connect(false,0);         
        }

        private void PlaybackDisconnect(Scenarios.Action a)
        {
            MainForm.Instance.Disconnect();
        }

        private void PlaybackRegister(Scenarios.Action a)
        {
            if (Safir.Dob.Typesystem.Operations.IsOfType(((RegInfo)a.scenarioInfo).typeId, Safir.Dob.Entity.ClassTypeId))
            {
                ExplorerPanel.Instance.RegisterEntity((RegInfo)a.scenarioInfo);
            }
            else
            {
                ExplorerPanel.Instance.RegisterService((RegInfo)a.scenarioInfo);
            }
         }

        private void PlaybackUnregister(Scenarios.Action a)
        {
            ExplorerPanel.Instance.Unregister((RegInfo)a.scenarioInfo);
        }

        private void PlaybackSubscribe(Scenarios.Action a)
        {
            SubInfo subInfo = (SubInfo)a.scenarioInfo;
            if (Safir.Dob.Typesystem.Operations.IsOfType(subInfo.typeId, Safir.Dob.Entity.ClassTypeId))
            {
                ExplorerPanel.Instance.SubscribeEntity(subInfo);
            }
            else
            {
                ExplorerPanel.Instance.SubscribeMessage(subInfo);
            }
        }

        private void PlaybackUnsubscribe(Scenarios.Action a)
        {
            SubInfo subInfo = (SubInfo)a.scenarioInfo;
            if (Safir.Dob.Typesystem.Operations.IsOfType(subInfo.typeId, Safir.Dob.Entity.ClassTypeId))
            {
                ExplorerPanel.Instance.UnsubscribeEntity(subInfo);
            }
            else
            {
                ExplorerPanel.Instance.UnsubscribeMessage(subInfo);
            }
        }

        private void PlaybackSubscribeRegistration(Scenarios.Action a)
        {
                ExplorerPanel.Instance.SubscribeRegistration((SubRegInfo)a.scenarioInfo);
        }

        private void PlaybackUnsubscribeRegistration(Scenarios.Action a)
        {
            ExplorerPanel.Instance.UnsubscribeRegistration((SubRegInfo)a.scenarioInfo);
        }

        private void PlaybackSetChangesEntity(Scenarios.Action a)
        {
            MainForm.Instance.SetChangesEntity((EntityInfo)a.objInfo);
        }

        private void PlaybackSetAllEntity(Scenarios.Action a)
        {
            MainForm.Instance.SetAllEntity((EntityInfo)a.objInfo);
        }

        private void PlaybackDeleteEntity(Scenarios.Action a)
        {
            MainForm.Instance.DeleteEntity((EntityInfo)a.objInfo);
        }

        private void PlaybackCreateRequest(Scenarios.Action a)
        {
            MainForm.Instance.CreateRequest((EntityInfo)a.objInfo);
        }

        private void PlaybackUpdateRequest(Scenarios.Action a)
        {
            MainForm.Instance.UpdateRequest((EntityInfo)a.objInfo);
        }

        private void PlaybackDeleteRequest(Scenarios.Action a)
        {
            MainForm.Instance.DeleteRequest((EntityIdInfo)a.scenarioInfo);
        }

        private void PlaybackServiceRequest(Scenarios.Action a)
        {
            MainForm.Instance.ServiceRequest((ServiceHandlerInfo)a.objInfo);
        }

        private void PlaybackSendMessage(Scenarios.Action a)
        {
            MainForm.Instance.SendMessage((MessageInfo)a.objInfo);
        }

        private void stopbutton_Click(object sender, System.EventArgs e)
        {
            if (status == Status.Recording)
            {
                OutputPanel.Instance.LogEvent("- Recording stopped", true);
                this.Text = "Scenario (unsaved recording)";
                BuildScenario();
                this.playbutton.Enabled = true;
            }
            else if (status == Status.Playing)
            {
                playTimer.Enabled = false;
                OutputPanel.Instance.LogEvent("- Playback stopped", true);
                this.playbutton.Enabled = true;
            }


            status = Status.Stopped;            
            this.recbutton.Enabled = true;
            this.statuslabel.Text = "Stopped";
        }

        //-----------------------------------------------------
        // Context menu for action listbox
        //----------------------------------------------------- 
        private Scenarios.Action GetAction(int index)
        {
            return (Scenarios.Action)this.reclistBox.Items[index];
        }

        private void deletemenuItem_Click(object sender, System.EventArgs e)
        {
            MessageBox.Show("Not implemented");
        }

        private void delaymenuItem_Click(object sender, System.EventArgs e)
        {
            DelayForm df = new DelayForm();
            if (df.ShowDialog() == DialogResult.OK)
            {
                foreach (int i in this.reclistBox.SelectedIndices)
                {
                    Scenarios.Action tmp = GetAction(i);
                    tmp.delay = df.Delay;
                    this.reclistBox.Items[i] = tmp;
                }
            }
            this.reclistBox.Invalidate();
        }

        private void incrInstmenuItem_Click(object sender, System.EventArgs e)
        {
            foreach (int i in this.reclistBox.SelectedIndices)
            {
                Scenarios.Action action = (Scenarios.Action)reclistBox.Items[i];
                if (action.HasInstanceNumber())
                {
                    action.instStepUp = !action.instStepUp;
                    this.reclistBox.Items[i] = action;
                }
            }
            this.reclistBox.Invalidate();
        }

        private void repeatmenuItem_Click(object sender, System.EventArgs e)
        {
            if (this.reclistBox.SelectedIndices.Count > 0)
            {
                RepeatForm rf = new RepeatForm(10);
                rf.ShowDialog();
                int start = this.reclistBox.SelectedIndices[0];
                int stop = this.reclistBox.SelectedIndices[this.reclistBox.SelectedIndices.Count - 1];

                Scenarios.RepeatStartMarker startMark = new Sate.Scenarios.RepeatStartMarker(rf.Repetitions);
                Scenarios.RepeatStopMarker stopMark = new Sate.Scenarios.RepeatStopMarker();

                this.reclistBox.Items.Insert(start, startMark);
                this.reclistBox.Items.Insert(stop + 2, stopMark);

                BuildScenario();
            }
        }

    }
}
