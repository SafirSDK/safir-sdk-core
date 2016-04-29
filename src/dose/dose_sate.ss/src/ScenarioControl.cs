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
using System.Collections;
using System.Windows.Forms;
using Safir.Dob;
using Safir.Dob.Typesystem;

namespace Sate
{
    public partial class ScenarioControl : UserControl
    {
        public enum Status
        {
            Stopped,
            Paused,
            Playing,
            Recording
        }

        private readonly DelayForm delayForm = new DelayForm();

        private readonly Timer playTimer = new Timer();
        private ScenarioTools.ScenarioIterator playIterator;
        private Action scenario;
        private Status _scenarioStatus = Status.Stopped;

        public ScenarioControl()
        {
            InitializeComponent();
            playTimer.Tick += playTimer_Tick;
        }

        public Status ScenarioStatus
        {
            get { return _scenarioStatus; }
            private set { _scenarioStatus = value; }
        }

        public bool ExistUnsavedScenario { get; private set; }

        private void playTimer_Tick(object sender, EventArgs e)
        {
            PlaybackAction();
        }


        public void Record(DobAction action, ObjectInfo objInfo)
        {
            if (ScenarioStatus != Status.Recording)
                return;

            var a = new Action(action, delayForm.Delay, objInfo);
            reclistBox.Items.Add(a);
        }

        public void Record(DobAction action, ScenarioInfo scenarioInfo)
        {
            if (ScenarioStatus != Status.Recording)
                return;

            var a = new Action(action, delayForm.Delay, scenarioInfo);
            reclistBox.Items.Add(a);
        }

        public void Record(Action action)
        {
            if (ScenarioStatus != Status.Recording)
                return;

            action.delay = delayForm.Delay;
            reclistBox.Items.Add(action);
        }

        private void delaylinkLabel_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            if (delayForm.ShowDialog() == DialogResult.OK)
            {
                delaylinkLabel.Text = delayForm.Delay.ToString();
            }
        }

        private void playbutton_Click(object sender, EventArgs e)
        {
            if (scenario == null)
            {
                return;
            }

            OutputPanel.Instance.LogEvent("- Playback of scenario started", true);
            playbutton.Enabled = false;
            recbutton.Enabled = false;
            statuslabel.Text = "Playing...";
            ScenarioStatus = Status.Playing;

            if (scenario.action == DobAction.Sequence)
            {
                playTimer.Interval = 10000;
                playTimer.Enabled = true;
                playIterator = new ScenarioTools.ScenarioIterator(scenario);
                PlaybackAction();
            }
        }

        private void recbutton_Click(object sender, EventArgs e)
        {
            OutputPanel.Instance.LogEvent("- Recording of scenario started", true);
            playbutton.Enabled = false;
            recbutton.Enabled = false;
            statuslabel.Text = "Recording...";
            ScenarioStatus = Status.Recording;
            reclistBox.Items.Clear();
            ExistUnsavedScenario = true;
        }

        private void BuildScenario()
        {
            var scn = new ArrayList();
            int i;
            for (i = 0; i < reclistBox.Items.Count; i++)
            {
                if (reclistBox.Items[i] is RepeatStartMarker)
                {
                    var startMark =
                        (RepeatStartMarker) reclistBox.Items[i];
                    scn.Add(CreateRepeat(startMark.Repetitions, ref i));
                }
                else
                {
                    scn.Add(reclistBox.Items[i]);
                }
            }

            scenario = new Action(scn, 1);
        }

        private Action CreateRepeat(int reps, ref int ix)
        {
            ix++;
            var r = new ArrayList();
            while (!(reclistBox.Items[ix] is RepeatStopMarker))
            {
                if (reclistBox.Items[ix] is RepeatStartMarker)
                {
                    var startMark =
                        (RepeatStartMarker) reclistBox.Items[ix];
                    r.Add(CreateRepeat(startMark.Repetitions, ref ix));
                    ix++;
                }
                else
                {
                    r.Add(reclistBox.Items[ix++]);
                }
            }
            return new Action(r, reps);
        }

        public void LoadScenario(string path)
        {
            Cursor = Cursors.WaitCursor;

            try
            {
                var tmp = ScenarioTools.ReadFile(path);
                if (tmp == null)
                {
                    Cursor = Cursors.Default;
                    MessageBox.Show("Failed to load scenario from file " + path,
                        "Load failed",
                        MessageBoxButtons.OK,
                        MessageBoxIcon.Error);
                }
                else
                {
                    reclistBox.Items.Clear();
                    scenario = tmp;
                    Text = "Scenario (" + path + ")";
                    playbutton.Enabled = true;
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
            Cursor = Cursors.Default;
        }

        public void SaveScenario(string path)
        {
            Cursor = Cursors.WaitCursor;
            ScenarioTools.WriteFile(path, scenario);
            Text = "Scenario (" + path + ")";
            Cursor = Cursors.Default;
        }

        private void HandleLoadedScenario(Action a)
        {
            var isLoop = a.repetitions > 1 || a.repetitions < 0;

            if (isLoop) //Start of loop
            {
                var startMark = new RepeatStartMarker(a.repetitions);
                reclistBox.Items.Add(startMark);
            }

            if (a.action == DobAction.Sequence)
            {
                for (var i = 0; i < a.sequence.Length; i++)
                {
                    HandleLoadedScenario(a.sequence[i]);
                }
            }
            else
            {
                reclistBox.Items.Add(a);
            }

            if (isLoop) //End of loop
            {
                var stopMark = new RepeatStopMarker();
                reclistBox.Items.Add(stopMark);
            }


            if (a.xmlObject != null)
            {
                try
                {
                    a.objInfo.Obj = Serialization.ToObject(a.xmlObject);
                    //long typeId = a.objInfo.Obj.GetTypeId();
                    //string className = Safir.Dob.Typesystem.Operations.GetName(typeId);
                    //System.Type dobType = MainForm.Instance.DotsGenerated.GetType(className);
                    //System.Reflection.ConstructorInfo constr =
                    //    dobType.GetConstructor(new System.Type[] { typeof(Safir.Dob.Typesystem.Object) });
                    //System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                    //a.obj = (Safir.Dob.Typesystem.Object)constr.Invoke(new object[] { a.obj });
                    //      a.numberOfInstances = Safir.Dob.Typesystem.Operations.GetMaxNumberOfInstances(typeId);
                }
                catch (IllegalValueException e)
                {
                    MessageBox.Show("Failed to load serialized object:\n\n" + a.xmlObject,
                        "Serialization Error: " + e.Message);
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

            var a = playIterator.Next();

            switch (a.action)
            {
                case DobAction.Connect:
                    PlaybackConnect(a);
                    break;
                case DobAction.Disconnect:
                    PlaybackDisconnect(a);
                    break;
                case DobAction.Register:
                    PlaybackRegister(a);
                    break;
                case DobAction.Unregister:
                    PlaybackUnregister(a);
                    break;
                case DobAction.Subscribe:
                    PlaybackSubscribe(a);
                    break;
                case DobAction.Unsubscribe:
                    PlaybackUnsubscribe(a);
                    break;
                case DobAction.SubscribeRegistration:
                    PlaybackSubscribeRegistration(a);
                    break;
                case DobAction.UnsubscribeRegistration:
                    PlaybackUnsubscribeRegistration(a);
                    break;
                case DobAction.SetChangesEntity:
                    PlaybackSetChangesEntity(a);
                    break;
                case DobAction.SetAllEntity:
                    PlaybackSetAllEntity(a);
                    break;
                case DobAction.DeleteEntity:
                    PlaybackDeleteEntity(a);
                    break;
                case DobAction.CreateRequest:
                    PlaybackCreateRequest(a);
                    break;
                case DobAction.UpdateRequest:
                    PlaybackUpdateRequest(a);
                    break;
                case DobAction.DeleteRequest:
                    PlaybackDeleteRequest(a);
                    break;
                case DobAction.ServiceRequest:
                    PlaybackServiceRequest(a);
                    break;
                case DobAction.SendMessage:
                    PlaybackSendMessage(a);
                    break;
            }

            if (a.instStepUp)
            {
                a.StepUpInstance();
            }

            playTimer.Interval = a.delay;
        }

        private void PlaybackConnect(Action a)
        {
            MainForm.Instance.Connect(false, 0);
        }

        private void PlaybackDisconnect(Action a)
        {
            MainForm.Instance.Disconnect();
        }

        private void PlaybackRegister(Action a)
        {
            if (Operations.IsOfType(((RegInfo) a.scenarioInfo).typeId, Entity.ClassTypeId))
            {
                ExplorerPanel.Instance.RegisterEntity((RegInfo) a.scenarioInfo);
            }
            else
            {
                ExplorerPanel.Instance.RegisterService((RegInfo) a.scenarioInfo);
            }
        }

        private void PlaybackUnregister(Action a)
        {
            ExplorerPanel.Instance.Unregister((RegInfo) a.scenarioInfo);
        }

        private void PlaybackSubscribe(Action a)
        {
            var subInfo = (SubInfo) a.scenarioInfo;
            if (Operations.IsOfType(subInfo.typeId, Entity.ClassTypeId))
            {
                ExplorerPanel.Instance.SubscribeEntity(subInfo);
            }
            else
            {
                ExplorerPanel.Instance.SubscribeMessage(subInfo);
            }
        }

        private void PlaybackUnsubscribe(Action a)
        {
            var subInfo = (SubInfo) a.scenarioInfo;
            if (Operations.IsOfType(subInfo.typeId, Entity.ClassTypeId))
            {
                ExplorerPanel.Instance.UnsubscribeEntity(subInfo);
            }
            else
            {
                ExplorerPanel.Instance.UnsubscribeMessage(subInfo);
            }
        }

        private void PlaybackSubscribeRegistration(Action a)
        {
            ExplorerPanel.Instance.SubscribeRegistration((SubRegInfo) a.scenarioInfo);
        }

        private void PlaybackUnsubscribeRegistration(Action a)
        {
            ExplorerPanel.Instance.UnsubscribeRegistration((SubRegInfo) a.scenarioInfo);
        }

        private void PlaybackSetChangesEntity(Action a)
        {
            MainForm.Instance.SetChangesEntity((EntityInfo) a.objInfo);
        }

        private void PlaybackSetAllEntity(Action a)
        {
            MainForm.Instance.SetAllEntity((EntityInfo) a.objInfo);
        }

        private void PlaybackDeleteEntity(Action a)
        {
            MainForm.Instance.DeleteEntity((EntityInfo) a.objInfo);
        }

        private void PlaybackCreateRequest(Action a)
        {
            MainForm.Instance.CreateRequest((EntityInfo) a.objInfo);
        }

        private void PlaybackUpdateRequest(Action a)
        {
            MainForm.Instance.UpdateRequest((EntityInfo) a.objInfo);
        }

        private void PlaybackDeleteRequest(Action a)
        {
            MainForm.Instance.DeleteRequest((EntityIdInfo) a.scenarioInfo);
        }

        private void PlaybackServiceRequest(Action a)
        {
            MainForm.Instance.ServiceRequest((ServiceHandlerInfo) a.objInfo);
        }

        private void PlaybackSendMessage(Action a)
        {
            MainForm.Instance.SendMessage((MessageInfo) a.objInfo);
        }

        private void stopbutton_Click(object sender, EventArgs e)
        {
            if (ScenarioStatus == Status.Recording)
            {
                OutputPanel.Instance.LogEvent("- Recording stopped", true);
                Text = "Scenario (unsaved recording)";
                BuildScenario();
                playbutton.Enabled = true;
            }
            else if (ScenarioStatus == Status.Playing)
            {
                playTimer.Enabled = false;
                OutputPanel.Instance.LogEvent("- Playback stopped", true);
                playbutton.Enabled = true;
            }


            ScenarioStatus = Status.Stopped;
            recbutton.Enabled = true;
            statuslabel.Text = "Stopped";
        }

        //-----------------------------------------------------
        // Context menu for action listbox
        //----------------------------------------------------- 
        private Action GetAction(int index)
        {
            return (Action) reclistBox.Items[index];
        }

        private void deletemenuItem_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Not implemented");
        }

        private void delaymenuItem_Click(object sender, EventArgs e)
        {
            var df = new DelayForm();
            if (df.ShowDialog() == DialogResult.OK)
            {
                foreach (int i in reclistBox.SelectedIndices)
                {
                    var tmp = GetAction(i);
                    tmp.delay = df.Delay;
                    reclistBox.Items[i] = tmp;
                }
            }
            reclistBox.Invalidate();
        }

        private void incrInstmenuItem_Click(object sender, EventArgs e)
        {
            foreach (int i in reclistBox.SelectedIndices)
            {
                var action = (Action) reclistBox.Items[i];
                if (action.HasInstanceNumber())
                {
                    action.instStepUp = !action.instStepUp;
                    reclistBox.Items[i] = action;
                }
            }
            reclistBox.Invalidate();
        }

        private void repeatmenuItem_Click(object sender, EventArgs e)
        {
            if (reclistBox.SelectedIndices.Count > 0)
            {
                var rf = new RepeatForm(10);
                rf.ShowDialog();
                var start = reclistBox.SelectedIndices[0];
                var stop = reclistBox.SelectedIndices[reclistBox.SelectedIndices.Count - 1];

                var startMark = new RepeatStartMarker(rf.Repetitions);
                var stopMark = new RepeatStopMarker();

                reclistBox.Items.Insert(start, startMark);
                reclistBox.Items.Insert(stop + 2, stopMark);

                BuildScenario();
            }
        }
    }
}