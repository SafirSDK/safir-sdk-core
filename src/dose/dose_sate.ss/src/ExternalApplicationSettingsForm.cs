using System;
using System.Windows.Forms;

namespace Sate
{
    public partial class ExternalApplicationSettingsForm : Form
    {
        public ExternalApplicationSettingsForm()
        {
            InitializeComponent();
        }

        public new void ShowDialog()
        {
            foreach (var app in ExternalApplicationSettings.Settings.Applications)
            {
                externalApplicationsGrid.Rows.Add(app.Description, app.Name, app.Arguments);
            }
        
            base.ShowDialog();
        }
        
        private void okButton_Click(object sender, EventArgs e)
        {
            bool saved = SaveSettings();
            if (!saved)
            {
                MessageBox.Show(
                    @"Failed to save external application settings. Do you have permission to write to " + ExternalApplicationSettings.GetFilename() + @"?");
            }

            DialogResult = DialogResult.OK;
        }

        private bool SaveSettings()
        {
            try
            {
                ExternalApplicationSettings.Settings.Applications.Clear();
                foreach (DataGridViewRow row in externalApplicationsGrid.Rows)
                {
                    if (!string.IsNullOrWhiteSpace((string) row.Cells[0].Value))
                    {
                        ExternalApplicationSettings.Settings.Applications.Add(new Application
                        {
                            Description = (string) row.Cells[0].Value,
                            Name = (string) row.Cells[1].Value,
                            Arguments = (string) row.Cells[2].Value
                        });
                    }
                }
                ExternalApplicationSettings.Save();
            }
            catch (Exception)
            {
                return false;
            }
            return true;
        }
        
    }
}
